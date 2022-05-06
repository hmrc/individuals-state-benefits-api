/*
 * Copyright 2022 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package v1r6.controllers

import cats.data.EitherT
import cats.implicits._
import config.AppConfig
import javax.inject.{Inject, Singleton}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Action, AnyContentAsJson, ControllerComponents}
import play.mvc.Http.MimeTypes
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditResult
import utils.{IdGenerator, Logging}
import v1r6.controllers.requestParsers.AmendBenefitRequestParser
import v1r6.hateoas.AmendHateoasBodies
import v1r6.models.audit.{AuditEvent, AuditResponse, GenericAuditDetail}
import v1r6.models.errors._
import v1r6.models.request.AmendBenefit.AmendBenefitRawData
import v1r6.services.{AmendBenefitService, AuditService, EnrolmentsAuthService, MtdIdLookupService}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AmendBenefitController @Inject() (val authService: EnrolmentsAuthService,
                                        val lookupService: MtdIdLookupService,
                                        appConfig: AppConfig,
                                        requestParser: AmendBenefitRequestParser,
                                        service: AmendBenefitService,
                                        auditService: AuditService,
                                        cc: ControllerComponents,
                                        val idGenerator: IdGenerator)(implicit ec: ExecutionContext)
    extends AuthorisedController(cc)
    with BaseController
    with Logging
    with AmendHateoasBodies {

  implicit val endpointLogContext: EndpointLogContext =
    EndpointLogContext(
      controllerName = "AmendBenefitController",
      endpointName = "amendBenefitAmounts"
    )

  def amend(nino: String, taxYear: String, benefitId: String): Action[JsValue] =
    authorisedAction(nino).async(parse.json) { implicit request =>
      implicit val correlationId: String = idGenerator.getCorrelationId
      logger.info(message = s"[${endpointLogContext.controllerName}][${endpointLogContext.endpointName}] " +
        s"with correlationId : $correlationId")

      val rawData = AmendBenefitRawData(
        nino = nino,
        taxYear = taxYear,
        benefitId = benefitId,
        body = AnyContentAsJson(request.body)
      )

      val result =
        for {
          parsedRequest   <- EitherT.fromEither[Future](requestParser.parseRequest(rawData))
          serviceResponse <- EitherT(service.updateBenefit(parsedRequest))
        } yield {
          logger.info(
            s"[${endpointLogContext.controllerName}][${endpointLogContext.endpointName}] - " +
              s"Success response received with CorrelationId: ${serviceResponse.correlationId}")

          val hateoasResponse = amendBenefitHateoasBody(appConfig, nino, taxYear, benefitId)

          auditSubmission(
            GenericAuditDetail(
              request.userDetails,
              Map("nino" -> nino, "taxYear" -> taxYear, "benefitId" -> benefitId),
              Some(request.body),
              serviceResponse.correlationId,
              AuditResponse(httpStatus = OK, response = Right(Some(hateoasResponse)))
            )
          )
          Ok(hateoasResponse)
            .withApiHeaders(serviceResponse.correlationId)
            .as(MimeTypes.JSON)
        }
      result.leftMap { errorWrapper =>
        val resCorrelationId = errorWrapper.correlationId
        val result           = errorResult(errorWrapper).withApiHeaders(resCorrelationId)
        logger.warn(
          s"[${endpointLogContext.controllerName}][${endpointLogContext.endpointName}] - " +
            s"Error response received with CorrelationId: $resCorrelationId")

        auditSubmission(
          GenericAuditDetail(
            request.userDetails,
            Map("nino" -> nino, "taxYear" -> taxYear, "benefitId" -> benefitId),
            Some(request.body),
            correlationId,
            AuditResponse(httpStatus = result.header.status, response = Left(errorWrapper.auditErrors))
          )
        )
        result
      }.merge
    }

  private def errorResult(errorWrapper: ErrorWrapper) = {
    (errorWrapper.error: @unchecked) match {
      case BadRequestError | NinoFormatError | TaxYearFormatError | BenefitIdFormatError | RuleTaxYearNotSupportedError |
          RuleTaxYearRangeInvalidError | RuleTaxYearNotEndedError | CustomMtdError(RuleIncorrectOrEmptyBodyError.code) | StartDateFormatError |
          EndDateFormatError | RuleEndDateBeforeStartDateError | RuleStartDateAfterTaxYearEndError | RuleEndDateBeforeTaxYearStartError =>
        BadRequest(Json.toJson(errorWrapper))
      case RuleUpdateForbiddenError => Forbidden(Json.toJson(errorWrapper))
      case NotFoundError            => NotFound(Json.toJson(errorWrapper))
      case DownstreamError          => InternalServerError(Json.toJson(errorWrapper))
    }
  }

  private def auditSubmission(details: GenericAuditDetail)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[AuditResult] = {
    val event = AuditEvent("AmendStateBenefit", "amend-state-benefit", details)
    auditService.auditEvent(event)
  }

}
