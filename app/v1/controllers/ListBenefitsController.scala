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

package v1.controllers

import cats.data.EitherT
import cats.implicits._
import config.AppConfig
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import play.mvc.Http.MimeTypes
import utils.{IdGenerator, Logging}
import v1.controllers.requestParsers.ListBenefitsRequestParser
import v1.hateoas.HateoasFactory
import v1.models.errors._
import v1.models.request.listBenefits.ListBenefitsRawData
import v1.models.response.listBenefits.ListBenefitsResponse.ListBenefitsLinksFactory
import v1.models.response.listBenefits.{CustomerStateBenefit, HMRCStateBenefit, ListBenefitsHateoasData, ListBenefitsResponse}
import v1.services.{EnrolmentsAuthService, ListBenefitsService, MtdIdLookupService}

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ListBenefitsController @Inject() (val authService: EnrolmentsAuthService,
                                        val lookupService: MtdIdLookupService,
                                        appConfig: AppConfig,
                                        requestParser: ListBenefitsRequestParser,
                                        service: ListBenefitsService,
                                        hateoasFactory: HateoasFactory,
                                        cc: ControllerComponents,
                                        idGenerator: IdGenerator)(implicit ec: ExecutionContext)
    extends AuthorisedController(cc)
    with BaseController
    with Logging {

  implicit val endpointLogContext: EndpointLogContext =
    EndpointLogContext(
      controllerName = "ListBenefitsController",
      endpointName = "ListBenefitsAmounts"
    )

  def listBenefits(nino: String, taxYear: String, benefitId: Option[String]): Action[AnyContent] =
    authorisedAction(nino).async { implicit request =>
      implicit val correlationId: String = idGenerator.getCorrelationId
      logger.info(message = s"[${endpointLogContext.controllerName}][${endpointLogContext.endpointName}] " +
        s"with correlationId : $correlationId")
      val rawData = ListBenefitsRawData(
        nino = nino,
        taxYear = taxYear,
        benefitId = benefitId
      )

      val result =
        for {
          parsedRequest   <- EitherT.fromEither[Future](requestParser.parseRequest(rawData))
          serviceResponse <- EitherT(service.listBenefits(parsedRequest))
          hateoasResponse <- EitherT.fromEither[Future](
            hateoasFactory
              .wrapList(
                serviceResponse.responseData,
                ListBenefitsHateoasData(nino, taxYear, benefitId.isDefined, hmrcBenefitIds(serviceResponse.responseData))
              )
              .asRight[ErrorWrapper])
        } yield {
          logger.info(
            s"[${endpointLogContext.controllerName}][${endpointLogContext.endpointName}] - " +
              s"Success response received with CorrelationId: ${serviceResponse.correlationId}")

          Ok(Json.toJson(hateoasResponse))
            .withApiHeaders(serviceResponse.correlationId)
            .as(MimeTypes.JSON)
        }

      result.leftMap { errorWrapper =>
        val resCorrelationId = errorWrapper.correlationId
        val result           = errorResult(errorWrapper).withApiHeaders(resCorrelationId)
        logger.warn(
          s"[${endpointLogContext.controllerName}][${endpointLogContext.endpointName}] - " +
            s"Error response received with CorrelationId: $resCorrelationId")

        result
      }.merge
    }

  private def errorResult(errorWrapper: ErrorWrapper) = {
    errorWrapper.error match {
      case _
          if errorWrapper.containsAnyOf(
            BadRequestError,
            NinoFormatError,
            TaxYearFormatError,
            BenefitIdFormatError,
            RuleTaxYearNotSupportedError,
            RuleTaxYearRangeInvalidError,
            RuleIncorrectOrEmptyBodyError
          ) =>
        BadRequest(Json.toJson(errorWrapper))

      case NotFoundError   => NotFound(Json.toJson(errorWrapper))
      case DownstreamError => InternalServerError(Json.toJson(errorWrapper))
    }
  }

  private def hmrcBenefitIds(response: ListBenefitsResponse[HMRCStateBenefit, CustomerStateBenefit]): Seq[String] =
    response.stateBenefits.getOrElse(Nil).map(_.benefitId)

}
