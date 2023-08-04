/*
 * Copyright 2023 HM Revenue & Customs
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

import api.controllers._
import api.hateoas.HateoasFactory
import api.services.{AuditService, EnrolmentsAuthService, MtdIdLookupService}
import config.AppConfig
import play.api.libs.json.JsValue
import play.api.mvc.{Action, AnyContentAsJson, ControllerComponents}
import routing.Version1
import utils.IdGenerator
import v1.controllers.requestParsers.AmendBenefitRequestParser
import v1.models.request.AmendBenefit.AmendBenefitRawData
import v1.models.response.amendBenefit.AmendBenefitHateoasData
import v1.models.response.amendBenefit.AmendBenefitResponse.AmendBenefitLinksFactory
import v1.services.AmendBenefitService

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class AmendBenefitController @Inject() (val authService: EnrolmentsAuthService,
                                        val lookupService: MtdIdLookupService,
                                        appConfig: AppConfig,
                                        parser: AmendBenefitRequestParser,
                                        service: AmendBenefitService,
                                        auditService: AuditService,
                                        hateoasFactory: HateoasFactory,
                                        cc: ControllerComponents,
                                        val idGenerator: IdGenerator)(implicit ec: ExecutionContext)
    extends AuthorisedController(cc) {

  implicit val endpointLogContext: EndpointLogContext =
    EndpointLogContext(
      controllerName = "AmendBenefitController",
      endpointName = "amendBenefitAmounts"
    )

  def amendBenefit(nino: String, taxYear: String, benefitId: String): Action[JsValue] =
    authorisedAction(nino).async(parse.json) { implicit request =>
      implicit val ctx: RequestContext = RequestContext.from(idGenerator, endpointLogContext)

      val rawData = AmendBenefitRawData(nino, taxYear, benefitId, AnyContentAsJson(request.body))

      val requestHandler = RequestHandler
        .withParser(parser)
        .withService(service.amendBenefit)
        .withAuditing(AuditHandler(
          auditService = auditService,
          auditType = "AmendStateBenefit",
          transactionName = "amend-state-benefit",
          version = Version1,
          pathParams = Map("nino" -> nino, "taxYear" -> taxYear, "benefitId" -> benefitId),
          requestBody = Some(request.body),
          includeResponse = true
        ))
        .withHateoasResult(hateoasFactory)(AmendBenefitHateoasData(nino, taxYear, benefitId))

      requestHandler.handleRequest(rawData)
    }

}
