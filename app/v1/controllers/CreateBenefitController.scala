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
import config.{AppConfig, FeatureSwitches}
import play.api.libs.json.JsValue
import play.api.mvc.{Action, AnyContentAsJson, ControllerComponents}
import utils.IdGenerator
import v1.controllers.requestParsers.CreateBenefitRequestParser
import v1.models.request.createBenefit.CreateBenefitRawData
import v1.models.response.createBenefit.AddBenefitHateoasData
import v1.services.CreateBenefitService

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class CreateBenefitController @Inject() (val authService: EnrolmentsAuthService,
                                         val lookupService: MtdIdLookupService,
                                         appConfig: AppConfig,
                                         parser: CreateBenefitRequestParser,
                                         service: CreateBenefitService,
                                         auditService: AuditService,
                                         hateoasFactory: HateoasFactory,
                                         cc: ControllerComponents,
                                         idGenerator: IdGenerator)(implicit ec: ExecutionContext)
    extends AuthorisedController(cc) {

  implicit val endpointLogContext: EndpointLogContext =
    EndpointLogContext(
      controllerName = "CreateStateBenefitController",
      endpointName = "CreateStateBenefit"
    )

  def createStateBenefit(nino: String, taxYear: String): Action[JsValue] =
    authorisedAction(nino).async(parse.json) { implicit request =>
      implicit val ctx: RequestContext = RequestContext.from(idGenerator, endpointLogContext)

      val rawData: CreateBenefitRawData = CreateBenefitRawData(
        nino = nino,
        taxYear = taxYear,
        body = AnyContentAsJson(request.body),
        temporalValidationEnabled = FeatureSwitches()(appConfig).isTemporalValidationEnabled
      )

      val requestHandler = RequestHandler
        .withParser(parser)
        .withService(service.addBenefit)
        .withAuditing(AuditHandler(
          auditService = auditService,
          auditType = "CreateStateBenefit",
          transactionName = "create-state-benefit",
          pathParams = Map("nino" -> nino, "taxYear" -> taxYear),
          requestBody = Some(request.body),
          includeResponse = true
        ))
        .withHateoasResultFrom(hateoasFactory)((_, response) => AddBenefitHateoasData(nino, taxYear, response.benefitId))

      requestHandler.handleRequest(rawData)
    }

}
