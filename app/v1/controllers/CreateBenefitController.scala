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
import play.api.libs.json.JsValue
import play.api.mvc.{Action, ControllerComponents}
import routing.{Version, Version1}
import utils.IdGenerator
import v1.controllers.validators.CreateBenefitValidatorFactory
import v1.models.response.createBenefit.CreateBenefitHateoasData
import v1.services.CreateBenefitService

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class CreateBenefitController @Inject() (val authService: EnrolmentsAuthService,
                                         val lookupService: MtdIdLookupService,
                                         validatorFactory: CreateBenefitValidatorFactory,
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

      val validator = validatorFactory.validator(nino, taxYear, request.body)

      val requestHandler = RequestHandler
        .withValidator(validator)
        .withService(service.createBenefit)
        .withAuditing(AuditHandler(
          auditService = auditService,
          auditType = "CreateStateBenefit",
          transactionName = "create-state-benefit",
          apiVersion = Version.from(request, orElse = Version1),
          params = Map("nino" -> nino, "taxYear" -> taxYear),
          requestBody = Some(request.body),
          includeResponse = true
        ))
        .withHateoasResultFrom(hateoasFactory)((_, response) => CreateBenefitHateoasData(nino, taxYear, response.benefitId))

      requestHandler.handleRequest()
    }

}
