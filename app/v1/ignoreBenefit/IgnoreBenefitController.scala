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

package v1.ignoreBenefit

import play.api.mvc.{Action, AnyContent, ControllerComponents}
import shared.config.SharedAppConfig
import shared.controllers._
import shared.hateoas.HateoasFactory
import shared.routing.Version
import shared.services.{AuditService, EnrolmentsAuthService, MtdIdLookupService}
import shared.utils.IdGenerator
import v1.ignoreBenefit.model.response.IgnoreBenefitHateoasData
import v1.ignoreBenefit.model.response.IgnoreBenefitResponse._

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class IgnoreBenefitController @Inject() (val authService: EnrolmentsAuthService,
                                         val lookupService: MtdIdLookupService,
                                         validatorFactory: IgnoreBenefitValidatorFactory,
                                         service: IgnoreBenefitService,
                                         auditService: AuditService,
                                         hateoasFactory: HateoasFactory,
                                         cc: ControllerComponents,
                                         idGenerator: IdGenerator)(implicit appConfig: SharedAppConfig, ec: ExecutionContext)
    extends AuthorisedController(cc) {

  val endpointName = "ignore-benefit"

  implicit val endpointLogContext: EndpointLogContext =
    EndpointLogContext(
      controllerName = "IgnoreBenefitController",
      endpointName = "ignoreBenefit"
    )

  def ignoreBenefit(nino: String, taxYear: String, benefitId: String): Action[AnyContent] =
    authorisedAction(nino).async { implicit request =>
      implicit val ctx: RequestContext = RequestContext.from(idGenerator, endpointLogContext)

      val validator = validatorFactory.validator(nino, taxYear, benefitId)

      val requestHandler = RequestHandler
        .withValidator(validator)
        .withService(service.ignoreBenefit)
        .withAuditing(AuditHandler(
          auditService = auditService,
          auditType = "IgnoreStateBenefit",
          transactionName = "ignore-state-benefit",
          apiVersion = Version(request),
          params = Map("nino" -> nino, "taxYear" -> taxYear, "benefitId" -> benefitId),
          requestBody = None,
          includeResponse = true
        ))
        .withHateoasResult(hateoasFactory)(IgnoreBenefitHateoasData(nino, taxYear, benefitId))

      requestHandler.handleRequest()
    }

}
