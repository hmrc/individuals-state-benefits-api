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
import api.services.{AuditService, EnrolmentsAuthService, MtdIdLookupService}
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import routing.{Version, Version1}
import utils.IdGenerator
import v1.controllers.requestParsers.DeleteBenefitAmountsRequestParser
import v1.models.request.deleteBenefitAmounts.DeleteBenefitAmountsRawData
import v1.services.DeleteBenefitAmountsService

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

@Singleton
class DeleteBenefitAmountsController @Inject() (val authService: EnrolmentsAuthService,
                                                val lookupService: MtdIdLookupService,
                                                parser: DeleteBenefitAmountsRequestParser,
                                                service: DeleteBenefitAmountsService,
                                                auditService: AuditService,
                                                cc: ControllerComponents,
                                                idGenerator: IdGenerator)(implicit ec: ExecutionContext)
    extends AuthorisedController(cc) {

  implicit val endpointLogContext: EndpointLogContext =
    EndpointLogContext(
      controllerName = "DeleteBenefitAmountsController",
      endpointName = "deleteBenefitAmounts"
    )

  def deleteBenefitAmounts(nino: String, taxYear: String, benefitId: String): Action[AnyContent] =
    authorisedAction(nino).async { implicit request =>
      implicit val ctx: RequestContext = RequestContext.from(idGenerator, endpointLogContext)

      val rawData = DeleteBenefitAmountsRawData(nino, taxYear, benefitId)

      val requestHandler = RequestHandlerOld
        .withParser(parser)
        .withService(service.delete)
        .withAuditing(AuditHandlerOld(
          auditService = auditService,
          auditType = "DeleteStateBenefitAmounts",
          transactionName = "delete-state-benefit-amounts",
          version = Version.from(request, orElse = Version1),
          pathParams = Map("nino" -> nino, "taxYear" -> taxYear, "benefitId" -> benefitId),
          queryParams = None,
          requestBody = None
        ))

      requestHandler.handleRequest(rawData)

    }

}
