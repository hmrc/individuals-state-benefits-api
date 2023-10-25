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

import api.controllers.{ControllerBaseSpec, ControllerTestRunner}
import api.mocks.services.MockAuditService
import api.models.audit.{AuditEvent, AuditResponse, GenericAuditDetailOld}
import api.models.domain.{BenefitId, Nino, TaxYear}
import api.models.errors._
import api.models.outcomes.ResponseWrapper
import play.api.libs.json.JsValue
import play.api.mvc.Result
import routing.Version1
import v1.mocks.requestParsers.MockDeleteBenefitRequestParser
import v1.models.request.deleteBenefit.{DeleteBenefitRawData, DeleteBenefitRequest}
import v1.services.MockDeleteBenefitService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DeleteBenefitControllerSpec
    extends ControllerBaseSpec
    with ControllerTestRunner
    with MockAuditService
    with MockDeleteBenefitService
    with MockDeleteBenefitRequestParser {

  val taxYear: String   = "2019-20"
  val benefitId: String = "b1e8057e-fbbc-47a8-a8b4-78d9f015c253"

  val rawData: DeleteBenefitRawData = DeleteBenefitRawData(
    nino = nino,
    taxYear = taxYear,
    benefitId = benefitId
  )

  val requestData: DeleteBenefitRequest = DeleteBenefitRequest(
    nino = Nino(nino),
    taxYear = TaxYear.fromMtd(taxYear),
    benefitId = BenefitId(benefitId)
  )

  "DeleteBenefitController" should {
    "return a successful response with status 204 (No Content)" when {
      "happy path" in new Test {
        MockDeleteBenefitRequestParser
          .parse(rawData)
          .returns(Right(requestData))

        MockDeleteBenefitService
          .deleteBenefit(requestData)
          .returns(Future.successful(Right(ResponseWrapper(correlationId, ()))))

        runOkTestWithAudit(expectedStatus = NO_CONTENT)
      }
    }

    "return the error as per spec" when {
      "the parser validation fails" in new Test {
        MockDeleteBenefitRequestParser
          .parse(rawData)
          .returns(Left(ErrorWrapper(correlationId, NinoFormatError)))

        runErrorTestWithAudit(NinoFormatError)
      }

      "service returns an error" in new Test {
        MockDeleteBenefitRequestParser
          .parse(rawData)
          .returns(Right(requestData))

        MockDeleteBenefitService
          .deleteBenefit(requestData)
          .returns(Future.successful(Left(ErrorWrapper(correlationId, RuleTaxYearNotSupportedError))))

        runErrorTestWithAudit(RuleTaxYearNotSupportedError)
      }
    }
  }

  trait Test extends ControllerTest with AuditEventChecking {

    val controller = new DeleteBenefitController(
      authService = mockEnrolmentsAuthService,
      lookupService = mockMtdIdLookupService,
      parser = mockDeleteBenefitRequestParser,
      service = mockDeleteBenefitService,
      auditService = mockAuditService,
      cc = cc,
      idGenerator = mockIdGenerator
    )

    protected def callController(): Future[Result] = controller.deleteBenefit(nino, taxYear, benefitId)(fakeDeleteRequest)

    def event(auditResponse: AuditResponse, requestBody: Option[JsValue]): AuditEvent[GenericAuditDetailOld] =
      AuditEvent(
        auditType = "DeleteStateBenefit",
        transactionName = "delete-state-benefit",
        detail = GenericAuditDetailOld(
          userType = "Individual",
          agentReferenceNumber = None,
          pathParams = Map("nino" -> nino, "taxYear" -> taxYear, "benefitId" -> benefitId),
          queryParams = None,
          requestBody = None,
          `X-CorrelationId` = correlationId,
          versionNumber = Version1.name,
          auditResponse = auditResponse
        )
      )

  }

}
