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

package v1.amendBenefit

import api.controllers.{ControllerBaseSpec, ControllerTestRunner}
import api.hateoas.Method.{DELETE, GET, PUT}
import api.hateoas.{HateoasWrapper, Link}
import api.mocks.hateoas.MockHateoasFactory
import api.mocks.services.MockAuditService
import api.models.audit.{AuditEvent, AuditResponse, GenericAuditDetail}
import api.models.domain.{Nino, TaxYear}
import api.models.errors._
import api.models.outcomes.ResponseWrapper
import mocks.MockAppConfig
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Result
import v1.amendBenefit.AmendBenefitController
import v1.amendBenefit.def1.model.request.{Def1_AmendBenefitRequestBody, Def1_AmendBenefitRequestData}
import v1.amendBenefit.model.response.AmendBenefitHateoasData
import v1.models.domain.BenefitId

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AmendBenefitControllerSpec
    extends ControllerBaseSpec
    with ControllerTestRunner
    with MockAppConfig
    with MockAmendBenefitService
    with MockAmendBenefitValidatorFactory
    with MockAuditService
    with MockHateoasFactory {

  private val taxYear   = "2020-21"
  private val benefitId = "4557ecb5-fd32-48cc-81f5-e6acd1099f3c"

  private val requestBodyJson = Json.parse(
    """
      |{
      |   "startDate": "2020-04-06",
      |   "endDate": "2021-01-01"
      |}
    """.stripMargin
  )

  private val requestBody = Def1_AmendBenefitRequestBody("2020-04-06", Some("2021-01-01"))

  private val requestData = Def1_AmendBenefitRequestData(Nino(nino), TaxYear.fromMtd(taxYear), BenefitId(benefitId), requestBody)

  private val testHateoasLinks = List(
    Link(s"/individuals/state-benefits/$nino/$taxYear/$benefitId", PUT, "amend-state-benefit"),
    Link(s"/individuals/state-benefits/$nino/$taxYear?benefitId=$benefitId", GET, "self"),
    Link(s"/individuals/state-benefits/$nino/$taxYear/$benefitId", DELETE, "delete-state-benefit"),
    Link(s"/individuals/state-benefits/$nino/$taxYear/$benefitId/amounts", PUT, "amend-state-benefit-amounts")
  )

  private val responseJson = Json.parse(
    s"""
       |{
       |  "links": [
       |    {
       |      "href": "/individuals/state-benefits/$nino/$taxYear/$benefitId",
       |      "method": "PUT",
       |      "rel": "amend-state-benefit"
       |    },
       |    {
       |      "href": "/individuals/state-benefits/$nino/$taxYear?benefitId=$benefitId",
       |      "method": "GET",
       |      "rel": "self"
       |    },
       |    {
       |      "href": "/individuals/state-benefits/$nino/$taxYear/$benefitId",
       |      "method": "DELETE",
       |      "rel": "delete-state-benefit"
       |    },
       |    {
       |      "href": "/individuals/state-benefits/$nino/$taxYear/$benefitId/amounts",
       |      "method": "PUT",
       |      "rel": "amend-state-benefit-amounts"
       |    }
       |  ]
       |}
    """.stripMargin
  )

  "AmendBenefitController" should {
    "return a successful response with status 200 (OK)" when {
      "the request received is valid" in new Test {
        willUseValidator(returningSuccess(requestData))

        MockAmendBenefitService
          .amendBenefit(requestData)
          .returns(Future.successful(Right(ResponseWrapper(correlationId, ()))))

        MockHateoasFactory
          .wrap((), AmendBenefitHateoasData(nino, taxYear, benefitId))
          .returns(HateoasWrapper((), testHateoasLinks))

        runOkTestWithAudit(
          expectedStatus = OK,
          maybeAuditRequestBody = Some(requestBodyJson),
          maybeExpectedResponseBody = Some(responseJson),
          maybeAuditResponseBody = Some(responseJson)
        )
      }
    }

    "return the error as per spec" when {
      "the parser validation fails" in new Test {
        willUseValidator(returning(NinoFormatError))

        runErrorTestWithAudit(NinoFormatError, Some(requestBodyJson))
      }

      "service returns an error" in new Test {
        willUseValidator(returningSuccess(requestData))

        MockAmendBenefitService
          .amendBenefit(requestData)
          .returns(Future.successful(Left(ErrorWrapper(correlationId, RuleTaxYearNotSupportedError))))

        runErrorTestWithAudit(RuleTaxYearNotSupportedError, maybeAuditRequestBody = Some(requestBodyJson))
      }
    }
  }

  private trait Test extends ControllerTest with AuditEventChecking {

    private val controller = new AmendBenefitController(
      authService = mockEnrolmentsAuthService,
      lookupService = mockMtdIdLookupService,
      validatorFactory = mockAmendBenefitValidatorFactory,
      service = mockAmendBenefitService,
      hateoasFactory = mockHateoasFactory,
      auditService = mockAuditService,
      cc = cc,
      idGenerator = mockIdGenerator
    )

    protected def callController(): Future[Result] = controller.amendBenefit(nino, taxYear, benefitId)(fakePutRequest(requestBodyJson))

    def event(auditResponse: AuditResponse, maybeRequestBody: Option[JsValue]): AuditEvent[GenericAuditDetail] =
      AuditEvent(
        auditType = "AmendStateBenefit",
        transactionName = "amend-state-benefit",
        detail = GenericAuditDetail(
          versionNumber = "1.0",
          userType = "Individual",
          agentReferenceNumber = None,
          params = Map("nino" -> nino, "taxYear" -> taxYear, "benefitId" -> benefitId),
          requestBody = maybeRequestBody,
          `X-CorrelationId` = correlationId,
          auditResponse = auditResponse
        )
      )

  }

}
