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
import api.mocks.hateoas.MockHateoasFactory
import api.mocks.services.MockAuditService
import api.models.audit.{AuditEvent, AuditResponse, GenericAuditDetail}
import api.models.domain.{Nino, TaxYear}
import api.models.errors._
import api.models.hateoas
import api.models.hateoas.HateoasWrapper
import api.models.hateoas.Method.{GET, POST}
import api.models.outcomes.ResponseWrapper
import mocks.MockAppConfig
import play.api.Configuration
import play.api.libs.json.{JsValue, Json}
import play.api.mvc.Result
import v1.mocks.requestParsers.MockIgnoreBenefitRequestParser
import v1.mocks.services.MockUnignoreBenefitService
import v1.models.request.ignoreBenefit.{IgnoreBenefitRawData, IgnoreBenefitRequest}
import v1.models.response.unignoreBenefit.UnignoreBenefitHateoasData

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class UnignoreBenefitControllerSpec
    extends ControllerBaseSpec
    with ControllerTestRunner
    with MockAppConfig
    with MockUnignoreBenefitService
    with MockIgnoreBenefitRequestParser
    with MockAuditService
    with MockHateoasFactory {

  val taxYear: String   = "2019-20"
  val benefitId: String = "b1e8057e-fbbc-47a8-a8b4-78d9f015c253"

  val rawData: IgnoreBenefitRawData = IgnoreBenefitRawData(
    nino = nino,
    taxYear = taxYear,
    benefitId = benefitId
  )

  val requestData: IgnoreBenefitRequest = IgnoreBenefitRequest(
    nino = Nino(nino),
    taxYear = TaxYear.fromMtd(taxYear),
    benefitId = benefitId
  )

  private val testHateoasLinks = Seq(
    hateoas.Link(href = s"/individuals/state-benefits/$nino/$taxYear?benefitId=$benefitId", method = GET, rel = "self"),
    hateoas.Link(href = s"/individuals/state-benefits/$nino/$taxYear/$benefitId/ignore", method = POST, rel = "ignore-state-benefit")
  )

  val hateoasResponse: JsValue = Json.parse(
    s"""
       |{
       |   "links":[
       |      {
       |         "href":"/individuals/state-benefits/$nino/$taxYear?benefitId=$benefitId",
       |         "rel":"self",
       |         "method":"GET"
       |      },
       |      {
       |         "href":"/individuals/state-benefits/$nino/$taxYear/$benefitId/ignore",
       |         "rel":"ignore-state-benefit",
       |         "method":"POST"
       |      }
       |   ]
       |}
    """.stripMargin
  )

  "UnignoreBenefitController" should {
    "return a successful response with status 200 (OK)" when {
      "happy path" in new Test {
        MockIgnoreBenefitRequestParser
          .parse(rawData)
          .returns(Right(requestData))

        MockUnignoreBenefitService
          .unignoreBenefit(requestData)
          .returns(Future.successful(Right(ResponseWrapper(correlationId, ()))))

        MockHateoasFactory
          .wrap((), UnignoreBenefitHateoasData(nino, taxYear, benefitId))
          .returns(HateoasWrapper((), testHateoasLinks))

        runOkTestWithAudit(
          expectedStatus = OK,
          maybeAuditRequestBody = None,
          maybeExpectedResponseBody = Some(hateoasResponse),
          maybeAuditResponseBody = Some(hateoasResponse)
        )
      }
    }

    "return the error as per spec" when {
      "the parser validation fails" in new Test {
        MockIgnoreBenefitRequestParser
          .parse(rawData)
          .returns(Left(ErrorWrapper(correlationId, NinoFormatError)))

        runErrorTestWithAudit(NinoFormatError, None)
      }

      "the service returns an error" in new Test {
        MockIgnoreBenefitRequestParser
          .parse(rawData)
          .returns(Right(requestData))

        MockUnignoreBenefitService
          .unignoreBenefit(requestData)
          .returns(Future.successful(Left(ErrorWrapper(correlationId, RuleTaxYearNotSupportedError))))

        runErrorTestWithAudit(RuleTaxYearNotSupportedError, maybeAuditRequestBody = None)
      }
    }
  }

  trait Test extends ControllerTest with AuditEventChecking {

    val controller = new UnignoreBenefitController(
      authService = mockEnrolmentsAuthService,
      lookupService = mockMtdIdLookupService,
      appConfig = mockAppConfig,
      parser = mockIgnoreBenefitRequestParser,
      service = mockUnignoreBenefitService,
      auditService = mockAuditService,
      hateoasFactory = mockHateoasFactory,
      cc = cc,
      idGenerator = mockIdGenerator
    )

    MockedAppConfig.featureSwitches.returns(Configuration("allowTemporalValidationSuspension.enabled" -> true)).anyNumberOfTimes()

    protected def callController(): Future[Result] = controller.unignoreBenefit(nino, taxYear, benefitId)(fakeRequest)

    def event(auditResponse: AuditResponse, requestBody: Option[JsValue]): AuditEvent[GenericAuditDetail] =
      AuditEvent(
        auditType = "UnignoreStateBenefit",
        transactionName = "unignore-state-benefit",
        detail = GenericAuditDetail(
          userType = "Individual",
          agentReferenceNumber = None,
          pathParams = Map("nino" -> nino, "taxYear" -> taxYear, "benefitId" -> benefitId),
          queryParams = None,
          requestBody = None,
          `X-CorrelationId` = correlationId,
          auditResponse = auditResponse
        )
      )

  }

}
