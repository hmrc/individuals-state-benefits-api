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

package v1.endpoints

import com.github.tomakehurst.wiremock.stubbing.StubMapping
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}
import play.api.http.HeaderNames.ACCEPT
import play.api.http.Status._
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.ws.{WSRequest, WSResponse}
import support.V1IntegrationBaseSpec
import v1.models.errors._
import v1.stubs.{AuditStub, AuthStub, DesStub, MtdIdLookupStub}

class UnignoreBenefitControllerISpec extends V1IntegrationBaseSpec {

  private trait Test {

    val nino: String          = "AA123456A"
    val taxYear: String       = "2019-20"
    val benefitId: String     = "b1e8057e-fbbc-47a8-a8b4-78d9f015c253"
    val correlationId: String = "a1e8057e-fbbc-47a8-a8b4-78d9f015c253"

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

    def uri: String = s"/$nino/$taxYear/$benefitId/unignore"

    def desUri: String = s"/income-tax/state-benefits/$nino/$taxYear/ignore/$benefitId"

    def setupStubs(): StubMapping

    def request(): WSRequest = {
      setupStubs()
      buildRequest(uri)
        .withHttpHeaders((ACCEPT, "application/vnd.hmrc.1.0+json"))
    }

  }

  "Calling the 'unignore benefit' endpoint" should {
    "return a 200 status code" when {
      "any valid request is made" in new Test {

        override def setupStubs(): StubMapping = {
          AuditStub.audit()
          AuthStub.authorised()
          MtdIdLookupStub.ninoFound(nino)
          DesStub.onSuccess(DesStub.DELETE, desUri, NO_CONTENT)
        }

        val response: WSResponse = await(request().post(JsObject.empty))
        response.status shouldBe OK
        response.body[JsValue] shouldBe hateoasResponse
        response.header("Content-Type") shouldBe Some("application/json")
      }
    }

    "return error according to spec" when {

      def getCurrentTaxYear: String = {
        val currentDate = DateTime.now(DateTimeZone.UTC)

        val taxYearStartDate: DateTime = DateTime.parse(
          currentDate.getYear + "-04-06",
          DateTimeFormat.forPattern("yyyy-MM-dd")
        )

        def fromDesIntToString(taxYear: Int): String =
          (taxYear - 1) + "-" + taxYear.toString.drop(2)

        if (currentDate.isBefore(taxYearStartDate)) {
          fromDesIntToString(currentDate.getYear)
        } else {
          fromDesIntToString(currentDate.getYear + 1)
        }
      }

      "validation error" when {
        def validationErrorTest(requestNino: String,
                                requestTaxYear: String,
                                requestBenefitId: String,
                                expectedStatus: Int,
                                expectedBody: MtdError,
                                scenario: Option[String]): Unit = {
          s"validation fails with ${expectedBody.code} error ${scenario.getOrElse("")}" in new Test {

            override val nino: String      = requestNino
            override val taxYear: String   = requestTaxYear
            override val benefitId: String = requestBenefitId

            override def setupStubs(): StubMapping = {
              AuditStub.audit()
              AuthStub.authorised()
              MtdIdLookupStub.ninoFound(nino)
            }

            val response: WSResponse = await(request().post(JsObject.empty))
            response.status shouldBe expectedStatus
            response.json shouldBe Json.toJson(expectedBody)
          }
        }

        val input = Seq(
          ("AA1123A", "2019-20", "4557ecb5-fd32-48cc-81f5-e6acd1099f3c", BAD_REQUEST, NinoFormatError, None),
          ("AA123456A", "20199", "78d9f015-a8b4-47a8-8bbc-c253a1e8057e", BAD_REQUEST, TaxYearFormatError, None),
          ("AA123456A", "2019-20", "ABCDE12345FG", BAD_REQUEST, BenefitIdFormatError, None),
          ("AA123456A", "2018-19", "78d9f015-a8b4-47a8-8bbc-c253a1e8057e", BAD_REQUEST, RuleTaxYearNotSupportedError, None),
          ("AA123456A", "2019-21", "4557ecb5-fd32-48cc-81f5-e6acd1099f3c", BAD_REQUEST, RuleTaxYearRangeInvalidError, None),
          ("AA123456A", getCurrentTaxYear, "78d9f015-a8b4-47a8-8bbc-c253a1e8057e", BAD_REQUEST, RuleTaxYearNotEndedError, None)
        )

        input.foreach(args => (validationErrorTest _).tupled(args))
      }

      "des service error" when {
        def serviceErrorTest(desStatus: Int, desCode: String, expectedStatus: Int, expectedBody: MtdError): Unit = {
          s"des returns an $desCode error and status $desStatus" in new Test {

            override def setupStubs(): StubMapping = {
              AuditStub.audit()
              AuthStub.authorised()
              MtdIdLookupStub.ninoFound(nino)
              DesStub.onError(DesStub.DELETE, desUri, desStatus, errorBody(desCode))
            }

            val response: WSResponse = await(request().post(JsObject.empty))
            response.status shouldBe expectedStatus
            response.json shouldBe Json.toJson(expectedBody)
          }
        }

        def errorBody(code: String): String =
          s"""
             |{
             |   "code": "$code",
             |   "reason": "des message"
             |}
            """.stripMargin

        val input = Seq(
          (BAD_REQUEST, "INVALID_TAXABLE_ENTITY_ID", BAD_REQUEST, NinoFormatError),
          (BAD_REQUEST, "INVALID_TAX_YEAR", BAD_REQUEST, TaxYearFormatError),
          (BAD_REQUEST, "INVALID_BENEFIT_ID", BAD_REQUEST, BenefitIdFormatError),
          (BAD_REQUEST, "INVALID_CORRELATIONID", INTERNAL_SERVER_ERROR, DownstreamError),
          (FORBIDDEN, "CUSTOMER_ADDED", FORBIDDEN, RuleUnignoreForbiddenError),
          (UNPROCESSABLE_ENTITY, "BEFORE_TAX_YEAR_ENDED", BAD_REQUEST, RuleTaxYearNotEndedError),
          (NOT_FOUND, "NO_DATA_FOUND", NOT_FOUND, NotFoundError),
          (INTERNAL_SERVER_ERROR, "SERVER_ERROR", INTERNAL_SERVER_ERROR, DownstreamError),
          (SERVICE_UNAVAILABLE, "SERVICE_UNAVAILABLE", INTERNAL_SERVER_ERROR, DownstreamError)
        )

        input.foreach(args => (serviceErrorTest _).tupled(args))
      }
    }
  }

}
