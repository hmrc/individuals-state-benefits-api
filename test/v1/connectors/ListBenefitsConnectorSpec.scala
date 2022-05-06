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

package v1.connectors

import mocks.MockAppConfig
import v1.models.domain.Nino
import v1.mocks.MockHttpClient
import v1.models.outcomes.ResponseWrapper
import v1.models.request.listBenefits.ListBenefitsRequest
import v1.models.response.listBenefits.{ListBenefitsResponse, StateBenefit}

import scala.concurrent.Future

class ListBenefitsConnectorSpec extends ConnectorSpec {

  val nino: String      = "AA111111A"
  val taxYear: String   = "2019"
  private val benefitId = Some("4557ecb5-fd32-48cc-81f5-e6acd1099f3c")

  def queryParams: Seq[(String, String)] =
    Seq("benefitId" -> benefitId)
      .collect { case (k, Some(v)) =>
        (k, v)
      }

  val request: ListBenefitsRequest = ListBenefitsRequest(Nino(nino), taxYear, benefitId)

  private val validResponse = ListBenefitsResponse(
    stateBenefits = Some(
      Seq(
        StateBenefit(
          benefitType = "incapacityBenefit",
          dateIgnored = Some("2019-04-04T01:01:01Z"),
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = None,
          createdBy = "HMRC"
        )
      )
    ),
    customerAddedStateBenefits = Some(
      Seq(
        StateBenefit(
          benefitType = "incapacityBenefit",
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = Some("2019-04-04T01:01:01Z"),
          createdBy = "CUSTOM"
        )
      )
    )
  )

  class Test extends MockHttpClient with MockAppConfig {

    val connector: ListBenefitsConnector = new ListBenefitsConnector(
      http = mockHttpClient,
      appConfig = mockAppConfig
    )

    val desRequestHeaders: Seq[(String, String)] = Seq(
      "Environment"   -> "des-environment",
      "Authorization" -> s"Bearer des-token"
    )

    MockAppConfig.desBaseUrl returns baseUrl
    MockAppConfig.desToken returns "des-token"
    MockAppConfig.desEnvironment returns "des-environment"
    MockAppConfig.desEnvironmentHeaders returns Some(allowedDesHeaders)
  }

  "ListBenefitsConnector" when {
    "listBenefits" must {
      "return a 200 status for a success scenario" in new Test {

        val outcome = Right(ResponseWrapper(correlationId, validResponse))

        MockHttpClient
          .parameterGet(
            url = s"$baseUrl/income-tax/income/state-benefits/$nino/$taxYear",
            queryParams,
            config = dummyDesHeaderCarrierConfig,
            requiredHeaders = desRequestHeaders,
            excludedHeaders = Seq("AnotherHeader" -> "HeaderValue")
          )
          .returns(Future.successful(outcome))

        await(connector.listBenefits(request)) shouldBe outcome
      }
    }
  }

}
