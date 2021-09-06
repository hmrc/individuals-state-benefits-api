/*
 * Copyright 2021 HM Revenue & Customs
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

package v1r6.connectors

import mocks.MockAppConfig
import v1r6.models.domain.Nino
import v1r6.mocks.MockHttpClient
import v1r6.models.outcomes.ResponseWrapper
import v1r6.models.request.EmptyBody
import v1r6.models.request.ignoreBenefit.IgnoreBenefitRequest

import scala.concurrent.Future

class IgnoreBenefitConnectorSpec extends ConnectorSpec {

  val nino: String = "AA111111A"
  val taxYear: String = "2019-20"
  val benefitId: String = "123e4567-e89b-12d3-a456-426614174000"
  val request: IgnoreBenefitRequest = IgnoreBenefitRequest(Nino(nino), taxYear, benefitId)

  class Test extends MockHttpClient with MockAppConfig {
    val connector: IgnoreBenefitConnector = new IgnoreBenefitConnector(
      http = mockHttpClient,
      appConfig = mockAppConfig
    )

    MockAppConfig.desBaseUrl returns baseUrl
    MockAppConfig.desToken returns "des-token"
    MockAppConfig.desEnvironment returns "des-environment"
    MockAppConfig.desEnvironmentHeaders returns Some(allowedDesHeaders)
  }

  "IgnoreBenefitConnector" when {
    "happy path" should {
      "return a successful response" in new Test {
        private val outcome = Right(ResponseWrapper(correlationId, ()))

        MockHttpClient.put(
          url = s"$baseUrl/income-tax/income/state-benefits/$nino/$taxYear/ignore/$benefitId",
          config = dummyDesHeaderCarrierConfig,
          body = EmptyBody,
          requiredHeaders = requiredDesHeaders,
          excludedHeaders = Seq("AnotherHeader" -> "HeaderValue")
        ).returns(Future.successful(outcome))

        await(connector.ignoreBenefit(request)) shouldBe outcome
      }
    }
  }
}
