/*
 * Copyright 2020 HM Revenue & Customs
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
import uk.gov.hmrc.domain.Nino
import v1.mocks.MockHttpClient
import v1.models.domain.BenefitType
import v1.models.outcomes.ResponseWrapper
import v1.models.request.addStateBenefit.{AddStateBenefitBody, AddStateBenefitRequest}
import v1.models.response.AddStateBenefitsResponse

import scala.concurrent.Future

class AddStateBenefitConnectorSpec extends ConnectorSpec {

  val nino: String = "AA111111A"
  val taxYear: String = "2021-22"

  val addStateBenefitRequestBody: AddStateBenefitBody = AddStateBenefitBody(
    benefitType = BenefitType.incapacityBenefit,
    startDate = "2020-08-03",
    endDate = Some("2020-12-03")
  )

  val request: AddStateBenefitRequest = AddStateBenefitRequest(
    nino = Nino(nino),
    taxYear = taxYear,
    body = addStateBenefitRequestBody
  )

  val response = AddStateBenefitsResponse("b1e8057e-fbbc-47a8-a8b4-78d9f015c253")

  class Test extends MockHttpClient with MockAppConfig {

    val connector: AddStateBenefitConnector = new AddStateBenefitConnector(
      http = mockHttpClient,
      appConfig = mockAppConfig
    )

    MockedAppConfig.desBaseUrl returns baseUrl
    MockedAppConfig.desToken returns "des-token"
    MockedAppConfig.desEnvironment returns "des-environment"
  }

  "AddStateBenefitConnector" when {
    "addStateBenefit" should {
      "return a 200 status upon HttpClient success" in new Test {
        val outcome = Right(ResponseWrapper(correlationId, response))

        MockedHttpClient
          .post(
            url = s"$baseUrl/income-tax/income/state-benefits/$nino/$taxYear/custom",
            body = addStateBenefitRequestBody,
            requiredHeaders = "Environment" -> "des-environment", "Authorization" -> s"Bearer des-token"
          ).returns(Future.successful(outcome))

        await(connector.addStateBenefit(request)) shouldBe outcome
      }
    }
  }
}