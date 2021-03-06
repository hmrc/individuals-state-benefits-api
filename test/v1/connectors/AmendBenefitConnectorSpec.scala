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

package v1.connectors

import mocks.MockAppConfig
import uk.gov.hmrc.http.HeaderCarrier
import v1.models.domain.Nino
import v1.mocks.MockHttpClient
import v1.models.outcomes.ResponseWrapper
import v1.models.request.AmendBenefit.{AmendBenefitRequest, AmendBenefitRequestBody}

import scala.concurrent.Future

class AmendBenefitConnectorSpec extends ConnectorSpec {

  val nino: String = "AA123456A"
  val taxYear: String = "2021-22"
  val benefitId: String = "123e4567-e89b-12d3-a456-426614174000"

  val updateBenefitRequestBody: AmendBenefitRequestBody = AmendBenefitRequestBody(
    startDate = "2020-08-03",
    endDate = Some("2020-12-03")
  )

  val request: AmendBenefitRequest = AmendBenefitRequest(
    nino = Nino(nino),
    taxYear = taxYear,
    benefitId = benefitId,
    body = updateBenefitRequestBody
  )

  class Test extends MockHttpClient with MockAppConfig {

    val connector: AmendBenefitConnector = new AmendBenefitConnector(
      http = mockHttpClient,
      appConfig = mockAppConfig
    )

    val desRequestHeaders: Seq[(String, String)] = Seq(
      "Environment" -> "des-environment",
      "Authorization" -> s"Bearer des-token"
    )

    MockAppConfig.desBaseUrl returns baseUrl
    MockAppConfig.desToken returns "des-token"
    MockAppConfig.desEnvironment returns "des-environment"
    MockAppConfig.desEnvironmentHeaders returns Some(allowedDesHeaders)
  }

  "UpdateBenefitConnector" when {
    "updateBenefit" must {
      "return a 201 status for a success scenario" in new Test {
        val outcome = Right(ResponseWrapper(correlationId, ()))

        implicit val hc: HeaderCarrier = HeaderCarrier(otherHeaders = otherHeaders ++ Seq("Content-Type" -> "application/json"))
        val requiredDesHeadersPut: Seq[(String, String)] = requiredDesHeaders ++ Seq("Content-Type" -> "application/json")

        MockHttpClient
          .put(
            url = s"$baseUrl/income-tax/income/state-benefits/$nino/$taxYear/custom/$benefitId",
            config = dummyDesHeaderCarrierConfig,
            body = request.body,
            requiredHeaders = requiredDesHeadersPut,
            excludedHeaders = Seq("AnotherHeader" -> "HeaderValue")
          ).returns(Future.successful(outcome))

        await(connector.amendBenefit(request)) shouldBe outcome
      }
    }
  }
}
