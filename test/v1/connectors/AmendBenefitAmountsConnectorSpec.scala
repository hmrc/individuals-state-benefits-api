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

package v1.connectors

import mocks.MockAppConfig
import uk.gov.hmrc.http.HeaderCarrier
import v1.models.domain.{Nino, TaxYear}
import v1.mocks.MockHttpClient
import v1.models.outcomes.ResponseWrapper
import v1.models.request.AmendBenefitAmounts.{AmendBenefitAmountsRequest, AmendBenefitAmountsRequestBody}

import scala.concurrent.Future

class AmendBenefitAmountsConnectorSpec extends ConnectorSpec {

  val nino: String      = "AA123456A"
  val taxYear: String   = "2021-22"
  val benefitId: String = "123e4567-e89b-12d3-a456-426614174000"

  val amendBenefitAmountsRequestBody: AmendBenefitAmountsRequestBody = AmendBenefitAmountsRequestBody(
    amount = 999.99,
    taxPaid = Some(123.13)
  )

  val request: AmendBenefitAmountsRequest = AmendBenefitAmountsRequest(
    nino = Nino(nino),
    taxYear = TaxYear.fromMtd(taxYear),
    benefitId = benefitId,
    body = amendBenefitAmountsRequestBody
  )

  class Test extends MockHttpClient with MockAppConfig {

    val connector: AmendBenefitAmountsConnector = new AmendBenefitAmountsConnector(
      http = mockHttpClient,
      appConfig = mockAppConfig
    )

    val ifsRequestHeaders: Seq[(String, String)] = Seq(
      "Environment"   -> "ifs-environment",
      "Authorization" -> s"Bearer ifs-token"
    )

    MockedAppConfig.api1651BaseUrl returns baseUrl
    MockedAppConfig.api1651Token returns "api1651-token"
    MockedAppConfig.api1651Environment returns "api1651-environment"
    MockedAppConfig.api1651EnvironmentHeaders returns Some(allowedIfsHeaders)
  }

  "AmendBenefitAmountsConnector" when {
    "amendBenefitAmounts" must {
      "return a 204 status for a success scenario" in new Test {
        val outcome = Right(ResponseWrapper(correlationId, ()))

        implicit val hc: HeaderCarrier                       = HeaderCarrier(otherHeaders = otherHeaders ++ Seq("Content-Type" -> "application/json"))
        val requiredApi1651HeadersPut: Seq[(String, String)] = requiredApi1651Headers ++ Seq("Content-Type" -> "application/json")

        MockedHttpClient
          .put(
            url = s"$baseUrl/income-tax/income/state-benefits/$nino/$taxYear/$benefitId",
            config = dummyIfsHeaderCarrierConfig,
            body = request.body,
            requiredHeaders = requiredApi1651HeadersPut,
            excludedHeaders = Seq("AnotherHeader" -> "HeaderValue")
          )
          .returns(Future.successful(outcome))

        await(connector.amendBenefitAmounts(request)) shouldBe outcome
      }
    }
  }

}
