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

import api.connectors.{ConnectorSpec, DownstreamOutcome}
import api.models.domain.{Nino, TaxYear}
import api.models.outcomes.ResponseWrapper
import api.models.request.EmptyBody
import v1.models.request.ignoreBenefit.IgnoreBenefitRequest

import scala.concurrent.Future

class IgnoreBenefitConnectorSpec extends ConnectorSpec {

  "IgnoreBenefitConnector" should {
    "return the expected response for a non-TYS request" when {
      "a valid request is made" in new IfsTest with Test {
        def taxYear: TaxYear = TaxYear.fromMtd("2019-20")

        private val outcome: Right[Nothing, ResponseWrapper[Unit]]
        = Right(ResponseWrapper(correlationId, ()))

        willPut(
          url = s"$baseUrl/income-tax/income/state-benefits/$nino/2019-20/ignore/$benefitId", EmptyBody
        ).returns(Future.successful(outcome))

        val result: DownstreamOutcome[Unit] = await(connector.ignoreBenefit(request))

        result shouldBe outcome
      }

      "return the expected response for a TYS request" when {
        "a valid request is made" in new TysIfsTest with Test {
          def taxYear: TaxYear = TaxYear.fromMtd("2023-24")

          private val outcome: Right[Nothing, ResponseWrapper[Unit]]
          = Right(ResponseWrapper(correlationId, ()))

          willPut(
            url = s"$baseUrl/income-tax/23-24/income/state-benefits/$nino/ignore/$benefitId", EmptyBody
          ).returns(Future.successful(outcome))

          val result: DownstreamOutcome[Unit] = await(connector.ignoreBenefit(request))

          result shouldBe outcome
        }
      }
    }
  }

  trait Test {
    _: ConnectorTest =>
    def taxYear: TaxYear

    val nino: String = "AA111111A"
    val benefitId: String = "123e4567-e89b-12d3-a456-426614174000"

    val request: IgnoreBenefitRequest = IgnoreBenefitRequest(Nino(nino), taxYear, benefitId)

    val connector: IgnoreBenefitConnector = new IgnoreBenefitConnector(
      http = mockHttpClient,
      appConfig = mockAppConfig
    )
  }
}
