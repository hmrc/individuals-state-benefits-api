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

package v1.ignoreBenefit

import shared.connectors.{ConnectorSpec, DownstreamOutcome}
import shared.models.domain.{EmptyJsonBody, Nino, TaxYear}
import shared.models.outcomes.ResponseWrapper
import v1.ignoreBenefit.def1.model.request.Def1_IgnoreBenefitRequestData
import v1.models.domain.BenefitId

import scala.concurrent.Future

class IgnoreBenefitConnectorSpec extends ConnectorSpec {

  private val nino      = "AA111111A"
  private val benefitId = "123e4567-e89b-12d3-a456-426614174000"

  "IgnoreBenefitConnector" should {
    "return the expected response for a non-TYS request" when {
      "a valid request is made" in new TysIfsTest with Test {
        def taxYear: TaxYear = TaxYear.fromMtd("2019-20")

        private val outcome: Right[Nothing, ResponseWrapper[Unit]] = Right(ResponseWrapper(correlationId, ()))

        willPut(
          url = s"$baseUrl/income-tax/19-20/income/state-benefits/$nino/ignore/$benefitId",
          EmptyJsonBody
        ).returns(Future.successful(outcome))

        val result: DownstreamOutcome[Unit] = await(connector.ignoreBenefit(request))

        result shouldBe outcome
      }
    }
  }

  trait Test {
    _: ConnectorTest =>
    def taxYear: TaxYear

    val request: Def1_IgnoreBenefitRequestData = Def1_IgnoreBenefitRequestData(Nino(nino), taxYear, BenefitId(benefitId))

    val connector: IgnoreBenefitConnector = new IgnoreBenefitConnector(http = mockHttpClient, appConfig = mockAppConfig)

  }

}
