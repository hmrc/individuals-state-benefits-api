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
import v1.models.outcomes.ResponseWrapper
import v1.models.request.listBenefits.ListBenefitsRequest
import v1.models.response.listBenefits.{CustomerAddedStateBenefits, CustomerAddedBenefit, StateBenefit, ListBenefitsResponse, StateBenefits}

import scala.concurrent.Future

class ListBenefitsConnectorSpec extends ConnectorSpec {

  val nino: String = "AA111111A"
  val taxYear: String = "2019"

  val request: ListBenefitsRequest = ListBenefitsRequest(Nino(nino), taxYear)

  private val validResponse = ListBenefitsResponse(None, None)
  //  ListBenefitsResponse(
//    stateBenefits = StateBenefits(
//      incapacityBenefit = Seq(
//        StateBenefit(
//          dateIgnored = Some("2019-04-04T01:01:01Z"),
//          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//          startDate = "2020-01-01",
//          endDate = Some("2020-04-01"),
//          amount = Some(2000.00),
//          taxPaid = Some(2132.22),
//          submittedOn = None
//        )
//      ),
//      statePension = StateBenefit(
//        dateIgnored = None,
//        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//        startDate = "2019-01-01",
//        endDate = None,
//        amount = Some(2000.00),
//        taxPaid = None,
//        submittedOn = None
//      ),
//      statePensionLumpSum = StateBenefit(
//        dateIgnored = None,
//        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//        startDate = "2019-01-01",
//        endDate = Some("2019-01-01"),
//        amount = Some(2000.00),
//        taxPaid = Some(2132.22),
//        submittedOn = None
//      ),
//      employmentSupportAllowance = Seq(
//        StateBenefit(
//          dateIgnored = None,
//          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//          startDate = "2020-01-01",
//          endDate = Some("2020-04-01"),
//          amount = Some(2000.00),
//          taxPaid = Some(2132.22),
//          submittedOn = None
//        )
//      ),
//      jobSeekersAllowance = Seq(
//        StateBenefit(
//          dateIgnored = None,
//          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//          startDate = "2020-01-01",
//          endDate = Some("2020-04-01"),
//          amount = Some(2000.00),
//          taxPaid = Some(2132.22),
//          submittedOn = None
//        )
//      ),
//      bereavementAllowance = StateBenefit(
//        dateIgnored = None,
//        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//        startDate = "2020-01-01",
//        endDate = Some("2020-04-01"),
//        amount = Some(2000.00),
//        taxPaid = None,
//        submittedOn = None
//      ),
//      otherStateBenefits = StateBenefit(
//        dateIgnored = None,
//        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//        startDate = "2020-01-01",
//        endDate = Some("2020-04-01"),
//        amount = Some(2000.00),
//        taxPaid = None,
//        submittedOn = None
//      )
//    ),
//    customerAddedStateBenefits = CustomerAddedStateBenefits(
//      incapacityBenefit = Seq(
//        CustomerAddedBenefit(
//          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//          startDate = "2020-01-01",
//          endDate = Some("2020-04-01"),
//          amount = Some(2000.00),
//          taxPaid = Some(2132.22),
//          submittedOn = Some("2019-04-04T01:01:01Z")
//        )
//      ),
//      statePension = CustomerAddedBenefit(
//        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//        startDate = "2019-01-01",
//        endDate = None,
//        amount = Some(2000.00),
//        taxPaid = None,
//        submittedOn = Some("2019-04-04T01:01:01Z")
//      ),
//      statePensionLumpSum = CustomerAddedBenefit(
//        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//        startDate = "2019-01-01",
//        endDate = Some("2019-01-01"),
//        amount = Some(2000.00),
//        taxPaid = Some(2132.22),
//        submittedOn = Some("2019-04-04T01:01:01Z")
//      ),
//      employmentSupportAllowance = Seq(
//        CustomerAddedBenefit(
//          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//          startDate = "2020-01-01",
//          endDate = Some("2020-04-01"),
//          amount = Some(2000.00),
//          taxPaid = Some(2132.22),
//          submittedOn = Some("2019-04-04T01:01:01Z")
//        )
//      ),
//      jobSeekersAllowance = Seq(
//        CustomerAddedBenefit(
//          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//          startDate = "2020-01-01",
//          endDate = Some("2020-04-01"),
//          amount = Some(2000.00),
//          taxPaid = Some(2132.22),
//          submittedOn = Some("2019-04-04T01:01:01Z")
//        )
//      ),
//      bereavementAllowance = CustomerAddedBenefit(
//        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//        startDate = "2020-01-01",
//        endDate = Some("2020-04-01"),
//        amount = Some(2000.00),
//        taxPaid = None,
//        submittedOn = Some("2019-04-04T01:01:01Z")
//      ),
//      otherStateBenefits = CustomerAddedBenefit(
//        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
//        startDate = "2020-01-01",
//        endDate = Some("2020-04-01"),
//        amount = Some(2000.00),
//        taxPaid = None,
//        submittedOn = Some("2019-04-04T01:01:01Z")
//      )
//    )
//  )

  class Test extends MockHttpClient with MockAppConfig {

    val connector: ListBenefitsConnector = new ListBenefitsConnector(
      http = mockHttpClient,
      appConfig = mockAppConfig
    )

    val desRequestHeaders: Seq[(String, String)] = Seq(
      "Environment" -> "des-environment",
      "Authorization" -> s"Bearer des-token"
    )

    MockedAppConfig.desBaseUrl returns baseUrl
    MockedAppConfig.desToken returns "des-token"
    MockedAppConfig.desEnvironment returns "des-environment"
  }

  "ListBenefitsConnector" when {
    "listBenefits" must {
      "return a 200 status for a success scenario" in new Test {

        val outcome = Right(ResponseWrapper(correlationId, validResponse))

        MockedHttpClient
          .get(
            url = s"$baseUrl/income-tax/income/state-benefits/$nino/$taxYear",
            requiredHeaders = desRequestHeaders: _*
          )
          .returns(Future.successful(outcome))

        await(connector.listBenefits(request)) shouldBe outcome
      }
    }
  }
}
