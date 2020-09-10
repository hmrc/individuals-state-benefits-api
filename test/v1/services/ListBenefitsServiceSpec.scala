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

package v1.services

import uk.gov.hmrc.domain.Nino
import v1.controllers.EndpointLogContext
import v1.mocks.connectors.MockListBenefitsConnector
import v1.models.errors._
import v1.models.outcomes.ResponseWrapper
import v1.models.request.listBenefits.ListBenefitsRequest
import v1.models.response.listBenefits.{CustomerAddedStateBenefits, CustomerAddedBenefit, StateBenefit, ListBenefitsResponse, StateBenefits}

import scala.concurrent.Future

class ListBenefitsServiceSpec extends ServiceSpec {

  private val nino = "AA112233A"
  private val taxYear = "2019"

  private val requestData = ListBenefitsRequest(Nino(nino), taxYear)

  private val validResponse = ListBenefitsResponse(None, None)
//    ListBenefitsResponse(
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


  trait Test extends MockListBenefitsConnector {
    implicit val logContext: EndpointLogContext = EndpointLogContext("c", "ep")

    val service: ListBenefitsService = new ListBenefitsService(connector = mockListBenefitsConnector)
  }

  "ListBenefitsService" when {
    "listBenefits" must {
      "return correct result for a success" in new Test {
        val outcome = Right(ResponseWrapper(correlationId, validResponse))


        MockListBenefitsConnector.listBenefits(requestData)
          .returns(Future.successful(outcome))

        await(service.listBenefits(requestData)) shouldBe outcome
      }

      "map errors according to spec" when {

        def serviceError(desErrorCode: String, error: MtdError): Unit =
          s"a $desErrorCode error is returned from the service" in new Test {

            MockListBenefitsConnector.listBenefits(requestData)
              .returns(Future.successful(Left(ResponseWrapper(correlationId, DesErrors.single(DesErrorCode(desErrorCode))))))

            await(service.listBenefits(requestData)) shouldBe Left(ErrorWrapper(Some(correlationId), error))
          }

        val input = Seq(
          ("INVALID_TAXABLE_ENTITY_ID", NinoFormatError),
          ("INVALID_TAX_YEAR", TaxYearFormatError),
          ("INVALID_BENEFIT_ID", DownstreamError),
          ("INVALID_CORRELATIONID", DownstreamError),
          ("NO_DATA_FOUND", NotFoundError),
          ("INVALID_DATE_RANGE", RuleTaxYearNotSupportedError),
          ("TAX_YEAR_NOT_SUPPORTED", RuleTaxYearNotSupportedError),
          ("SERVER_ERROR", DownstreamError),
          ("SERVICE_UNAVAILABLE", DownstreamError)
        )

        input.foreach(args => (serviceError _).tupled(args))
      }
    }
  }
}
