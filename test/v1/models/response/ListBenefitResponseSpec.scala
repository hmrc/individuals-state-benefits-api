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

package v1.models.response

import play.api.libs.json.Json
import support.UnitSpec
import v1.models.response.listBenefits.{CustomerAddedStateBenefits, CustomerIncapacityBenefit, IncapacityBenefit, StateBenefits}

class ListBenefitResponseSpec extends UnitSpec {

  val json = Json.parse(
    """
      |{
      |  "stateBenefits": {
      |    "incapacityBenefit": [
      |    {
      |      "dateIgnored": "2019-04-04T01:01:01Z",
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2020-01-01",
      |      "endDate": "2020-04-01",
      |      "amount": 2000.00,
      |      "taxPaid": 2132.22
      |     }
      |    ],
      |    "statePension": {
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2019-01-01",
      |      "amount": 2000.00
      |    },
      |    "statePensionLumpSum": {
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2019-01-01",
      |      "endDate"  : "2019-01-01",
      |      "amount": 2000.00,
      |      "taxPaid": 2132.22
      |    },
      |    "employmentSupportAllowance": [
      |      {
      |        "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |        "startDate": "2020-01-01",
      |        "endDate": "2020-04-01",
      |        "amount": 2000.00,
      |        "taxPaid": 2132.22
      |      }
      |    ],
      |    "jobSeekersAllowance": [
      |      {
      |        "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |        "startDate": "2020-01-01",
      |        "endDate": "2020-04-01",
      |        "amount": 2000.00,
      |        "taxPaid": 2132.22
      |      }
      |    ],
      |    "bereavementAllowance": {
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2020-01-01",
      |      "endDate": "2020-04-01",
      |      "amount": 2000.00
      |    },
      |    "otherStateBenefits": {
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2020-01-01",
      |      "endDate": "2020-04-01",
      |      "amount": 2000.00
      |    }
      |  },
      |  "customerAddedStateBenefits": {
      |    "incapacityBenefit": [
      |      {
      |        "submittedOn": "2019-04-04T01:01:01Z",
      |        "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |        "startDate": "2020-01-01",
      |        "endDate": "2020-04-01",
      |        "amount": 2000.00,
      |        "taxPaid": 2132.22
      |      }
      |    ],
      |    "statePension": {
      |      "submittedOn": "2019-04-04T01:01:01Z",
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2019-01-01",
      |      "amount": 2000.00
      |    },
      |    "statePensionLumpSum": {
      |      "submittedOn": "2019-04-04T01:01:01Z",
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2019-01-01",
      |      "endDate" : "2019-01-01",
      |      "amount": 2000.00,
      |      "taxPaid": 2132.22
      |    },
      |    "employmentSupportAllowance": [
      |      {
      |        "submittedOn": "2019-04-04T01:01:01Z",
      |        "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |        "startDate": "2020-01-01",
      |        "endDate": "2020-04-01",
      |        "amount": 2000.00,
      |        "taxPaid": 2132.22
      |      }
      |    ],
      |    "jobSeekersAllowance": [
      |      {
      |        "submittedOn": "2019-04-04T01:01:01Z",
      |        "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |        "startDate": "2020-01-01",
      |        "endDate": "2020-04-01",
      |        "amount": 2000.00,
      |        "taxPaid": 2132.22
      |      }
      |    ],
      |    "bereavementAllowance": {
      |      "submittedOn": "2019-04-04T01:01:01Z",
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2020-01-01",
      |      "endDate": "2020-04-01",
      |      "amount": 2000.00
      |    },
      |    "otherStateBenefits": {
      |      "submittedOn": "2019-04-04T01:01:01Z",
      |      "benefitId": "f0d83ac0-a10a-4d57-9e41-6d033832779f",
      |      "startDate": "2020-01-01",
      |      "endDate": "2020-04-01",
      |      "amount": 2000.00
      |    }
      |  }
      |}
      |""".stripMargin
  )

  val model: ListBenefitResponse = ListBenefitResponse(
    stateBenefits = StateBenefits(
      incapacityBenefit = Seq(
        IncapacityBenefit(
          dateIgnored = Some("2019-04-04T01:01:01Z"),
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = None
        )
      ),
      statePension = IncapacityBenefit(
        dateIgnored = None,
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2019-01-01",
        endDate = None,
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = None
      ),
      statePensionLumpSum = IncapacityBenefit(
        dateIgnored = None,
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2019-01-01",
        endDate = Some("2019-01-01"),
        amount = Some(2000.00),
        taxPaid = Some(2132.22),
        submittedOn = None
      ),
      employmentSupportAllowance = Seq(
        IncapacityBenefit(
          dateIgnored = None,
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = None
        )
      ),
      jobSeekersAllowance = Seq(
        IncapacityBenefit(
          dateIgnored = None,
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = None
        )
      ),
      bereavementAllowance = IncapacityBenefit(
        dateIgnored = None,
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2020-01-01",
        endDate = Some("2020-04-01"),
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = None
      ),
      otherStateBenefits = IncapacityBenefit(
        dateIgnored = None,
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2020-01-01",
        endDate = Some("2020-04-01"),
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = None
      )
    ),
    customerAddedStateBenefits = CustomerAddedStateBenefits(
      incapacityBenefit = Seq(
        CustomerIncapacityBenefit(
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = Some("2019-04-04T01:01:01Z")
        )
      ),
      statePension = CustomerIncapacityBenefit(
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2019-01-01",
        endDate = None,
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = Some("2019-04-04T01:01:01Z")
      ),
      statePensionLumpSum = CustomerIncapacityBenefit(
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2019-01-01",
        endDate = Some("2019-01-01"),
        amount = Some(2000.00),
        taxPaid = Some(2132.22),
        submittedOn = Some("2019-04-04T01:01:01Z")
      ),
      employmentSupportAllowance = Seq(
        CustomerIncapacityBenefit(
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = Some("2019-04-04T01:01:01Z")
        )
      ),
      jobSeekersAllowance = Seq(
        CustomerIncapacityBenefit(
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = Some("2019-04-04T01:01:01Z")
        )
      ),
      bereavementAllowance = CustomerIncapacityBenefit(
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2020-01-01",
        endDate = Some("2020-04-01"),
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = Some("2019-04-04T01:01:01Z")
      ),
      otherStateBenefits = CustomerIncapacityBenefit(
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2020-01-01",
        endDate = Some("2020-04-01"),
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = Some("2019-04-04T01:01:01Z")
      )
    )
  )

  "ListBenefitResponse" when {
    "read from valid JSON" should {
      "produce the expected ListBenefitResponse object" in {

        json.as[ListBenefitResponse] shouldBe model
      }
    }

    "written to JSON" should {
      "produce the expected JsObject" in {

        Json.toJson(model) shouldBe json
      }
    }
  }
}
