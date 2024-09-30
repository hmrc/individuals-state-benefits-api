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

package v1.listBenefits.model.response

import play.api.libs.json.{JsObject, JsValue, Json}
import shared.models.domain.Timestamp
import shared.models.utils.JsonErrorValidators
import shared.utils.UnitSpec

class HMRCStateBenefitSpec extends UnitSpec with JsonErrorValidators {

  val json: JsValue = Json.parse("""{
      |     "benefitType": "incapacityBenefit",
      |		"dateIgnored": "2019-04-04T01:01:01.000Z",
      |     "submittedOn": "2019-04-04T01:01:01.000Z",
      |		"benefitId": "9d51a3eb-e374-5349-aa02-96db92561138",
      |		"startDate": "2020-01-01",
      |		"endDate": "2020-04-01",
      |		"amount": 34345.55,
      |		"taxPaid": 345.55
      |}""".stripMargin)

  val model: HMRCStateBenefit = HMRCStateBenefit(
    benefitType = "incapacityBenefit",
    dateIgnored = Some(Timestamp("2019-04-04T01:01:01.000Z")),
    submittedOn = Some(Timestamp("2019-04-04T01:01:01.000Z")),
    benefitId = "9d51a3eb-e374-5349-aa02-96db92561138",
    startDate = "2020-01-01",
    endDate = Some("2020-04-01"),
    amount = Some(34345.55),
    taxPaid = Some(345.55)
  )

  testJsonFields[HMRCStateBenefit](json)(
    mandatoryFields = Seq(
      "benefitType",
      "benefitId",
      "startDate"
    ),
    optionalFields = Seq(
      "dateIgnored",
      "submittedOn",
      "endDate",
      "amount",
      "taxPaid"
    )
  )

  "StateBenefits" when {
    "read from valid JSON" should {
      "produce the expected CustomerStateBenefit object" in {
        json.as[HMRCStateBenefit] shouldBe model
      }
    }

    "writes from valid object" should {
      "produce the expected json" in {
        Json.toJson(model) shouldBe json.as[JsObject]
      }
    }
  }

}
