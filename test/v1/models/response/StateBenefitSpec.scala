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

import play.api.libs.json.{JsObject, JsValue, Json}
import support.UnitSpec

class StateBenefitSpec extends UnitSpec {

  val json =
    """{
      |	"stateBenefits": {
      |		"incapacityBenefit": [{
      |			"dateIgnored": "2019-04-04T01:01:01Z",
      |			"benefitId": "9d51a3eb-e374-5349-aa02-96db92561138",
      |			"startDate": "2020-01-01",
      |			"endDate": "2020-04-01",
      |			"amount": 34345.55,
      |			"taxPaid": 345.55
      |		}],
      |		"statePension": {
      |			"benefitId": "d65d6853-8455-4bfa-9995-746354320840",
      |			"startDate": "2019-08-01",
      |			"amount": 9583.44
      |		},
      |		"statePensionLumpSum": {
      |			"benefitId": "e4bf331e-0e55-297f-9a74-c766674775d4",
      |			"startDate": "2019-09-01",
      |			"endDate": "2019-09-02",
      |			"amount": 345.55,
      |			"taxPaid": 78.88
      |		},
      |		"employmentSupportAllowance": [{
      |			"benefitId": "61152305-5d52-2329-9b18-7ab8383313bd",
      |			"startDate": "2020-02-12",
      |			"endDate": "2020-04-03",
      |			"amount": 1230.30,
      |			"taxPaid": 506.66
      |		}],
      |		"jobSeekersAllowance": [{
      |			"benefitId": "412b6427-0142-500f-828d-d2a44eeffd9e",
      |			"startDate": "2020-02-05",
      |			"endDate": "2020-03-17",
      |			"amount": 284.33,
      |			"taxPaid": 11.23
      |		}],
      |		"bereavementAllowance": {
      |			"benefitId": "ffbe2472-cc9c-5171-bf73-005882109f32",
      |			"startDate": "2020-03-12",
      |			"endDate": "2020-04-01",
      |			"amount": 39234.44
      |		},
      |		"otherStateBenefits": {
      |			"benefitId": "f6457da0-546f-53bf-86f2-fbb83a76eb44",
      |			"startDate": "2020-01-13",
      |			"endDate": "2020-02-21",
      |			"amount": 403.33
      |		}
      |	}
      |}""".stripMargin

  "AddBenefitResponse" when {
    "read from valid JSON" should {
      "produce the expected AddBenefitResponse object" in {
        val result = Json.parse(json).as[JsObject].fields.map {
          case (field, arr: Seq[JsValue]) => println(" ----------------- "+field)
            arr.map(element => {
              element.as[JsObject] + ("benefitType" -> Json.toJson(field))})
          case (field, obj: JsValue) => obj.as[JsObject] + ("benefitType" -> Json.toJson(field))
        }

        println(result)
      }
    }
  }
}
