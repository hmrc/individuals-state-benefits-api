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

package v1.models.request.ignoreBenefit

import play.api.libs.json.{JsError, JsObject, JsValue, Json}
import support.UnitSpec

class IgnoreBenefitRequestBodySpec extends UnitSpec {

  val json: JsValue = Json.parse(
    """
      |{
      |  "ignoreBenefit": true
      |}
    """.stripMargin
  )

  val model: IgnoreBenefitRequestBody = IgnoreBenefitRequestBody(ignoreBenefit = true)

  "IgnoreBenefitRequestBody" when {
    "read from valid JSON" should {
      "produce the expected object" in {
        json.as[IgnoreBenefitRequestBody] shouldBe model
      }
    }

    "read from invalid JSON" should {
      "produce a JsError" in {
        JsObject.empty.validate[IgnoreBenefitRequestBody] shouldBe a[JsError]
      }
    }

    "written to JSON" should {
      "produce the expected JsObject" in {
        Json.toJson(model) shouldBe json
      }
    }
  }
}
