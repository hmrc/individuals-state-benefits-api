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

package v1.createBenefit.model.response

import play.api.libs.json.{JsValue, Json}
import support.UnitSpec

class CreateBenefitResponseSpec extends UnitSpec {

  val model: CreateBenefitResponse = CreateBenefitResponse("b1e8057e-fbbc-47a8-a8b4-78d9f015c258")

  "CreateBenefitResponse" when {
    "read from valid JSON" should {
      "produce the expected CreateBenefitResponse object" in {
        val json: JsValue = Json.parse(
          """
            |{
            |  "benefitId": "b1e8057e-fbbc-47a8-a8b4-78d9f015c258"
            |}
          """.stripMargin
        )

        json.as[CreateBenefitResponse] shouldBe model
      }
    }

    "written to JSON" should {
      "produce the expected JsObject" in {
        val json: JsValue = Json.parse("""
            |{
            |  "benefitId": "b1e8057e-fbbc-47a8-a8b4-78d9f015c258"
            |}
          """.stripMargin)

        Json.toJson(model) shouldBe json
      }
    }
  }

}
