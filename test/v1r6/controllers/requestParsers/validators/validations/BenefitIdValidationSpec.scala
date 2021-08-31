/*
 * Copyright 2021 HM Revenue & Customs
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

package v1r6.controllers.requestParsers.validators.validations

import support.UnitSpec
import v1r6.models.errors.BenefitIdFormatError

class BenefitIdValidationSpec extends UnitSpec {

  "BenefitIdValidation" when {
    "validate" should {
      "return an empty list for a valid benefit ID" in {
        BenefitIdValidation.validate("b1e8057e-fbbc-47a8-a8b4-78d9f015c253") shouldBe NoValidationErrors
      }

      "return an BenefitIdFormatError error for an invalid benefit ID" in {
        BenefitIdValidation.validate("") shouldBe List(BenefitIdFormatError)
      }
    }
  }
}
