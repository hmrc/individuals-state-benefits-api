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
import v1r6.models.errors.MtdError

class DateFormatValidationSpec extends UnitSpec {

  "DateFormatValidation.validate" should {
    "return an empty list for a valid date" when {
      object DateError extends MtdError("FORMAT_DATE", "Invalid Date format")
      "valid params are supplied" in {

        DateFormatValidation.validate(
          date = "2019-04-20",
          error = DateError
        ) shouldBe NoValidationErrors
      }

      "return a DateFormatError for an invalid date" in {
        DateFormatValidation.validate(
          date = "2019-04-40",
          error = DateError
        ) shouldBe List(DateError)
      }
    }
  }
}
