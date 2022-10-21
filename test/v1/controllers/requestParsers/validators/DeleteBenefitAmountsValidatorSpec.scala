/*
 * Copyright 2022 HM Revenue & Customs
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

package v1.controllers.requestParsers.validators

import config.AppConfig
import mocks.MockAppConfig
import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormat, DateTimeFormatter}
import support.UnitSpec
import utils.CurrentDateTime
import v1.mocks.MockCurrentDateTime
import v1.models.errors._
import v1.models.request.deleteBenefitAmounts.DeleteBenefitAmountsRawData

class DeleteBenefitAmountsValidatorSpec extends UnitSpec {

  private val validNino      = "AA123456A"
  private val validTaxYear   = "2020-21"
  private val validBenefitId = "b1e8057e-fbbc-47a8-a8b4-78d9f015c253"

  class Test extends MockCurrentDateTime with MockAppConfig {

    implicit val dateTimeProvider: CurrentDateTime = mockCurrentDateTime
    val dateTimeFormatter: DateTimeFormatter       = DateTimeFormat.forPattern("yyyy-MM-dd")

    implicit val appConfig: AppConfig = mockAppConfig

    val validator = new DeleteBenefitAmountsValidator()

    MockCurrentDateTime.getCurrentDate
      .returns(DateTime.parse("2022-07-11", dateTimeFormatter))
      .anyNumberOfTimes()

    MockAppConfig.minimumPermittedTaxYear returns 2021

  }

  "running a validation" should {
    "return no errors" when {
      "a valid request is supplied" in new Test {
        validator.validate(DeleteBenefitAmountsRawData(validNino, validTaxYear, validBenefitId)) shouldBe Nil
      }
    }

    "return NinoFormatError error" when {
      "an invalid nino is supplied" in new Test {
        validator.validate(DeleteBenefitAmountsRawData("A12344A", validTaxYear, validBenefitId)) shouldBe
          List(NinoFormatError)
      }
    }

    "return TaxYearFormatError error" when {
      "an invalid tax year is supplied" in new Test {
        validator.validate(DeleteBenefitAmountsRawData(validNino, "20178", validBenefitId)) shouldBe
          List(TaxYearFormatError)
      }
    }

    "return BenefitIdFormatError error" when {
      "an invalid benefit ID is supplied" in new Test {
        validator.validate(DeleteBenefitAmountsRawData(validNino, validTaxYear, "ABCDE12345FG")) shouldBe
          List(BenefitIdFormatError)
      }
    }

    "return RuleTaxYearRangeInvalidError error" when {
      "an out of range tax year is supplied" in new Test {
        validator.validate(DeleteBenefitAmountsRawData(validNino, "2020-22", validBenefitId)) shouldBe
          List(RuleTaxYearRangeInvalidError)
      }
    }

    "return RuleTaxYearNotSupportedError error" when {
      "a tax year that is not supported is supplied" in new Test {
        validator.validate(DeleteBenefitAmountsRawData(validNino, "2018-19", validBenefitId)) shouldBe
          List(RuleTaxYearNotSupportedError)
      }
    }

    "return multiple errors" when {
      "request supplied has multiple invalid data fields" in new Test {
        validator.validate(DeleteBenefitAmountsRawData("A12344A", "20178", "ABCDE12345FG")) shouldBe
          List(NinoFormatError, TaxYearFormatError, BenefitIdFormatError)
      }
    }
  }

}
