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

package v1.deleteBenefit

import config.MockStateBenefitsAppConfig
import shared.controllers.validators.Validator
import shared.utils.UnitSpec
import v1.deleteBenefit.def1.Def1_DeleteBenefitValidator
import v1.deleteBenefit.model.request.DeleteBenefitRequestData

class DeleteBenefitValidatorFactorySpec extends UnitSpec with MockStateBenefitsAppConfig {

  private val validNino        = "AA123456A"
  private val validTaxYear     = "2023-24"
  private val invalidTaxYear   = "2023-2"
  private val validBenefitId   = "b1e8057e-fbbc-47a8-a8b4-78d9f015c253"
  private val validatorFactory = new DeleteBenefitValidatorFactory

  "validator" should {
    "return the Def1 validator" when {
      "given a valid request" in new AppConfigTest {
        val result: Validator[DeleteBenefitRequestData] = validatorFactory.validator(validNino, validTaxYear, validBenefitId)
        result shouldBe a[Def1_DeleteBenefitValidator]
      }

      "given an invalid taxYear" in new AppConfigTest {
        val result: Validator[DeleteBenefitRequestData] = validatorFactory.validator(validNino, invalidTaxYear, validBenefitId)
        result shouldBe a[Def1_DeleteBenefitValidator]
      }
    }

  }
}
