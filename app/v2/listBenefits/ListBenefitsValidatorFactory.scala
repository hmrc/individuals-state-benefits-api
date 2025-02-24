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

package v2.listBenefits

import config.StateBenefitsAppConfig
import shared.controllers.validators.Validator
import v2.listBenefits.def1.Def1_ListBenefitsValidator
import v2.listBenefits.model.request.ListBenefitsRequestData

import javax.inject.{Inject, Singleton}

@Singleton
class ListBenefitsValidatorFactory @Inject() (implicit stateBenefitsAppConfig: StateBenefitsAppConfig) {

  def validator(nino: String, taxYear: String, benefitId: Option[String]): Validator[ListBenefitsRequestData] =
    new Def1_ListBenefitsValidator(nino: String, taxYear: String, benefitId: Option[String])

}
