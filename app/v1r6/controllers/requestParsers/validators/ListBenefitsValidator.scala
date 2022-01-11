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

package v1r6.controllers.requestParsers.validators

import config.AppConfig
import javax.inject.Inject
import v1r6.controllers.requestParsers.validators.validations._
import v1r6.models.errors.MtdError
import v1r6.models.request.listBenefits.ListBenefitsRawData

class ListBenefitsValidator @Inject()(implicit appConfig: AppConfig)
  extends Validator[ListBenefitsRawData] {

  private val validationSet = List(parameterFormatValidation, parameterRuleValidation)

  override def validate(data: ListBenefitsRawData): List[MtdError] = {
    run(validationSet, data).distinct
  }

  private def parameterFormatValidation: ListBenefitsRawData => List[List[MtdError]] = (data: ListBenefitsRawData) => {
    List(
      NinoValidation.validate(data.nino),
      TaxYearValidation.validate(data.taxYear),
      data.benefitId.map(BenefitIdValidation.validate).getOrElse(Nil)
    )
  }

  private def parameterRuleValidation: ListBenefitsRawData => List[List[MtdError]] = (data: ListBenefitsRawData) => {
    List(
      TaxYearNotSupportedValidation.validate(data.taxYear)
    )
  }
}