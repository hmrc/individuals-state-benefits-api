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

import v1.models.errors.MtdError
import config.AppConfig
import v1.controllers.requestParsers.validators.validations.{BenefitIdValidation, NinoValidation, TaxYearNotSupportedValidation, TaxYearValidation}
import v1.models.request.deleteBenefitAmounts.DeleteBenefitAmountsRawData

import javax.inject.{Inject, Singleton}

@Singleton
class DeleteBenefitAmountsValidator @Inject()(implicit appConfig: AppConfig)
  extends Validator[DeleteBenefitAmountsRawData] {

  private val validationSet = List(parameterValidation)

  override def validate(data: DeleteBenefitAmountsRawData): List[MtdError] = {
    run(validationSet, data).distinct
  }

  private def parameterValidation: DeleteBenefitAmountsRawData => List[List[MtdError]] =
    (data: DeleteBenefitAmountsRawData) => {
      implicit val minimumTaxYear: Int = appConfig.minimumPermittedTaxYear

      List(
        NinoValidation.validate(data.nino),
        TaxYearValidation.validate(data.taxYear),
        TaxYearNotSupportedValidation.validate(data.taxYear),
        BenefitIdValidation.validate(data.benefitId)
      )
    }
}