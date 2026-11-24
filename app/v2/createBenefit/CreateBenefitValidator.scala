/*
 * Copyright 2026 HM Revenue & Customs
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

package v2.createBenefit

import api.controllers.validators.Validator
import api.controllers.validators.resolvers.{ResolveDetailedTaxYear, ResolveNino, ResolveNonEmptyJsonObject}
import api.models.domain.TaxYear
import api.models.errors.MtdError
import cats.data.Validated
import cats.implicits.*
import config.StateBenefitsAppConfig
import play.api.libs.json.JsValue
import CreateBenefitRulesValidator.validateBusinessRules
import v2.createBenefit.model.request.{CreateBenefitRequestBody, CreateBenefitRequestData}

class CreateBenefitValidator(nino: String, taxYear: String, body: JsValue, temporalValidationEnabled: Boolean)(implicit
    stateBenefitsAppConfig: StateBenefitsAppConfig)
    extends Validator[CreateBenefitRequestData] {

  private val resolveJson = new ResolveNonEmptyJsonObject[CreateBenefitRequestBody]()

  private val resolveTaxYear = ResolveDetailedTaxYear(
    minimumTaxYear = TaxYear.ending(stateBenefitsAppConfig.minimumPermittedTaxYear),
    allowIncompleteTaxYear = !temporalValidationEnabled
  )

  def validate: Validated[Seq[MtdError], CreateBenefitRequestData] =
    (
      ResolveNino(nino),
      resolveTaxYear(taxYear),
      resolveJson(body)
    ).mapN(CreateBenefitRequestData.apply).andThen(validateBusinessRules)

}
