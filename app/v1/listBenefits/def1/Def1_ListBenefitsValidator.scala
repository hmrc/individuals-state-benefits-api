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

package v1.listBenefits.def1

import api.controllers.validators.Validator
import api.controllers.validators.resolvers.{DetailedResolveTaxYear, ResolveNino}
import api.models.errors.MtdError
import cats.data.Validated
import cats.data.Validated._
import cats.implicits._
import v1.controllers.validators.minimumPermittedTaxYear
import v1.controllers.validators.resolvers.ResolveBenefitId
import v1.listBenefits.model.request.ListBenefitsRequestData

import javax.inject.Singleton

@Singleton
class Def1_ListBenefitsValidator(nino: String, taxYear: String, benefitId: Option[String]) extends Validator[ListBenefitsRequestData] {

  private val resolveTaxYear = DetailedResolveTaxYear(maybeMinimumTaxYear = Some(minimumPermittedTaxYear.year))

      def validate: Validated[Seq[MtdError], ListBenefitsRequestData] = {
        (
          ResolveNino(nino),
          resolveTaxYear(taxYear),
          ResolveBenefitId(benefitId)
        ) mapN ListBenefitsRequestData
      }
}
