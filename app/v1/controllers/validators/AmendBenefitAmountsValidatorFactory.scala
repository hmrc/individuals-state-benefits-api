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

package v1.controllers.validators

import api.controllers.validators.Validator
import api.controllers.validators.resolvers.{DetailedResolveTaxYear, ResolveBenefitId, ResolveNino, ResolveNonEmptyJsonObject, ResolveParsedNumber}
import api.models.domain.TaxYear
import api.models.errors.MtdError
import cats.data.Validated
import cats.data.Validated.Valid
import cats.implicits.{catsSyntaxTuple4Semigroupal, toFoldableOps}
import play.api.libs.json.JsValue
import v1.models.request.amendBenefitAmounts.{AmendBenefitAmountsRequestBody, AmendBenefitAmountsRequestData}

import javax.inject.Singleton
import scala.annotation.nowarn

@Singleton
class AmendBenefitAmountsValidatorFactory {

  @nowarn("cat=lint-byname-implicit")
  private val resolveJson = new ResolveNonEmptyJsonObject[AmendBenefitAmountsRequestBody]()

  private val resolveTaxYear = DetailedResolveTaxYear(maybeMinimumTaxYear = Some(TaxYear.minimumPermittedTaxYear.year))

  private val resolveAmountNumber = ResolveParsedNumber()
  private val resolveTaxPaid      = ResolveParsedNumber(min = -99999999999.99)

  def validator(nino: String, taxYear: String, benefitId: String, body: JsValue): Validator[AmendBenefitAmountsRequestData] =
    new Validator[AmendBenefitAmountsRequestData] {

      def validate: Validated[Seq[MtdError], AmendBenefitAmountsRequestData] =
        (
          ResolveNino(nino),
          resolveTaxYear(taxYear),
          ResolveBenefitId(benefitId),
          resolveJson(body)
        ).mapN(AmendBenefitAmountsRequestData) andThen validateBusinessRules

      private def validateBusinessRules(parsed: AmendBenefitAmountsRequestData): Validated[Seq[MtdError], AmendBenefitAmountsRequestData] = {
        import parsed.body._

        List(
          resolveAmountNumber(amount, path = Some("/amount")),
          taxPaid.map(resolveTaxPaid(_, path = Some("/taxPaid"))).getOrElse(Valid(()))
        ).traverse_(identity)
          .map(_ => parsed)
      }

    }

}
