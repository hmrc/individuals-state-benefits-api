/*
 * Copyright 2024 HM Revenue & Customs
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

package v2.amendBenefit

import shared.controllers.validators.Validator
import shared.models.errors.MtdError
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import play.api.libs.json.JsValue
import v2.amendBenefit.model.request.AmendBenefitRequestData

trait MockAmendBenefitValidatorFactory extends MockFactory {

  val mockAmendBenefitValidatorFactory: AmendBenefitValidatorFactory =
    mock[AmendBenefitValidatorFactory]

  object MockedAmendBenefitValidatorFactory {

    def validator(): CallHandler[Validator[AmendBenefitRequestData]] =
      (mockAmendBenefitValidatorFactory.validator(_: String, _: String, _: String, _: JsValue)).expects(*, *, *, *)

  }

  def willUseValidator(use: Validator[AmendBenefitRequestData]): CallHandler[Validator[AmendBenefitRequestData]] = {
    MockedAmendBenefitValidatorFactory
      .validator()
      .anyNumberOfTimes()
      .returns(use)
  }

  def returningSuccess(result: AmendBenefitRequestData): Validator[AmendBenefitRequestData] =
    new Validator[AmendBenefitRequestData] {
      def validate: Validated[Seq[MtdError], AmendBenefitRequestData] = Valid(result)
    }

  def returning(result: MtdError*): Validator[AmendBenefitRequestData] = returningErrors(result)

  def returningErrors(result: Seq[MtdError]): Validator[AmendBenefitRequestData] =
    new Validator[AmendBenefitRequestData] {
      def validate: Validated[Seq[MtdError], AmendBenefitRequestData] = Invalid(result)
    }

}