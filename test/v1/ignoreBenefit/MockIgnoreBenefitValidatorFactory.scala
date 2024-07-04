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

package v1.ignoreBenefit

import api.controllers.validators.Validator
import api.models.errors.MtdError
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import v1.ignoreBenefit.def1.model.request.Def1_IgnoreBenefitRequestData

trait MockIgnoreBenefitValidatorFactory extends MockFactory {

  val mockIgnoreBenefitValidatorFactory: IgnoreBenefitValidatorFactory =
    mock[IgnoreBenefitValidatorFactory]

  object MockedIgnoreBenefitValidatorFactory {

    def validator(): CallHandler[Validator[Def1_IgnoreBenefitRequestData]] =
      (mockIgnoreBenefitValidatorFactory.validator(_: String, _: String, _: String)).expects(*, *, *)

  }

  def willUseValidator(use: Validator[Def1_IgnoreBenefitRequestData]): CallHandler[Validator[Def1_IgnoreBenefitRequestData]] = {
    MockedIgnoreBenefitValidatorFactory
      .validator()
      .anyNumberOfTimes()
      .returns(use)
  }

  def returningSuccess(result: Def1_IgnoreBenefitRequestData): Validator[Def1_IgnoreBenefitRequestData] =
    new Validator[Def1_IgnoreBenefitRequestData] {
      def validate: Validated[Seq[MtdError], Def1_IgnoreBenefitRequestData] = Valid(result)
    }

  def returning(result: MtdError*): Validator[Def1_IgnoreBenefitRequestData] = returningErrors(result)

  def returningErrors(result: Seq[MtdError]): Validator[Def1_IgnoreBenefitRequestData] =
    new Validator[Def1_IgnoreBenefitRequestData] {
      def validate: Validated[Seq[MtdError], Def1_IgnoreBenefitRequestData] = Invalid(result)
    }

}
