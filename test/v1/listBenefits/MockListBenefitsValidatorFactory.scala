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

package v1.listBenefits

import shared.controllers.validators.Validator
import shared.models.errors.MtdError
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import v1.listBenefits.model.request.ListBenefitsRequestData

trait MockListBenefitsValidatorFactory extends MockFactory {

  val mockListBenefitsValidatorFactory: ListBenefitsValidatorFactory =
    mock[ListBenefitsValidatorFactory]

  object MockedListBenefitsValidatorFactory {

    def validator(): CallHandler[Validator[ListBenefitsRequestData]] =
      (mockListBenefitsValidatorFactory.validator(_: String, _: String, _: Option[String])).expects(*, *, *)

  }

  def willUseValidator(use: Validator[ListBenefitsRequestData]): CallHandler[Validator[ListBenefitsRequestData]] = {
    MockedListBenefitsValidatorFactory
      .validator()
      .anyNumberOfTimes()
      .returns(use)
  }

  def returningSuccess(result: ListBenefitsRequestData): Validator[ListBenefitsRequestData] =
    new Validator[ListBenefitsRequestData] {
      def validate: Validated[Seq[MtdError], ListBenefitsRequestData] = Valid(result)
    }

  def returning(result: MtdError*): Validator[ListBenefitsRequestData] = returningErrors(result)

  def returningErrors(result: Seq[MtdError]): Validator[ListBenefitsRequestData] =
    new Validator[ListBenefitsRequestData] {
      def validate: Validated[Seq[MtdError], ListBenefitsRequestData] = Invalid(result)
    }

}
