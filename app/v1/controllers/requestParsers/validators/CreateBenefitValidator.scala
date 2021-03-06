/*
 * Copyright 2021 HM Revenue & Customs
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

import config.{AppConfig, FeatureSwitch}
import javax.inject.Inject
import utils.CurrentDateTime
import v1.controllers.requestParsers.validators.validations._
import v1.models.errors.MtdError
import v1.models.request.createBenefit.{CreateBenefitRawData, CreateBenefitRequestBody}

class CreateBenefitValidator @Inject()(implicit currentDateTime: CurrentDateTime, appConfig: AppConfig) extends Validator[CreateBenefitRawData] {

  private val validationSet = List(parameterFormatValidation, parameterRuleValidation, bodyFormatValidation, bodyParameterValidation)

  private def parameterFormatValidation: CreateBenefitRawData => List[List[MtdError]] = (data: CreateBenefitRawData) => {
    List(
      NinoValidation.validate(data.nino),
      TaxYearValidation.validate(data.taxYear),
    )
  }

  private def parameterRuleValidation: CreateBenefitRawData => List[List[MtdError]] = { data =>
    val featureSwitch = FeatureSwitch(appConfig.featureSwitch)

    List(
      TaxYearNotSupportedValidation.validate(data.taxYear),
      if (featureSwitch.isTaxYearNotEndedRuleEnabled) TaxYearNotEndedValidation.validate(data.taxYear) else List.empty[MtdError]
    )
  }

  private def bodyFormatValidation: CreateBenefitRawData => List[List[MtdError]] = { data =>

    List(
      JsonFormatValidation.validate[CreateBenefitRequestBody](data.body.json)
    )
  }

  private def bodyParameterValidation: CreateBenefitRawData => List[List[MtdError]] = { data =>
    val body = data.body.json.as[CreateBenefitRequestBody]

    List(
      BenefitTypeValidation.validate(body.benefitType),
      StateBenefitsDateValidation.validate(body.startDate, body.endDate, data.taxYear)
    )
  }

  override def validate(data: CreateBenefitRawData): List[MtdError] = {
    run(validationSet, data).distinct
  }
}
