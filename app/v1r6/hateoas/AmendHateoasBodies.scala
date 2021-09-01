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

package v1r6.hateoas

import config.AppConfig
import play.api.libs.json.{JsValue, Json}

trait AmendHateoasBodies extends HateoasLinks {

  def amendBenefitHateoasBody(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): JsValue = {

    val links = Seq(
      updateBenefit(appConfig, nino, taxYear, benefitId),
      retrieveSingleBenefit(appConfig, nino, taxYear, benefitId),
      deleteBenefit(appConfig, nino, taxYear, benefitId),
      updateBenefitAmounts(appConfig, nino, taxYear, benefitId)
    )

    Json.obj("links" -> links)
  }

  def amendBenefitAmountsHateoasBody(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): JsValue = {

    val links = Seq(
      retrieveSingleBenefit(appConfig, nino, taxYear, benefitId),
      updateBenefitAmounts(appConfig, nino, taxYear, benefitId),
      deleteBenefitAmounts(appConfig, nino, taxYear, benefitId)
    )

    Json.obj("links" -> links)
  }
}