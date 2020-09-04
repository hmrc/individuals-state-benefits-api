/*
 * Copyright 2020 HM Revenue & Customs
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

package v1.hateoas

import config.AppConfig
import play.api.libs.json.{JsValue, Json}

trait UpdateHateoasBodies extends HateoasLinks {

  def updateBenefitHateoasBody(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): JsValue = {

    val links = Seq(
      updateBenefit(appConfig, nino, taxYear, benefitId),
      listBenefits(appConfig, nino, taxYear),
      deleteBenefit(appConfig, nino, taxYear, benefitId),
      updateBenefitAmounts(appConfig, nino, taxYear, benefitId)
    )

    Json.obj("links" -> links)
  }

  def updateBenefitAmountsHateoasBody(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): JsValue = {

    val links = Seq(
      listBenefits(appConfig, nino, taxYear),
      updateBenefitAmounts(appConfig, nino, taxYear, benefitId),
      deleteBenefitAmounts(appConfig, nino, taxYear, benefitId)
    )

    Json.obj("links" -> links)
  }
}