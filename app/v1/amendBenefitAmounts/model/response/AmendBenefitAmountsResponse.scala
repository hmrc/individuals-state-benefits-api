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

package v1.amendBenefitAmounts.model.response

import shared.config.SharedAppConfig
import shared.hateoas.{HateoasData, HateoasLinksFactory, Link}
import v1.HateoasLinks

object AmendBenefitAmountsResponse extends HateoasLinks {

  implicit object AmendBenefitAmountsLinksFactory extends HateoasLinksFactory[Unit, AmendBenefitAmountsHateoasData] {

    override def links(appConfig: SharedAppConfig, data: AmendBenefitAmountsHateoasData): Seq[Link] = {
      import data._
      Seq(
        listSingleBenefit(appConfig, nino, taxYear, benefitId),
        amendBenefitAmounts(appConfig, nino, taxYear, benefitId),
        deleteBenefitAmounts(appConfig, nino, taxYear, benefitId)
      )
    }

  }

}

case class AmendBenefitAmountsHateoasData(nino: String, taxYear: String, benefitId: String) extends HateoasData
