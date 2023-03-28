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

package v1.models.response.unignoreBenefit

import api.models.hateoas
import api.models.hateoas.Method._
import mocks.MockAppConfig
import support.UnitSpec

class UnignoreBenefitResponseSpec extends UnitSpec with MockAppConfig {

  "LinksFactory" should {
    "produce the correct links" when {
      "called" in {
        val data: UnignoreBenefitHateoasData = UnignoreBenefitHateoasData("mynino", "mytaxyear", "mybenefitid")

        MockedAppConfig.apiGatewayContext.returns("my/context").anyNumberOfTimes()

        UnignoreBenefitResponse.UnignoreBenefitLinksFactory.links(mockAppConfig, data) shouldBe Seq(
          hateoas.Link(href = s"/my/context/${data.nino}/${data.taxYear}?benefitId=${data.benefitId}", method = GET, rel = "self"),
          hateoas.Link(href = s"/my/context/${data.nino}/${data.taxYear}/${data.benefitId}/ignore", method = POST, rel = "ignore-state-benefit")
        )
      }
    }
  }

}