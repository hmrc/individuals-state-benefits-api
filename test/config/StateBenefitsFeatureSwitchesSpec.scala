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

package config

import play.api.Configuration
import shared.config.FeatureSwitchesBehaviour
import shared.utils.UnitSpec


class StateBenefitsFeatureSwitchesSpec extends UnitSpec with FeatureSwitchesBehaviour[StateBenefitsFeatureSwitches] {
  override def featureSwitches(configuration: Configuration): StateBenefitsFeatureSwitches = StateBenefitsFeatureSwitches(configuration)

  "isDesIf_MigrationEnabled" should {
    behave like aFeatureSwitchWithKey("desIf_Migration.enabled", _.isDesIf_MigrationEnabled)
  }

}
