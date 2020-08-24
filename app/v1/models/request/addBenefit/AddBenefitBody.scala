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

package v1.models.request.addBenefit

import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import v1.models.domain.BenefitType

case class AddBenefitBody(benefitType: BenefitType, startDate: String, endDate: Option[String])

object AddBenefitBody {

  implicit val reads: Reads[AddBenefitBody] = (
    (JsPath \ "benefitType").read[BenefitType] and
      (JsPath \ "startDate").read[String] and
      (JsPath \ "endDate").readNullable[String]
    ) (AddBenefitBody.apply _)


  implicit val writes: Writes[AddBenefitBody] = Json.writes[AddBenefitBody]
}
