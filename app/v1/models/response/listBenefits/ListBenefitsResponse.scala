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

package v1.models.response.listBenefits

import config.AppConfig
import play.api.libs.json.{__, _}
import utils.JsonUtils
import v1.hateoas.{HateoasLinks, HateoasLinksFactory}
import v1.models.hateoas.{HateoasData, Link}

case class ListBenefitsResponse(stateBenefits: Option[Seq[StateBenefit]],
                                customerAddedStateBenefits: Option[Seq[CustomerAddedBenefit]])

object ListBenefitsResponse extends HateoasLinks with JsonUtils {

  implicit object ListBenefitsLinksFactory extends HateoasLinksFactory[ListBenefitsResponse, ListBenefitsHateoasData] {
    override def links(appConfig: AppConfig, data: ListBenefitsHateoasData): Seq[Link] = {
      import data._
      Seq(
        addBenefit(appConfig, nino, taxYear),
        listBenefits(appConfig, nino, taxYear)
      )
    }
  }

  implicit class StateBenefitsReads(jsPath: JsPath) {

    def readSeqBenefit(field: String): Reads[Seq[StateBenefit]] =
      jsPath.readNestedNullable[Seq[StateBenefit]].map {
        case Some(benefits: Seq[StateBenefit]) => benefits.map(ben => ben.copy(benefitType = Some(field)))
        case _ => Seq.empty[StateBenefit]
      }
    def readBenefit(field: String): Reads[Option[StateBenefit]] =
      jsPath.readNestedNullable[StateBenefit].map {
        case Some(benefit) => Some(benefit.copy(benefitType = Some(field)))
        case _ => None
      }
    def readSeqCustomerBenefit(field: String): Reads[Seq[CustomerAddedBenefit]] =
      jsPath.readNestedNullable[Seq[CustomerAddedBenefit]].map {
        case Some(benefits: Seq[CustomerAddedBenefit]) => benefits.map(ben => ben.copy(benefitType = Some(field)))
        case _ => Seq.empty[CustomerAddedBenefit]
      }
    def readCustomBenefit(field: String): Reads[Option[CustomerAddedBenefit]] =
      jsPath.readNestedNullable[CustomerAddedBenefit].map {
        case Some(benefit) => Some(benefit.copy(benefitType = Some(field)))
        case _ => None
      }

  }

  implicit val writes: OWrites[ListBenefitsResponse] = Json.writes[ListBenefitsResponse]

  implicit val reads: Reads[ListBenefitsResponse] = for {
    incapacities <- (__ \ "stateBenefits" \\ "incapacityBenefit").readSeqBenefit("incapacityBenefit")
    stateBenefits <- (__ \ "stateBenefits" \ "statePension").readBenefit("statePension")
    statePensionLumpSum <- (__ \ "stateBenefits" \ "statePensionLumpSum").readBenefit("statePensionLumpSum")
    employmentSupportAllowance <- (__ \ "stateBenefits" \\ "employmentSupportAllowance").readSeqBenefit("employmentSupportAllowance")
    jobSeekersAllowance <- (__ \ "stateBenefits" \\ "jobSeekersAllowance").readSeqBenefit("jobSeekersAllowance")
    bereavementAllowance <- (__ \ "stateBenefits" \ "bereavementAllowance").readBenefit("bereavementAllowance")
    otherStateBenefits <- (__ \ "stateBenefits" \ "otherStateBenefits").readBenefit("otherStateBenefits")

    customerIncapacities <- (__ \ "customerAddedStateBenefits" \\ "incapacityBenefit").readSeqCustomerBenefit("incapacityBenefit")
    customerStateBenefits <- (__ \ "customerAddedStateBenefits" \ "statePension").readCustomBenefit("statePension")
    customerStatePensionLumpSum <- (__ \ "customerAddedStateBenefits" \ "statePensionLumpSum").readCustomBenefit("statePensionLumpSum")
    customerEmploymentSupportAllowance <-
      (__ \ "customerAddedStateBenefits" \\ "employmentSupportAllowance").readSeqCustomerBenefit("employmentSupportAllowance")
    customerJobSeekersAllowance <- (__ \ "customerAddedStateBenefits" \\ "jobSeekersAllowance").readSeqCustomerBenefit("jobSeekersAllowance")
    customerBereavementAllowance <- (__ \ "customerAddedStateBenefits" \ "bereavementAllowance").readCustomBenefit("bereavementAllowance")
    customerOtherStateBenefits <- (__ \ "customerAddedStateBenefits" \ "otherStateBenefits").readCustomBenefit("otherStateBenefits")
  } yield {

    val sb: Seq[StateBenefit] =
      incapacities ++ stateBenefits ++ statePensionLumpSum ++ employmentSupportAllowance ++
      jobSeekersAllowance ++ bereavementAllowance ++ otherStateBenefits

    val customerSb: Seq[CustomerAddedBenefit] =
      customerIncapacities ++ customerStateBenefits ++ customerStatePensionLumpSum ++ customerEmploymentSupportAllowance ++
        customerJobSeekersAllowance ++ customerBereavementAllowance++ customerOtherStateBenefits

    (sb, customerSb) match {
      case (Nil, Nil) => ListBenefitsResponse(None, None)
      case (sb, Nil) => ListBenefitsResponse(Some(sb), None)
      case (Nil, customerSb) => ListBenefitsResponse(None, Some(customerSb))
      case _ => ListBenefitsResponse.apply(Some(sb), Some(customerSb))
    }
  }

}

case class ListBenefitsHateoasData(nino: String, taxYear: String) extends HateoasData
