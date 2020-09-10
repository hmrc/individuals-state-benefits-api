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

  implicit val writes: OWrites[ListBenefitsResponse] = Json.writes[ListBenefitsResponse]

  implicit val reads: Reads[ListBenefitsResponse] = for {
    incapacities <- (__ \ "stateBenefits" \\ "incapacityBenefit").readNestedNullable[Seq[StateBenefit]].map {
      case Some(benefits) => benefits.map(ben => ben.copy(benefitType = Some("incapacityBenefit")))
      case _ => Seq.empty[StateBenefit]
    }
    stateBenefits <- (__ \ "stateBenefits" \ "statePension").readNestedNullable[StateBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some("statePension")))
      case _ => None
    }
    statePensionLumpSum <- (__ \ "stateBenefits" \ "statePensionLumpSum").readNestedNullable[StateBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some("statePensionLumpSum")))
      case _ => None
    }
    employmentSupportAllowance <- (__ \ "stateBenefits" \\ "employmentSupportAllowance").readNestedNullable[Seq[StateBenefit]].map {
      case Some(benefits) => benefits.map(ben => ben.copy(benefitType = Some("employmentSupportAllowance")))
      case _ => Seq.empty[StateBenefit]
    }
    jobSeekersAllowance <- (__ \ "stateBenefits" \\ "jobSeekersAllowance").readNestedNullable[Seq[StateBenefit]].map {
      case Some(benefits) => benefits.map(ben => ben.copy(benefitType = Some("jobSeekersAllowance")))
      case _ => Seq.empty[StateBenefit]
    }
    bereavementAllowance <- (__ \ "stateBenefits" \ "bereavementAllowance").readNestedNullable[StateBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some("bereavementAllowance")))
      case _ => None
    }
    otherStateBenefits <- (__ \ "stateBenefits" \ "otherStateBenefits").readNestedNullable[StateBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some("otherStateBenefits")))
      case _ => None
    }

    customerIncapacities <- (__ \ "customerAddedStateBenefits" \\ "incapacityBenefit").readNestedNullable[Seq[CustomerAddedBenefit]].map {
      case Some(benefits) => benefits.map(ben => ben.copy(benefitType = Some("incapacityBenefit")))
      case _ => Seq.empty[CustomerAddedBenefit]
    }
    customerStateBenefits <- (__ \ "customerAddedStateBenefits" \ "statePension").readNestedNullable[CustomerAddedBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some("statePension")))
      case _ => None
    }
    customerStatePensionLumpSum <- (__ \ "customerAddedStateBenefits" \ "statePensionLumpSum").readNestedNullable[CustomerAddedBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some("statePensionLumpSum")))
      case _ => None
    }
    customerEmploymentSupportAllowance <- (__ \ "customerAddedStateBenefits" \\ "employmentSupportAllowance")
      .readNestedNullable[Seq[CustomerAddedBenefit]].map {
      case Some(benefits) => benefits.map(ben => ben.copy(benefitType = Some("employmentSupportAllowance")))
      case _ => Seq.empty[CustomerAddedBenefit]
    }
    customerJobSeekersAllowance <- (__ \ "customerAddedStateBenefits" \\ "jobSeekersAllowance").readNestedNullable[Seq[CustomerAddedBenefit]].map {
      case Some(benefits) => benefits.map(ben => ben.copy(benefitType = Some("jobSeekersAllowance")))
      case _ => Seq.empty[CustomerAddedBenefit]
    }
    customerBereavementAllowance <- (__ \ "customerAddedStateBenefits" \ "bereavementAllowance").readNestedNullable[CustomerAddedBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some("bereavementAllowance")))
      case _ => None
    }
    customerOtherStateBenefits <- (__ \ "customerAddedStateBenefits" \ "otherStateBenefits").readNestedNullable[CustomerAddedBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some("otherStateBenefits")))
      case _ => None
    }
  } yield {

    val sb: Seq[StateBenefit] =
      incapacities ++ stateBenefits ++ statePensionLumpSum ++ employmentSupportAllowance ++
      jobSeekersAllowance ++ bereavementAllowance ++ otherStateBenefits toList

    val customerSb: Seq[CustomerAddedBenefit] =
      customerIncapacities ++ customerStateBenefits ++ customerStatePensionLumpSum ++ customerEmploymentSupportAllowance ++
      customerJobSeekersAllowance ++ customerBereavementAllowance++ customerOtherStateBenefits toList

    (sb, customerSb) match {
      case (Nil, Nil) => ListBenefitsResponse(None, None)
      case (sb, Nil) => ListBenefitsResponse(Some(sb), None)
      case (Nil, customerSb) => ListBenefitsResponse(None, Some(customerSb))
      case _ => ListBenefitsResponse.apply(Some(sb), Some(customerSb))
    }
  }

}

case class ListBenefitsHateoasData(nino: String, taxYear: String) extends HateoasData
