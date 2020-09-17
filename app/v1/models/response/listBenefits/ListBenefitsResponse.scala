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
import cats.Functor
import play.api.libs.json.{__, _}
import utils.JsonUtils
import v1.hateoas.{HateoasLinks, HateoasListLinksFactory}
import v1.models.hateoas.{HateoasData, Link}

case class ListBenefitsResponse[B](stateBenefits: Option[Seq[B]],
                                customerAddedStateBenefits: Option[Seq[B]])

object ListBenefitsResponse extends HateoasLinks with JsonUtils {

  implicit object ListBenefitsLinksFactory extends HateoasListLinksFactory[ListBenefitsResponse, StateBenefit, ListBenefitsHateoasData] {

    override def itemLinks(appConfig: AppConfig, data: ListBenefitsHateoasData, stateBenefit: StateBenefit): Seq[Link] = {
      import data._
      Seq(
        retrieveSingleBenefit(appConfig, nino, taxYear, stateBenefit.benefitId)
      )
    }

    override def links(appConfig: AppConfig, data: ListBenefitsHateoasData): Seq[Link] = {
      import data._
      Seq(
        addBenefit(appConfig, nino, taxYear),
        listBenefits(appConfig, nino, taxYear)
      )
    }
  }

    implicit object ResponseFunctor extends Functor[ListBenefitsResponse] {
      override def map[A, B](fa: ListBenefitsResponse[A])(f: A => B): ListBenefitsResponse[B] =
        ListBenefitsResponse(
          fa.stateBenefits.map(x => x.map(f)), fa.customerAddedStateBenefits.map(y => y.map(f)))
    }

  implicit def writes[B : Writes]: OWrites[ListBenefitsResponse[B]] = new OWrites[ListBenefitsResponse[B]] {
    def writes(response: ListBenefitsResponse[B]): JsObject =
      Json.obj(
        "stateBenefits"-> response.stateBenefits,
        "customerAddedStateBenefits" -> response.customerAddedStateBenefits
      )
  }

  implicit def reads[B]: Reads[ListBenefitsResponse[B]] = for {
    incapacities <- readSeqBenefit("stateBenefits", "incapacityBenefit")
    stateBenefits <- readBenefit("stateBenefits","statePension")
    statePensionLumpSum <- readBenefit("stateBenefits", "statePensionLumpSum")
    employmentSupportAllowance <- readSeqBenefit("stateBenefits", "employmentSupportAllowance")
    jobSeekersAllowance <- readSeqBenefit("stateBenefits", "jobSeekersAllowance")
    bereavementAllowance <- readBenefit("stateBenefits", "bereavementAllowance")
    otherStateBenefits <- readBenefit("stateBenefits", "otherStateBenefits")

    customerIncapacities <- readSeqBenefit("customerAddedStateBenefits", "incapacityBenefit")
    customerStateBenefits <- readBenefit("customerAddedStateBenefits", "statePension")
    customerStatePensionLumpSum <- readBenefit("customerAddedStateBenefits", "statePensionLumpSum")
    customerEmploymentSupportAllowance <- readSeqBenefit("customerAddedStateBenefits", "employmentSupportAllowance")
    customerJobSeekersAllowance <- readSeqBenefit("customerAddedStateBenefits", "jobSeekersAllowance")
    customerBereavementAllowance <- readBenefit("customerAddedStateBenefits", "bereavementAllowance")
    customerOtherStateBenefits <- readBenefit("customerAddedStateBenefits", "otherStateBenefits")
  } yield {

    val sb: Option[Seq[StateBenefit]] =
      Option(incapacities ++ stateBenefits ++ statePensionLumpSum ++ employmentSupportAllowance ++
      jobSeekersAllowance ++ bereavementAllowance ++ otherStateBenefits).filter(_.nonEmpty)

    val customerSb: Option[Seq[StateBenefit]] =
      Option(customerIncapacities ++ customerStateBenefits ++ customerStatePensionLumpSum ++ customerEmploymentSupportAllowance ++
        customerJobSeekersAllowance ++ customerBereavementAllowance++ customerOtherStateBenefits).filter(_.nonEmpty)

    ListBenefitsResponse.apply(sb, customerSb)
  }
  def readSeqBenefit[B](path: String, field: String): Reads[Seq[B]] =
    (__ \ path \\ field).readNestedNullable[Seq[B]].map {
      case Some(benefits: Seq[StateBenefit]) => benefits.map(ben => ben.copy(benefitType = Some(field)))
      case _ => Seq.empty[StateBenefit]
    }
  def readBenefit(path: String, field: String): Reads[Option[StateBenefit]] =
    (__ \ path \ field).readNestedNullable[StateBenefit].map {
      case Some(benefit) => Some(benefit.copy(benefitType = Some(field)))
      case _ => None
    }


}

case class ListBenefitsHateoasData(nino: String, taxYear: String) extends HateoasData
