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

package v1.models.response.listBenefits

import cats.Functor
import config.AppConfig
import play.api.libs.json._
import utils.JsonUtils
import v1.hateoas.{HateoasLinks, HateoasListLinksFactory}
import v1.models.hateoas.{HateoasData, Link}

case class ListBenefitsResponse[B](stateBenefits: Option[Seq[B]],
                                customerAddedStateBenefits: Option[Seq[B]])

object ListBenefitsResponse extends HateoasLinks with JsonUtils {

  implicit object ListBenefitsLinksFactory extends HateoasListLinksFactory[ListBenefitsResponse, StateBenefit, ListBenefitsHateoasData] {

    //noinspection ScalaStyle
    override def itemLinks(appConfig: AppConfig, data: ListBenefitsHateoasData, stateBenefit: StateBenefit): Seq[Link] = {
      import data._

      val retrieveLink = retrieveSingleBenefit(appConfig, nino, taxYear, stateBenefit.benefitId)
      val updateAmountsLink = updateBenefitAmounts(appConfig, nino, taxYear, stateBenefit.benefitId)
      val deleteAmountsLink = deleteBenefitAmounts(appConfig, nino, taxYear, stateBenefit.benefitId)
      val deleteLink = deleteBenefit(appConfig, nino, taxYear, stateBenefit.benefitId)
      val updateLink = updateBenefit(appConfig, nino, taxYear, stateBenefit.benefitId)
      val ignoreLink = ignoreBenefit(appConfig, nino, taxYear, stateBenefit.benefitId)
      val unignoreLink = unignoreBenefit(appConfig, nino, taxYear, stateBenefit.benefitId)

      val commonLinks = Seq(retrieveLink, updateAmountsLink)

      val hmrcLinks = if(stateBenefit.dateIgnored.isEmpty){
        commonLinks :+ ignoreLink
      }else{
        commonLinks :+ unignoreLink
      }

      // Pattern matching based on benefit amounts, duplicate/common benefit on both HMRC and CUSTOM
      val links = (stateBenefit.hasAmounts, stateBenefit.isCommon) match {
        case (true, true) if stateBenefit.createdBy == "CUSTOM" => commonLinks :+ deleteAmountsLink
        case (true, false) if stateBenefit.createdBy == "CUSTOM" => commonLinks ++ Seq(deleteAmountsLink, deleteLink, updateLink)
        case (false, false) if stateBenefit.createdBy == "CUSTOM" => commonLinks ++ Seq(deleteLink, updateLink)
        case (_, _) if stateBenefit.createdBy == "HMRC" => hmrcLinks
      }

      // Differentiate the links based on the call list/single by benefitId passed in the request
      // for list only retrieve (self)
      data.benefitId match {
        case None => Seq(retrieveSingleBenefit(appConfig, nino, taxYear, stateBenefit.benefitId))
        case _ => links
      }
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

  implicit def writes[B: Writes]: OWrites[ListBenefitsResponse[B]] = Json.writes[ListBenefitsResponse[B]]

  // Added temporary field "createdBy" to identify the type of state benefits
  // Only used in json reads
  def readJson[T](createdBy: String)(implicit rds: Reads[Seq[T]]): Reads[Seq[T]] = (json: JsValue) => {
    json
      .validate[JsValue]
      .flatMap(
        readJson => {
          Json.toJson(readJson.as[JsObject].fields.flatMap {
            case (field, arr: JsArray) =>
              arr.value.map {
                element =>
                  element.as[JsObject] + ("benefitType" -> Json.toJson(field)) + ("createdBy" -> Json.toJson(createdBy))
              }
            case (field, obj: JsObject) =>
              Seq(obj.as[JsObject] + ("benefitType" -> Json.toJson(field)) + ("createdBy" -> Json.toJson(createdBy)))
            case (_, _) => Seq.empty
          }).validate[Seq[T]]})
  }

  implicit def reads[B: Reads]: Reads[ListBenefitsResponse[B]] = for {
    stateBenefits <- (__ \ "stateBenefits").readNullable(readJson[B](createdBy = "HMRC")).mapEmptySeqToNone
    customerAddedStateBenefits <- (__ \ "customerAddedStateBenefits").readNullable(readJson[B](createdBy = "CUSTOM")).mapEmptySeqToNone
  } yield {
    ListBenefitsResponse[B](stateBenefits, customerAddedStateBenefits)
  }

}

case class ListBenefitsHateoasData(nino: String, taxYear: String, benefitId: Option[String]) extends HateoasData
