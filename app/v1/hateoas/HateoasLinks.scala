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
import v1.models.hateoas.Link
import v1.models.hateoas.Method.{PUT, _}
import v1.models.hateoas.RelType.{AMEND_SAMPLE_REL, DELETE_SAMPLE_REL, _}

trait HateoasLinks {

  //Domain URIs
  private def stateBenefitUri(appConfig: AppConfig, nino: String, taxYear: String) =
    s"/${appConfig.apiGatewayContext}/$nino/$taxYear"

  private def stateBenefitWithIDUri(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String) =
    s"/${appConfig.apiGatewayContext}/$nino/$taxYear/$benefitId"

  private def stateBenefitAmountsUri(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String) =
    s"/${appConfig.apiGatewayContext}/$nino/$taxYear/$benefitId/amounts"

  // URI with ID
  private def uriWithId(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String) =
    s"/${appConfig.apiGatewayContext}/state-benefits/$nino/$taxYear/$benefitId"

  private def baseUri(appConfig: AppConfig, nino: String, taxYear: String) =
    s"/${appConfig.apiGatewayContext}/state-benefits/$nino/$taxYear"

  //Sample links
  def addStateBenefit(appConfig: AppConfig, nino: String, taxYear: String): Link =
    Link(
      href = stateBenefitUri(appConfig, nino, taxYear),
      method = POST,
      rel = ADD_STATE_BENEFIT
    )

  def listStateBenefit(appConfig: AppConfig, nino: String, taxYear: String, isSelf: Boolean): Link =
    if (isSelf) {
      Link(
        href = stateBenefitUri(appConfig, nino, taxYear),
        method = GET,
        rel = SELF
      )
    }
    else {
      Link(
        href = stateBenefitUri(appConfig, nino, taxYear),
        method = GET,
        rel = LIST_STATE_BENEFITS
      )
    }

  def updateStateBenefit(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): Link =
    Link(
      href = stateBenefitWithIDUri(appConfig, nino, taxYear, benefitId),
      method = PUT,
      rel = UPDATE_STATE_BENEFIT
    )

  def deleteStateBenefit(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): Link =
    Link(
      href = stateBenefitWithIDUri(appConfig, nino, taxYear, benefitId),
      method = DELETE,
      rel = DELETE_STATE_BENEFIT
    )

  def updateStateBenefitAmounts(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): Link =
    Link(
      href = stateBenefitAmountsUri(appConfig, nino, taxYear, benefitId),
      method = PUT,
      rel = UPDATE_STATE_BENEFIT_AMOUNTS
    )

  def deleteStateBenefitAmounts(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): Link =
    Link(
      href = stateBenefitAmountsUri(appConfig, nino, taxYear, benefitId),
      method = PUT,
      rel = DELETE_STATE_BENEFIT_AMOUNTS
    )

  // State benefits Hateoas

  def updateStateBenefits(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): Link =
    Link(
      href = uriWithId(appConfig, nino, taxYear, benefitId),
      method = PUT,
      rel = UPDATE_STATE_BENEFITS_URL
    )

  def deleteStateBenefits(appConfig: AppConfig, nino: String, taxYear: String, benefitId: String): Link =
    Link(
      href = uriWithId(appConfig, nino, taxYear, benefitId),
      method = DELETE,
      rel = DELETE_STATE_BENEFITS_URL
    )

  def listStateBenefits(appConfig: AppConfig, nino: String, taxYear: String): Link =
    Link(
      href = baseUri(appConfig, nino, taxYear),
      method = GET,
      rel = SELF
    )
}
