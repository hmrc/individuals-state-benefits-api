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

package v1.unignoreBenefit

import api.controllers.RequestContext
import api.models.errors._
import api.services.{BaseService, ServiceOutcome}
import cats.implicits._
import v1.unignoreBenefit.model.request.Def1_UnignoreBenefitRequestData

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UnignoreBenefitService @Inject() (connector: UnignoreBenefitConnector) extends BaseService {

  def unignoreBenefit(request: Def1_UnignoreBenefitRequestData)(implicit ctx: RequestContext, ec: ExecutionContext): Future[ServiceOutcome[Unit]] = {

    connector.unignoreBenefit(request).map(_.leftMap(mapDownstreamErrors(downstreamErrorMap)))

  }

  private val downstreamErrorMap: Map[String, MtdError] = Map(
    ("INVALID_TAXABLE_ENTITY_ID", NinoFormatError),
    ("INVALID_TAX_YEAR", TaxYearFormatError),
    ("INVALID_BENEFIT_ID", BenefitIdFormatError),
    ("CUSTOMER_ADDED", RuleUnignoreForbiddenError),
    ("NO_DATA_FOUND", NotFoundError),
    ("TAX_YEAR_NOT_SUPPORTED", RuleTaxYearNotSupportedError),
    ("BEFORE_TAX_YEAR_ENDED", RuleTaxYearNotEndedError),
    ("SERVICE_ERROR", InternalError),
    ("SERVICE_UNAVAILABLE", InternalError)
  )

}