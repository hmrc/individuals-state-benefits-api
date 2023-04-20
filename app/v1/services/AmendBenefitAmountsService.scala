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

package v1.services

import api.controllers.RequestContext
import api.models.errors._
import api.services.BaseService
import cats.implicits._
import v1.connectors.AmendBenefitAmountsConnector
import v1.models.request.AmendBenefitAmounts.AmendBenefitAmountsRequest

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AmendBenefitAmountsService @Inject() (connector: AmendBenefitAmountsConnector) extends BaseService {

  def amendBenefitAmounts(
      request: AmendBenefitAmountsRequest)(implicit ctx: RequestContext, ec: ExecutionContext): Future[AmendBenefitAmountsServiceOutcome] = {

    connector.amendBenefitAmounts(request).map(_.leftMap(mapDownstreamErrors(downstreamErrorMap)))

  }

  private def downstreamErrorMap: Map[String, MtdError] = {
    val errors = Map(
      "INCOME_SOURCE_NOT_FOUND"         -> NotFoundError,
      "INVALID_TAXABLE_ENTITY_ID"       -> NinoFormatError,
      "INVALID_TAX_YEAR"                -> TaxYearFormatError,
      "INVALID_BENEFIT_ID"              -> BenefitIdFormatError,
      "INVALID_CORRELATIONID"           -> StandardDownstreamError,
      "INVALID_PAYLOAD"                 -> StandardDownstreamError,
      "INVALID_REQUEST_BEFORE_TAX_YEAR" -> RuleTaxYearNotEndedError,
      "SERVER_ERROR"                    -> StandardDownstreamError,
      "SERVICE_UNAVAILABLE"             -> StandardDownstreamError
    )

    val extraTysErrors = Map(
      "INVALID_CORRELATION_ID" -> StandardDownstreamError,
      "TAX_YEAR_NOT_SUPPORTED" -> RuleTaxYearNotSupportedError
    )

    errors ++ extraTysErrors
  }

}
