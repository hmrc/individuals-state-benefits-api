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

import cats.implicits._
import uk.gov.hmrc.http.HeaderCarrier
import utils.Logging
import v1.connectors.IgnoreBenefitConnector
import v1.controllers.EndpointLogContext
import v1.models.errors._
import v1.models.outcomes.ResponseWrapper
import v1.models.request.ignoreBenefit.IgnoreBenefitRequest
import v1.support.DownstreamResponseMappingSupport

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class IgnoreBenefitService @Inject()(connector: IgnoreBenefitConnector) extends DownstreamResponseMappingSupport with Logging {

  def ignoreBenefit(request: IgnoreBenefitRequest)(implicit
                                                   hc: HeaderCarrier,
                                                   ec: ExecutionContext,
                                                   logContext: EndpointLogContext,
                                                   correlationId: String): Future[Either[ErrorWrapper, ResponseWrapper[Unit]]] = {

    connector.ignoreBenefit(request).map(_.leftMap(mapDownstreamErrors(downstreamErrorMap)))
  }

  private def downstreamErrorMap: Map[String, MtdError] = {
    val errors = Map(
      ("INVALID_TAXABLE_ENTITY_ID", NinoFormatError),
      ("INVALID_TAX_YEAR", TaxYearFormatError),
      ("INVALID_BENEFIT_ID", BenefitIdFormatError),
      ("INVALID_CORRELATIONID", StandardDownstreamError),
      ("INVALID_PAYLOAD", StandardDownstreamError),
      ("IGNORE_FORBIDDEN", RuleIgnoreForbiddenError),
      ("NOT_SUPPORTED_TAX_YEAR", RuleTaxYearNotEndedError),
      ("NO_DATA_FOUND", NotFoundError),
      ("SERVICE_ERROR", StandardDownstreamError),
      ("SERVICE_UNAVAILABLE", StandardDownstreamError)
    )

    val extraTysErrors = Map(
      ("INVALID_CORRELATION_ID", StandardDownstreamError),
      ("TAX_YEAR_NOT_SUPPORTED", RuleTaxYearNotSupportedError)
    )

    errors ++ extraTysErrors
  }

}
