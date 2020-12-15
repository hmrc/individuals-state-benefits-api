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

package v1.services

import cats.data.EitherT
import cats.implicits._
import javax.inject.{Inject, Singleton}
import uk.gov.hmrc.http.HeaderCarrier
import utils.Logging
import v1.connectors.UnIgnoreBenefitConnector
import v1.controllers.EndpointLogContext
import v1.models.errors._
import v1.models.outcomes.ResponseWrapper
import v1.models.request.ignoreBenefit.IgnoreBenefitRequest
import v1.support.DesResponseMappingSupport

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UnIgnoreBenefitService @Inject()(connector: UnIgnoreBenefitConnector)
  extends DesResponseMappingSupport with Logging {

  def unIgnoreBenefit(request: IgnoreBenefitRequest)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext,
    logContext: EndpointLogContext,
    correlationId: String): Future[Either[ErrorWrapper, ResponseWrapper[Unit]]] = {

    val result = for {
      desResponseWrapper <- EitherT(connector.unIgnoreBenefit(request)).leftMap(mapDesErrors(desErrorMap))
    } yield desResponseWrapper

    result.value
  }

  private def desErrorMap: Map[String, MtdError] = Map(
    ("INVALID_TAXABLE_ENTITY_ID", NinoFormatError),
    ("INVALID_TAX_YEAR", TaxYearFormatError),
    ("INVALID_BENEFIT_ID", NotFoundError),
    ("INVALID_CORRELATIONID", DownstreamError),
    ("INVALID_PAYLOAD", DownstreamError),
    ("UNIGNORE_FORBIDDEN", RuleUnIgnoreForbiddenError),
    ("MATCHING_RESOURCE_NOT_FOUND", NotFoundError),
    ("NOT_SUPPORTED_TAX_YEAR", RuleTaxYearNotEndedError),
    ("SERVICE_ERROR", DownstreamError),
    ("SERVICE_UNAVAILABLE", DownstreamError)
  )
}
