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

import api.controllers.EndpointLogContext
import api.models.errors._
import api.models.outcomes.ResponseWrapper
import api.support.DownstreamResponseMappingSupport
import cats.data.EitherT
import cats.implicits._
import uk.gov.hmrc.http.HeaderCarrier
import utils.Logging
import v1.connectors.CreateBenefitConnector
import v1.models.request.createBenefit.CreateBenefitRequest
import v1.models.response.createBenefit.AddBenefitResponse

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CreateBenefitService @Inject()(connector: CreateBenefitConnector) extends DownstreamResponseMappingSupport with Logging {

  def addBenefit(request: CreateBenefitRequest)(implicit
                                                hc: HeaderCarrier,
                                                ec: ExecutionContext,
                                                logContext: EndpointLogContext,
                                                correlationId: String): Future[Either[ErrorWrapper, ResponseWrapper[AddBenefitResponse]]] = {

    val result = for {
      desResponseWrapper <- EitherT(connector.addBenefit(request)).leftMap(mapDownstreamErrors(desErrorMap))
    } yield desResponseWrapper

    result.value
  }

  private def desErrorMap: Map[String, MtdError] =
    Map(
      "INVALID_TAXABLE_ENTITY_ID" -> NinoFormatError,
      "INVALID_TAX_YEAR" -> TaxYearFormatError,
      "INVALID_CORRELATIONID" -> StandardDownstreamError,
      "INVALID_PAYLOAD" -> StandardDownstreamError,
      "BENEFIT_TYPE_ALREADY_EXISTS" -> RuleBenefitTypeExists,
      "NOT_SUPPORTED_TAX_YEAR" -> RuleTaxYearNotEndedError,
      "INVALID_START_DATE" -> RuleStartDateAfterTaxYearEndError,
      "INVALID_CESSATION_DATE" -> RuleEndDateBeforeTaxYearStartError,
      "SERVER_ERROR" -> StandardDownstreamError,
      "SERVICE_UNAVAILABLE" -> StandardDownstreamError
    )

}
