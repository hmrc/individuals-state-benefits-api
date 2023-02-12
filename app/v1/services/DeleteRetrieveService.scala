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

import api.connectors.DownstreamUri
import api.controllers.EndpointLogContext
import api.models.errors._
import api.models.outcomes.ResponseWrapper
import api.support.DownstreamResponseMappingSupport
import cats.implicits._
import play.api.libs.json.Format
import uk.gov.hmrc.http.HeaderCarrier
import utils.Logging
import v1.connectors.DeleteRetrieveConnector

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DeleteRetrieveService @Inject()(connector: DeleteRetrieveConnector) extends DownstreamResponseMappingSupport with Logging {

  def delete(downstreamErrorMap: Map[String, MtdError] = defaultDownstreamErrorMap)(implicit
                                                                                    hc: HeaderCarrier,
                                                                                    ec: ExecutionContext,
                                                                                    logContext: EndpointLogContext,
                                                                                    downstreamUri: DownstreamUri[Unit],
                                                                                    correlationId: String): Future[Either[ErrorWrapper, ResponseWrapper[Unit]]] = {

    connector.delete().map(_.leftMap(mapDownstreamErrors(downstreamErrorMap)))
  }

  def retrieve[Resp: Format](downstreamErrorMap: Map[String, MtdError] = defaultDownstreamErrorMap)(implicit
                                                                                                    hc: HeaderCarrier,
                                                                                                    ec: ExecutionContext,
                                                                                                    logContext: EndpointLogContext,
                                                                                                    downstreamUri: DownstreamUri[Resp],
                                                                                                    correlationId: String): Future[Either[ErrorWrapper, ResponseWrapper[Resp]]] = {

    connector.retrieve[Resp]().map(_.leftMap(mapDownstreamErrors(downstreamErrorMap)))
  }

  private val defaultDownstreamErrorMap: Map[String, MtdError] = Map(
    "INVALID_NINO" -> NinoFormatError,
    "INVALID_TAX_YEAR" -> TaxYearFormatError,
    "NOT_FOUND" -> NotFoundError,
    "SERVER_ERROR" -> StandardDownstreamError,
    "SERVICE_UNAVAILABLE" -> StandardDownstreamError
  )

}
