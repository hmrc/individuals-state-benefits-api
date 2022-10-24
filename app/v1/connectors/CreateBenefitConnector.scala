/*
 * Copyright 2022 HM Revenue & Customs
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

package v1.connectors

import config.AppConfig
import play.api.http.Status
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import v1.connectors.DownstreamUri.IfsUri
import v1.models.request.createBenefit.CreateBenefitRequest
import v1.models.response.AddBenefitResponse

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class CreateBenefitConnector @Inject() (val http: HttpClient, val appConfig: AppConfig) extends BaseDownstreamConnector {

  def addBenefit(request: CreateBenefitRequest)(implicit
      hc: HeaderCarrier,
      ec: ExecutionContext,
      correlationId: String): Future[DownstreamOutcome[AddBenefitResponse]] = {

    import v1.connectors.httpparsers.StandardDownstreamHttpParser._
    implicit val successCode: SuccessCode = SuccessCode(Status.OK)

    val nino    = request.nino.nino
    val taxYear = request.taxYear

    post(request.body, IfsUri[AddBenefitResponse](s"income-tax/income/state-benefits/$nino/$taxYear/custom"))
  }

}
