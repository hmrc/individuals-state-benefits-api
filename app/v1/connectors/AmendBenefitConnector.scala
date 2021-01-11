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

package v1.connectors

import config.AppConfig
import javax.inject.{Inject, Singleton}
import play.api.http.Status
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import v1.models.request.AmendBenefit.AmendBenefitRequest

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AmendBenefitConnector @Inject()(val http: HttpClient,
                                      val appConfig: AppConfig) extends BaseDesConnector {

  def amendBenefit(request: AmendBenefitRequest)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext,
    correlationId: String): Future[DesOutcome[Unit]] = {

    import v1.connectors.httpparsers.StandardDesHttpParser._
    implicit val successCode: SuccessCode = SuccessCode(Status.CREATED)

    val nino = request.nino
    val taxYear = request.taxYear
    val benefitId = request.benefitId

    put(request.body,
      DesUri[Unit](s"income-tax/income/state-benefits/$nino/$taxYear/custom/$benefitId")
    )
  }
}
