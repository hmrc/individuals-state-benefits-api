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

package v1.deleteBenefitAmounts

import config.StateBenefitsFeatureSwitches
import play.api.http.Status.NO_CONTENT
import shared.config.AppConfig
import shared.connectors.DownstreamUri.{DesUri, IfsUri, TaxYearSpecificIfsUri}
import shared.connectors.{BaseDownstreamConnector, DownstreamOutcome}
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import v1.deleteBenefitAmounts.model.request.DeleteBenefitAmountsRequestData

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DeleteBenefitAmountsConnector @Inject() (val http: HttpClient, val appConfig: AppConfig) extends BaseDownstreamConnector {

  def deleteBenefitAmounts(request: DeleteBenefitAmountsRequestData)(implicit
      hc: HeaderCarrier,
      ec: ExecutionContext,
      correlationId: String): Future[DownstreamOutcome[Unit]] = {

    import shared.connectors.httpparsers.StandardDownstreamHttpParser._

    implicit val successCode: SuccessCode = SuccessCode(NO_CONTENT)

    import request._

    val downstreamUri = {
      if (taxYear.useTaxYearSpecificApi) {
        TaxYearSpecificIfsUri[Unit](s"income-tax/income/state-benefits/${taxYear.asTysDownstream}/$nino/$benefitId")
      } else if (StateBenefitsFeatureSwitches().isDesIf_MigrationEnabled) {
        IfsUri[Unit](s"income-tax/income/state-benefits/$nino/${taxYear.asMtd}/$benefitId")
      } else {
        DesUri[Unit](s"income-tax/income/state-benefits/$nino/${taxYear.asMtd}/$benefitId")
      }
    }

    delete(uri = downstreamUri)
  }

}
