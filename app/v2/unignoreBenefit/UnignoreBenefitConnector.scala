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

package v2.unignoreBenefit

import shared.config.{ConfigFeatureSwitches, SharedAppConfig}
import shared.connectors.DownstreamUri._
import shared.connectors.httpparsers.StandardDownstreamHttpParser._
import shared.connectors.{BaseDownstreamConnector, DownstreamOutcome, DownstreamUri}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.client.HttpClientV2
import v2.unignoreBenefit.model.request.UnignoreBenefitRequestData

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class UnignoreBenefitConnector @Inject()(val http: HttpClientV2, val appConfig: SharedAppConfig) extends BaseDownstreamConnector {

  def unignoreBenefit(request: UnignoreBenefitRequestData)(implicit
                                                           hc: HeaderCarrier,
                                                           ec: ExecutionContext,
                                                           correlationId: String): Future[DownstreamOutcome[Unit]] = {

    import request._

    val downstreamUri: DownstreamUri[Unit] =
      if (ConfigFeatureSwitches().isEnabled("ifs_hip_migration_1945")) {
        HipUri[Unit](s"itsd/income/ignore/state-benefits/$nino/$benefitId?taxYear=${taxYear.asTysDownstream}")
      } else {
        IfsUri[Unit](s"income-tax/${taxYear.asTysDownstream}/state-benefits/$nino/ignore/$benefitId")
      }

    delete(downstreamUri)
  }

}
