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

package v1.amendBenefitAmounts

import api.connectors.DownstreamUri._
import api.connectors.httpparsers.StandardDownstreamHttpParser._
import api.connectors.{BaseDownstreamConnector, DownstreamOutcome}
import config.AppConfig
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient}
import v1.amendBenefitAmounts.def1.model.request.Def1_AmendBenefitAmountsRequestData
import v1.amendBenefitAmounts.model.request.AmendBenefitAmountsRequestData

import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class AmendBenefitAmountsConnector @Inject() (val http: HttpClient, val appConfig: AppConfig) extends BaseDownstreamConnector {

  def amendBenefitAmounts(request: AmendBenefitAmountsRequestData)(implicit
                                                                   hc: HeaderCarrier,
                                                                   ec: ExecutionContext,
                                                                   correlationId: String): Future[DownstreamOutcome[Unit]] = {

    import request._

    val downstreamUri =
      if (taxYear.useTaxYearSpecificApi) {
        TaxYearSpecificIfsUri[Unit](s"income-tax/${taxYear.asTysDownstream}/income/state-benefits/$nino/$benefitId")
      } else {
        Api1651Uri[Unit](s"income-tax/income/state-benefits/$nino/${taxYear.asMtd}/$benefitId")
      }

    request match {
      case def1: Def1_AmendBenefitAmountsRequestData =>
        import def1._
        put(body, downstreamUri)
      case _ => throw new IllegalArgumentException("Request type is not known")

    }

  }

}