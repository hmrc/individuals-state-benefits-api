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

package v1r6.mocks.connectors

import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier
import v1r6.connectors.{DownstreamOutcome, UnignoreBenefitConnector}
import v1r6.models.request.ignoreBenefit.IgnoreBenefitRequest

import scala.concurrent.{ExecutionContext, Future}

trait MockUnignoreBenefitConnector extends MockFactory {

  val mockUnignoreBenefitConnector: UnignoreBenefitConnector = mock[UnignoreBenefitConnector]

  object MockUnignoreBenefitConnector {

    def unignoreBenefit(request: IgnoreBenefitRequest): CallHandler[Future[DownstreamOutcome[Unit]]] = {
      (mockUnignoreBenefitConnector
        .unignoreBenefit(_: IgnoreBenefitRequest)(_: HeaderCarrier, _: ExecutionContext, _: String))
        .expects(request, *, *, *)
    }
  }

}