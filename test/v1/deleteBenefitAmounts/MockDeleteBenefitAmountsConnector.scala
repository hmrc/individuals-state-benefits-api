/*
 * Copyright 2024 HM Revenue & Customs
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

import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import org.scalatest.TestSuite
import shared.connectors.DownstreamOutcome
import uk.gov.hmrc.http.HeaderCarrier
import v1.deleteBenefitAmounts.model.request.DeleteBenefitAmountsRequestData

import scala.concurrent.{ExecutionContext, Future}

trait MockDeleteBenefitAmountsConnector extends TestSuite with MockFactory {

  val mockDeleteBenefitAmountsConnector: DeleteBenefitAmountsConnector = mock[DeleteBenefitAmountsConnector]

  object MockDeleteBenefitAmountsConnector {

    def deleteBenefitAmounts(request: DeleteBenefitAmountsRequestData): CallHandler[Future[DownstreamOutcome[Unit]]] = {
      (
        mockDeleteBenefitAmountsConnector
          .deleteBenefitAmounts(_: DeleteBenefitAmountsRequestData)(
            _: HeaderCarrier,
            _: ExecutionContext,
            _: String
          )
        )
        .expects(request, *, *, *)
    }

  }

}
