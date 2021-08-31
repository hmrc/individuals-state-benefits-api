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
import play.api.libs.json.Reads
import uk.gov.hmrc.http.HeaderCarrier
import v1r6.connectors.DownstreamUri.DesUri
import v1r6.connectors.{DeleteRetrieveConnector, DownstreamOutcome}

import scala.concurrent.{ExecutionContext, Future}

trait MockDeleteRetrieveConnector extends MockFactory {

  val mockDeleteRetrieveConnector: DeleteRetrieveConnector = mock[DeleteRetrieveConnector]

  object MockDeleteRetrieveConnector {

    def delete(): CallHandler[Future[DownstreamOutcome[Unit]]] = {
      (mockDeleteRetrieveConnector
        .delete()(_: HeaderCarrier, _: ExecutionContext, _: DesUri[Unit], _: String))
        .expects(*, *, *, *)
    }

    def retrieve[Resp: Reads](): CallHandler[Future[DownstreamOutcome[Resp]]] = {
      (mockDeleteRetrieveConnector
        .retrieve[Resp]()(_: Reads[Resp], _: HeaderCarrier, _: ExecutionContext, _: DesUri[Resp], _: String))
        .expects(*, *, *, *, *)
    }
  }

}
