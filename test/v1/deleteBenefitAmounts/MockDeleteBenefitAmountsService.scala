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
import shared.controllers.RequestContext
import shared.models.errors.ErrorWrapper
import shared.models.outcomes.ResponseWrapper
import v1.deleteBenefitAmounts.def1.model.request.Def1_DeleteBenefitAmountsRequestData

import scala.concurrent.{ExecutionContext, Future}

trait MockDeleteBenefitAmountsService extends MockFactory {

  val mockDeleteBenefitAmountsService: DeleteBenefitAmountsService =
    mock[DeleteBenefitAmountsService]

  object MockDeleteBenefitAmountsService {

    def deleteBenefitAmounts(requestData: Def1_DeleteBenefitAmountsRequestData): CallHandler[Future[Either[ErrorWrapper, ResponseWrapper[Unit]]]] = (
      mockDeleteBenefitAmountsService
        .delete(_: Def1_DeleteBenefitAmountsRequestData)(
          _: RequestContext,
          _: ExecutionContext
        )
      )
      .expects(requestData, *, *)

  }

}
