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

package v1.listBenefits

import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import shared.controllers.RequestContext
import shared.models.errors.ErrorWrapper
import shared.models.outcomes.ResponseWrapper
import v1.listBenefits.model.request.ListBenefitsRequestData
import v1.listBenefits.model.response.{CustomerStateBenefit, HMRCStateBenefit, ListBenefitsResponse}

import scala.concurrent.{ExecutionContext, Future}

trait MockListBenefitsService extends MockFactory {

  val mockListBenefitsService: ListBenefitsService = mock[ListBenefitsService]

  object MockListBenefitsService {

    def listBenefits(requestData: ListBenefitsRequestData)
    : CallHandler[Future[Either[ErrorWrapper, ResponseWrapper[ListBenefitsResponse[HMRCStateBenefit, CustomerStateBenefit]]]]] = {
      (mockListBenefitsService
        .listBenefits(_: ListBenefitsRequestData)(_: RequestContext, _: ExecutionContext))
        .expects(requestData, *, *)
    }

  }

}
