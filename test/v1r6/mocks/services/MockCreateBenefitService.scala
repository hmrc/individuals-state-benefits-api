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

package v1r6.mocks.services

import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import uk.gov.hmrc.http.HeaderCarrier
import v1r6.controllers.EndpointLogContext
import v1r6.models.errors.ErrorWrapper
import v1r6.models.outcomes.ResponseWrapper
import v1r6.models.request.createBenefit.CreateBenefitRequest
import v1r6.models.response.AddBenefitResponse
import v1r6.services.CreateBenefitService

import scala.concurrent.{ExecutionContext, Future}

trait MockCreateBenefitService extends MockFactory {

  val mockCreateStateBenefitService: CreateBenefitService = mock[CreateBenefitService]

  object MockCreateStateBenefitService {

    def createStateBenefit(requestData: CreateBenefitRequest): CallHandler[Future[Either[ErrorWrapper, ResponseWrapper[AddBenefitResponse]]]] = {
      (mockCreateStateBenefitService
        .addBenefit(_: CreateBenefitRequest)(_: HeaderCarrier, _: ExecutionContext, _: EndpointLogContext, _: String))
        .expects(requestData, *, *, *, *)
    }
  }
}
