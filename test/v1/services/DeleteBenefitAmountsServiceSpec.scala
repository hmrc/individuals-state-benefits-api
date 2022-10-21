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

package v1.services

import v1.controllers.EndpointLogContext
import v1.mocks.connectors.MockDeleteBenefitAmountsConnector
import v1.models.domain.Nino
import v1.models.errors.{ErrorWrapper, MtdError}
import v1.models.outcomes.ResponseWrapper
import v1.models.request.deleteBenefitAmounts.DeleteBenefitAmountsRequest

import scala.concurrent.Future

class DeleteBenefitAmountsServiceSpec extends ServiceSpec {

  private val request = DeleteBenefitAmountsRequest(
    nino = Nino("AA112233A"),
    taxYear = TaxYear.fromMtd("2023-24"),
    benefitId = "b1e8057e-fbbc-47a8-a8b4-78d9f015c253"
  )

  "DeleteOtherEmploymentIncomeServiceSpec" when {
    "the downstream request is successful" must {
      "return a success result" in new Test {
        val outcome = Right(ResponseWrapper(correlationId, ()))

        MockDeleteBenefitAmountsConnector
          .deleteOtherEmploymentIncome(request)
          .returns(Future.successful(outcome))

        val result = await(service.delete(request))

        result shouldBe outcome
      }

      "map errors according to spec" when {

        def serviceError(downstreamErrorCode: String, error: MtdError): Unit = {

          s"downstream returns $downstreamErrorCode" in new Test {
            MockDeleteBenefitAmountsConnector
              .deleteOtherEmploymentIncome(request)
              .returns(Future.successful(Left(ResponseWrapper(correlationId, DownstreamErrors.single(DownstreamErrorCode(downstreamErrorCode))))))

            val result: Either[ErrorWrapper, ResponseWrapper[Unit]] = await(service.delete(request))
            result shouldBe Left(ErrorWrapper(correlationId, error))
          }
        }

        val errors = Seq(
          "INVALID_TAXABLE_ENTITY_ID" -> NinoFormatError,
          "INVALID_TAX_YEAR"          -> TaxYearFormatError,
          "INVALID_BENEFIT_ID"        -> BenefitIdFormatError
          "INVALID_CORRELATIONID"     -> DownstreamError,
          "NO_DATA_FOUND"             -> NotFoundError,
          "SERVER_ERROR"              -> DownstreamError,
          "SERVICE_UNAVAILABLE"       -> DownstreamError
        )

        val extraTysErrors = Seq(
          ("TAX_YEAR_NOT_SUPPORTED", RuleTaxYearNotSupportedError)
        )

        (errors ++ extraTysErrors).foreach(args => (serviceError _).tupled(args))
      }
    }
  }

  trait Test extends MockDeleteBenefitAmountsConnector {
    implicit val logContext: EndpointLogContext = EndpointLogContext("c", "ep")

    val service: DeleteBenefitAmountsService =
      new DeleteBenefitAmountsService(mockDeleteBenefitAmountsConnector)

  }

}