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

package v1.controllers

import api.controllers.{ControllerBaseSpec, ControllerTestRunner}
import api.hateoas.HateoasLinks
import api.mocks.hateoas.MockHateoasFactory
import api.mocks.services.MockAuditService
import api.models.errors._
import api.models.outcomes.ResponseWrapper
import mocks.MockAppConfig
import play.api.libs.json.JsObject
import play.api.mvc.Result
import v1.fixtures.ListBenefitsFixture._
import v1.mocks.requestParsers.MockListBenefitsRequestParser
import v1.mocks.services.MockListBenefitsService
import v1.models.response.listBenefits.{CustomerStateBenefit, HMRCStateBenefit, ListBenefitsHateoasData}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ListBenefitsControllerSpec
    extends ControllerBaseSpec
    with ControllerTestRunner
    with MockAppConfig
    with MockListBenefitsService
    with MockListBenefitsRequestParser
    with MockHateoasFactory
    with MockAuditService
    with HateoasLinks {

  "ListBenefitsController" should {
    "return OK with full HATEOAS" when {
      "happy path" in new Test {
        MockListBenefitsRequestParser
          .parse(rawData(None))
          .returns(Right(requestData(None)))

        MockListBenefitsService
          .listBenefits(requestData(None))
          .returns(Future.successful(Right(ResponseWrapper(correlationId, responseData))))

        MockHateoasFactory
          .wrapList(responseData, ListBenefitsHateoasData(nino, taxYear, queryIsFiltered = false, hmrcBenefitIds = Seq(benefitId)))
          .returns(hateoasResponse)

        runOkTest(
          expectedStatus = OK,
          maybeExpectedResponseBody = Some(responseBody)
        )
      }
    }

    "return OK with no delete amount HATEOAS" when {
      "state benefits has no amount properties" in new Test {
        MockListBenefitsRequestParser
          .parse(rawData(queryBenefitId))
          .returns(Right(requestData(queryBenefitId)))

        MockListBenefitsService
          .listBenefits(requestData(queryBenefitId))
          .returns(Future.successful(Right(ResponseWrapper(correlationId, responseDataWithNoAmounts))))

        MockHateoasFactory
          .wrapList(responseDataWithNoAmounts, ListBenefitsHateoasData(nino, taxYear, queryIsFiltered = true, hmrcBenefitIds = Seq(benefitId)))
          .returns(hateoasResponseWithOutAmounts)

        runOkTest(
          expectedStatus = OK,
          maybeExpectedResponseBody = Some(responseBodyWithNoAmounts)
        )
      }
    }

    "return OK with only HMRC state benefit HATEOAS" when {
      "only HMRC state benefits returned" in new Test {

        MockListBenefitsRequestParser
          .parse(rawData(None))
          .returns(Right(requestData(None)))

        MockListBenefitsService
          .listBenefits(requestData(None))
          .returns(Future.successful(Right(ResponseWrapper(correlationId, responseData.copy(customerAddedStateBenefits = None)))))

        MockHateoasFactory
          .wrapList(
            responseData.copy(customerAddedStateBenefits = Option.empty[Seq[CustomerStateBenefit]]),
            ListBenefitsHateoasData(nino, taxYear, queryIsFiltered = false, hmrcBenefitIds = Seq(benefitId))
          )
          .returns(hmrcOnlyHateoasResponse)

        runOkTest(
          expectedStatus = OK,
          maybeExpectedResponseBody = Some(responseBody.as[JsObject] - "customerAddedStateBenefits")
        )
      }
    }

    "return OK with only CUSTOM state benefit HATEOAS" when {
      "only CUSTOM state benefits returned" in new Test {
        MockListBenefitsRequestParser
          .parse(rawData(None))
          .returns(Right(requestData(None)))

        MockListBenefitsService
          .listBenefits(requestData(None))
          .returns(Future.successful(Right(ResponseWrapper(correlationId, responseData.copy(stateBenefits = None)))))

        MockHateoasFactory
          .wrapList(
            responseData.copy(stateBenefits = Option.empty[Seq[HMRCStateBenefit]]),
            ListBenefitsHateoasData(nino, taxYear, queryIsFiltered = false, hmrcBenefitIds = Nil))
          .returns(customOnlyHateoasResponse)

        runOkTest(
          expectedStatus = OK,
          maybeExpectedResponseBody = Some(responseBody.as[JsObject] - "stateBenefits")
        )
      }
    }

    "return OK with single state benefit with HATEOAS" when {
      "benefitId is passed for single retrieval" in new Test {
        MockListBenefitsRequestParser
          .parse(rawData(queryBenefitId))
          .returns(Right(requestData(queryBenefitId)))

        MockListBenefitsService
          .listBenefits(requestData(queryBenefitId))
          .returns(Future.successful(Right(ResponseWrapper(correlationId, responseData.copy(stateBenefits = None)))))

        MockHateoasFactory
          .wrapList(
            responseData.copy(stateBenefits = Option.empty[Seq[HMRCStateBenefit]]),
            ListBenefitsHateoasData(nino, taxYear, queryIsFiltered = true, hmrcBenefitIds = Nil))
          .returns(singleCustomOnlyHateoasResponse)

        runOkTest(
          expectedStatus = OK,
          maybeExpectedResponseBody = Some(singleRetrieveWithAmounts)
        )
      }
    }

    "return the error as per spec" when {
      "the parser validation fails" in new Test {
        MockListBenefitsRequestParser
          .parse(rawData(queryBenefitId))
          .returns(Left(ErrorWrapper(correlationId, NinoFormatError)))

        runErrorTest(NinoFormatError)
      }

      "the service returns an error" in new Test {
        MockListBenefitsRequestParser
          .parse(rawData(queryBenefitId))
          .returns(Right(requestData(queryBenefitId)))

        MockListBenefitsService
          .listBenefits(requestData(queryBenefitId))
          .returns(Future.successful(Left(ErrorWrapper(correlationId, RuleTaxYearNotSupportedError))))

        runErrorTest(RuleTaxYearNotSupportedError)
      }
    }
  }

  trait Test extends ControllerTest {

    val controller = new ListBenefitsController(
      authService = mockEnrolmentsAuthService,
      lookupService = mockMtdIdLookupService,
      appConfig = mockAppConfig,
      parser = mockListBenefitsRequestParser,
      service = mockListBenefitsService,
      hateoasFactory = mockHateoasFactory,
      cc = cc,
      idGenerator = mockIdGenerator
    )

//    MockedAppConfig.apiGatewayContext.returns("individuals/state-benefits").anyNumberOfTimes()
//
//    val links: List[Link] = List(
//      listBenefits(mockAppConfig, nino, taxYear),
//      addBenefit(mockAppConfig, nino, taxYear)
//    )

    protected def callController(): Future[Result] = controller.listBenefits(nino, taxYear, Some(benefitId))(fakeGetRequest)

  }

}
