/*
 * Copyright 2020 HM Revenue & Customs
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

package v1.controllers.requestParsers

import play.api.libs.json.{JsValue, Json}
import play.api.mvc.AnyContentAsJson
import support.UnitSpec
import uk.gov.hmrc.domain.Nino
import v1.mocks.validators.MockAddBenefitValidator
import v1.models.errors._
import v1.models.request.addBenefit._

class AddBenefitRequestParserSpec extends UnitSpec {

  private val nino: String = "AA123456B"
  private val taxYear: String = "2017-18"

  val startDate = "2020-08-03"
  val endDate = "2020-12-03"


  private val validRequestBodyJson: JsValue = Json.parse(
    s"""
      |{
      |  "benefitType": "otherStateBenefits",
      |  "startDate": "$startDate",
      |  "endDate" : "$endDate"
      |}
    """.stripMargin
  )

  private val validRawBody = AnyContentAsJson(validRequestBodyJson)

  private val addBenefitRawData = AddBenefitRawData(
    nino = nino,
    taxYear = taxYear,
    body = validRawBody
  )

  private val addBenefitBody = AddBenefitRequestBody("otherStateBenefits", startDate, Some(endDate))

  private val addBenefitRequest = AddBenefitRequest(
    nino = Nino(nino),
    taxYear = taxYear,
    body = addBenefitBody
  )

  trait Test extends MockAddBenefitValidator {
    lazy val parser: AddBenefitRequestParser = new AddBenefitRequestParser(
      validator = mockAddBenefitValidator
    )
  }

  "AddBenefitRequestParser" should {
    "return a request object" when {
      "valid request data is supplied" in new Test {
        MockAddBenefitValidator.validate(addBenefitRawData).returns(Nil)
        parser.parseRequest(addBenefitRawData) shouldBe Right(addBenefitRequest)
      }
    }

    "return an ErrorWrapper" when {
      "a single validation error occurs" in new Test {
        MockAddBenefitValidator.validate(addBenefitRawData.copy(nino = "notANino"))
          .returns(List(NinoFormatError))

        parser.parseRequest(addBenefitRawData.copy(nino = "notANino")) shouldBe
          Left(ErrorWrapper(None, NinoFormatError, None))
      }

      "multiple path parameter validation errors occur" in new Test {
        MockAddBenefitValidator.validate(addBenefitRawData.copy(nino = "notANino", taxYear = "notATaxYear"))
          .returns(List(NinoFormatError, TaxYearFormatError))

        parser.parseRequest(addBenefitRawData.copy(nino = "notANino", taxYear = "notATaxYear")) shouldBe
          Left(ErrorWrapper(None, BadRequestError, Some(Seq(NinoFormatError, TaxYearFormatError))))
      }

      "multiple field value validation errors occur" in new Test {

        private val invalidValueRequestJson: JsValue = Json.parse(
          s"""
             |{
             |  "benefitType": "notValid",
             |  "startDate": "notValid",
             |  "endDate": "notValid"
             |}
            """.stripMargin
        )

        private val invalidValueRawBody = AnyContentAsJson(invalidValueRequestJson)

        private val errors = List(
          StartDateFormatError,
          EndDateFormatError,
          BenefitTypeFormatError
        )

        MockAddBenefitValidator.validate(addBenefitRawData.copy(body = invalidValueRawBody))
          .returns(errors)

        parser.parseRequest(addBenefitRawData.copy(body = invalidValueRawBody)) shouldBe
          Left(ErrorWrapper(None, BadRequestError, Some(errors)))
      }
    }
  }
}
