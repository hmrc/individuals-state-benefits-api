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

package v1.controllers.validators

import api.models.domain.{Nino, TaxYear}
import api.models.errors._
import play.api.libs.json.{JsObject, JsValue, Json}
import support.UnitSpec
import v1.models.request.createBenefit._

class CreateBenefitValidatorFactorySpec extends UnitSpec {

  implicit private val correlationId: String = "a1e8057e-fbbc-47a8-a8b4-78d9f015c253"

  private val validNino    = "AA123456B"
  private val validTaxYear = "2019-20"

  private val parsedNino    = Nino(validNino)
  private val parsedTaxYear = TaxYear.fromMtd(validTaxYear)

  private val startDate = "2020-08-03"
  private val endDate   = "2020-12-03"

  private def requestJson(benefitType: String = "statePension", startDate: String = startDate, endDate: String = endDate) =
    Json.parse(
      s"""
         |{
         |  "benefitType": "$benefitType",
         |  "startDate": "$startDate",
         |  "endDate": "$endDate"
         |}
      """.stripMargin
    )

  private val validRequestBodyJson = Json.parse(
    s"""
       |{
       |  "benefitType": "otherStateBenefits",
       |  "startDate": "$startDate",
       |  "endDate" : "$endDate"
       |}
    """.stripMargin
  )

  private val parsedCreateBenefitBody = CreateBenefitRequestBody("otherStateBenefits", startDate, Some(endDate))

  private val validatorFactory = new CreateBenefitValidatorFactory

  private def validator(nino: String, taxYear: String, body: JsValue) =
    validatorFactory.validator(nino, taxYear, body)

  "Validator" should {
    "return the parsed domain object" when {
      "passed a valid request" in {
        val result = validator(validNino, validTaxYear, validRequestBodyJson).validateAndWrapResult()
        result shouldBe Right(CreateBenefitRequestData(parsedNino, parsedTaxYear, parsedCreateBenefitBody))
      }
    }

    "return a single error" when {
      "passed an invalid nino" in {
        val result = validator("A12344A", validTaxYear, validRequestBodyJson).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, NinoFormatError))
      }
      "passed an invalid tax year" in {
        val result = validator(validNino, "202223", validRequestBodyJson).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, TaxYearFormatError))
      }

      "passed a tax year with an invalid range" in {
        val result = validator(validNino, "2022-24", validRequestBodyJson).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, RuleTaxYearRangeInvalidError))
      }

      "passed a tax year that precedes the minimum" in {
        val result = validator(validNino, "2018-19", validRequestBodyJson).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, RuleTaxYearNotSupportedError))
      }

      "passed an empty JSON body" in {
        val invalidBody = JsObject.empty
        val result      = validator(validNino, validTaxYear, invalidBody).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, RuleIncorrectOrEmptyBodyError))
      }

      "passed an incorrect request body" in {
        val paths: Seq[String] = List("/benefitType", "/endDate", "/startDate")
        val body = Json.parse(s"""
             |{
             |  "benefitType": true,
             |  "startDate": true,
             |  "endDate": false
             |}""".stripMargin)

        val result = validator(validNino, validTaxYear, body).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, RuleIncorrectOrEmptyBodyError.withPaths(paths)))
      }

      "passed an incorrect benefitType" in {
        val result = validator(validNino, validTaxYear, requestJson(benefitType = "invalidBenefit")).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, BenefitTypeFormatError))
      }

      "passed an incorrect End Date" in {
        val result = validator(validNino, validTaxYear, requestJson(startDate = endDate, endDate = startDate)).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, RuleEndBeforeStartDateError))
      }

      "return EndDateFormatError error for an incorrect End Date" in {
        val result = validator(validNino, validTaxYear, requestJson(endDate = "20201203")).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, EndDateFormatError))
      }

      "passed an incorrect Start Date" in {
        val result = validator(validNino, validTaxYear, requestJson(startDate = "20201203")).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, StartDateFormatError))
      }

      "passed a start date that is before 1900" in {
        val result = validator(validNino, validTaxYear, requestJson(startDate = "1809-02-01")).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, StartDateFormatError))
      }

      "passed an end date that is after 2100" in {
        val result = validator(validNino, validTaxYear, requestJson(endDate = "2149-02-21")).validateAndWrapResult()
        result shouldBe Left(ErrorWrapper(correlationId, EndDateFormatError))
      }
    }

    "return multiple errors" when {
      "passed multiple invalid fields" in {
        val result = validator("not-a-nino", "not-a-tax-year", validRequestBodyJson).validateAndWrapResult()

        result shouldBe Left(
          ErrorWrapper(
            correlationId,
            BadRequestError,
            Some(List(NinoFormatError, TaxYearFormatError))
          )
        )
      }

      "passed multiple invalid json field formats" in {
        val result = validator(validNino, validTaxYear, requestJson("invalid", "invalid", "invalid")).validateAndWrapResult()
        result shouldBe Left(
          ErrorWrapper(
            correlationId,
            BadRequestError,
            Some(List(BenefitTypeFormatError, EndDateFormatError, StartDateFormatError))
          )
        )
      }
    }
  }

}
