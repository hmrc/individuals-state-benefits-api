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

package api.models.errors

import play.api.http.Status._

// Format Errors
object NinoFormatError extends MtdError("FORMAT_NINO", "The provided NINO is invalid", BAD_REQUEST)

object TaxYearFormatError extends MtdError("FORMAT_TAX_YEAR", "The provided tax year is invalid", BAD_REQUEST)

object StartDateFormatError extends MtdError("FORMAT_START_DATE", "The provided start date is invalid", BAD_REQUEST)

object EndDateFormatError extends MtdError("FORMAT_END_DATE", "The provided end date is invalid", BAD_REQUEST)

object BenefitIdFormatError extends MtdError("FORMAT_BENEFIT_ID", "The provided benefit ID is invalid", BAD_REQUEST)

object BenefitTypeFormatError extends MtdError("FORMAT_BENEFIT_TYPE", "The provided benefit type is invalid", BAD_REQUEST)

object ValueFormatError extends MtdError("FORMAT_VALUE", "", BAD_REQUEST)

// Rule Errors
object RuleTaxYearNotSupportedError
    extends MtdError("RULE_TAX_YEAR_NOT_SUPPORTED", "The tax year specified does not lie within the supported range", BAD_REQUEST)

object RuleTaxYearRangeInvalidError
    extends MtdError(code = "RULE_TAX_YEAR_RANGE_INVALID", message = "Tax year range invalid. A tax year range of one year is required", BAD_REQUEST)

object RuleTaxYearNotEndedError extends MtdError(code = "RULE_TAX_YEAR_NOT_ENDED", "Tax year not ended", BAD_REQUEST)

object RuleEndDateBeforeStartDateError
    extends MtdError("RULE_END_DATE_BEFORE_START_DATE", "The end date cannot be earlier than the start date", BAD_REQUEST)

object RuleStartDateAfterTaxYearEndError
    extends MtdError("RULE_START_DATE_AFTER_TAX_YEAR_END", "The start date cannot be later than the tax year end", BAD_REQUEST)

object RuleEndDateBeforeTaxYearStartError
    extends MtdError("RULE_END_DATE_BEFORE_TAX_YEAR_START", "The end date cannot be before the tax year starts", BAD_REQUEST)

object RuleIncorrectOrEmptyBodyError
    extends MtdError("RULE_INCORRECT_OR_EMPTY_BODY_SUBMITTED", "An empty or non-matching body was submitted", BAD_REQUEST)

object RuleDeleteForbiddenError extends MtdError("RULE_DELETE_FORBIDDEN", "A deletion of a HMRC held state benefit is not permitted", BAD_REQUEST)

object RuleUpdateForbiddenError extends MtdError("RULE_UPDATE_FORBIDDEN", "The update for a HMRC held benefit is not permitted", BAD_REQUEST)

object RuleIgnoreForbiddenError extends MtdError("RULE_IGNORE_FORBIDDEN", "A customer added state benefit cannot be ignored", BAD_REQUEST)

object RuleUnignoreForbiddenError extends MtdError("RULE_UNIGNORE_FORBIDDEN", "A customer added state benefit cannot be unignored", BAD_REQUEST)

object RuleBenefitTypeExists extends MtdError("RULE_BENEFIT_TYPE_EXISTS", "A benefit of this type has already been created", BAD_REQUEST)

//Standard Errors
object NotFoundError extends MtdError("MATCHING_RESOURCE_NOT_FOUND", "Matching resource not found", NOT_FOUND)

object StandardDownstreamError extends MtdError("INTERNAL_SERVER_ERROR", "An internal server error occurred", INTERNAL_SERVER_ERROR)

object BadRequestError extends MtdError("INVALID_REQUEST", "Invalid request", BAD_REQUEST)

object BVRError extends MtdError("BUSINESS_ERROR", "Business validation error", BAD_REQUEST)

object ServiceUnavailableError extends MtdError("SERVICE_UNAVAILABLE", "Internal server error", INTERNAL_SERVER_ERROR)

//Authorisation Errors
object ClientNotAuthorisedError extends MtdError("CLIENT_OR_AGENT_NOT_AUTHORISED", "The client or agent is not authorised", FORBIDDEN)

object ClientNotAuthenticatedError extends MtdError("CLIENT_OR_AGENT_NOT_AUTHORISED", "The client or agent is not authorised", UNAUTHORIZED)

object InvalidBearerTokenError extends MtdError("UNAUTHORIZED", "Bearer token is missing or not authorized", UNAUTHORIZED)

// Accept header Errors
object InvalidAcceptHeaderError extends MtdError("ACCEPT_HEADER_INVALID", "The accept header is missing or invalid", NOT_ACCEPTABLE)

object UnsupportedVersionError extends MtdError("NOT_FOUND", "The requested resource could not be found", NOT_FOUND)

object InvalidBodyTypeError extends MtdError("INVALID_BODY_TYPE", "Expecting text/json or application/json body", UNSUPPORTED_MEDIA_TYPE)
