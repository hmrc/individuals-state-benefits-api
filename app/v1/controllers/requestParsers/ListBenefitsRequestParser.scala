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

package v1.controllers.requestParsers

import api.controllers.requestParsers.RequestParser
import api.models.domain.{BenefitId, Nino, TaxYear}
import v1.controllers.requestParsers.validators.ListBenefitsValidator
import v1.models.request.listBenefits.{ListBenefitsRawData, ListBenefitsRequest}

import javax.inject.Inject

class ListBenefitsRequestParser @Inject() (val validator: ListBenefitsValidator) extends RequestParser[ListBenefitsRawData, ListBenefitsRequest] {

  override protected def requestFor(data: ListBenefitsRawData): ListBenefitsRequest =
    ListBenefitsRequest(Nino(data.nino), TaxYear.fromMtd(data.taxYear), data.benefitId.map(BenefitId))

}
