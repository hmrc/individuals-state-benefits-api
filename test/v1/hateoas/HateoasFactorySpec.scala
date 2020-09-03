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

package v1.hateoas

import cats.Functor
import config.AppConfig
import mocks.MockAppConfig
import support.UnitSpec
import v1.models.hateoas.Method.{DELETE, GET, POST, PUT}
import v1.models.hateoas.{HateoasData, HateoasWrapper, Link}
import v1.models.response.listBenefits.{CustomerAddedStateBenefits, CustomerIncapacityBenefit, IncapacityBenefit, ListBenefitsHateoasData, ListBenefitsResponse, StateBenefits}
import v1.models.response.{AddBenefitHateoasData, AddBenefitResponse}

class HateoasFactorySpec extends UnitSpec with MockAppConfig {

  val hateoasFactory = new HateoasFactory(mockAppConfig)

  case class Response(foo: String)
  case class ListResponse[A](items: Seq[A])

  case class Data1(id: String) extends HateoasData
  case class Data2(id: String) extends HateoasData

  val response: Response = Response("X")

  val nino: String = "AA123456A"
  val taxYear: String = "2020-21"
  val benefitId: String = "b1e8057e-fbbc-47a8-a8b4-78d9f015c253"

  val addStateBenefitResponse: AddBenefitResponse = AddBenefitResponse(benefitId)
  val addStateBenefitsHateoasData: AddBenefitHateoasData = AddBenefitHateoasData(nino, taxYear, benefitId)

  val listBenefitsHateoasData: ListBenefitsHateoasData = ListBenefitsHateoasData(nino, taxYear, benefitId)
  val listBenefitsResponse: ListBenefitsResponse = ListBenefitsResponse(
    stateBenefits = StateBenefits(
      incapacityBenefit = Seq(
        IncapacityBenefit(
          dateIgnored = Some("2019-04-04T01:01:01Z"),
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = None
        )
      ),
      statePension = IncapacityBenefit(
        dateIgnored = None,
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2019-01-01",
        endDate = None,
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = None
      ),
      statePensionLumpSum = IncapacityBenefit(
        dateIgnored = None,
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2019-01-01",
        endDate = Some("2019-01-01"),
        amount = Some(2000.00),
        taxPaid = Some(2132.22),
        submittedOn = None
      ),
      employmentSupportAllowance = Seq(
        IncapacityBenefit(
          dateIgnored = None,
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = None
        )
      ),
      jobSeekersAllowance = Seq(
        IncapacityBenefit(
          dateIgnored = None,
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = None
        )
      ),
      bereavementAllowance = IncapacityBenefit(
        dateIgnored = None,
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2020-01-01",
        endDate = Some("2020-04-01"),
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = None
      ),
      otherStateBenefits = IncapacityBenefit(
        dateIgnored = None,
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2020-01-01",
        endDate = Some("2020-04-01"),
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = None
      )
    ),
    customerAddedStateBenefits = CustomerAddedStateBenefits(
      incapacityBenefit = Seq(
        CustomerIncapacityBenefit(
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = Some("2019-04-04T01:01:01Z")
        )
      ),
      statePension = CustomerIncapacityBenefit(
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2019-01-01",
        endDate = None,
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = Some("2019-04-04T01:01:01Z")
      ),
      statePensionLumpSum = CustomerIncapacityBenefit(
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2019-01-01",
        endDate = Some("2019-01-01"),
        amount = Some(2000.00),
        taxPaid = Some(2132.22),
        submittedOn = Some("2019-04-04T01:01:01Z")
      ),
      employmentSupportAllowance = Seq(
        CustomerIncapacityBenefit(
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = Some("2019-04-04T01:01:01Z")
        )
      ),
      jobSeekersAllowance = Seq(
        CustomerIncapacityBenefit(
          benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
          startDate = "2020-01-01",
          endDate = Some("2020-04-01"),
          amount = Some(2000.00),
          taxPaid = Some(2132.22),
          submittedOn = Some("2019-04-04T01:01:01Z")
        )
      ),
      bereavementAllowance = CustomerIncapacityBenefit(
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2020-01-01",
        endDate = Some("2020-04-01"),
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = Some("2019-04-04T01:01:01Z")
      ),
      otherStateBenefits = CustomerIncapacityBenefit(
        benefitId = "f0d83ac0-a10a-4d57-9e41-6d033832779f",
        startDate = "2020-01-01",
        endDate = Some("2020-04-01"),
        amount = Some(2000.00),
        taxPaid = None,
        submittedOn = Some("2019-04-04T01:01:01Z")
      )
    )
  )

  class Test {
    MockedAppConfig.apiGatewayContext.returns("context").anyNumberOfTimes
  }

  "wrap" should {

    implicit object LinksFactory1 extends HateoasLinksFactory[Response, Data1] {
      override def links(appConfig: AppConfig, data: Data1): Seq[Link] = Seq(Link(s"${appConfig.apiGatewayContext}/${data.id}", GET, "rel1"))
    }

    implicit object LinksFactory2 extends HateoasLinksFactory[Response, Data2] {
      override def links(appConfig: AppConfig, data: Data2): Seq[Link] = Seq(Link(s"${appConfig.apiGatewayContext}/${data.id}", GET, "rel2"))
    }

    "use the response specific links" in new Test {
      hateoasFactory.wrap(response, Data1("id")) shouldBe HateoasWrapper(response, Seq(Link("context/id", GET, "rel1")))
    }

    "use the endpoint HateoasData specific links" in new Test {
      hateoasFactory.wrap(response, Data2("id")) shouldBe HateoasWrapper(response, Seq(Link("context/id", GET, "rel2")))
    }

    "use the add state benefits HateoasData specific links" in new Test {
      hateoasFactory.wrap(addStateBenefitResponse, addStateBenefitsHateoasData) shouldBe
        HateoasWrapper(
          AddBenefitResponse("b1e8057e-fbbc-47a8-a8b4-78d9f015c253"),
          List(
            Link("/context/AA123456A/2020-21",GET,"self"),
            Link("/context/AA123456A/2020-21/b1e8057e-fbbc-47a8-a8b4-78d9f015c253",PUT,"update-state-benefit"),
            Link("/context/AA123456A/2020-21/b1e8057e-fbbc-47a8-a8b4-78d9f015c253",DELETE,"delete-state-benefit")
          )
        )
    }

    "use the list state benefits HateoasData specific links" in new Test {
      hateoasFactory.wrap(listBenefitsResponse, listBenefitsHateoasData) shouldBe
        HateoasWrapper(
          listBenefitsResponse,
          List(
            Link("/context/AA123456A/2020-21",POST,"add-state-benefit"),
            Link("/context/AA123456A/2020-21",GET,"self")
          )
        )
    }
  }

  "wrapList" should {

    implicit object ListResponseFunctor extends Functor[ListResponse] {
      override def map[A, B](fa: ListResponse[A])(f: A => B): ListResponse[B] = ListResponse(fa.items.map(f))
    }

    implicit object LinksFactory extends HateoasListLinksFactory[ListResponse, Response, Data1] {
      override def itemLinks(appConfig: AppConfig, data: Data1, item: Response): Seq[Link] =
        Seq(Link(s"${appConfig.apiGatewayContext}/${data.id}/${item.foo}", GET, "item"))

      override def links(appConfig: AppConfig, data: Data1): Seq[Link] = Seq(Link(s"${appConfig.apiGatewayContext}/${data.id}", GET, "rel"))
    }

    "work" in new Test {
      hateoasFactory.wrapList(ListResponse(Seq(response)), Data1("id")) shouldBe
        HateoasWrapper(ListResponse(Seq(HateoasWrapper(response, Seq(Link("context/id/X", GET, "item"))))), Seq(Link("context/id", GET, "rel")))
    }
  }
}
