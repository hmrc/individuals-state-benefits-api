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

package api.connectors

import api.connectors.DownstreamUri._
import api.mocks.MockHttpClient
import api.models.outcomes.ResponseWrapper
import config.AppConfig
import mocks.MockAppConfig
import play.api.Configuration
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpReads}

import scala.concurrent.Future

class BaseDownstreamConnectorSpec extends ConnectorSpec {

  // WLOG
  case class Result(value: Int)

  // WLOG
  val body                               = "body"
  val queryParams: Seq[(String, String)] = Seq("aParam" -> "aValue")
  val outcome                            = Right(ResponseWrapper(correlationId, Result(2)))

  val url         = "some/url?param=value"
  val absoluteUrl = s"$baseUrl/$url"

  implicit val httpReads: HttpReads[DownstreamOutcome[Result]] = mock[HttpReads[DownstreamOutcome[Result]]]

  class DesConnectTest(desEnvironmentHeaders: Option[Seq[String]]) extends MockHttpClient with MockAppConfig {

    val connector: BaseDownstreamConnector = new BaseDownstreamConnector {
      val http: HttpClient     = mockHttpClient
      val appConfig: AppConfig = mockAppConfig
    }

    MockedAppConfig.desBaseUrl returns baseUrl
    MockedAppConfig.desToken returns "des-token"
    MockedAppConfig.desEnvironment returns "des-environment"
    MockedAppConfig.desEnvironmentHeaders returns desEnvironmentHeaders
    MockedAppConfig.featureSwitches returns Configuration("tys-api.enabled" -> false)
  }

  "BaseDesConnector" when {
    val requiredHeaders: Seq[(String, String)] = Seq(
      "Environment"       -> "des-environment",
      "Authorization"     -> s"Bearer des-token",
      "User-Agent"        -> "individuals-state-benefits-api",
      "CorrelationId"     -> correlationId,
      "Gov-Test-Scenario" -> "DEFAULT"
    )

    val excludedHeaders: Seq[(String, String)] = Seq(
      "AnotherHeader" -> "HeaderValue"
    )

    "making a HTTP request to a downstream service (i.e DES)" must {
      desTestHttpMethods(dummyDesHeaderCarrierConfig, requiredHeaders, excludedHeaders, Some(allowedDesHeaders))

      "exclude all `otherHeaders` when no external service header allow-list is found" should {
        val requiredHeaders: Seq[(String, String)] = Seq(
          "Environment"   -> "des-environment",
          "Authorization" -> s"Bearer des-token",
          "User-Agent"    -> "individuals-state-benefits-api",
          "CorrelationId" -> correlationId
        )

        desTestHttpMethods(dummyDesHeaderCarrierConfig, requiredHeaders, otherHeaders, None)
      }
    }
  }

  def desTestHttpMethods(config: HeaderCarrier.Config,
                         requiredHeaders: Seq[(String, String)],
                         excludedHeaders: Seq[(String, String)],
                         desEnvironmentHeaders: Option[Seq[String]]): Unit = {

    "complete the request successfully with the required headers" when {
      "GET" in new DesConnectTest(desEnvironmentHeaders) {
        MockedHttpClient
          .get(absoluteUrl, config, requiredHeaders, excludedHeaders)
          .returns(Future.successful(outcome))

        await(connector.get(DesUri[Result](url))) shouldBe outcome
      }

      "GET with query params" in new DesConnectTest(desEnvironmentHeaders) {
        val params = Seq("param1" -> "value")
        MockedHttpClient
          .parameterGet(absoluteUrl, params, config, requiredHeaders, excludedHeaders)
          .returns(Future.successful(outcome))

        await(connector.get(DesUri[Result](url), params)) shouldBe outcome
      }

      "POST" in new DesConnectTest(desEnvironmentHeaders) {
        implicit val hc: HeaderCarrier                 = HeaderCarrier(otherHeaders = otherHeaders ++ Seq("Content-Type" -> "application/json"))
        val requiredHeadersPost: Seq[(String, String)] = requiredHeaders ++ Seq("Content-Type" -> "application/json")

        MockedHttpClient
          .post(absoluteUrl, config, body, requiredHeadersPost, excludedHeaders)
          .returns(Future.successful(outcome))

        await(connector.post(body, DesUri[Result](url))) shouldBe outcome
      }

      "PUT" in new DesConnectTest(desEnvironmentHeaders) {
        implicit val hc: HeaderCarrier                = HeaderCarrier(otherHeaders = otherHeaders ++ Seq("Content-Type" -> "application/json"))
        val requiredHeadersPut: Seq[(String, String)] = requiredHeaders ++ Seq("Content-Type" -> "application/json")

        MockedHttpClient
          .put(absoluteUrl, config, body, requiredHeadersPut, excludedHeaders)
          .returns(Future.successful(outcome))

        await(connector.put(body, DesUri[Result](url))) shouldBe outcome
      }

      "DELETE" in new DesConnectTest(desEnvironmentHeaders) {
        MockedHttpClient
          .delete(absoluteUrl, config, requiredHeaders, excludedHeaders)
          .returns(Future.successful(outcome))

        await(connector.delete(DesUri[Result](url))) shouldBe outcome
      }
    }
  }

  class IfsConnectTest(ifsEnvironmentHeaders: Option[Seq[String]]) extends MockHttpClient with MockAppConfig {

    val connector: BaseDownstreamConnector = new BaseDownstreamConnector {
      val http: HttpClient     = mockHttpClient
      val appConfig: AppConfig = mockAppConfig
    }

    MockedAppConfig.ifsBaseUrl returns baseUrl
    MockedAppConfig.ifsToken returns "ifs-token"
    MockedAppConfig.ifsEnvironment returns "ifs-environment"
    MockedAppConfig.ifsEnvironmentHeaders returns ifsEnvironmentHeaders
    MockedAppConfig.featureSwitches returns Configuration("tys-api.enabled" -> false)
  }

  "BaseDownstreamConnector" when {
    val requiredHeaders: Seq[(String, String)] = Seq(
      "Environment"       -> "ifs-environment",
      "Authorization"     -> s"Bearer ifs-token",
      "User-Agent"        -> "individuals-state-benefits-api",
      "CorrelationId"     -> correlationId,
      "Gov-Test-Scenario" -> "DEFAULT"
    )

    val excludedHeaders: Seq[(String, String)] = Seq(
      "AnotherHeader" -> "HeaderValue"
    )

    "making a HTTP request to a downstream service (i.e IFS)" must {
      ifsTestHttpMethods(dummyIfsHeaderCarrierConfig, requiredHeaders, excludedHeaders, Some(allowedIfsHeaders))

      "exclude all `otherHeaders` when no external service header allow-list is found" should {
        val requiredHeaders: Seq[(String, String)] = Seq(
          "Environment"   -> "ifs-environment",
          "Authorization" -> s"Bearer ifs-token",
          "User-Agent"    -> "individuals-state-benefits-api",
          "CorrelationId" -> correlationId
        )

        ifsTestHttpMethods(dummyIfsHeaderCarrierConfig, requiredHeaders, otherHeaders, None)
      }
    }
  }

  def ifsTestHttpMethods(config: HeaderCarrier.Config,
                         requiredHeaders: Seq[(String, String)],
                         excludedHeaders: Seq[(String, String)],
                         ifsEnvironmentHeaders: Option[Seq[String]]): Unit = {

    "complete the request successfully with the required headers" when {
      "POST" in new IfsConnectTest(ifsEnvironmentHeaders) {
        implicit val hc: HeaderCarrier                 = HeaderCarrier(otherHeaders = otherHeaders ++ Seq("Content-Type" -> "application/json"))
        val requiredHeadersPost: Seq[(String, String)] = requiredHeaders ++ Seq("Content-Type" -> "application/json")

        MockedHttpClient
          .post(absoluteUrl, config, body, requiredHeadersPost, excludedHeaders)
          .returns(Future.successful(outcome))

        await(connector.post(body, IfsUri[Result](url))) shouldBe outcome
      }

      "complete the request successfully with the required headers" when {
        "GET" in new IfsConnectTest(ifsEnvironmentHeaders) {
          MockedHttpClient
            .get(absoluteUrl, config, requiredHeaders, excludedHeaders)
            .returns(Future.successful(outcome))

          await(connector.get(IfsUri[Result](url))) shouldBe outcome
        }

        "GET with query params" in new IfsConnectTest(ifsEnvironmentHeaders) {
          val params = Seq("param1" -> "value")
          MockedHttpClient
            .parameterGet(absoluteUrl, params, config, requiredHeaders, excludedHeaders)
            .returns(Future.successful(outcome))

          await(connector.get(IfsUri[Result](url), params)) shouldBe outcome
        }

        "PUT" in new IfsConnectTest(ifsEnvironmentHeaders) {
          implicit val hc: HeaderCarrier                = HeaderCarrier(otherHeaders = otherHeaders ++ Seq("Content-Type" -> "application/json"))
          val requiredHeadersPut: Seq[(String, String)] = requiredHeaders ++ Seq("Content-Type" -> "application/json")

          MockedHttpClient
            .put(absoluteUrl, config, body, requiredHeadersPut, excludedHeaders)
            .returns(Future.successful(outcome))

          await(connector.put(body, IfsUri[Result](url))) shouldBe outcome
        }

        "DELETE" in new IfsConnectTest(ifsEnvironmentHeaders) {
          MockedHttpClient
            .delete(absoluteUrl, config, requiredHeaders, excludedHeaders)
            .returns(Future.successful(outcome))

          await(connector.delete(IfsUri[Result](url))) shouldBe outcome
        }
      }
    }
  }

}
