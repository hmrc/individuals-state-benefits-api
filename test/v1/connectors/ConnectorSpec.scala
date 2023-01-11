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

package v1.connectors

import v1.mocks.MockHttpClient
import mocks.MockAppConfig
import org.scalamock.handlers.CallHandler
import play.api.Configuration
import play.api.http.{HeaderNames, MimeTypes, Status}
import support.UnitSpec
import uk.gov.hmrc.http.HeaderCarrier
import v1.mocks.MockHttpClient

import scala.concurrent.{ExecutionContext, Future}

trait ConnectorSpec extends UnitSpec with Status with MimeTypes with HeaderNames {

  lazy val baseUrl                   = "http://test-BaseUrl"
  implicit val correlationId: String = "a1e8057e-fbbc-47a8-a8b4-78d9f015c253"

  val otherHeaders: Seq[(String, String)] = Seq(
    "Gov-Test-Scenario" -> "DEFAULT",
    "AnotherHeader"     -> "HeaderValue"
  )

  implicit val hc: HeaderCarrier    = HeaderCarrier(otherHeaders = otherHeaders)
  implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

  val dummyDesHeaderCarrierConfig: HeaderCarrier.Config =
    HeaderCarrier.Config(
      Seq("^not-test-BaseUrl?$".r),
      Seq.empty[String],
      Some("individuals-income-received-api")
    )

  val requiredDesHeaders: Seq[(String, String)] = Seq(
    "Authorization"     -> "Bearer des-token",
    "Environment"       -> "des-environment",
    "User-Agent"        -> "individuals-income-received-api",
    "CorrelationId"     -> correlationId,
    "Gov-Test-Scenario" -> "DEFAULT"
  )

  val allowedDesHeaders: Seq[String] = Seq(
    "Accept",
    "Gov-Test-Scenario",
    "Content-Type",
    "Location",
    "X-Request-Timestamp",
    "X-Session-Id"
  )

  val dummyIfsHeaderCarrierConfig: HeaderCarrier.Config =
    HeaderCarrier.Config(
      Seq("^not-test-BaseUrl?$".r),
      Seq.empty[String],
      Some("individuals-income-received-api")
    )

  val dummyHeaderCarrierConfig: HeaderCarrier.Config =
    HeaderCarrier.Config(
      Seq("^not-test-BaseUrl?$".r),
      Seq.empty[String],
      Some("individuals-income-received-api")
    )

  val requiredIfsHeaders: Seq[(String, String)] = Seq(
    "Authorization"     -> "Bearer ifs-token",
    "Environment"       -> "ifs-environment",
    "User-Agent"        -> "individuals-income-received-api",
    "CorrelationId"     -> correlationId,
    "Gov-Test-Scenario" -> "DEFAULT"
  )

  val allowedIfsHeaders: Seq[String] = Seq(
    "Accept",
    "Gov-Test-Scenario",
    "Content-Type",
    "Location",
    "X-Request-Timestamp",
    "X-Session-Id"
  )

  val requiredTysIfsHeaders: Seq[(String, String)] = Seq(
    "Environment"   -> "TYS-IFS-environment",
    "Authorization" -> s"Bearer TYS-IFS-token",
    "CorrelationId" -> s"$correlationId"
  )

  val requiredRelease6Headers: Seq[(String, String)] = Seq(
    "Authorization"     -> "Bearer release6-token",
    "Environment"       -> "release6-environment",
    "User-Agent"        -> "individuals-income-received-api",
    "CorrelationId"     -> correlationId,
    "Gov-Test-Scenario" -> "DEFAULT"
  )

  val requiredApi1651Headers: Seq[(String, String)] = Seq(
    "Authorization"     -> "Bearer api1651-token",
    "Environment"       -> "api1651-environment",
    "User-Agent"        -> "individuals-income-received-api",
    "CorrelationId"     -> correlationId,
    "Gov-Test-Scenario" -> "DEFAULT"
  )

  protected trait ConnectorTest extends MockHttpClient with MockAppConfig {
    protected val baseUrl: String = "http://test-BaseUrl"

    implicit protected val hc: HeaderCarrier = HeaderCarrier(otherHeaders = otherHeaders)

    protected val requiredHeaders: Seq[(String, String)]

    protected def willGet[T](url: String, parameters: Seq[(String, String)] = Seq()): CallHandler[Future[T]] = {
      MockedHttpClient
        .parameterGet(
          url = url,
          parameters = parameters,
          config = dummyHeaderCarrierConfig,
          requiredHeaders = requiredHeaders,
          excludedHeaders = Seq("AnotherHeader" -> "HeaderValue")
        )
    }

    protected def willPost[BODY, T](url: String, body: BODY): CallHandler[Future[T]] = {
      MockedHttpClient
        .post(
          url = url,
          config = dummyHeaderCarrierConfig,
          body = body,
          requiredHeaders = requiredHeaders ++ Seq("Content-Type" -> "application/json"),
          excludedHeaders = Seq("AnotherHeader" -> "HeaderValue")
        )
    }

    protected def willPut[BODY, T](url: String, body: BODY): CallHandler[Future[T]] = {
      MockedHttpClient
        .put(
          url = url,
          config = dummyHeaderCarrierConfig,
          body = body,
          requiredHeaders = requiredHeaders ++ Seq("Content-Type" -> "application/json"),
          excludedHeaders = Seq("AnotherHeader" -> "HeaderValue")
        )
    }

    protected def willDelete[T](url: String): CallHandler[Future[T]] = {
      MockedHttpClient
        .delete(
          url = url,
          config = dummyHeaderCarrierConfig,
          requiredHeaders = requiredHeaders,
          excludedHeaders = Seq("AnotherHeader" -> "HeaderValue")
        )
    }

  }

  protected trait DesTest extends ConnectorTest {

    protected lazy val requiredHeaders: Seq[(String, String)] = requiredDesHeaders

    MockedAppConfig.desBaseUrl returns baseUrl
    MockedAppConfig.desToken returns "des-token"
    MockedAppConfig.desEnvironment returns "des-environment"
    MockedAppConfig.desEnvironmentHeaders returns Some(allowedDesHeaders)

    MockedAppConfig.featureSwitches returns Configuration("tys-api.enabled" -> false)

  }

  protected trait IfsTest extends ConnectorTest {

    protected lazy val requiredHeaders: Seq[(String, String)] = requiredIfsHeaders

    MockedAppConfig.ifsBaseUrl returns baseUrl
    MockedAppConfig.ifsToken returns "ifs-token"
    MockedAppConfig.ifsEnvironment returns "ifs-environment"
    MockedAppConfig.ifsEnvironmentHeaders returns Some(allowedIfsHeaders)

    MockedAppConfig.featureSwitches returns Configuration("tys-api.enabled" -> false)
  }

  protected trait Api1651Test extends ConnectorTest {

    protected lazy val requiredHeaders: Seq[(String, String)] = requiredApi1651Headers

    MockedAppConfig.api1651BaseUrl returns baseUrl
    MockedAppConfig.api1651Token returns "api1651-token"
    MockedAppConfig.api1651Environment returns "api1651-environment"
    MockedAppConfig.api1651EnvironmentHeaders returns Some(allowedIfsHeaders)

    MockedAppConfig.featureSwitches returns Configuration("tys-api.enabled" -> false)
  }

  protected trait TysIfsTest extends ConnectorTest {

    protected lazy val requiredHeaders: Seq[(String, String)] = requiredTysIfsHeaders

    MockedAppConfig.tysIfsBaseUrl returns baseUrl
    MockedAppConfig.tysIfsToken returns "TYS-IFS-token"
    MockedAppConfig.tysIfsEnvironment returns "TYS-IFS-environment"
    MockedAppConfig.tysIfsEnvironmentHeaders returns Some(allowedIfsHeaders)

    MockedAppConfig.featureSwitches returns Configuration("tys-api.enabled" -> true)
  }

}
