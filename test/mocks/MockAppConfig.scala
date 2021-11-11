/*
 * Copyright 2021 HM Revenue & Customs
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

package mocks

import config.{AppConfig, ConfidenceLevelConfig}
import org.scalamock.handlers.CallHandler
import org.scalamock.scalatest.MockFactory
import play.api.Configuration

trait MockAppConfig extends MockFactory {

  val mockAppConfig: AppConfig = mock[AppConfig]

  object MockAppConfig {
    //DES Config
    def desBaseUrl: CallHandler[String] = (mockAppConfig.desBaseUrl _: () => String).expects()
    def desToken: CallHandler[String] = (mockAppConfig.desToken _).expects()
    def desEnvironment: CallHandler[String] = (mockAppConfig.desEnv _).expects()
    def desEnvironmentHeaders: CallHandler[Option[Seq[String]]] = (mockAppConfig.desEnvironmentHeaders _).expects()

    //IFS Config
    def ifsBaseUrl: CallHandler[String] = (mockAppConfig.ifsBaseUrl _: () => String).expects()
    def ifsToken: CallHandler[String] = (mockAppConfig.ifsToken _).expects()
    def ifsEnvironment: CallHandler[String] = (mockAppConfig.ifsEnv _).expects()
    def ifsEnvironmentHeaders: CallHandler[Option[Seq[String]]] = (mockAppConfig.ifsEnvironmentHeaders _).expects()

    //Api1651 Config
    def api1651BaseUrl: CallHandler[String] = (mockAppConfig.api1651BaseUrl _: () => String).expects()
    def api1651Token: CallHandler[String] = (mockAppConfig.api1651Token _).expects()
    def api1651Environment: CallHandler[String] = (mockAppConfig.api1651Env _).expects()
    def api1651EnvironmentHeaders: CallHandler[Option[Seq[String]]] = (mockAppConfig.api1651EnvironmentHeaders _).expects()

    //Release7 Config
    def release7BaseUrl: CallHandler[String] = (mockAppConfig.release7BaseUrl _: () => String).expects()
    def release7Token: CallHandler[String] = (mockAppConfig.release7Token _).expects()
    def release7Environment: CallHandler[String] = (mockAppConfig.release7Env _).expects()
    def release7EnvironmentHeaders: CallHandler[Option[Seq[String]]] = (mockAppConfig.release7EnvironmentHeaders _).expects()

    //MTD IF Lookup Config
    def mtdIdBaseUrl: CallHandler[String] = (mockAppConfig.mtdIdBaseUrl _: () => String).expects()

    //Business Rule Config
    def minimumPermittedTaxYear: CallHandler[Int] = (mockAppConfig.minimumPermittedTaxYear _).expects()

    //API Config
    def featureSwitch: CallHandler[Option[Configuration]] = (mockAppConfig.featureSwitch _: () => Option[Configuration]).expects()
    def apiGatewayContext: CallHandler[String] = (mockAppConfig.apiGatewayContext _: () => String).expects()
    def apiStatus: CallHandler[String] = (mockAppConfig.apiStatus: String => String).expects("1.0")
    def endpointsEnabled: CallHandler[Boolean] = (mockAppConfig.endpointsEnabled: String => Boolean).expects("1.0")
    def confidenceLevelCheckEnabled: CallHandler[ConfidenceLevelConfig] = (mockAppConfig.confidenceLevelConfig _: () => ConfidenceLevelConfig).expects()
  }
}