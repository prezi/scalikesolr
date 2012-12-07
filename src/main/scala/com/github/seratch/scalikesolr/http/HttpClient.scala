/*
 * Copyright 2011 Kazuhiro Sera
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */

package com.github.seratch.scalikesolr.http

import java.net.{ URL, HttpURLConnection }
import java.io._
import com.github.seratch.scalikesolr.util.{ Log, IO }
import collection.JavaConverters._
import org.apache.solr.common.util.{ NamedList, JavaBinCodec }
import reflect.BeanProperty
import org.slf4j.LoggerFactory
import javax.net.ssl.{ SSLSocketFactory, HttpsURLConnection, SSLContext, TrustManagerFactory, KeyManagerFactory }
import java.security.KeyStore

object HttpClient {
  val DEFAULT_CONNECT_TIMEOUT_MILLIS = 3000
  val DEFAULT_READ_TIMEOUT_MILLIS = 10000
}

class HttpClient(@BeanProperty val connectTimeout: Int = HttpClient.DEFAULT_CONNECT_TIMEOUT_MILLIS,
                 @BeanProperty val readTimeout: Int = HttpClient.DEFAULT_READ_TIMEOUT_MILLIS,
                 @BeanProperty val keyStoreFile: String = null, @BeanProperty val keyStorePassword: String = null) {

  val sslsocketfactory: SSLSocketFactory = createSocketFactory(keyStoreFile, keyStorePassword)

  val log: Log = new Log(LoggerFactory.getLogger(classOf[HttpClient].getCanonicalName))

  def createSocketFactory(keyStoreFile: String, keyStorePassword: String): SSLSocketFactory = {
    if (keyStoreFile != null) {

      val keyStore: KeyStore = KeyStore.getInstance("JKS")
      val trustStore: KeyStore = KeyStore.getInstance("JKS")
      trustStore.load(new java.io.FileInputStream(keyStoreFile+".ca"),keyStorePassword.toCharArray())
      val keyStoreContentStream = new java.io.FileInputStream(keyStoreFile)
      keyStore.load(keyStoreContentStream, keyStorePassword.toCharArray())
      keyStoreContentStream.close()
      val tmf: TrustManagerFactory = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm())
      tmf.init(trustStore)
      val kmf: KeyManagerFactory = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm());
      kmf.init(keyStore, keyStorePassword.toCharArray());
      val ctx: SSLContext = SSLContext.getInstance("TLS")
      ctx.init(kmf.getKeyManagers(), tmf.getTrustManagers(), null)
      val factory = ctx.getSocketFactory()
      return factory
    }
    return null
  }

  def getAsJavabin(urlString: String): JavabinHttpResponse = {
    val url = new URL(urlString)
    val conn = if (url.getProtocol == "https") {
      val sslConnn = url.openConnection().asInstanceOf[HttpsURLConnection]
      if (sslsocketfactory != null) {
        sslConnn.setSSLSocketFactory(sslsocketfactory)
      }
      sslConnn
    } else {
      url.openConnection().asInstanceOf[HttpURLConnection]
    }
    conn.setConnectTimeout(connectTimeout)
    conn.setReadTimeout(readTimeout)
    conn.setRequestMethod("GET")
    try {
      new JavabinHttpResponse(
        conn.getResponseCode,
        conn.getHeaderFields.asScala.map {
          case (k, v) => (k, v.asScala.toList)
        }.toMap,
        new JavaBinCodec().unmarshal(conn.getInputStream).asInstanceOf[NamedList[Any]]
      )
    } catch {
      case e: IOException =>
        IO.using(conn.getErrorStream()) {
          case error: InputStream =>
            val body = IO.readAsString(error)
            log.debug("Failed because " + e.getMessage + "! Body: [" + body + "]");
        }
        throw e
    }
  }

  def get(url: String, charset: String): HttpResponse = {

    val conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection];
    conn.setConnectTimeout(connectTimeout)
    conn.setReadTimeout(readTimeout)
    conn.setRequestMethod("GET")
    try {
      new HttpResponse(
        conn.getResponseCode,
        conn.getHeaderFields.asScala.map {
          case (k, v) => (k, v.asScala.toList)
        }.toMap,
        getResponseContent(conn, charset)
      )
    } catch {
      case e: IOException =>
        IO.using(conn.getErrorStream()) {
          case error: InputStream =>
            val body = IO.readAsString(error)
            log.debug("Failed because " + e.getMessage + "! Body: [" + body + "]");
        }
        throw e
    }

  }

  private val POST_CONTENT_TYPE = "application/x-www-form-urlencoded";

  def post(url: String, dataBinary: String, charset: String): HttpResponse = {
    post(url, dataBinary, POST_CONTENT_TYPE, charset)
  }

  def post(url: String, dataBinary: String, contentType: String, charset: String): HttpResponse = {

    val conn = new URL(url).openConnection().asInstanceOf[HttpURLConnection];
    conn.setConnectTimeout(connectTimeout)
    conn.setReadTimeout(readTimeout)
    conn.setRequestMethod("POST")
    conn.setRequestProperty("Content-Type", contentType)
    conn.setRequestProperty("Content-Length", dataBinary.size.toString)
    conn.setDoOutput(true);
    IO.using(conn.getOutputStream) {
      os =>
        IO.using(new OutputStreamWriter(os, charset))(writer => writer.write(dataBinary))
    }
    try {
      new HttpResponse(
        conn.getResponseCode,
        conn.getHeaderFields.asScala.map {
          case (k, v) => (k, v.asScala.toList)
        }.toMap,
        getResponseContent(conn, charset)
      )
    } catch {
      case e: IOException =>
        IO.using(conn.getErrorStream()) {
          case error: InputStream =>
            val body = IO.readAsString(error)
            log.debug("Failed because " + e.getMessage + "! Body: [" + body + "]");
        }
        throw e
    }

  }

  private def getResponseContent(conn: HttpURLConnection, charset: String): String = {
    IO.readAsString(conn.getInputStream, charset)
  }

}