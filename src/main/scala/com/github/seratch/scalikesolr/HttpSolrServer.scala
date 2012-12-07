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

package com.github.seratch.scalikesolr

import java.net.URL
import util.Log

class HttpSolrServer(val url: URL, val keyStoreFile: String, val keyStorePassword: String) {

  def this(url: URL) = {
    this(url, null, null)
  }

  def newClient(): SolrClient = new HttpSolrClient(url, keyStoreFile, keyStorePassword)

  def getNewClient(): SolrClient = newClient()

  def newClient(connectTimeout: Int, readTimeout: Int): SolrClient = {
    new HttpSolrClient(
      url = url,
      connectTimeout = connectTimeout,
      readTimeout = readTimeout,
      keyStoreFile = keyStoreFile,
      keyStorePassword = keyStorePassword
    )
  }

  def getNewClient(connectTimeout: Int, readTimeout: Int): SolrClient = {
    newClient(connectTimeout, readTimeout)
  }

  def newClient(log: Log): SolrClient = new HttpSolrClient(url, log)

  def getNewClient(log: Log): SolrClient = newClient(log)

  def newClient(log: Log, connectTimeout: Int, readTimeout: Int): SolrClient = {
    new HttpSolrClient(
      url = url,
      connectTimeout = connectTimeout,
      readTimeout = readTimeout,
      log = log,
      keyStoreFile = null,
      keyStorePassword = null
    )
  }

  def getNewClient(log: Log, connectTimeout: Int, readTimeout: Int): SolrClient = {
    newClient(log, connectTimeout, readTimeout)
  }

}
