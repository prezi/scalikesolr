package com.github.seratch.scalikesolr.request.query.highlighting

import org.scalatest._
import org.scalatest.matchers._

class IsFieldMatchEnabledSpec extends FlatSpec with ShouldMatchers {

  behavior of "IsFieldMatchEnabled"

  it should "be available" in {
    val requireFieldMatch: Boolean = false
    val instance = new IsFieldMatchEnabled(requireFieldMatch)
    instance should not be null
    instance.getKey() should equal("hl.requireFieldMatch")
  }

}
