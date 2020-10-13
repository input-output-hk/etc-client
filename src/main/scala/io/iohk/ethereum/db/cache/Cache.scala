package io.iohk.ethereum.db.cache

import io.iohk.ethereum.common.SimpleMap
import scala.collection.Seq

trait Cache[K, V] extends SimpleMap[K, V, Cache[K,V]] {
  def getValues: Seq[(K, V)]
  def clear(): Unit
  def shouldPersist: Boolean
}
