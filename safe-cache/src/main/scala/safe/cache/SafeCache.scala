/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * @author Vamsi Thummala {vamsi@cs.duke.edu}, Copyright (C) 2013-2015
 *
 */



package safe.cache

//import java.util.concurrent.ConcurrentHashMap // TODO: prompt for ConcurrentHashMap or ConcurrentLinkedHashMap during class initialization
import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap

trait CacheLike[K, V] // TODO: to implement later?

class SafeCache[K, V](
    initialCapacity: Int   = 1024 * 1024 // initialCapacity, i.e., 1024 statements in a context
  , loadFactor: Float      = 0.99f       // loadFactor; we do not expect to rehash often
  , concurrencyLevel: Int  = 16          // concurrencyLevel; not many writers at the same time?
) extends CacheLike[K, V] {

  import scala.collection.JavaConversions._

  private[this] val store = new ConcurrentLinkedHashMap.Builder[K, V]
    .initialCapacity(initialCapacity)
    .maximumWeightedCapacity(initialCapacity)
    .concurrencyLevel(concurrencyLevel)
    .build()

  def get(key: K): Option[V] = Option(store.get(key))
  def put(key: K, value: V): V = store.put(key, value)
  def putIfAbsent(key: K, value: V): V = store.putIfAbsent(key, value)
  def clear(): Boolean = {
    store.clear()
    true
  }
  def values(): java.util.Collection[V] = store.values()
  def containsKey(key: K): Boolean = store.containsKey(key)
  def keySet(): Set[K] = store.keySet().toSet
  def remove(key: K): V = store.remove(key)
  def remove(key: K, value: V): Boolean = store.remove(key, value)
  def replace(key: K, value: V): V = store.replace(key, value)
  def replace(key: K, oldvalue: V, newvalue: V): Boolean = store.replace(key, oldvalue, newvalue)
  def capacity(): Long = store.capacity()
  def isEmpty(): Boolean = store.isEmpty()
  def size(): Int = store.size()
}
