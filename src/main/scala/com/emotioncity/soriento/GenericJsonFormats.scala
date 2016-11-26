package com.emotioncity.soriento

import java.util

import scala.collection.JavaConversions

import play.api.libs.json._

/**
 * Created by baddlan on 16-11-25.
 */
object GenericJsonFormats {

  def JsObjectToMap(j: JsObject): util.Map[String, Any] = {
    val newMap = j.value.map {
      case (k, v: JsNumber) => (k, if (v.value.isValidInt) new java.lang.Integer(v.value.intValue()) else new java.lang.Double(v.value.doubleValue()))
      case (k, v: JsString) => (k, new java.lang.String(v.value))
      case (k, v: JsBoolean) => (k, new java.lang.Boolean(v.value))
      case (k, v: JsObject) => (k, JsObjectToMap(v))
      case (k, v: JsArray) => (k, JsArrayToMap(v))
      case (k, _: JsValue) => (k, null)
    }

    JavaConversions.mapAsJavaMap(newMap)
  }

  def JsArrayToMap(j: JsArray): util.List[Any] = {
    val list = j.value.map {
      case (v: JsNumber) => if (v.value.isValidInt) new java.lang.Integer(v.value.intValue()) else new java.lang.Double(v.value.doubleValue())
      case (v: JsString) => new java.lang.String(v.value)
      case (v: JsBoolean) => new java.lang.Boolean(v.value)
      case (v: JsObject) => JsObjectToMap(v)
      case (v: JsArray) => JsArrayToMap(v)
      case (_: JsValue) => null
    }

    JavaConversions.seqAsJavaList(list)
  }

}
