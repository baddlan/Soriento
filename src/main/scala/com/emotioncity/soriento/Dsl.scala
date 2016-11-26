package com.emotioncity.soriento

import java.util.Collections

import com.emotioncity.soriento.ReflectionUtils._
import com.orientechnologies.orient.core.record.impl.ODocument

import scala.collection.JavaConverters._

import play.api.libs.json.JsObject

/**
  * Created by stream on 31.10.14.
  */
trait Dsl {

  implicit def productToDocument[T >: Any](cc: Product): ODocument = {
    val modelName = cc.getClass.getSimpleName
    //println(s"Product name: $modelName")
    val ridOpt = rid(cc)
    val document = if (ridOpt.isDefined) new ODocument(modelName, ridOpt.get) else new ODocument(modelName)
    //println(s"document rid: ${document.getIdentity}")
    val values = cc.productIterator
    val fieldList = cc.getClass.getDeclaredFields.toList
    val purifiedFromId = values.zip(fieldList.iterator).toList.filter { case (v, f) => !isId(f.getName, cc.getClass) }
    purifiedFromId.foreach { case (value, field) =>
      val fieldName = field.getName
      val fieldValue = value match {
        case p: Product if p.productArity > 0 =>
          p match {
            case Some(v) =>
              if (ReflectionUtils.isCaseClass(ReflectionUtils.getTypeForClass(v.getClass))) {
                if (v.isInstanceOf[play.api.libs.json.JsObject]) {
                  GenericJsonFormats.JsObjectToMap(v.asInstanceOf[JsObject])
                } else {
                  productToDocument(v.asInstanceOf[Product])
                }
              } else {
                v
              }
            case _: List[_] =>
              p.asInstanceOf[List[_]].map {
                case cc: Product =>
                  productToDocument(cc)
                case item =>
                  item
              }.asJavaCollection
            case _ => productToDocument(p)
          }
        case x =>
          x match {
            case _: Set[_] =>
              x.asInstanceOf[Set[_]].map {
                case cc: Product =>
                  productToDocument(cc)
                case item =>
                  item
              }.asJavaCollection
            case Nil =>
              Collections.emptyList
            case _ => x
          }
      }
      if (fieldValue != None) {
        val oType = getOType(fieldName, field, field.getDeclaringClass)
        //println(s"Field metadata: Name: $fieldName, Value: $fieldValue, OType: $oType")
        document.field(fieldName, fieldValue, oType)
      }
    }
    document
  }

  /*

    def saveAs[T](implicit reader: ODocumentReader[T], orientDb: ODatabaseDocument): Option[T] = {
      import RichODatabaseDocumentImpl._
      orientDb.saveAs[T](oDocument)
    }*/

}

