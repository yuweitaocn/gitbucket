package gitbucket.core.util

import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper
import org.elasticsearch.action.search.SearchResponse

import scala.reflect.ClassTag
import scala.collection.JavaConverters._

object Elastic4sSupport {

  private val mapper = new ObjectMapper with ScalaObjectMapper
  mapper.enable(DeserializationFeature.UNWRAP_SINGLE_VALUE_ARRAYS)
  mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false)
  mapper.registerModule(DefaultScalaModule)

  private def serialize(doc: AnyRef): String = mapper.writeValueAsString(doc)

  private def deserialize[T](json: String)(implicit c: ClassTag[T]): T = mapper.readValue(json, c.runtimeClass).asInstanceOf[T]

  private def structuredMap(map: Map[String, AnyRef]): Map[String, AnyRef] = {
    def structuredMap0(group: List[(List[String], AnyRef)]): AnyRef = {
      group.groupBy { case (key, value) => key.head }.map { case (key, value) =>
        key -> (if(value.head._1.length == 1){
          value.head._2
        } else {
          structuredMap0(value.map { case (key, value) => key.tail -> value })
        })
      }
    }

    val list = map.map { case (key, value) => key.split("\\.").toList -> value }.toList
    structuredMap0(list).asInstanceOf[Map[String, AnyRef]]
  }

  implicit class RichRichSearchResponse(resp: SearchResponse){
    def docs[T](implicit c: ClassTag[T]): Seq[T] = {
      resp.getHits.getHits.flatMap { hit =>
        Option(hit.sourceAsString).map { json =>
          // deserialize from "_source"
          deserialize[T](json)
        } orElse {
          // deserialize from "fields"
          Option(hit.fields).map(_.asScala.toSeq).map {
            _.map { case (name, values) =>
              name -> values.getValues
            }.toMap
          }.map { _fields =>
            deserialize[T](serialize(structuredMap(_fields)))
          }
        } orElse None
      }.toSeq
    }
  }

}
