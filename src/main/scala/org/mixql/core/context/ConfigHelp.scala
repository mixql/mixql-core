package org.mixql.core.context

import scala.collection.mutable.{Map => MutMap}
import scala.collection.JavaConverters._
import java.{util => ju}
import org.mixql.core.context.gtype._
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}
import com.typesafe.config.ConfigValueType._

object ConfigHelp {
  def parseConfig(params: ConfigObject): MutMap[String, Type] = recurseParseParams(params)

  private def recurseParseParams(params: ConfigObject, scope: String = ""): MutMap[String, Type] = {
    val res = MutMap[String, Type]()
    params.asScala.foreach(kv => {
      if (kv._2.valueType() == OBJECT)
        res ++= recurseParseParams(kv._2.asInstanceOf[ConfigObject], scope + kv._1 + ".")
      else
        res += scope + kv._1 -> convertConfigValue(kv._2.unwrapped)
    })
    res
  }

  private def convertConfigValue(value: Object): Type = {
    if (value == null)
      new Null()
    else if (value.isInstanceOf[Boolean])
      new bool(value.asInstanceOf[Boolean])
    else if (value.isInstanceOf[String])
      new string(value.asInstanceOf[String])
    else if (value.isInstanceOf[Integer])
      new gInt(value.asInstanceOf[Integer])
    else if (value.isInstanceOf[Double])
      new gDouble(value.asInstanceOf[Double])
    else if (value.isInstanceOf[ju.List[Object]])
      new array(value.asInstanceOf[ju.List[Object]].asScala.map(convertConfigValue).toArray)
    else
      throw new Exception("unknown param type")
  }
}
