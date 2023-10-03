package org.mixql.core.context

import scala.collection.mutable.{Map => MutMap}
import scala.collection.JavaConverters._
import java.{util => ju}
import org.mixql.core.context.mtype._
import com.typesafe.config.{Config, ConfigFactory, ConfigObject}
import com.typesafe.config.ConfigValueType._

object ConfigHelp {
  def parseConfig(params: ConfigObject): MutMap[String, MType] = recurseParseParams(params)

  private def recurseParseParams(params: ConfigObject, scope: String = ""): MutMap[String, MType] = {
    val res = MutMap[String, MType]()
    params.asScala.foreach(kv => {
      if (kv._2.valueType() == OBJECT)
        res ++= recurseParseParams(kv._2.asInstanceOf[ConfigObject], scope + kv._1 + ".")
      else
        res += scope + kv._1 -> convertConfigValue(kv._2.unwrapped)
    })
    res
  }

  private def convertConfigValue(value: Object): MType = {
    if (value == null)
      MNull.get()
    else if (value.isInstanceOf[Boolean])
      new MBool(value.asInstanceOf[Boolean])
    else if (value.isInstanceOf[String])
      new MString(value.asInstanceOf[String])
    else if (value.isInstanceOf[Integer])
      new MInt(value.asInstanceOf[Integer])
    else if (value.isInstanceOf[Long])
      new MInt(value.asInstanceOf[Long])
    else if (value.isInstanceOf[Double])
      new MDouble(value.asInstanceOf[Double])
    else if (value.isInstanceOf[ju.List[Object]])
      new MArray(value.asInstanceOf[ju.List[Object]].asScala.map(convertConfigValue).toArray)
    else
      throw new Exception("unknown param type")
  }
}
