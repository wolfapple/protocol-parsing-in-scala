package TextProtocol

import javax.tools.Diagnostic

import scala.util.parsing.combinator.RegexParsers

case class Name(name: String) {
  override def toString: String = s"""%NAME\n"$name"\n"""
}

case class Address(domain: Int, subnet: Int, node: Int) {
  override def toString() = s"%ADDRESS\n$domain.$subnet.$node\n"
}

case class HardwareID(id: String) {
  override def toString() = s"%HWID\n$id\n"
}

case class DevInfo(name: Name, address: Address, hardwareID: HardwareID) {
  override def toString() = s"%DEVINFO<$name$address$hardwareID>"
}

trait DaqRateType
case class DaqRateEvent() extends DaqRateType {
  override def toString: String = "E"
}
case class DaqRatePeriodic(interval: Int) extends DaqRateType {
  override def toString: String = s"P,$interval"
}

case class DaqRate(daqRate: DaqRateType) {
  override def toString() = s"%DR\n$daqRate\n"
}

case class Range(min: Double, max: Double) {
  override def toString() = s"%RANGE\n$min,$max\n"
}

case class SAType(saType: Int) {
  override def toString() = s"%TYPE\n$saType\n"
}

case class Enabled(enabled: Boolean) {
  override def toString() = "%EN\n" + (if (enabled) "1" else "0") + "\n"
}

case class Diagnostic(diagnostic: String) {
  override def toString() = s"%DIAGSTART\n$diagnostic%DIAGEND\n"
}

case class Sensor(index: Int,
                  name: Name,
                  daqRate: Option[DaqRate],
                  range: Option[Range],
                  saType: Option[SAType],
                  enabled: Option[Enabled],
                  diagnostic: Option[Diagnostic]) {
  override def toString() =
    s"%SENSOR$index<$name" +
      daqRate.map(_.toString).getOrElse("") +
      range.map(_.toString).getOrElse("") +
      saType.map(_.toString).getOrElse("") +
      enabled.map(_.toString).getOrElse("") +
      diagnostic.map(_.toString).getOrElse("") + ">"
}

case class Actuator(index: Int,
                    name: Name,
                    range: Option[Range],
                    saType: Option[SAType],
                    enabled: Option[Enabled],
                    diagnostic: Option[Diagnostic]) {
  override def toString() =
    s"%ACTUATOR$index<$name" +
      range.map(_.toString).getOrElse("") +
      saType.map(_.toString).getOrElse("") +
      enabled.map(_.toString).getOrElse("") +
      diagnostic.map(_.toString).getOrElse("") + ">"
}

case class Device(devInfo: DevInfo,
                  sensors: List[Sensor],
                  actuators: List[Actuator]) {
  override def toString() =
    "[" + devInfo.toString +
      sensors.mkString + actuators.mkString + "]"
}

object TextProtocol extends RegexParsers {
  override def skipWhitespace: Boolean = false

  def nl: Parser[String] = "\n"

  def int = "[-+]?[0-9]+".r ^^ { _.toInt }

  def double = """[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?""".r ^^ { _.toDouble }

  def qstring = "\"" ~> ("\"\"" | "[^\"]".r).* <~ "\"" ^^ { _.mkString }

  def bool = ("0" | "1") ^^ { _.toInt != 0 }

  def name = "%NAME" ~ nl ~ qstring ~ nl ^^ {
    case _ ~ _ ~ name ~ _ => Name(name)
  }

  def address = "%ADDRESS" ~ nl ~ int ~ "." ~ int ~ "." ~ int ~ nl ^^ {
    case _ ~ _ ~ domain ~ _ ~ subnet ~ _ ~ node ~ _ =>
      Address(domain, subnet, node)
  }

  def hardwareID = "%HWID" ~ nl ~ "[0-9A-F]{8}".r ~ nl ^^ {
    case _ ~ _ ~ id ~ _ => HardwareID(id)
  }

  def devInfo = "%DEVINFO" ~ "<" ~ name ~ address ~ hardwareID ~ ">" ^^ {
    case _ ~ _ ~ name ~ address ~ hardwareId ~ _ =>
      DevInfo(name, address, hardwareId)
  }

  def daqRateEvent = "E" ^^ {
    case _ => DaqRateEvent()
  }

  def daqRate_Periodic = "P," ~ int ^^ {
    case _ ~ interval => DaqRatePeriodic(interval)
  }

  def daqRate = "%DR" ~ nl ~ (daqRateEvent | daqRate_Periodic) ~ nl ^^ {
    case _ ~ _ ~ daqRate ~ _ => DaqRate(daqRate)
  }

  def range = "%RANGE" ~ nl ~ double ~ "," ~ double ~ nl ^^ {
    case _ ~ _ ~ min ~ _ ~ max ~ _ => Range(min, max)
  }

  def saType = "%TYPE" ~ nl ~ int ~ nl ^^ {
    case _ ~ _ ~ saType ~ _ => SAType(saType)
  }

  def enabled = "%EN" ~ nl ~ bool ~ nl ^^ {
    case _ ~ _ ~ enabled ~ _ => Enabled(enabled)
  }

  def diagnostic = new Parser[Diagnostic] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      val secStart = "%DIAGSTART\n"
      val secEnd = "%DIAGEND\n"
      val text = source.subSequence(start, source.length).toString
      val iStart = text.indexOf(secStart)
      if (iStart > -1) {
        val content = text.drop(iStart + secStart.length)
        val iEnd = content.indexOf(secEnd)
        if (iEnd == -1)
          Success(Diagnostic(content),
                  in.drop(start + secStart.length + content.length - offset))
        else {
          val strippedContent = content.take(iEnd)
          Success(Diagnostic(strippedContent),
                  in.drop(
                    start + secStart.length + strippedContent.length +
                      secEnd.length - offset))
        }
      } else
        Failure(s"$secStart not found", in.drop(start - offset))
    }
  }

  def sensor =
    "%SENSOR" ~ int ~ "<" ~ name ~ daqRate.? ~ range.? ~
      saType.? ~ enabled.? ~ diagnostic.? ~ ">" ^^ {
      case _ ~ index ~ _ ~ name ~ daqRate ~ range ~
            saType ~ enabled ~ diagnostic ~ _ =>
        Sensor(index, name, daqRate, range, saType, enabled, diagnostic)
    }

  def actuator =
    "%ACTUATOR" ~ int ~ "<" ~ name ~ range.? ~ saType.? ~ enabled.? ~ diagnostic.? ~ ">" ^^ {
      case _ ~ index ~ _ ~ name ~ range ~ saType ~ enabled ~ diagnostic ~ _ =>
        Actuator(index, name, range, saType, enabled, diagnostic)
    }

  def device = "[" ~> devInfo ~ sensor.* ~ actuator.* <~ "]" ^^ {
    case devInfo ~ sensors ~ actuators => Device(devInfo, sensors, actuators)
  }

  def parseDevice(s: String): Option[Device] = {
    parse(device, s) match {
      case Success(matched, _) => Some(matched)
      case Failure(msg, _) => {
        println("Parse failure of " + s + " due to: " + msg); None
      }
      case Error(msg, _) => {
        println("Parse error of " + s + " due to: " + msg); None
      }
    }
  }
}
