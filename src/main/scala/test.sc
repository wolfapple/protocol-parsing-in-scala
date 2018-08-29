import BinaryProtocol.{CommandType, NetworkAddress, Packet}
import BinaryProtocol.PacketImplicits._
import scodec.Codec
import scodec.bits._

val packet = Packet(false, 0, NetworkAddress(1, 2, 3), CommandType.REQUEST, 4, Vector(1, 2, 3))
val binary = Codec.encode(packet).require.toBin
val decoded = Codec[Packet].decode(BitVector.fromBin(binary).get).require.value
packet == decoded





import TextProtocol._

TextProtocol.parse(TextProtocol.qstring, "\"barney\"").get == "barney"
TextProtocol.parse(TextProtocol.qstring, "barney\"").successful

val name = "%NAME\n\"barney\"\n"
TextProtocol.parse(TextProtocol.name, name).get.name == "barney"

val addr = "%ADDRESS\n1.2.3\n"
TextProtocol.parse(TextProtocol.address, addr).get

val hwId = "%HWID\n0123ABCD\n"
TextProtocol.parse(TextProtocol.hardwareID, hwId).get

val devInfo = s"%DEVINFO<$name$addr$hwId>"
TextProtocol.parse(TextProtocol.devInfo, devInfo).successful

val sensor1 = "%SENSOR1<%NAME\n\"Temperature\"\n%DR\nP,60\n%RANGE\n-20.0,120.0\n%TYPE\n1\n%EN\n1\n%DIAGSTART\nI'm broken%DIAGEND\n>"
TextProtocol.parse(TextProtocol.sensor, sensor1).successful

val sensor2 = "%SENSOR2<%NAME\n\"IsRaining\"\n%DR\nE\n%RANGE\n0.0,1.0\n%TYPE\n3\n>"
TextProtocol.parse(TextProtocol.sensor, sensor2).successful

val actuator = "%ACTUATOR1<%NAME\n\"Alarm\"\n%RANGE\n0.0,1.0\n%TYPE\n1\n%EN\n1\n>"
TextProtocol.parse(TextProtocol.actuator, actuator).successful

val deviceString = s"[$devInfo$sensor1$sensor2$actuator]"
val device = TextProtocol.parseDevice(deviceString).get
device.devInfo
device.sensors
device.actuators

println(device.toString())