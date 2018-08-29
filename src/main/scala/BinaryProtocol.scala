package BinaryProtocol

import scala.language.implicitConversions
import scodec._
import scodec.bits._
import scodec.codecs._
import scodec.codecs.implicits._

case class HardwareAddress(address: Vector[Int]) {
  require(address.length == 8)
  address.foreach(digit => require((0x00 <= digit) && (digit <= 0x0F)))
}

object HardwareAddress {
  implicit val codec: Codec[HardwareAddress] = {
    (constant(bin"0")) ::
      (ignore(3)) ::
      ("address" | vectorOfN(provide(8), uint(4)))
  }.as[HardwareAddress]
}

case class NetworkAddress(domain: Int, subnet: Int, node: Int) {
  require((0 <= domain) && (domain <= 7))
  require((0 <= subnet) && (subnet <= 255))
  require((0 <= node) && (node <= 255))
}

object NetworkAddress {
  implicit val codec: Codec[NetworkAddress] = {
    (constant(bin"1")) ::
      ("domain" | uint(3)) ::
      ("subnet" | uint8) ::
      ("node" | uint8)
  }.as[NetworkAddress]
}

object AddressType extends Enumeration {
  type AddressType = Value
  val HARDWARE_ADDRESS = Value(0)
  val NETWORK_ADDRESS = Value(1)
}
import AddressType._

class AddressTypeCodec extends Codec[AddressType] {
  override def sizeBound: SizeBound = SizeBound.exact(1)
  override def encode(value: AddressType): Attempt[BitVector] =
    Attempt.successful(BitVector.empty)
  override def decode(bits: BitVector): Attempt[DecodeResult[AddressType]] =
    bits.acquire(1) match {
      case Left(e) => Attempt.failure(Err.insufficientBits(1, bits.size))
      case Right(b) =>
        Attempt.successful(
          DecodeResult(if (b(0)) NETWORK_ADDRESS else HARDWARE_ADDRESS, bits))
    }
  override def toString: String = s"AddressTypeCodec"
}

object CommandType extends Enumeration {
  type CommandType = Value
  val REQUEST = Value(0)
  val RESPONSE = Value(1)
  val UNSOLICITED = Value(2)
}
import CommandType._

object PacketImplicits {
  implicit val addressTypeCodec = new AddressTypeCodec()
  implicit val commandTypeCodec: Codec[CommandType] =
    mappedEnum(uint(2), CommandType.values.map(v => (v, v.id)).toMap)
  implicit def hardwareAddress2Address(address: HardwareAddress) =
    new Address(HARDWARE_ADDRESS, Some(address), None)
  implicit def networkAddress2Address(address: NetworkAddress) =
    new Address(NETWORK_ADDRESS, None, Some(address))
}
import PacketImplicits._

case class Address(addressType: AddressType,
                   hwAddress: Option[HardwareAddress],
                   netAddress: Option[NetworkAddress])

object Address {
  implicit val codec = {
    (("address_type" | Codec[AddressType]) >>:~ { addressType =>
      ("hardward_address" | conditional(addressType == HARDWARE_ADDRESS,
                                        Codec[HardwareAddress])) ::
        ("network_address" | conditional(addressType == NETWORK_ADDRESS,
                                         Codec[NetworkAddress]))
    }).as[Address]
  }
}

case class Packet(highPriority: Boolean,
                  tag: Int,
                  address: Address,
                  commandType: CommandType,
                  command: Int,
                  dataLength: Int,
                  data: Vector[Int]) {
  require(dataLength == data.length)
}

object Packet {
  implicit val codec: Codec[Packet] = {
    ("high_priority" | bool) ::
      ("tag" | uint(3)) ::
      ("address" | Codec[Address]) ::
      ("command_type" | Codec[CommandType]) ::
      ("command" | uint(6)) ::
      (("data_length" | uint16) >>:~ { length =>
      ("data" | vectorOfN(provide(length), uint8)).hlist
    })
  }.as[Packet]

  def apply(highPriority: Boolean,
            tag: Int,
            address: Address,
            commandType: CommandType,
            command: Int,
            data: Vector[Int] = Vector()) =
    new Packet(highPriority,
               tag,
               address,
               commandType,
               command,
               data.length,
               data)
}
