object MyMath {
  opaque type Logarithm = Double

  object Logarithm {
    // These are the two ways to lift to the Logarithm type

    def apply(d: Double): Logarithm = math.log(d)

    def safe(d: Double): Option[Logarithm] =
      if d > 0.0 then Some(math.log(d)) else None
  }

  // Extension methods define opaque types' public APIs
  extension (x: Logarithm) {
    def toDouble: Double = math.exp(x)
    def +(y: Logarithm): Logarithm = Logarithm(math.exp(x) + math.exp(y))
    def *(y: Logarithm): Logarithm = x + y
  }

}

class Logarithms {
  opaque type Logarithm = Double

  def apply(d: Double): Logarithm = math.log(d)

  def safe(d: Double): Option[Logarithm] =
    if d > 0.0 then Some(math.log(d)) else None

  def mul(x: Logarithm, y: Logarithm) = x + y
}

object Access {
  opaque type Permissions = Int
  opaque type PermissionChoice = Int
  opaque type Permission <: Permissions & PermissionChoice = Int

  extension (x: PermissionChoice)
    def |(y: PermissionChoice): PermissionChoice = x | y
  extension (x: Permissions) def &(y: Permissions): Permissions = x | y
  extension (granted: Permissions)
    def is(required: Permissions) = (granted & required) == required
    def isOneOf(required: PermissionChoice) = (granted & required) != 0

  val NoPermission: Permission = 0
  val Read: Permission = 1
  val Write: Permission = 2
  val ReadWrite: Permissions = Read | Write
  val ReadOrWrite: PermissionChoice = Read | Write
}

object User {
  import Access.*

  case class Item(rights: Permissions)
  extension (item: Item)
    def +(other: Item): Item = Item(item.rights & other.rights)

  val roItem = Item(Read) // OK, since Permission <: Permissions
  val woItem = Item(Write)
  val rwItem = Item(ReadWrite)
  val noItem = Item(NoPermission)

  def run(): Unit = {
    assert(!roItem.rights.is(ReadWrite))
    assert(roItem.rights.isOneOf(ReadOrWrite))

    assert(rwItem.rights.is(ReadWrite))
    assert(rwItem.rights.isOneOf(ReadOrWrite))

    assert(!noItem.rights.is(ReadWrite))
    assert(!noItem.rights.isOneOf(ReadOrWrite))

    assert((roItem + woItem).rights.is(ReadWrite))

    // 型エラー
    // assert(roItem.rights.isOneOf(ReadWrite))
    //                              ^^^^^^^^^
    //                              Found:    (Access.ReadWrite : Access.Permissions)
    //                              Required: Access.PermissionChoice
  }
}

object Main {

  import MyMath.Logarithm

  type MyInt = Int

  def main(args: Array[String]): Unit = {
    val l = Logarithm(1.0)
    val l2 = Logarithm(2.0)
    val l3 = l * l2
    val l4 = l + l2

    val x: MyInt = 1
    val y: Int = 2
    x + y

    User.run()

    val cl1 = new Logarithms
    val cl2 = new Logarithms
    val a = cl1(1.5)
    val b = cl1(2.6)
    val c = cl2(3.1)
    cl1.mul(a, b) // type checks
    // cl2.mul(a, c) // error: found cl2.Logarithm, required cl1.Logarithm
  }
}
