package parte1

/**
 * Created by leonardo on 10/03/15.
 */
class User(var name:String) {
  var phones = List[Int]()

  def addName(user: User) = {
    name = user.name
  }

  implicit def stringToUser(name: String) ={
    new User(name)
  }

  def findPhone( number: Int): Option[Int] = {
    val ret = phones find {
      case x: Int if x == number => return Some(x)
      case _ => throw new IllegalArgumentException("There is no user named ".concat(name) )
    }
    ret
  }
  def addNumber(number:Int) = {
    require(number!=null, "Contact cannot be null")

    val ret = findPhone(number) match {
      case x: Some[Int] => throw new IllegalArgumentException("requirement failed: Cannot add repeated number to a contact")
      case None => phones = number :: phones
    }
    ret
  }
  override def toString(): String ={
    (name, phones).toString()
  }

  def main(args: Array[String]) {
    val a = new User("João");
    print(a.name)
    a.addName("Luis")
    print(a.name)
  }
}
