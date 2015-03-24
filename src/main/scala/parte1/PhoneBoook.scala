package parte1

/**
 * Created by leonardo on 10/03/15.
 */
class PhoneBook(owner: User) {
  require(owner!=null, "Phonebook owner cannot be null")

  def findUser( name: String): Option[User] = {
    val ret = contacts find {
      case x: User if x.name == name => return Some(x)
      case _ => return None
    }
    ret
  }

  def findContact(name: String) = {
    require(name!=null, "Cannot search for a null user")
    val ret = contacts filter({x:User => x.name.toLowerCase.contains(name.toLowerCase())})
    ret.toList.sortBy(_.name)
  }

  def addPhoneNumber(nameUser: String, number: Int) = {
    val user = new User(nameUser)
    findUser(nameUser) match {
      case x: Some[User] => x.get.addNumber(number)
      case _ => throw new IllegalArgumentException("There is no user named ".concat(nameUser) )
    }
  }

  var contacts = List[User]()

  def addContact(user: User) = {
    user match {
      case null => throw new IllegalArgumentException("requirement failed: Contact cannot be null")
      case _ => user
    }

    val ret = findUser(user.name) match {
      case x: Some[User] => throw new IllegalArgumentException("requirement failed: Cannot add repeated contacts")
      case None => contacts = user :: contacts
    }
    ret
  }




}
