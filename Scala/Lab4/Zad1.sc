import scala.annotation.tailrec

sealed trait tree[+A] // nowy typ

case object Leaf extends tree[Nothing]

case class Node[+A](elem: A, child: tree[A]) extends tree[A]


sealed trait snapshot

case class Origin(string: String) extends snapshot

case class Insert(pos: Int, insertion: String) extends snapshot

case class Delete(pos: Int, len: Int) extends snapshot

case class Move(pos: Int, len: Int, dest: Int) extends snapshot


def getString(a: Option[String]): String = {
  a.toString.substring(7, a.toString.length - 1)
}

def insertPos(insertion: String, i: Int, str: String): Option[String] = {
  val length = str.length()

  if length >= i && i >= 0 then Some(str.substring(0, i) + insertion + str.substring(i, length))
  else None
}

def deletePos(pos: Int, len: Int, str: String): Option[String] = {
  val length = str.length()

  if (pos + len) > length || pos < 0 || len <= 0 then None
  else Some(str.substring(0, pos) + str.substring(pos + len, length))
}

def movePos(pos: Int, len: Int, dest: Int, str: String): Option[String] = {
  val length = str.length()

  if pos > length || dest > length || (pos + len) > length || pos < 0 || len <= 0 || dest < 0 then None
  else {
    val toMove = str.substring(pos, pos + len)
    insertPos(toMove, dest, getString(deletePos(pos, len, str)))
  }
}

def insert[A](x: A, t: tree[A]): tree[A] = {
  t match
    case Leaf => Node(x, Leaf)
    case Node(value, child) => Node(value, insert(x, child))
}

def review(tr: tree[snapshot], snap: snapshot): Option[String] = {
  @tailrec
  def reviewRec(str: Option[String], t: tree[snapshot]): Option[String] = {
    str match
      case None => None
      case Some(_) => t match
        case Leaf => str
        case Node(snap, child) => snap match
          case Origin(st) => reviewRec(Some(st), child)
          case Insert(i, st) => reviewRec(insertPos(st, i, getString(str)), child)
          case Delete(i, j) => reviewRec(deletePos(i, j, getString(str)), child)
          case Move(i, j, k) => reviewRec(movePos(i, j, k, getString(str)), child)
  }

  reviewRec(Some(""), insert(snap, tr))
}

val insert1 = insertPos("LOOL", 0, "Wladyslaw")
val insert2 = insertPos("LOOL", 4, "Wladyslaw")
val insert3 = insertPos("LOOL", 9, "Wladyslaw")
val insert4 = insertPos("LOOL", 12, "Wladyslaw")
val insert5 = insertPos("LOOL", (-1), "Wladyslaw")

val delete1 = deletePos(7, 2, "Wladyslaw")
val delete2 = deletePos(0, 3, "Wladyslaw")
val delete3 = deletePos(0, 9, "Wladyslaw")
val delete4 = deletePos(4, 9, "Wladyslaw")
val delete5 = deletePos((-3), (-5), "Wladyslaw")

val move1 = movePos(3, 3, 5, "Wladyslaw")
val move2 = movePos(0, 3, 3, "Wladyslaw")
val move3 = movePos(5, 4, 0, "Wladyslaw")
val move4 = movePos(5, 5, 0, "Wladyslaw")
val move5 = movePos((-3), (-3), (-5), "Wladyslaw")
val move6 = movePos(0, 3, 10, "Wladyslaw")

val tree = Node(Origin("ZDZISLAW"),
  Node(Insert(1, "ha"),
    Node(Insert(1, "lol"),
      Node(Insert(3, "bu"),
        Node(Insert(4, "kek"),
          Node(Insert(5, "meh"),
            Node(Insert(6, "hi"),
              Node(Insert(7, "bum"),
                Node(Delete(4, 1),
                  Node(Move(1, 17, 8),
                    Node(Insert(8, " <-> "),
                      Leaf)))))))))))

review(tree, Delete(14, 2))