abstract class List {
  def isEmpty: Boolean
}

def show(e: Expr): String = e match {
  case Sum(e1, e2) => show(e1) + " + " show(e1)
  case Number(n) => n.toString
}