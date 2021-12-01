package p2p.chat

import scala.quoted.*

inline def showStruct[T]: String = ${ showStructImpl[T] }
private def showStructImpl[T: Type](using Quotes): Expr[String] =
  import quotes.reflect.*
  Expr(TypeRepr.of[T].widen.show)
