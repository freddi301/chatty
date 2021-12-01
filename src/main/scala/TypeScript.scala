package typescript
import scala.collection.immutable.HashMap
import scala.deriving.*
import scala.compiletime.{constValue, erasedValue, summonInline}

def dts[T](using TypeScript[T]): String = summon[TypeScript[T]].dts("")

trait TypeScript[T]:
  def dts(name: String): String

object TypeScript:
  given TypeScript[Boolean] with
    def dts(name: String) = "boolean"
  given TypeScript[Int] with
    def dts(name: String) = "number"
  given TypeScript[Long] with
    def dts(name: String) = "number"
  given TypeScript[String] with
    def dts(name: String) = "string"
  inline given derived[T](using m: Mirror.Of[T]): TypeScript[T] =
    lazy val elemInstances = summonAllTypes[m.MirroredElemTypes]
    lazy val elemLabels = summonAllLabels[m.MirroredElemLabels]
    inline m match
      case s: Mirror.SumOf[T]     => tsSum(s, elemInstances, elemLabels)
      case p: Mirror.ProductOf[T] => tsProduct(p, elemInstances, elemLabels)
  private def tsSum[T](s: Mirror.SumOf[T], elems: => List[TypeScript[_]], labels: => List[String]): TypeScript[T] = new TypeScript[T]:
    def dts(name: String): String =
      "(" + (labels zip elems).map { case (label, ts) => ts.dts(label) }.mkString(" | ") + ")"
  private def tsProduct[T](p: Mirror.ProductOf[T], elems: => List[TypeScript[_]], labels: => List[String]): TypeScript[T] =
    new TypeScript[T]:
      def dts(name: String): String =
        val typeField = if name != "" then "\"@type\":\"" + name + "\", " else ""
        "{ " + typeField + (labels zip elems).map { case (label, ts) => label + ": " + ts.dts("") }.mkString(", ") + " }"

private inline def summonAllLabels[T <: Tuple]: List[String] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => constString[t] :: summonAllLabels[ts]
private inline def summonAllTypes[T <: Tuple]: List[TypeScript[_]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts)  => summonInline[TypeScript[t]] :: summonAllTypes[ts]
private inline def constString[T]: String = constValue[T & String]
