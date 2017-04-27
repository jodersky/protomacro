package macros

import scala.collection.immutable.Map

object Test extends App {

  sealed trait JsValue
  case class JsObject(fields: Map[String, JsValue]) extends JsValue
  case class JsString(str: String) extends JsValue
  case class JsNumber(num: Int) extends JsValue

  trait MyWriter[A] {
    def write(value: A): JsValue
    def other(a: A): Int = 2
  }
  trait BaseWriters {
    implicit val idWriter = new MyWriter[JsValue] {
      def write(value: JsValue) = value
    }
    implicit val intWriter = new MyWriter[Int] {
      def write(value: Int) = JsNumber(value)
    }
    implicit val stringWriter = new MyWriter[String] {
      def write(value: String) = JsString(value)
    }
  }
  trait CompositeWriters {
    implicit def mapWriter[A: MyWriter] = new MyWriter[Map[String, A]] {
      def write(mp: Map[String, A]) = {
        def writeA(v: A) = implicitly[MyWriter[A]].write(v)
        JsObject(mp.map { case (k, v) => k -> writeA(v) })
      }
    }
  }

  object Util {
    def save[A: MyWriter](a: A): Unit = {
      val x = implicitly[MyWriter[A]].write(a)
      println("saving " + x)
    }
  }

  case class Foo(x: Int)
  case class Composite(x: Int, y: String, other: Foo)

  object Protocol
      extends BaseWriters
      with CompositeWriters
      with ProductWriters[MyWriter]
  import Protocol._

  class Bar
  Util.save(2)
  //Util.save(new Bar)
  Util.save(Composite(2, "some string", Foo(3)))
  //Util.save((2, "some string", "1")

}
