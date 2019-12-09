package wolfendale.playnunjucks.helpers

import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatest.OptionValues._

class RoutesHelperSpec extends FreeSpec with MustMatchers {

  "Tree" - {

    "apply should" - {

      "build single node trees" in {
        val a = Tree("key", "value")
        val b = Node[String, String](Map("key" -> Leaf("value")))

        a mustEqual b
      }

      "build nested trees" in {
        val a = Tree(List("a", "long", "key"), "value")
        val b = Node[String, String](
          Map(
            "a" -> Node(
              Map(
                "long" -> Node(Map(
                  "key" -> Leaf("value")
                ))
              ))
          ))

        a mustEqual b
      }

    }

    "combination should" - {

      "be commutative" in {
        val a = Tree(List("long", "path"), "value")
        val b = Tree(List("long", "other path"), "other value")

        a.combine(b) mustEqual b.combine(a)
      }

      "be associative" in {
        val a = Tree(List("long", "path"), "value")
        val b = Tree(List("long", "other path"), "other value")
        val c = Tree(List("long", "another path", "extra path"), "another value")

        a.combine(b).combine(c) mustEqual a.combine(c).combine(b)
      }

      "have empty as identity" in {
        val a = Tree(List("long", "path"), "value")
        val b = Tree(List("long", "other path"), "other value")
        val c = Tree(List("long", "another path", "extra path"), "another value")
        val empty = Tree.empty[String, String]

        a.combine(empty) mustEqual a
        b.combine(empty) mustEqual b
        c.combine(empty) mustEqual c
      }

      "merge trees with distinct paths" in {
        val a = Tree(List("long", "path"), "value")
        val b = Tree(List("long", "other path"), "other value")

        val expected = Node[String, String](
          Map(
            "long" -> Node(
              Map(
                "path"       -> Leaf("value"),
                "other path" -> Leaf("other value")
              ))
          ))

        a.combine(b) mustEqual expected
      }

      "fail for leaf and leaf" in {
        val a = Leaf[String, String]("hello")
        val b = Leaf[String, String]("world")

        assertThrows[RuntimeException] { a.combine(b) }
        assertThrows[RuntimeException] { b.combine(a) }
      }

      "fail for leaf and node" in {
        val a = Leaf[String, String]("hello")
        val b = Node[String, String](Map.empty)

        assertThrows[RuntimeException] { a.combine(b) }
        assertThrows[RuntimeException] { b.combine(a) }
      }

      "fail for nodes with same path" in {
        val a = Tree(List("long", "path"), "value")
        val b = Tree(List("long", "path"), "other value")

        assertThrows[RuntimeException] { a.combine(b) }
        assertThrows[RuntimeException] { b.combine(a) }
      }

    }

    "get should" - {

      "retrieve value from leaf with empty keys" in {
        val a = Leaf[String, String]("hello")

        a.get().value mustEqual "hello"
      }

      "retrieve empty from leaf with remaining keys" in {
        val a = Leaf[String, String]("hello")

        a.get("key") mustBe None
      }

      "retrieve value from a node with remaining keys" in {
        val a = Node[String, String](Map("key" -> Leaf("value")))

        a.get("key").value mustEqual "value"
      }

      "retrieve value from a nested node" in {
        val a = Node[String, String](
          Map(
            "long" -> Node(
              Map(
                "key" ->
                  Leaf("value")
              ))
          ))

        a.get("long", "key").value mustEqual "value"
      }

      "retrieve empty from a node with empty keys" in {
        val a = Node[String, String](Map("key" -> Leaf("value")))

        a.get() mustBe None
      }

    }

  }

}
