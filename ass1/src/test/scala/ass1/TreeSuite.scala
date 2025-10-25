package ass1

class TreeSuite extends munit.FunSuite:
  trait TestTree:
    val t = Tree("")
      .updated("foo/a".toPath, 1)
      .updated("foo/b".toPath, 2)
      .updated("bar/a".toPath, 11)
      .updated("bar/b".toPath, 12)

    extension (s: String)
      def toPath: Tree.Path[String] = Tree.Path(s.split("/").toList.filter(_.nonEmpty))

  // ========== 原有测试 ==========
  test("Sample tree"):
    new TestTree:
      assertEquals(t.toString, "Node(,List(Node(foo,List(Leaf(a,1), Leaf(b,2))), Node(bar,List(Leaf(a,11), Leaf(b,12)))))")
  
  test("Tree.get"):
    new TestTree:
      assertEquals(t.get(Tree.Path.empty).toString, "Some(Left(List(foo, bar)))")

  test("Tree.updated"):
    new TestTree:
      assertEquals(t.updated("foo/b/c/d".toPath, 42).toString, "Node(,List(Node(foo,List(Leaf(a,1), Node(b,List(Node(c,List(Leaf(d,42))))))), Node(bar,List(Leaf(a,11), Leaf(b,12)))))")

  // ========== contains 测试 ==========
  test("contains - empty path on any node"):
    new TestTree:
      assert(t.contains(Tree.Path.empty))
      
  test("contains - existing paths"):
    new TestTree:
      assert(t.contains("foo".toPath))
      assert(t.contains("foo/a".toPath))
      assert(t.contains("bar/b".toPath))
      
  test("contains - non-existing paths"):
    new TestTree:
      assert(!t.contains("baz".toPath))
      assert(!t.contains("foo/c".toPath))
      assert(!t.contains("foo/a/extra".toPath))

  // ========== get 测试 ==========
  test("get - leaf nodes"):
    new TestTree:
      assertEquals(t.get("foo/a".toPath), Some(Right(1)))
      assertEquals(t.get("bar/b".toPath), Some(Right(12)))
      
  test("get - internal nodes"):
    new TestTree:
      val result = t.get("foo".toPath)
      assert(result.isDefined)
      result.foreach { either =>
        assert(either.isLeft)
        either.left.foreach { keys =>
          assertEquals(keys.toSet, Set("a", "b"))
        }
      }
      
  test("get - non-existing paths"):
    new TestTree:
      assertEquals(t.get("nonexistent".toPath), None)
      assertEquals(t.get("foo/a/extra".toPath), None)

  // ========== flatten 测试 ==========
  test("flatten - returns absolute paths"):
    new TestTree:
      val result = t.flatten
      assertEquals(result.length, 4)
      assert(result.forall { case (Tree.Path(segments), _) => 
        segments.headOption.contains("")
      })

  test("flatten - correct paths and values"):
    new TestTree:
      val result = t.flatten.toSet
      val expected = Set(
        (Tree.Path(List("", "foo", "a")), 1),
        (Tree.Path(List("", "foo", "b")), 2),
        (Tree.Path(List("", "bar", "a")), 11),
        (Tree.Path(List("", "bar", "b")), 12)
      )
      assertEquals(result, expected)

  test("flatten - empty node"):
    val emptyNode = Tree("root")
    assertEquals(emptyNode.flatten, List.empty)

  test("flatten - single leaf"):
    val leaf = Tree(Tree.Path(List("root", "leaf")), 42)
    assertEquals(leaf.flatten, List((Tree.Path(List("root", "leaf")), 42)))

  test("flatten - nested structure"):
    new TestTree:
      val tree = Tree("root")
        .updated("a/b/c".toPath, 1)
        .updated("x/y".toPath, 2)
      val result = tree.flatten.toSet
      val expected = Set(
        (Tree.Path(List("root", "a", "b", "c")), 1),
        (Tree.Path(List("root", "x", "y")), 2)
      )
      assertEquals(result, expected)

  // ========== updated 测试 ==========
  test("updated - create empty node at path"):
    new TestTree:
      val result = t.updated("foo/b/c/d".toPath)
      assert(result.contains("foo/b/c/d".toPath))
      assertEquals(result.get("foo/b/c/d".toPath), Some(Left(List())))

  test("updated - replace existing node with empty node"):
    new TestTree:
      val result = t.updated("bar".toPath)
      assert(result.contains("bar".toPath))
      assertEquals(result.get("bar".toPath), Some(Left(List())))
      // Original children should be gone
      assert(!result.contains("bar/a".toPath))

  test("updated - empty path creates empty node"):
    new TestTree:
      val result = t.updated(Tree.Path.empty)
      assertEquals(result.get(Tree.Path.empty), Some(Left(List())))

  test("updated with payload - create leaf at path"):
    new TestTree:
      val result = t.updated("foo/b/c/d".toPath, 42)
      assertEquals(result.get("foo/b/c/d".toPath), Some(Right(42)))

  test("updated with payload - replace node with leaf"):
    new TestTree:
      val result = t.updated("bar".toPath, 0)
      assertEquals(result.get("bar".toPath), Some(Right(0)))
      // Original children should be gone
      assert(!result.contains("bar/a".toPath))

  test("updated with payload - empty path on root throws exception"):
    new TestTree:
      interceptMessage[Tree.IllegalPathException.type](null) {
        t.updated(Tree.Path.empty, 0)
      }


  test("updated - preserve unrelated subtrees"):
    new TestTree:
      val result = t.updated("foo/c".toPath, 99)
      // bar subtree should be unchanged
      assertEquals(result.get("bar/a".toPath), Some(Right(11)))
      assertEquals(result.get("bar/b".toPath), Some(Right(12)))
      // foo/a and foo/b should still exist
      assertEquals(result.get("foo/a".toPath), Some(Right(1)))
      assertEquals(result.get("foo/b".toPath), Some(Right(2)))

  // ========== Companion object 测试 ==========
  test("Tree.apply(key) creates empty node"):
    val tree = Tree("root")
    assertEquals(tree.get(Tree.Path.empty), Some(Left(List())))

  test("Tree.apply(path) creates nested empty nodes"):
    val tree = Tree(Tree.Path(List("a", "b", "c")))
    assert(tree.contains(Tree.Path(List("b", "c"))))
    assertEquals(tree.get(Tree.Path(List("b", "c"))), Some(Left(List())))

  test("Tree.apply(path) with empty path throws"):
    interceptMessage[Tree.IllegalPathException.type]("") {
      Tree(Tree.Path(List()))
    }

  test("Tree.apply(path, payload) creates nested nodes with leaf"):
    val tree = Tree(Tree.Path(List("a", "b", "c")), 42)
    assertEquals(tree.get(Tree.Path(List("b", "c"))), Some(Right(42)))

  test("Tree.apply(path, payload) with empty path throws"):
    interceptMessage[Tree.IllegalPathException.type]("") {
      Tree(Tree.Path(List()), 42)
    }

  // ========== display 测试 ==========
  test("display - simple tree"):
    new TestTree:
      val output = t.display
      assert(output.contains("Node()"))
      assert(output.contains("Node(foo)"))
      assert(output.contains("Leaf(a, 1)"))
      assert(output.contains("\u251c") || output.contains("\u2514")) // Has tree characters

  test("display - empty node"):
    val tree = Tree("root")
    assertEquals(tree.display, "Node(root)")

  test("display - single leaf"):
    val tree = Tree(Tree.Path(List("root", "leaf")), 1)
    val output = tree.display
    assert(output.contains("Node(root)"))
    assert(output.contains("Leaf(leaf, 1)"))
  

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
