import org.scalatest.FlatSpec

import scala.language.postfixOps
import scala.util.Random

class TreeTest extends FlatSpec {

  import Tree._

  private def intComparatorLeftLessThanRight(left: Tree, right: Tree): Boolean = {
    (left, right) match {
      case (DataTree(l: Int, _, _), DataTree(r: Int, _, _)) => l < r
      case (_: EmptyTree.type, _) => true
      case (_, _: EmptyTree.type) => false
      case _ => throw new Error("intComparatorLeftLessThanRight() function should only be called on Int types")
    }
  }

  private def intComparatorLeftGreaterThanRight(left: Tree, right: Tree): Boolean = {
    (left, right) match {
      case (DataTree(l: Int, _, _), DataTree(r: Int, _, _)) => l > r
      case (_: EmptyTree.type, _) => false
      case (_, _: EmptyTree.type) => true
      case _ => throw new Error("intComparatorLeftGreaterThanRightr() function should only be called on Int types")
    }
  }

  behavior of "insert"

  it should "put data to the right sub tree" in {
    val root: Tree = DataTree(5, EmptyTree, EmptyTree)

    insert(7, root, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight) match {
      case _: EmptyTree.type => fail("Should not be empty tree")
      case newTree: DataTree[_] =>

        val expectedTree: DataTree[Int] = DataTree(data = 5,
          left = EmptyTree,
          right = DataTree(data = 7,
            left = EmptyTree,
            right = EmptyTree
          )
        )

        assert(newTree == expectedTree)
    }

  }

  it should "put data to the left sub tree" in {
    val root = DataTree(data = 5,
      left = EmptyTree,
      right = DataTree(data = 7,
        left = EmptyTree,
        right = EmptyTree
      )
    )

    insert(4, root, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight) match {
      case _: EmptyTree.type => fail("Should not be empty tree")
      case newTree: DataTree[_] =>
        val expectedTree = root.copy(left = DataTree(data = 4,
          left = EmptyTree,
          right = EmptyTree)
        )

        assert(newTree == expectedTree)
    }

  }

  it should "work when putting data more than 1 level deep" in {
    val root = DataTree(data = 5,
      left = EmptyTree,
      right = DataTree(data = 7,
        left = EmptyTree,
        right = EmptyTree
      )
    )

    insert(11, root, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight) match {
      case _: EmptyTree.type => fail("Should not be empty tree")
      case newTree: DataTree[_] =>
        val expectedTree = DataTree(
          data = 5,
          left = EmptyTree,
          right = DataTree(
            data = 7,
            left = EmptyTree,
            right = DataTree(
              data = 11,
              left = EmptyTree,
              right = EmptyTree)
          )
        )

        assert(newTree == expectedTree)
    }

  }

  it should "not put any duplicate values" in {

    //generate a set of numbers to insert
    val numbers = 0 to 5 map { _ =>
      Random.nextInt(10)
    } toSet

    //initialize the root
    val firstNode: Tree = DataTree(numbers.head, EmptyTree, EmptyTree)

    //gneerate a tree with all the numbers in
    val tree: Tree = numbers.tail.foldLeft(firstNode) { (acc, x) =>
      insert(x, acc, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight)
    }

    //use the same number on the tree we made above
    val tree2 = numbers.foldLeft(tree) { (acc, x) =>
      insert(x, acc, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight)
    }

    //the new tree should be exactly as the first one
    assert(tree == tree2)

  }

  behavior of "delete"

  it should "return the same tree if the value isn't present" in {
    val tree = DataTree(data = 5,
      left = EmptyTree,
      right = DataTree(data = 7,
        left = EmptyTree,
        right = EmptyTree
      )
    )

    delete(11, tree, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight) match {
      case _: EmptyTree.type => fail("Should not not be empty tree")
      case newTree: DataTree[_] =>
        assert(newTree == tree)
    }
  }

  it should "delete a node with no child" in {
    val tree = DataTree(data = 5,
      left = EmptyTree,
      right = DataTree(data = 7,
        left = EmptyTree,
        right = EmptyTree
      )
    )

    delete(7, tree, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight) match {
      case _: EmptyTree.type => fail("Should not not be empty tree")
      case newTree: DataTree[_] =>
        val expectedTree = DataTree(data = 5,
          left = EmptyTree,
          right = EmptyTree
        )
        assert(newTree == expectedTree)
    }
  }

  it should "delete a node with only one right child" in {
    val tree = DataTree(data = 5,
      left = EmptyTree,
      right = DataTree(data = 7,
        left = EmptyTree,
        right = EmptyTree
      )
    )

    delete(5, tree, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight) match {
      case _: EmptyTree.type => fail("Should not not be empty tree")
      case newTree: DataTree[_] =>
        val expectedTree = DataTree(data = 7,
          left = EmptyTree,
          right = EmptyTree
        )
        assert(newTree == expectedTree)
    }
  }

  it should "delete a node with only one left child" in {
    val tree = DataTree(data = 5,
      left = DataTree(data = 4,
        left = EmptyTree,
        right = EmptyTree
      ),
      right = EmptyTree
    )

    delete(5, tree, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight) match {
      case _: EmptyTree.type => fail("Should not not be empty tree")
      case newTree: DataTree[_] =>
        val expectedTree = DataTree(data = 4,
          left = EmptyTree,
          right = EmptyTree
        )
        assert(newTree == expectedTree)
    }
  }

  it should "delete a node with two children" in {
    val tree = DataTree(data = 5,
      left = DataTree(data = 4,
        left = EmptyTree,
        right = EmptyTree
      ),
      right = DataTree(data = 7,
        left = EmptyTree,
        right = EmptyTree
      )
    )

    delete(5, tree, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight) match {
      case _: EmptyTree.type => fail("Should not not be empty tree")
      case newTree: DataTree[_] =>
        val expectedTree = DataTree(data = 4,
          left = EmptyTree,
          right = DataTree(data = 7,
            left = EmptyTree,
            right = EmptyTree
          )
        )
        assert(newTree == expectedTree)
    }
  }

  it should "work if we insert a bunch of elements and then remove all of them. The result should be an empty tree" in {
    val numbers = 0 to 5 map { _ =>
      Random.nextInt(10)
    } toSet

    val firstNode: Tree = DataTree(numbers.head, EmptyTree, EmptyTree)

    val tree: Tree = numbers.tail.foldLeft(firstNode) { (acc, x) =>
      insert(x, acc, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight)
    }

    val tree2 = numbers.foldLeft(tree) { (acc, x) =>
      delete(x, acc, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight)
    }

    assert(tree2 == EmptyTree)

  }

  behavior of "minimum"

  it should "return the root if the root of the tree has no left child" in {
    val root = DataTree(data = 5,
      left = EmptyTree,
      right = DataTree(
        data = 7,
        left = EmptyTree,
        right = DataTree(
          data = 11,
          left = EmptyTree,
          right = EmptyTree)
      )
    )

    assert(minimum(root) == root)
  }

  it should "return the root if the root itself is empty" in {
    val root = EmptyTree
    assert(minimum(root) == root)
  }

  it should "return the 4 when root has a left node which doesn't have any children" in {
    val root = DataTree(data = 5,
      left = DataTree(data = 4,
        left = EmptyTree,
        right = EmptyTree
      ),
      right = DataTree(data = 7,
        left = EmptyTree,
        right = DataTree(data = 11,
          left = EmptyTree,
          right = EmptyTree)
      )
    )

    val expected = DataTree(data = 4,
      left = EmptyTree,
      right = EmptyTree
    )

    assert(minimum(root) == expected)
  }

  it should "return the 3 when it resides in level more than 1" in {
    val root = DataTree(
      data = 5,
      left = DataTree(
        data = 4,
        left = DataTree(
          data = 3,
          left = EmptyTree,
          right = EmptyTree
        ),
        right = EmptyTree
      ),
      right = DataTree(
        data = 7,
        left = EmptyTree,
        right = DataTree(
          data = 11,
          left = EmptyTree,
          right = EmptyTree)
      )
    )

    val expected = DataTree(
      data = 3,
      left = EmptyTree,
      right = EmptyTree
    )

    assert(minimum(root) == expected)
  }

  behavior of "print"

  it should "print correctly" in {
    val numbers = 0 to 5 map { _ =>
      Random.nextInt(10)
    } toSet

    val firstNode: Tree = DataTree(numbers.head, EmptyTree, EmptyTree)

    val tree: Tree = numbers.tail.foldLeft(firstNode) { (acc, x) =>
      insert(x, acc, intComparatorLeftLessThanRight, intComparatorLeftGreaterThanRight)
    }

    print(tree)

  }
}
