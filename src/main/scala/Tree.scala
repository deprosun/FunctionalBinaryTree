

object Tree {

  /**
    * Returns true if the node has ONLY 1 child, otherwise false
    */
  private def onlyOneChild(node: Tree): Boolean = node match {
    case _: EmptyTree.type => false
    case DataTree(_, EmptyTree, right) if right != EmptyTree => true
    case DataTree(_, left, EmptyTree) if left != EmptyTree => true
    case _ => false
  }

  /**
    * Returns true if node has no child, otherwise false
    */
  private def hasNoChild(node: Tree): Boolean = node match {
    case _: EmptyTree.type => true
    case DataTree(_, EmptyTree, EmptyTree) => true
    case _ => false
  }

  /**
    * Returns a child of the node.
    * If the node has left child, that node is returned. Otherwise, right child is returned.
    */
  private def getChild(node: Tree): Tree = node match {
    case _: EmptyTree.type => node
    case DataTree(_, left, EmptyTree) => left
    case DataTree(_, _, right) => right
  }

  /**
    * Finds the minimum value in the tree
    *
    * @param root root of the tree
    * @return returns a node that is the minimum value in the tree
    */
  def minimum(root: Tree): Tree = {

    def find(node: Tree): Tree = {
      node match {
        case dataNode: DataTree[_] if dataNode.left == EmptyTree => dataNode
        case dataNode: DataTree[_] => find(dataNode.left)
      }
    }

    if (root == EmptyTree) root else find(root)
  }

  /**
    * Finds the maximum value in the tree
    *
    * @param root root of the tree
    * @return returns a node that is the maximum value in the tree
    */
  def maximum(root: Tree): Tree = {

    def find(node: Tree): Tree = {
      node match {
        case dataNode: DataTree[_] if dataNode.right == EmptyTree => dataNode
        case dataNode: DataTree[_] => find(dataNode.right)
      }
    }

    if (root == EmptyTree) root else find(root)
  }

  /**
    * Inserts `node` to the `root` tree
    *
    * @param node                 New node to put
    * @param root                 the root of the tree where new node is going to be put
    * @param leftLessThanRight    function that takes two nodes and checks if left node is less than right node
    * @param leftGreaterThanRight function that takes two nodes and checks if left node is greater than right node
    * @return returns a new tree where the new value is inserted
    */
  private def insertNode(node: Tree,
                         root: Tree,
                         leftLessThanRight: (Tree, Tree) => Boolean,
                         leftGreaterThanRight: (Tree, Tree) => Boolean): Tree = {

    def put(parent: Tree): Tree = {
      parent match {
        //if the parent is an empty, just return node
        case _: EmptyTree.type => node

        //if the node.data == parent.data and parent.left is empty, return the tree unaltered
        case dataNode: DataTree[_] if !leftLessThanRight(node, dataNode) && !leftGreaterThanRight(node, dataNode) =>
          dataNode

        //if the node.data < parent.data and parent.left is empty, return the parent with node as left child
        case dataNode: DataTree[_] if leftLessThanRight(node, dataNode) && dataNode.left == EmptyTree =>
          dataNode.copy(left = node)

        //if the node.data < parent.data, insert the node to left of the parent
        case dataNode: DataTree[_] if leftLessThanRight(node, dataNode) =>
          dataNode.copy(left = put(dataNode.left))

        //if the node.data >= parent.data and parent.right is empty, return the parent with node as right child
        case dataNode: DataTree[_] if !leftLessThanRight(node, dataNode) && dataNode.right == EmptyTree =>
          dataNode.copy(right = node)

        //if the node.data >= parent.data, insert the node to right of the parent
        case dataNode: DataTree[_] =>
          dataNode.copy(right = put(dataNode.right))
      }
    }

    put(root)
  }

  /**
    * Deletes the `node` from the tree `root`
    *
    * @param node                 Node to be removed
    * @param root                 root of the tree
    * @param leftLessThanRight    function that takes two nodes and checks if left node is less than right node
    * @param leftGreaterThanRight function that takes two nodes and checks if left node is greater than right node
    * @return returns a new tree where the node is removed
    */
  private def deleteNode(node: Tree,
                         root: Tree,
                         leftLessThanRight: (Tree, Tree) => Boolean,
                         leftGreaterThanRight: (Tree, Tree) => Boolean): Tree = {

    def equal(n1: Tree, n2: Tree) = !leftLessThanRight(n1, n2) && !leftGreaterThanRight(n1, n2)

    def del(root: Tree, toBeDeleted: Tree): Tree = {
      root match {
        //if root itself is empty, then nothing to delete just return the root
        case _: EmptyTree.type => root

        //if root == node and root has no child, just return empty node
        case _ if equal(root, toBeDeleted) && hasNoChild(root) => EmptyTree

        //if root == node and root has ONLY one child, just return that child
        case _ if equal(root, toBeDeleted) && onlyOneChild(root) => getChild(root)

        //if root == node are equal AND root has two children,
        //find the next node, X to replace root from either of the subtree
        //in this routine, we will prefer the left sub tree's maximum
        case datanode: DataTree[_] if equal(datanode, toBeDeleted) =>

          //get the next data node root
          val nextRoot = maximum(datanode.left).asInstanceOf[DataTree[_]]

          //get a tree with nextRoot node removed
          val treeWithDeletedNextRoot = del(datanode, nextRoot).asInstanceOf[DataTree[_]]

          //return a new tree where the nextRoot left and right children are from treeWithDeletedNextRoot's node left and right
          nextRoot.copy(left = treeWithDeletedNextRoot.left, right = treeWithDeletedNextRoot.right)

        case datanode: DataTree[_] if leftLessThanRight(toBeDeleted, datanode) =>
          datanode.copy(left = del(datanode.left, toBeDeleted))

        case datanode: DataTree[_] =>
          datanode.copy(right = del(datanode.right, toBeDeleted))
      }
    }

    del(root, node)

  }

  /**
    * Inserts `value` to the `root` tree
    *
    * @param value                New element to put in the tree
    * @param root                 the root of the tree where new element is going to be put
    * @param leftLessThanRight    function that takes two nodes and checks if left node is less than right node
    * @param leftGreaterThanRight function that takes two nodes and checks if left node is greater than right node
    * @return returns a new tree where the new value is inserted
    */
  def insert[T](value: T, root: Tree, leftLessThanRight: (Tree, Tree) => Boolean, leftGreaterThanRight: (Tree, Tree) => Boolean): Tree = {
    insertNode(DataTree(value, EmptyTree, EmptyTree), root, leftLessThanRight, leftGreaterThanRight)
  }

  /**
    * Deletes the `node` from the tree `root`
    *
    * @param value                Value to be removed
    * @param root                 root of the tree
    * @param leftLessThanRight    function that takes two nodes and checks if left node is less than right node
    * @param leftGreaterThanRight function that takes two nodes and checks if left node is greater than right node
    * @return returns a new tree where the value is removed
    */
  def delete[T](value: T, root: Tree, leftLessThanRight: (Tree, Tree) => Boolean, leftGreaterThanRight: (Tree, Tree) => Boolean): Tree = {
    deleteNode(DataTree(value, EmptyTree, EmptyTree), root, leftLessThanRight, leftGreaterThanRight)
  }

  /**
    * A nice-to-have printing routine for small BSTs.
    * Credits to <a href="https://stackoverflow.com/a/4973083/1204326">the stackoverflow answer</a> :)
    */
  def print(root: Tree): Unit = {
    def convertToJavaNode[T <: Comparable[_]](node: Tree): Node[T] = {
      node match {
        case _: EmptyTree.type => null
        case DataTree(data, left, right) =>
          val newNode = new Node[T](data.asInstanceOf[T])
          val newLeft = convertToJavaNode[T](left)
          val newRight = convertToJavaNode[T](right)
          newNode.left = newLeft
          newNode.right = newRight
          newNode
      }
    }

    BTreePrinter.printNode(convertToJavaNode(root))
  }

}

sealed trait Tree

case object EmptyTree extends Tree

case class DataTree[T](data: T, left: Tree, right: Tree) extends Tree