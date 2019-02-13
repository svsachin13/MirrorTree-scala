package com.sachin

case class SNBranch(node: Int, left: Option[SNBranch] = None, right: Option[SNBranch] = None)

object MirrorTree extends App {

  val tree = SNBranch(1, Some(SNBranch(2, Some(SNBranch(4)), Some(SNBranch(5)))), Some(SNBranch(3, Some(SNBranch(6)), Some(SNBranch(7)))))

  def preOrder(branch: SNBranch): Unit = {
    println(branch.node)
    if (branch.left.isDefined) preOrder(branch.left.get)
    if (branch.right.isDefined) preOrder(branch.right.get)
  }

  def findSwap(tree: SNBranch): Unit = {
    println(tree.node)
    val newTree = doSwap(tree)
    if (tree.left.isDefined) findSwap(newTree.left.get)
    if (tree.right.isDefined) findSwap(newTree.right.get)
  }

  def doSwap(snb: SNBranch): SNBranch = {
    SNBranch(snb.node, snb.right, snb.left)
  }

  println("before ===> ")
  preOrder(tree)

  println("after ===> ")
  findSwap(tree)
}
