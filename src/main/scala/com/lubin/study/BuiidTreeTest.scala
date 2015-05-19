
package com.lubin.study

import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

object BuildTreeTest extends App {
  case class Mapping(name: String, parents: Seq[String] = Seq[String]())
  val mappings = Seq(
    Mapping("aaa"),
    Mapping("bbb"),
    Mapping("ccc"),
    Mapping("ddd", Seq("aaa", "bbb")),
    Mapping("eee", Seq("ccc")),
    Mapping("fff", Seq("ddd")),
    Mapping("ggg", Seq("aaa", "fff")),
    Mapping("hhh"))

  case class Node(name: String, children: Buffer[Node] = Buffer[Node]())
  
  def buildTrees(data: Seq[Mapping]): Seq[Node] = {
    val nodeMap = HashMap[String, Node]()
    var tree: Seq[Node] = Seq[Node]()
    mappings.foreach { mapping =>
      mapping match {
        case Mapping(name, parents) if parents.size == 0 => {
          val node = Node(name)
          nodeMap.put(name, node)
          tree = tree :+ node
        }
        case Mapping(name, parents) => {
          val node = Node(name)
          nodeMap.put(name, node)
        }
      }
    }
    mappings.foreach { mapping =>
      mapping match {
        case Mapping(name, parents) => {
          val chidNode = nodeMap.get(name).get
          parents.foreach { parentName =>
            {
              val parentNode = nodeMap.get(parentName).get
              parentNode.children.append(chidNode)
            }
          }
        }
        case _ =>
      }
    }
    tree
  }
  
  println(buildTrees(mappings))
}



