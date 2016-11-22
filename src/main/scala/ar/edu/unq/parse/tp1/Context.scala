package ar.edu.unq.parse.tp1

import ar.edu.unq.parse.tp1.semantics.SemanticException

import scala.collection.mutable

class Context[A](message: String => String) extends mutable.HashMap[String, A] {
  override def apply(key: String): A = getOrElse(key, throw SemanticException(message(key)))
}
