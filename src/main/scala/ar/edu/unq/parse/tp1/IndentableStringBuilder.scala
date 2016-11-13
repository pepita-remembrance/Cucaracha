package ar.edu.unq.parse.tp1

class IndentableStringBuilder(indentStep: String) {
  val builder = new StringBuilder
  private var indentLevel = 0

  def appendln(newLine: String) = {
    builder.++=(indentStep * indentLevel).++=(newLine).++=("\n")
    this
  }

  def indent() = {
    indentLevel += 1
    this
  }

  def dedent() = {
    indentLevel = Math.max(indentLevel - 1, 0)
    this
  }

  override def toString = builder.toString

}
