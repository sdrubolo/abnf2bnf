package object transformer {

  private [transformer] def isInternalRule(name: String):Boolean = name.startsWith("_")

}
