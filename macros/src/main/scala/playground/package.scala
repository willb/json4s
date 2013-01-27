package object playground {

  object by {
    object Nothing extends Separator("", "", "", "")
    object Dots extends Separator(".", "", "[", "]")
    object Colon extends Separator(":", "", "[", "]")
    object DoubleColon extends Separator("::", "", "[", "]")
    object SquareBrackets extends Separator("[", "]", "(", ")")
    object Brackets extends Separator("(", ")", "[", "]")
    object ForwardSlash extends Separator("/", "", "[", "]")
    object Backslash extends Separator("\\", "", "[", "]")
  }
}
