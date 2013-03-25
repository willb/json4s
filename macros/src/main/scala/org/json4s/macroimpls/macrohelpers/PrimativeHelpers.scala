package org.json4s.macroimpls.macrohelpers

import org.json4s.ParserUtil.ParseException

//class JsonStructureException(expected: Class[_], received: Class[_]) extends ParseException(
//  s"Received JValue type ($received) but expected ($expected).", null)

object PrimativeHelpers {
  def optIdent[U](opt: Option[U]):Option[U] = opt
}

