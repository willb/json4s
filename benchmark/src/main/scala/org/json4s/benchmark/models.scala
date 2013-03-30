package org.json4s
package benchmark

import java.util.Date

case class Project(name: String, startDate: Date, lang: Option[Language], teams: List[Team]) {
  override def equals(other: Any) = other match {
    case Project(name, startDate, lang, teams) if (name == this.name &&
                    lang == this.lang &&
                    teams == this.teams &&
                    startDate.toString() == this.startDate.toString())  => true
    case _ => false
  }
}
case class Language(name: String, version: Double)
case class Team(role: String, members: List[Employee])
case class Employee(name: String, experience: Int)