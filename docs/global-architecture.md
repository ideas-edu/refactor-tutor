


# Global Architecture

The tutor is build up from a server-client architecture. The client, a webpage, communicates with the server over HTTP requests to a single CGI endpoint. The figure below shows this in more detail.

```plantuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/d193a84febce699caf236e3115dbed5ac8418397/C4_Container.puml
!define ICONS https://raw.githubusercontent.com/tupadr3/plantuml-icon-font-sprites/fa3f885dbd45c9cd0cdf6c0e5e4fb51ec8b76582

!include ICONS/devicons/haskell.puml
!include ICONS/devicons/html5.puml
!include ICONS/material/http.puml
!include ICONS/material/web.puml

scale 4
SHOW_PERSON_OUTLINE()

'Actors
Person(researcher, "Researcher", "Someone using the system to conduct experiments on the behavior of students in an automated feedback tutoring system.")
Person(teacher, "Teacher", "A lecturer that uses the system during classes to teach students what refactoring is.")
Person(student, "Student", "Novice programmer that is doing the refactoring exercises.")

'Containers
System_Boundary(rpto, "Refactor Tutor", "Tutoring application that teaches refactoring in Java") {
  Container(rpt, "Backend", "CGI Haskell backend based on the Ideas framework", "Contains exercises, diagnoses students code and generates feedback", $sprite="haskell")

  Container(frontend, "Frontend", "Static HTML/CSS/JS frontend", "Allows the student to do exercises.", $sprite="html5")
}

'Relations
Rel_L(frontend, rpt, "Single JSON endpoint")
Rel_D(student, frontend, "Uses web frontend")
Rel_D(researcher, rpt, "Configures exercises and evaluates logs")
Rel_D(teacher, rpt, "Configures exercises")

SHOW_LEGEND()

```
