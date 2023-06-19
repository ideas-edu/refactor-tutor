


# Containers

On the next page, in figure \ref{containers}, an overview of the different containers is given. This overview is not complete: it only contains the parts of the refactor tutor that are relevant to this project.

The tutor consists of a Domain Reasoner, a datatype from the Ideas framework. This Domain Reasoner contains all necessary configuration for the Ideas framework to generate feedback. This configurations most importantly contains a list of exercises: this is what the student works on. An exercise is defined by the following properties:

- A difficulty, starting examples, description and a list of test cases.
- A parser that parses the code and returns an Abstract Syntax Tree (AST).
- A pretty printer that converts the AST back into code.
- A test case runner to check if the test cases pass.
- A strategy that gets used to generate feedback.
- A list of buggy rules that gets applied to find possible mistakes.

The teacher and researcher interact with the exercises by modifying the exercise definitions.

```plantuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/d193a84febce699caf236e3115dbed5ac8418397/C4_Component.puml
!define ICONS https://raw.githubusercontent.com/tupadr3/plantuml-icon-font-sprites/fa3f885dbd45c9cd0cdf6c0e5e4fb51ec8b76582

!include ICONS/devicons/haskell.puml
!include ICONS/font-awesome/circle_o.puml
!include ICONS/font-awesome-5/bug.puml
!include ICONS/font-awesome-5/code.puml
!include ICONS/font-awesome-5/map.puml
!include ICONS/font-awesome-5/check.puml
!include ICONS/font-awesome-5/print.puml
!include ICONS/font-awesome-5/cogs.puml
!include ICONS/font-awesome-5/list.puml
!include ICONS/font-awesome-5/lightbulb.puml
!include ICONS/font-awesome-5/question.puml
!include ICONS/devicons/html5.puml
!include ICONS/material/web.puml

scale 2
SHOW_PERSON_OUTLINE()

'Actors
Person(researcher, "Researcher", "Someone using the system to conduct experiments on the behavior of students in an automated feedback tutoring system.")
Person(teacher, "Teacher", "A lecturer that uses the system during classes to teach students what refactoring is.")
Person(student, "Student", "Novice programmer that is doing the refactoring exercises.")

Container(frontend, "Frontend", "Static HTML/CSS/JS frontend", "Allows the student to do exercises.", $sprite="html5")

Container_Boundary(rpt, "Backend") {
  Component_Ext(ideas, "Ideas Framework", "Library", "Framework for refactoring on a domain and creating feedback.", $sprite="haskell")

  Component(dr, "Domain Reasoner", "Datatype", "Entrypoint for everything in the ideas framework.", $sprite="circle_o")
  Component(services, "Services", "List", "List of services provided by Ideas and self supplied services. These get exposed at the domain reasoner for HTTP requests.", $sprite="list")

  Component(exercise, "Exercises", "Datatype", "List of exercises containing relevant configuration for reasoning over the exercise.", $sprite="code")
  Component(buggy, "Buggy Rules", "List of Rule datatypes that have instances for the buggy typeclass", "Rules that perform incorrect transformations that break the program.", $sprite="bug")
  Component(strategy, "Strategy", "Strategy datatype", "Single strategy that contains all strategies and rules used in the reasoner.",  $sprite="map")
  Component(testCases, "Test Cases", "Predicate", "Collection of testcases that need to succeed for the solution to be accepted as correct.", $sprite="check")
  Component(pretty, "Pretty Printer", "AST -> String", "Serializer function that turns internal representation back into Java code", $sprite="print")
  Component(parser, "Parser", "String -> AST", "Deserializer function that parses Java code and returns internal representation", $sprite="cogs")

  Component(diagnose, "Diagnose", "Service", "Service that looks at current code and assesses state: Are there improvements possible, is the behavior still correct?", $sprite="lightbulb")
  Component(hints, "Hints", "Service", "Service that returns a hint tree containing multiple layers of hints.", $sprite="question")
}

Rel_U(student, frontend, "Uses web frontend")
Rel_R(teacher, exercise, "Configures exercises")
Rel_R(researcher, exercise, "Configures exercises and evaluates logs")
Lay_U(teacher, researcher)
Lay_R(parser, teacher)
Lay_R(dr, student)
Lay_D(student, researcher)
Rel_U(dr, ideas, "")
Rel_R(frontend, ideas, "Sends JSON request querying a service")

Lay_D(frontend, researcher)

Rel_R(services, dr, "")

Rel_U(diagnose, services, "")
Rel_U(hints, services, "")


Rel_U(exercise, dr, "")

Rel_U(buggy, exercise, "")
Rel_U(strategy, exercise, "")
Rel_U(testCases, exercise, "")
Rel_U(pretty, exercise, "")
Rel_U(parser, exercise, "")

SHOW_LEGEND()


```

