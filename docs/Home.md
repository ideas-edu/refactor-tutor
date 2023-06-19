Welcome to the refactor tutor wiki!

This wiki contains the design of a tutoring system designed to let novice programmers learn refactoring. @keuning_tutoring_2021 gives a description of the tutoring system:

> In the last few decades, numerous tutoring systems and assessment tools have been developed to support students with learning programming, giving hints on correcting errors, showing which test cases do not succeed, and grading their overall solutions. The focus has been less on helping students write code with good style and quality. There are several professional tools that can help, but they are not targeted at novice programmers.
> 
> This paper describes a tutoring system that lets students practice with improving small programs that are already functionally correct. The system is based on rules that are extracted from input by teachers collected in a preliminary study, a subset of rules taken from professional tools, and other literature. Rules define how a code construct can be rewritten into a better variant, without changing its functionality. Rules can be combined to form rewrite strategies, similar to refactorings offered by most IDEs. The student can ask for hints and feedback at each step.

## System Context

In the figure below, an overview of the external dependencies of the tutor has been given. As the tutor is completely self-contained and does not depend on third party services there are no other services visible. 

There are three different users of the system:

- **Students**: The main target audience of the refactor tutor is novice programmers. The main goal of the application is to let students practice with refactoring code.
- **Lecturers**: Lecturers have an interest in the tutor as well, as it enables them to give their students specific personalized feedback without having to individually give every student feedback.
- **Researchers**: Researchers have similar interests as lecturers, but have the added interest of using the system to advance research on automated feedback.

```plantuml
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/d193a84febce699caf236e3115dbed5ac8418397/C4_Context.puml
!define ICONS https://raw.githubusercontent.com/tupadr3/plantuml-icon-font-sprites/fa3f885dbd45c9cd0cdf6c0e5e4fb51ec8b76582

!include ICONS/devicons/haskell.puml
!include ICONS/devicons/html5.puml
!include ICONS/material/http.puml
!include ICONS/material/web.puml
!include ICONS/font-awesome-5/laptop_code.puml

scale 4
SHOW_PERSON_OUTLINE()

'Actors
Person(researcher, "Researcher", "Someone using the system to conduct experiments on the behavior of students in an automated feedback tutoring system.")
Person(teacher, "Teacher", "A lecturer that uses the system during classes to teach students what refactoring is.")
Person(student, "Student", "Novice programmer that is doing the refactoring exercises.")

'Systems
System(rpt, "Refactor Tutor", "Tutoring application that teaches refactoring in Java", $sprite="laptop_code")

'Relations
Rel_D(student, rpt, "Does exercises")
Rel_D(researcher, rpt, "Configures exercises and evaluates results")
Rel_D(teacher, rpt, "Configures exercises")

SHOW_LEGEND()

```

## References
