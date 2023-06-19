# Refactor Tutor

![Build status](https://img.shields.io/github/actions/workflow/status/ideas-edu/refactor-tutor/build.yml)
![Github tag](https://img.shields.io/github/v/tag/ideas-edu/refactor-tutor?label=version)
![Docker version](https://img.shields.io/docker/v/stephanstanisic/refactor-tutor?label=docker%20hub)

The Refactor Tutor is a tutoring system that lets students practice with improving small programs that are already functionally correct. The system is based on rules that are extracted from input by teachers, a subset of rules taken from professional tools, and other literature. Rules define how a code construct can be rewritten into a better variant, without changing its functionality.
The student can ask for hints and feedback at each step.

<p align="center">
  <img src="https://stanisic.nl/EC.svg" width="800" /><br />
  A screenshot of the refactor tutor, showing partially expanded feedback.
</p>

You can read more about the tutor in the following papers:
* [A Tutoring System to Learn Code Refactoring](https://doi.org/10.1145/3408877.3432526). Hieke Keuning, Bastiaan Heeren and Johan Jeuring. Proceedings of the SIGCSE Technical Symposium on Computer Science Education 2021.
* [Student Refactoring Behaviour in a Programming Tutor](https://doi.org/10.1145/3428029.3428043). Hieke Keuning, Bastiaan Heeren and Johan Jeuring. Proceedings of the Koli Calling Conference on Computing Education Research 2020.

## Getting started

If you want to try out the tutor, you can use the [online version](https://ideas.science.uu.nl/rpt/).

To create your own exercises you will have to self host the tutoring system. Instructions how to do so are available for [Docker](wiki/Instructions-docker) and [Apache 2](wiki/Instructions-apache2).

## Developing

Instruction on how to setup a development environment are available on the [wiki](#). Instructions for building and releasing can also be found there.

## Credits
The Refactor Tutor was developed by [Hieke Keuning](https://github.com/hiekekeuning) (h.w.keuning@uu.nl), in collaboration with [Bastiaan Heeren](https://github.com/bastiaanheeren) and [Johan Jeuring](https://github.com/johanjeuring). Several improvements have been made by [Stephan Stanisic](https://github.com/stephanstanisic).
