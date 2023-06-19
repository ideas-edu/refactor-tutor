# Refactor Tutor

![Build status](https://img.shields.io/github/actions/workflow/status/ideas-edu/refactor-tutor/build.yml)
![Github tag](https://img.shields.io/github/v/tag/ideas-edu/refactor-tutor?label=version)
![Docker version](https://img.shields.io/docker/v/stephanstanisic/refactor-tutor?label=docker%20hub)

In the last few decades, numerous tutoring systems and assessment tools have been developed to support students with learning programming, giving hints on correcting errors, showing which test cases do not succeed, and grading their overall solutions. The focus has been less on helping students write code with good style and quality. There are several professional tools that can help, but they are not targeted at novice programmers.

This repository contains the source code for a tutoring system that lets students practice with improving small programs that are already functionally correct. The system is based on rules that are extracted from input by teachers collected in a preliminary study, a subset of rules taken from professional tools, and other literature. Rules define how a code construct can be rewritten into a better variant, without changing its functionality. Rules can be combined to form rewrite strategies, similar to refactorings offered by most IDEs. The student can ask for hints and feedback at each step.

<p align="center">
  <img src="https://stanisic.nl/EC.svg" width="800" /><br />
  A screenshot of the refactor tutor, showing partially expanded feedback.
</p>

## Getting started

If you want to try out the tutor, you can use the [online version](https://ideas.science.uu.nl/rpt/). This will contain the same set of exercises as used in 

To create your own exercises you will have to self host the tutoring system. Instructions how to do so are available for [Docker](wiki/Instructions-docker) and [Apache 2](wiki/Instructions-apache2).

## Developing

Instruction on how to setup a development environment are available on the [wiki](#). Instructions for building and releasing can also be found there.
