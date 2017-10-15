
# Prolog code 

This repository contains Prolog code:

- [Source/context.pl](context.pl): An implementation of a Contextual Object Oriented Logic Programming paradigm for SWI-Prolog,
- [Source/eapi.pl](eapi.pl): A DCG Grammar for reading EAPI-6 (or earlier) compliant Gentoo ebuild information into Prolog facts and rules,

- [Source/repository.pl](repository.pl): A Prolog class representing a Gentoo Portage repository (with Git, Rsync or Web-rsync syncronisation support),
- [Source/knowledgebase.pl](knowledgebase.pl)A datalog-style query language for query-ing a knowledge based with Gentoo Portage facts and rules. 


- An ebuild [Source/reader.pl](reader) & [Source/parser.pl](parser), 
- An [Source/prover.pl](prover) which uses declarative reasoning to compute a Model and a logic Proof for realisation of a given ebuild,
- A [Source/planner.pl](planner) capable of creating a build plan (Makefile) for realisation of a given ebuid proof,
- A pretty [Source/printer.pl](printer) for build plans,
- A [Source/grapher.pl](grapher) capable of creating graphviz DOT files and interactive SVG files,
- Domain-specific [Source/rules.pl](rules) for reasoning about ebuilds and their possible configurations, 

This repository also contains some bash scripts to emulate Gentoo emerge.

## Some examples: 

## Contextual logic programming 

![Imgur](https://i.imgur.com/KxLU7tL.png)

In the example shown, we create an instance from the 'person' class. We shown rules are properly guarded and the instance has its own data members. 


## Reading, parsing & querying ebuilds 

![Imgur](https://i.imgur.com/14mCiGp.png)

In the example shown, we sync a Gentoo Portage repository using Prolog. 



## Proving & Planning & Building ebuilds


![Imgur](https://i.imgur.com/14mCiGp.png)

In the example shown, we construct a proof for installation of an ebuild. 



## Licence: GPL v3

## Installation instructions: 

This code has as audience: Experienced Prolog programmers. 
