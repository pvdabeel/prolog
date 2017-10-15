
# Prolog code 

This repository contains Prolog code:

- [context](Source/context.pl): An implementation of a Contextual Object Oriented Logic Programming paradigm for SWI-Prolog,
- [eapi](Source/eapi.pl): A DCG Grammar for reading EAPI-6 (or earlier) compliant Gentoo ebuild information into Prolog facts and rules,
- [knowledgebase](Source/knowledgebase.pl): A structure to which Gentoo overlays and repositories can be registered. Offers datalog-style querying.

- A Prolog class representing a Gentoo Portage [repository](Source/repository.pl) (with Git, Rsync or Web-rsync syncronisation support),
- An ebuild [reader](Source/reader.pl) & [parser](Source/parser.pl), 
- A [prover](Source/prover.pl) which uses declarative reasoning to compute a Model and a logic Proof for realisation of a given ebuild,
- A [planner](Source/planner.pl) capable of creating a build plan (Makefile) for realisation of a given ebuid proof,
- A pretty [printer](Source/printer.pl) for build plans,
- A [grapher](Source/grapher.pl) capable of creating graphviz DOT files and interactive SVG files,
- Domain-specific [rules](Source/rules.pl) for reasoning about ebuilds and their possible configurations, 

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
