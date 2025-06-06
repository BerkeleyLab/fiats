---
title: 'Fiats: Functional inference and training for surrogates'
tags:
  - deep learning
  - artificial neural networks
  - Fortran
  - high-performance computing
  - parallel programming
authors:
  - name: Damian Rouson
    orcid: 0000-0002-2344-868X
    equal-contrib: true # (This is how you can denote equal contributions between multiple authors)
    affiliation: 1
affiliations:
 - name: Lawrence Berkeley National Laboratory, United States
   index: 1
date: 28 May 2025
bibliography: paper.bib

# Invite authors: Dan Bonachea, Katherine Rasmussen, David Torres, Ethan Gutmann, Jordan Welsman

---

# Summary
[Fiats] provides a platform for research on the training and deployment of neural-network surrogate models for computational science.
Fiats also supports exploring, advancing, and combining functional programming patterns with Fortran 2023 object-oriented and parallel programming features [@fortran2023].
As such, the Fiats name has dual expansions: one in this article's title and a second being ``Fortran inference and training for science.''
Fiats inference functions are `pure` and thus suitable for invocation inside Fortran's loop-level parallelism construct: `do concurrent`. 
Fiats training procedures center around a `do concurrent` construct with a Fortran 2023 parallel reduction.
Several compilers can automatically parallelize `do concurrent` on Central Processing Units (CPUs) or Graphics Processing Units (GPUs).
Fiats thus aims to achieve performance portability through standard language mechanisms.

In addition to simple, illustrative examples, the Fiats repository's `demo/app` subdirecation contains three demonstration applications, one each of which does the following:

1. Trains a cloud-microphysics surrogate for use with the Berkeley Lab fork of the Intermediate Complexity Atmospheric Research (ICAR) model,
2. Calculates tensor statistics from the inputs and outputs of ICAR's physics-based microphysics models, and
3. Performs batch inference calculations using an aerosols surrogate for the Energy Exascale Earth Systems Model (E3SM).

Ongoing research explores how Fiats can exploit multi-image execution, a set of Fortran features designed for Single-Program, Multiple-Data (SPMD) parallel programming with a Partioned Global Address Space (PGAS) [@numrich].
To explore how new language features and novel uses of established language features can support deep learing, Fiats contributors engage in several endeavors to advance Fortran itself by

* Participating in the Fortran standardization process,
* Contributing to compiler development through 
  - Writing unit tests [@rasmussen],
  - Studying performance [@rouson2025automatic],
  - Isolating and reporting compiler bugs and fixing front-end bugs,
  - Publishing revisions of the Parallel Runtime Interface for Fortran (PRIF) [bonachea2024prif,prif-0.5], and
  - Developing the first PRIF implementation: Caffeine [@rouson2022caffeine; @caffeine-site].

Fiats thus serves as a platform both for studying deep learning for science and researching programming idioms, paradigms, and patterns for deep learning in Fortran 2023.

# Statement of need
Fortran 2008 introduced `do concurrent` for expressing loop-level parallelism and multi-image execution for SPMD/PGAS parallel programming in shared or distributed memory.
Fortran 2018 and 2023 expanded and refined these features by adding, for example, 

1. `Do concurrent`: loop-iteration locality specifiers, including reductions,
2. Multi-image execution: coarray distributed data structures, collective subroutines, image teams, events (semaphores), and more.

This creates a need for libraries that support users who adopt these features.
For example, the Fortran standard stipulates that procedures invoked inside a `do concurrent` construct must be `pure`. 
This requirement cascades downward in the call stack because procedures invoked by a `pure` procedure must also be `pure`.

Every intrinsic function defined in the Fortran 2023 standard is `pure` by virtue of being `simple`, which are `pure` procedures that satisfy additional constraints.
Libraries that export `pure` procedures thus behave like extensions of the language.
To wit, the Fortran 2023 standard states, ``It is expected that most library procedures will conform to the constraints required of pure procedures, and so can be declared pure and referenced in `do concurrent` constructs, `forall` statements and constructs, and within user-defined `pure` procedures.''

Conversely, multi-image execution in a library places a requirement on the user code.
The Fortran standard defines steps for synchronized image launch, synchronized normal termination, and single-image initiation of global error termination.
Multi-image execution in a library thus requires a multi-image Fortran main program or at least some mechanism for emulating the same.
 
# State of the field
## Fortran deep learning software
At least six open-source software packages provide deep learning services to Fortran software.
Three provide Fortran application programming interfaces (APIs) wrapping C++ libraries:

* Fortran-TF-Lib [@ftorch] is a TensorFlow [@tensorflow] API,
* FTorch [@ftorch] is an API for the PyTorch [@pytorch] back-end libtorch [@libtorch], and
* TorchFort [torchfort] is a libtorch API.

As of this writing, recursive searches in the root directories of the these three projects find no `pure` procedures as determined by
```
grep -e "pure " -e "simple " -e "elemental " -ri . | grep -v "impure "
```
where the flags reflect that Fortran 2023 defines `simple` and `elemental` procedures as `pure` unless an `elemental` procedures is declared `impure`.
The absence of `pure` procedures precludes the use of these APIs anywhere in the call stack inside a `do concurrent` construct.
Also, as APIs backed by C++ libraries, none use Fortran's multi-image execution features.

Three packages supporting deep learning in Fortran are themselves written in Fortran:

* Athena [@athena]
* Fiats [@fiats]
* neural-fortran [@neural-fortran]

Searching the latter three repositories' `src` directories and culling false positives suggests that over half of the procedures in each are `pure`, including 75\% of the procedures in Fiats.
Each of the three native Fortran repositories exploits this property by extensively using `do concurrent`.
Only Fiats leverages the locality specifiers that Fortran 2018 introduced and Fortran 2023 expanded by adding parallel reductions.

Of the six APIs and libraries discussed here, only neural-fortran and Fiats employ Fortran's multi-image features -- neural-fortran in its core library and Fiats in one of its demonstration applications.
Future work on Fiats will include researching ways to leverage multi-image execution to parallelize neural-network training algorithms.

## Activity level
Each of the six projects discussed in this paper appears to be actively developed except fortran-tf-lib.
The fortran-tf-lib repository has a most recent commit dated in 2023 and has no releases.
Each of the rest has a most-recent commit no older than May 2025.

# Recent research and scholarly publications
Fiats supports research in training surrogate models and parallelizing batch inference calculations for atmospheric sciences.
This research has generated two peer-reviewed paper submissions, including one accepted to appear in workshop proceedings [rouson2025automatically] and one in open review [rouson2025cloud].
Fiats also supports ongoing research in data-reduction strategies for cloud microphysics training data sets.


# Acknowledgments

LDRD

SciDAC NUCLEI

# References
