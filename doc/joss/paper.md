---
title: 'Fiats: Functional inference and training for surrogates'
tags:
  - artificial neural networks
  - high-performance computing
  - parallel programming
  - deep learning
  - Fortran
authors:
  - name: Damian Rouson
    orcid: 0000-0002-2344-868X
    equal-contrib: false
    affiliation: 1
affiliations:
 - name: Lawrence Berkeley National Laboratory, United States
   index: 1
date: 9 June 2025
bibliography: paper.bib

---

# Summary
[Fiats](https://go.lbl.gov/fiats) provides a platform for research on the training and deployment of neural-network surrogate models for computational science.
Fiats also supports exploring, advancing, and combining functional, object-oriented, and parallel programming patterns in Fortran 2023 [@fortran2023].
As such, the Fiats name has dual expansions: ``Functional inference and training for surrogates'' or ``Fortran inference and training for science.''
Fiats inference functions are `pure` and therefore suitable for invocation inside Fortran's loop-level parallelism construct: `do concurrent`. 
Fiats training procedures center around a `do concurrent` construct with a Fortran 2023 parallel reduction.
Several compilers can automatically parallelize `do concurrent` on Central Processing Units (CPUs) or Graphics Processing Units (GPUs).
Fiats thus aims to achieve performance portability through standard language mechanisms.

In addition to simple, illustrative examples, the Fiats repository's `demo/app` subdirectory contains three demonstration applications:

1. One trains a cloud-microphysics surrogate for the Berkeley Lab [fork](https://go.lbl.gov/icar) of the Intermediate Complexity Atmospheric Research (ICAR) model,
2. Another calculates input- and output-tensor statistics for ICAR's physics-based microphysics models, and
3. A third performs batch inference using an aerosols surrogate for the Energy Exascale Earth Systems Model ([E3SM](https://e3sm.org)).

Ongoing research explores how Fiats can exploit multi-image execution, a set of Fortran features for Single-Program, Multiple-Data (SPMD) parallel programming with a Partioned Global Address Space (PGAS) [@numrich2018co-arrays], where the PGAS features center around ``coarray'' distributed data structures.

To explore how new language features and novel uses of features can power deep learning in Fortran, Fiats contributors work to advance Fortran by

* Participating in the Fortran standardization process and
* Contributing to compiler development through 
  - Writing unit tests [@rasmussen2022agile],
  - Studying performance [@rouson2025automatic],
  - Isolating and reporting compiler bugs and fixing front-end bugs,
  - Publishing and updating the Parallel Runtime Interface for Fortran (PRIF) [bonachea2024prif,prif-0.5], and
  - Developing the first PRIF implementation: [Caffeine](https://go.lbl.gov/caffeine) [@rouson2022caffeine,bonachea2025caffeine].

Fiats thus supports the study of deep learning for science and the study of programming paradigms and patterns for deep learning in Fortran 2023.

# Statement of need
Fortran 2008 introduced `do concurrent` for expressing loop-level parallelism and multi-image execution for SPMD/PGAS parallel programming in shared or distributed memory.
Fortran 2018 and 2023 expanded and refined these features by adding, for example,

1. `Do concurrent` loop-iteration locality specifiers, including reductions and
2. Collective subroutines, image teams, events (semaphores), atomic subroutines, and more,

which creates a need for libraries that support users who adopt these features.
For example, one requirement that impacts library design stems from Fortran's stipulation that procedures invoked inside a `do concurrent` construct must be `pure`. 

All intrinsic functions defined in the Fortran 2023 standard are `simple`, an attribute that implies `pure` plus additional constraints.
Libraries that export `pure` procedures thus behave like extensions of the language.
To wit, the Fortran 2023 standard states, ``It is expected that most library procedures will conform to the constraints required of pure procedures, and so can be declared pure and referenced in `do concurrent` constructs... and within user-defined `pure` procedures.''

Conversely, multi-image execution in a library places a requirement on the user code.
The Fortran standard defines steps for synchronized image launch, synchronized normal termination, and single-image initiation of global error termination.
Multi-image execution in a library thus requires support for multi-image execution in the main program.

A surrogate model's utility hinges upon exhibiting execution times faster than the physics-based model the surrogate replaces.
This restricts the surrogate neural network to a few thousand tunable parameters.
For networks of modest size, useful insights can sometimes be gleaned from visually inspecting the network parameters.
Fiats stores networks in human-readable JavaScript Object Notation (JSON) format.
The Fiats companion package [Nexport](https://go.lbl.gov/nexport) facilitates exchanging such files with [PyTorch](https://pytorch.org).
 
# State of the field
## Fortran deep learning software
At least six open-source software packages provide deep learning services to Fortran.
Three provide Fortran application programming interfaces (APIs) that wrap C++ libraries:

* [Fortran-TF-Lib](https://github.com/Cambridge-ICCS/fortran-tf-lib) is a Fortran API for [TensorFlow](https://tensorflow.org),
* [FTorch](https://github.com/Cambridge-ICCS/FTorch) is a Fortran API for libtorach, the PyTorch back-end, and
* [TorchFort](https://github.com/NVIDIA/TorchFort) is also a Fortran API for libtorch.

As of this writing, recursive searches in the root directories of the these three projects find no `pure` procedures.
Procedures are `pure` if declared as such or if declared `simple` or if declared `elemental` without the `impure` attribute.
Because any procedure invoked within a `pure` procedure must also be `pure`, the absence of `pure` procedures precludes the use of these APIs anywhere in the call stack inside a `do concurrent` construct.
Also, as APIs backed by C++ libraries, none use Fortran's multi-image execution features.

Three packages supporting deep learning in Fortran are themselves written in Fortran:

* [Athena](https://github.com/nedtaylor/athena) [@taylor2024athena]
* [Fiats](https://go.lbl.gov/fiats) [@rouson2025automatic]
* [neural-fortran](https://github.com/) [@curcic2019parallel]

Searching Athena, Fiats, and neural-fortran `src` subdirectories finds that over half of the procedures in each are `pure`, including 75\% of Fiats procedures.
Included in these tallies are procedures explicitly marked as `pure` along with `simple` procedures and `elemnetal` procedures without the `impure` attribute.
Athena, Fiats, and neural-fortran each employ `do concurrent` extensively.
Only Fiats, however, leverages the locality specifiers introduced in Fortran 2018 and expanded in Fortran 2023 to include parallel reductions.

Of the APIs and libraries discussed here, only neural-fortran and Fiats employ multi-image features: neural-fortran in its core library and Fiats in a demonstration application.
Both use multi-image features minimally, leaving considerable room for future research into achieving parallel speedup.

## Activity level
Each of the Fortran deep learning APIs and libraries discussed in this paper is actively developed except Fortran-TF-Lib.
Fortran-TF-Lib's most recent commit was in 2023, and it is the only Fortran API or library that has never produced releases.
Each of the rest has a most-recent commit no older than May 2025.


# Recent research and scholarly publications
## Publications
Fiats supports research in training surrogate models and parallelizing batch inference calculations for atmospheric sciences.
This research has generated two peer-reviewed paper submissions, including one accepted to appear in workshop proceedings [@rouson2025automatically] and one in open review [@rouson2025cloud].
Fiats also supports ongoing research in data-reduction strategies for cloud microphysics training data sets.

## Use of Fiats in research
\autoref{fig:derived-types} contains a Unified Modeling Language (UML) class diagram depicting the Fiats derived types that supported the research publications cited in the previous subsection:

1. [`example/concurrent-inferences.f90`](https://github.com/BerkeleyLab/fiats/blob/460f31e5df2f3a50800b6792822754b04a91f5c9/example/concurrent-inferences.f90#L1),
2. [`example/learn-saturated-mixing-ratio.f90`](https://github.com/BerkeleyLab/fiats/blob/460f31e5df2f3a50800b6792822754b04a91f5c9/example/learn-saturated-mixing-ratio.F90#L1),
3. [`app/demo/infer-aerosols.f90`](https://github.com/BerkeleyLab/fiats/blob/460f31e5df2f3a50800b6792822754b04a91f5c9/demo/app/infer-aerosol.f90#L1), and
4. [`app/demo/train-cloud-microphysics.f90`](https://github.com/BerkeleyLab/fiats/blob/460f31e5df2f3a50800b6792822754b04a91f5c9/demo/app/train-cloud-microphysics.F90#L1).

![Fiats class diagram.\label{fig:derived-types}](class-diagram){ width=100% }

[@rouson2025automatically] reported the results of research on automatically parallelizing batch inference calculations via Fortran's '`do concurrent` construct using program 1.
[@rouson2025cloud] reported the results of research on neural-network training for cloud microphysics and inference for atmospheric aerosols using programs 2 and 3.
Program 4 supports ongoing research on developing a cloud microphysics surrogate for the Intermediate Complexity Atmospheric Research (ICAR) atmospheric model.
In addition to Fiats types, \autoref{fig:derived-types} includes class diagarams for two derived types from the [Julienne](https://go.lbl.gov/julienne) correctness-checking utility: the `string_t` and `file_t` types.
Other parts of the diagram reference these Julienne types so the `string_t` and `filet_t` class diagrams are include for completeness.


# Acknowledgments
This material is based upon work supported by the U.S. Department of Energy, Office of Science, Office of Advanced Scientific Computing Research and Office of Nuclear Physics, Scientific Discovery through Advanced Computing (SciDAC)  Next-Generation Scientific Software Technologies (NGSST) programs under Contract No. DE-AC02-05CH11231.
This material is also based on work supported by Laboratory Directed Research and Development (LDRD) funding from Lawrence Berkeley National Laboratory, provided by the Director, Office of Science, of the U.S. DOE under Contract No. DE-AC02-05CH11231.
This manuscript has been authored by an author at Lawrence Berkeley National Laboratory under Contract No. DE-AC02-05CH11231 with the U.S. Department of Energy.
The U.S. Government retains, and the publisher, by accepting the article for publication, acknowledges, that the U.S. Government retains a non-exclusive, paid-up, irrevocable, world-wide license to publish or reproduce the published form of this manuscript, or allow others to do so, for U.S. Government purposes.

# References
