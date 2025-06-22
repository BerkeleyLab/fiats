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
  - name: Dan Bonachea
    orcid: 0000-0002-0724-9349
    equal-contrib: false
    affiliation: 1
  - name: Brad Richardson
    orcid: 0000-0002-3205-2169
    equal-contrib: false
    affiliation: 1
  - name: Jordan A. Welsman
    orcid: 0000-0002-2882-594X
    equal-contrib: false
    affiliation: 1
  - name: Jeremiah Bailey
    orcid: 0000-0002-0436-9118
    equal-contrib: false
    affiliation: 1
  - name: Ethan D Gutmann
    orcid: 0000-0003-4077-3430
    equal-contrib: false
    affiliation: 2
  - name: David Torres
    orcid: 0000-0003-2469-5284
    equal-contrib: false
    affiliation: 3
  - name: Katherine Rasmussen
    orcid: 0000-0001-7974-1853
    equal-contrib: false
    affiliation: 1
  - name: Baboucarr Dibba
    orcid: 0009-0008-0479-3948
    equal-contrib: false
    affiliation: 1
  - name: Yunhao Zhang
    orcid: 0009-0009-3182-9296
    equal-contrib: false
    affiliation: 1
  - name: Kareem Weaver
    orcid: 0009-0009-3846-6248
    equal-contrib: false
    affiliation: 1
  - name: Zhe Bai
    orcid: 0000-0002-3092-0903
    equal-contrib: false
    affiliation: 1
affiliations:
 - name: Lawrence Berkeley National Laboratory, United States
   index: 1
 - name: NSF National Center for Atmospheric Research, United States
   index: 2
 - name: Northern New Mexico College, United States
   index: 3
date: 20 June 2025
bibliography: paper.bib

---

# Summary
[Fiats](https://go.lbl.gov/fiats) provides a platform for research on the training and deployment of neural-network surrogate models for computational science.
Fiats also supports exploring, advancing, and combining functional, object-oriented, and parallel programming patterns in Fortran 2023 [@fortran2023].
As such, the Fiats name has dual expansions: ``Functional Inference And Training for Surrogates'' or ``Fortran Inference And Training for Science.''
Fiats inference functions are `pure` and therefore suitable for invocation inside Fortran's loop-level parallelism construct: `do concurrent`.
Fiats training procedures center around a `do concurrent` construct with a Fortran 2023 parallel reduction.
Several compilers can automatically parallelize `do concurrent` on Central Processing Units (CPUs) or Graphics Processing Units (GPUs).
Fiats thus aims to achieve performance portability through standard language mechanisms.

In addition to simple, illustrative examples, the Fiats repository's `demo/app` subdirectory contains three demonstration applications:

1. One trains a cloud-microphysics surrogate for the Berkeley Lab [fork](https://go.lbl.gov/icar) of the Intermediate Complexity Atmospheric Research (ICAR) model,
2. Another calculates input- and output-tensor statistics for ICAR's physics-based microphysics models, and
3. A third performs batch inference using an aerosols surrogate for the Energy Exascale Earth Systems Model ([E3SM](https://e3sm.org)).

Ongoing research explores how Fiats can exploit multi-image execution, a set of Fortran features for Single-Program, Multiple-Data (SPMD) parallel programming with a Partitioned Global Address Space (PGAS) [@numrich2018co-arrays], where the PGAS features center around ``coarray'' distributed data structures.

To explore how new language features and novel uses of features can power deep learning in Fortran, Fiats contributors work to advance Fortran by:

* Participating in the Fortran standardization process and
* Contributing to compiler development through
  - Writing unit tests [@rasmussen2022agile],
  - Studying performance [@rouson2025automatically],
  - Isolating and reporting compiler bugs and fixing front-end bugs,
  - Publishing and updating the Parallel Runtime Interface for Fortran (PRIF) [@bonachea2024prif; @prif-0.5], and
  - Developing the first PRIF implementation: [Caffeine](https://go.lbl.gov/caffeine) [@rouson2022caffeine; @bonachea2025caffeine].

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
To wit, the Fortran 2023 standard states: "It is expected that most library procedures will conform to the constraints required of pure procedures, and so can be declared pure and referenced in `do concurrent` constructs... and within user-defined `pure` procedures."

Conversely, multi-image execution in a library places a requirement on the client code.
The Fortran standard defines steps for synchronized image launch, synchronized normal termination, and single-image initiation of global error termination.
Multi-image execution in a library thus requires support for multi-image execution in the main program.

A surrogate model's utility hinges upon exhibiting inference execution times faster than the physics-based model the surrogate replaces.
This commonly restricts the surrogate neural network to a few thousand tunable parameters.
For networks of modest size, useful insights can sometimes be gleaned from visually inspecting the network parameters.
Fiats stores networks in human-readable JavaScript Object Notation (JSON) format.
The Fiats companion package [Nexport](https://go.lbl.gov/nexport) facilitates exchanging such files with [PyTorch](https://pytorch.org).

# State of the field
## Fortran deep learning software
At least six open-source software packages provide deep learning services to Fortran.
Three provide Fortran application programming interfaces (APIs) that wrap C++ libraries:

* [Fortran-TF-Lib](https://github.com/Cambridge-ICCS/fortran-tf-lib) is a Fortran API for [TensorFlow](https://tensorflow.org),
* [FTorch](https://github.com/Cambridge-ICCS/FTorch) is a Fortran API for libtorch, the PyTorch back-end, and
* [TorchFort](https://github.com/NVIDIA/TorchFort) is also a Fortran API for libtorch.

As of this writing, recursive searches in the root directories of the these three projects find no `pure` procedures.
Procedures are `pure` if declared as such or if declared `simple` or if declared `elemental` without the `impure` attribute.
Because any procedure invoked within a `pure` procedure must also be `pure`, the absence of `pure` procedures precludes the use of these APIs anywhere in the call stack inside a `do concurrent` construct.
Also, as APIs backed by C++ libraries, none use Fortran's multi-image execution features.

Three packages supporting deep learning in Fortran are themselves written in Fortran:

* [Athena](https://github.com/nedtaylor/athena) [@taylor2024athena]
* [Fiats](https://go.lbl.gov/fiats) [@rouson2025automatically]
* [neural-fortran](https://github.com/modern-fortran/neural-fortran) [@curcic2019parallel]

Searching Athena, Fiats, and neural-fortran `src` subdirectories finds that over half of the procedures in each are `pure`, including 75\% of Fiats procedures.
Included in these tallies are procedures explicitly marked as `pure` along with `simple` procedures and `elemental` procedures without the `impure` attribute.
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

1. [`example/concurrent-inferences.f90`](https://github.com/BerkeleyLab/fiats/blob/joss-line-references/example/concurrent-inferences.f90#L1),
2. [`app/demo/infer-aerosols.f90`](https://github.com/BerkeleyLab/fiats/blob/joss-line-references/demo/app/infer-aerosol.f90#L1), and
3. [`example/learn-saturated-mixing-ratio.f90`](https://github.com/BerkeleyLab/fiats/blob/joss-line-references/example/learn-saturated-mixing-ratio.F90#L1),
4. [`app/demo/train-cloud-microphysics.f90`](https://github.com/BerkeleyLab/fiats/blob/joss-line-references/demo/app/train-cloud-microphysics.F90#L1).

![Class diagram: type extension (open triangles), composition (solid diamonds), or directional relationship (arrows).  Read relationship annotations (gray boxes) as a sentence with the boxed text preceded by the derived type at the base of an arrow (the subject) and followed by the type at the head of an arrow (the sentence's object).  Type extension reads with the type on the end of the open triangle as the subject.  Composition reads with the type on the side of the closed diamond as the subject. \label{fig:derived-types}](class-diagram){ width=40% }

[@rouson2025automatically] reported the results of research on automatically parallelizing batch inference calculations via Fortran's '`do concurrent` construct using program 1.
[@rouson2025cloud] reported the results of research on neural-network training for cloud microphysics and inference for atmospheric aerosols using programs 2 and 3.
Program 4 supports ongoing research on developing a cloud microphysics surrogate for the Intermediate Complexity Atmospheric Research (ICAR) atmospheric model.
In addition to Fiats types, \autoref{fig:derived-types} includes class diagrams for two derived types from the [Julienne](https://go.lbl.gov/julienne) correctness-checking utility: the `string_t` and `file_t` types.
Other parts of the diagram reference these Julienne types so the `string_t` and `filet_t` class diagrams are included for completeness.

Each class diagram for a derived type in \autoref{fig:derived-types} displays three panels: one containing the type name, an empty panel where private components have been omitted, a third panel listing public procedure bindings.
The third panel also lists user-defined structure constructors, which are generic interfaces through which to invoke one or more functions that define a result of the named type.
The figure's rightmost four class diagrams exist primarily to serve the needs of those seeking to perform inference.
The leftmost six diagrams serve training needs.
Because inference is considerably simpler, it makes sense to describe the right side of the diagram before the left side.

The `concurrent-inferences` example program, the simplest case, centers around performing a batch of inferences.  
From the bottom of the class hierarchy up, the program

1. Gets a `character` file name from the command line,
2. Passes the name to a `string_t` constructor,
3. Passes the resulting `string_t` object to a `file_t` constructor,
4. Passes the resulting `file_t` object to a `neural_network_t` constructor.

The program then repeatedly invokes the `infer` type-bound procedure on a three-dimensional (3D) array of `tensor_t` objects in various ways such as using OpenMP directives, or `do concurrent`, or an array statement that takes advantage of `infer` being `elemental`.
Lines 101 and 109 of `example/concurrent-inferences.f90` at `git` tag `joss-line-references` demonstrate neural-network construction from a file and using neural network for inference, respectively.

The `infer-aerosols` program performs inferences by invoking `double precision` versions of the `infer` generic binding on an object of type `unmapped_network_t`, a parameterized derived type (PDT) that has a `kind` type parameter.
To match the expected behavior of the aerosol model, which was trained in PyTorch, the `unmapped_network_t` implementation ensures the use of raw network input and output tensors without the normalizations and remappings that are performed by default for a `neural_network_t` object.
The `double_precision_file_t` type serves to control the interpretation of the JSON network file: JSON does not distinguish between categories of numerical values such as `real`, `double precision`, or even `integer`, so something external to the file must determine the interpretation of the numbers a JSON file stores.

The `learn-saturated-mixing-ratio` and `train-cloud-microphysics` programs center around the use of a `trainable_network_t` object for training.
The former trains neural network surrogates for a thermodynamic function from ICAR: the saturated mixing ratio, a scalar function of temperature and pressure.
The latter trains surrogates for the complete cloud microphysics models in ICAR -- models that require thousands of lines of code to implement.
Whereas diagrammed relationships of `neural_network_t` reflect direct dependencies of only two types (`file_t` and `tensor_t`), even describing the basic behaviors of `trainable_network_t` requires showing dependencies on five types:

* A `training_configuration_t` object, which holds hyperparameters such as the learning rate and choice of optimization algorithms,
* A `file_t` object from which the `training_configuration` is read inside the `trainable_network_t` constructor,
* A `mini_batch_t` object that stores an array of `input_output_pair` objects from the training data set,
* Two `tensor_map_t` objects storing the linear functions applied to map inputs to training data range and to map outputs from the training data range back to the application domain, and
* A parent `neural_network_t` object storing the network architecture, including weights, biases, layer widths, etc.

The `trainable_network_t` serves to store a `workspace_t` (not shown) as a scratch-pad for training purposes.
The workspace is not needed for inference.
During each training step, a `trainable_network_t` object passes its `workspace_t` into a corresponding `learn` procedure binding (not shown) on its parent `neural_network_t`.
Lines 388--396 of `demo/app/train-cloud-microphysics.f90` at `git` tag `joss-line-references` demonstrate:

1. A loop over epochs,
2. The shuffling of the `input_output_pair_t` objects at the beginning of each epoch,
3. The grouping of `input_output_pair_t` objects into `mini_batch_t` objects, and
4. The invocation of the `train` procedure for each mini-batch,

where steps 2 and 3 express the stochastic gradient descent algorithm at the heart of deep learning methods.

# Acknowledgments
This material is based upon work supported by the U.S. Department of Energy, Office of Science, Office of Advanced Scientific Computing Research and Office of Nuclear Physics, Scientific Discovery through Advanced Computing (SciDAC)  Next-Generation Scientific Software Technologies (NGSST) programs under Contract No. DE-AC02-05CH11231.
This material is also based on work supported by Laboratory Directed Research and Development (LDRD) funding from Lawrence Berkeley National Laboratory, provided by the Director, Office of Science, of the U.S. DOE under Contract No. DE-AC02-05CH11231.
This manuscript has been authored by an author at Lawrence Berkeley National Laboratory under Contract No. DE-AC02-05CH11231 with the U.S. Department of Energy.
The U.S. Government retains, and the publisher, by accepting the article for publication, acknowledges, that the U.S. Government retains a non-exclusive, paid-up, irrevocable, world-wide license to publish or reproduce the published form of this manuscript, or allow others to do so, for U.S. Government purposes.

# References
