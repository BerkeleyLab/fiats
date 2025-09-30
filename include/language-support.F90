! Copyright (c) 2024-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#ifndef _FIATS_LANGUAGE_SUPPORT_H
#define _FIATS_LANGUAGE_SUPPORT_H

#ifdef __GNUC__
#  define GCC_VERSION (__GNUC__ * 10000 + __GNUC_MINOR__ * 100 + __GNUC_PATCHLEVEL__)
#else
#  define GCC_VERSION 0
#endif

#ifndef F2023_LOCALITY
#  if (__INTEL_COMPILER >= 202400) || (__clang_major__ >= 22) || (GCC_VERSION >= 150100)
#    define F2023_LOCALITY 1
#  else
#    define F2023_LOCALITY 0
#  endif
#endif

#ifndef F2018_LOCALITY
#  if defined(_CRAYFTN)
#    define F2018_LOCALITY 1
#  endif
#endif

! If not already determined, make a compiler-dependent determination of
! whether to use multi-image features
#ifndef HAVE_MULTI_IMAGE_SUPPORT
#  if defined(_CRAYFTN) || defined(__GFORTRAN__) || defined(__INTEL_COMPILER) || defined(NAGFOR) || __flang_major__ >= 22
#    define HAVE_MULTI_IMAGE_SUPPORT 1
#  else
#    define HAVE_MULTI_IMAGE_SUPPORT 0
#  endif
#endif

! If not already determined, make a compiler-dependent determination of whether Julienne may pass
! procedure actual arguments to procedure pointer dummy arguments, a feature introduced in
! Fortran 2008 and described in Fortran 2023 clause 15.5.2.10 paragraph 5.
#ifndef HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY
#  if defined(_CRAYFTN) || defined(__INTEL_COMPILER) || defined(NAGFOR) || defined(__flang__) || (GCC_VERSION > 140200)
#    define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 1
#  else
#    define HAVE_PROCEDURE_ACTUAL_FOR_POINTER_DUMMY 0
#  endif
#endif

#endif
