! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

! This file provides preprocessor-based macros to call procedures that exist
! solely to make calls to 'assert' and that therefore will be eliminated completely
! whenever the ASSERTIONS macro is defined to a value other than 0.

! Enable repeated includes to toggle assertions based on current settings:
#undef call_assert_consistency
#undef call_assert_conformable

#ifndef ASSERTIONS
! Assertions are off by default
#define ASSERTIONS 0
#endif

#if ASSERTIONS
# define call_assert_consistency(obj) call assert_consistency(obj)
# define call_assert_conformable(lhs,rhs) call assert_conformable(lhs,rhs)
#else
# define call_assert_consistency(obj)
# define call_assert_conformable(lhs,rhs)
#endif
