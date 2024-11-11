#!/bin/bash
# Copyright (c), The Regents of the University of California
# Terms of use are as specified in LICENSE.txt
# ----
# This script concatenates the Fiats software stack into single-source-file programs.

export assert_dir="../build/dependencies/assert"
export julienne_dir="../build/dependencies/julienne"
export fiats_dir=".."

echo ""
echo "---------------------- create-single-source-file.sh ----------------------------"
if [ ! -d "$assert_dir" ] || [ ! -d $julienne_dir ]; then
  echo ""
  echo ""
  echo "Dependencies assert and/or julienne were not found in ../build/dependencies."
  echo "Running fpm build to download the dependencies."
  echo "This unavoidably builds fiats too."
  echo ""
  echo ""
  fpm build
fi

if [ -d ../build/dependencies/assert ] && [ -d ../build/dependencies/julienne ]; then
  echo "------------- Dependencies assert and julienne are in ../build/dependencies ------------------"
else
  echo "Dependencies assert and julienne are not in ../build/dependencies. Something went wrong."
  exit 1
fi

echo "Concatenating Assert." 
assert_modules=$(find "$assert_dir/src" -name '*_m.?90')
assert_submodules=$(find "$assert_dir/src" -name '*_s.?90')
cat ${assert_include:-} $assert_modules $assert_submodules > assert.F90

echo "Concatenating Julienne." 
julienne_modules=$(find "$julienne_dir/src" -name '*_m.?90')
julienne_submodules=$(find "$julienne_dir/src" -name '*_s.?90')
cat $julienne_modules $julienne_submodules > julienne.F90

echo "Concatenating Fiats." 
fiats_modules=$(find "$fiats_dir/src" -name '*_m.?90')
fiats_submodules=$(find "$fiats_dir/src" -name '*_s.?90')
cat $fiats_modules $fiats_submodules > fiats.F90

echo "Copying include files." 
cp "$assert_dir/include/assert_macros.h" .
cat "$julienne_dir/include/language-support.F90" "$fiats_dir/include/language-support.F90" > language-support.F90


echo "Concatenating Assert, Julienne, Fiats, and example/concurrent-inferences.f90." 
cat assert.F90 julienne.F90 fiats.F90 "$fiats_dir/example/concurrent-inferences.f90" > concurrent-inferences-single-file.F90
