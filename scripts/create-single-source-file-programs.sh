#!/bin/bash
# Copyright (c), The Regents of the University of California
# Terms of use are as specified in LICENSE.txt
# ----
# This script concatenates the Fiats software stack into single-source-file programs.

export assert_dir="../build/dependencies/assert"
export julienne_dir="../build/dependencies/julienne"
export fiats_dir=".."
export destination_dir="../build/single-file-programs"

if [ -d $destination_dir ]; then
  echo "Destination directory exists: $destination_dir"
else
  echo "Creating destination directory: $destination_dir"
  mkdir "$destination_dir"
fi

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
  echo "Dependencies Assert and Julienne are in ../build/dependencies"
else
  echo "Dependencies Assert and Julienne are not in ../build/dependencies. Something went wrong."
  exit 1
fi

echo "Concatenating Assert." 
assert_modules=$(find "$assert_dir/src" -name '*_m.?90')
assert_submodules=$(find "$assert_dir/src" -name '*_s.?90')
cat $assert_modules $assert_submodules > "$destination_dir"/assert.F90

echo "Concatenating Julienne." 
julienne_modules=$(find "$julienne_dir/src" -name '*_m.?90')
julienne_submodules=$(find "$julienne_dir/src" -name '*_s.?90')
cat $julienne_modules $julienne_submodules > "$destination_dir"/julienne.F90

echo "Concatenating Fiats." 
fiats_modules=$(find "$fiats_dir/src" -name '*_m.?90')
fiats_submodules=$(find "$fiats_dir/src" -name '*_s.?90')
cat $fiats_modules $fiats_submodules > "$destination_dir"/fiats.F90

echo "Copying include files." 
cp "$assert_dir/include/assert_macros.h" "$destination_dir"
cat "$julienne_dir/include/language-support.F90" "$fiats_dir/include/language-support.F90" > "$destination_dir"/language-support.F90
cp "$fiats_dir/include/compound_assertions.h" "$destination_dir"

echo "Concatenating Assert, Julienne, Fiats, and example/concurrent-inferences.f90 into $destination_dir/concurrent-inferences-single-file.F90"
cat "$destination_dir"/assert.F90 "$destination_dir"/julienne.F90 "$destination_dir"/fiats.F90 "$fiats_dir/example/concurrent-inferences.f90" > $destination_dir/concurrent-inferences-single-file.F90
