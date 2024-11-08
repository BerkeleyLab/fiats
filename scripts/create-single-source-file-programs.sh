#!/bin/bash
# Copyright (c), The Regents of the University of California
# Terms of use are as specified in LICENSE.txt
# ----
# This script concatenates the Fiats software stack into single-source-file programs.

echo ""
echo "---------------------- create-single-source-file.sh ----------------------------"
if [ ! -d ../build/dependencies/assert ] || [ ! -d ../build/dependencies/julienne ]; then
  echo ""
  echo ""
  echo "Dependencies assert and julienne were not found in ../build/dependencies." 
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

echo "Concatenating Assert, Julienne, and Fiats." 
cat \
  ../build/dependencies/assert/src/assert/characterizable_m.f90 \
  ../build/dependencies/assert/src/assert/intrinsic_array_m.F90 \
  ../build/dependencies/assert/src/assert/assert_subroutine_m.F90 \
  ../build/dependencies/assert/src/assert/intrinsic_array_s.F90 \
  ../build/dependencies/assert/src/assert/assert_subroutine_s.F90 \
  ../build/dependencies/assert/src/assert_m.f90 \
> single-file.F90

cat \
  ../build/dependencies/julienne/src/julienne/julienne_github_ci_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_bin_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_command_line_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_formats_m.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_user_defined_collectives_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_string_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_file_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_result_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_description_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_vector_test_description_m.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_bin_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_command_line_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_formats_s.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_user_defined_collectives_s.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_string_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_file_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_result_s.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_description_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_test_s.F90 \
  ../build/dependencies/julienne/src/julienne/julienne_vector_test_description_s.f90 \
  ../build/dependencies/julienne/src/julienne/julienne_github_ci_s.f90 \
  ../build/dependencies/julienne/src/julienne_m.f90 \
>> single-file.F90
  
cat \
  ../src/fiats/kind_parameters_m.f90 \
  ../src/fiats/double_precision_file_m.f90 \
  ../src/fiats/double_precision_string_m.f90 \
  ../src/fiats/activation_m.f90 \
  ../src/fiats/hyperparameters_m.f90 \
  ../src/fiats/network_configuration_m.f90 \
  ../src/fiats/training_configuration_m.f90 \
  ../src/fiats/neuron_m.f90 \
  ../src/fiats/neural_network_m.f90 \
  ../src/fiats/metadata_m.f90 \
  ../src/fiats/tensor_m.f90 \
  ../src/fiats/tensor_names_m.f90 \
  ../src/fiats/input_output_pair_m.f90 \
  ../src/fiats/mini_batch_m.f90 \
  ../src/fiats/tensor_map_m.f90 \
  ../src/fiats/layer_m.f90 \
  ../src/fiats/trainable_network_m.F90 \
  ../src/fiats_m.f90 \
  ../src/fiats/double_precision_file_s.f90 \
  ../src/fiats/double_precision_string_s.f90 \
  ../src/fiats/training_configuration_s.F90 \
  ../src/fiats/activation_s.f90 \
  ../src/fiats/unmapped_network_s.f90 \
  ../src/fiats/workspace_s.f90 \
  ../src/fiats/network_configuration_s.F90 \
  ../src/fiats/hyperparameters_s.f90 \
  ../src/fiats/input_output_pair_s.f90 \
  ../src/fiats/neuron_s.f90 \
  ../src/fiats/neural_network_s.F90 \
  ../src/fiats/layer_s.f90 \
  ../src/fiats/metadata_s.f90 \
  ../src/fiats/mini_batch_s.f90 \
  ../src/fiats/tensor_s.f90 \
  ../src/fiats/tensor_map_s.f90 \
  ../src/fiats/tensor_names_s.f90 \
  ../src/fiats/trainable_network_s.F90 \
>> single-file.F90

echo "Creating concurrent-inferences.F90"
cat single-file.F90 \
  ../example/concurrent-inferences.f90 \
> concurrent-inferences.F90

if [ ! -f concurrent-inferences.F90 ]; then
  echo "concurrent-inferences.F90 wasn't created -- something went wrong"
  exit 1
fi

echo "Creating saturated-mixing-ratio.F90"
cat single-file.F90 \
  ../example/supporting-modules/saturated_mixing_ratio_m.f90 \
  ../example/learn-saturated-mixing-ratio.F90 \
> saturated-mixing-ratio.F90

if [ ! -f saturated-mixing-ratio.F90 ]; then
  echo "saturated-mixing-ratio.F90 wasn't created -- something went wrong"
  exit 1
fi

echo "saturated-mixing-ratio.F90 and concurrent-inferences.F90 created."
echo ""
echo "Example compile commands:"
echo 'gfortran -fcoarray=single -O3 -o saturated-mixing-ratio saturated-mixing-ratio.F90'
echo 'flang-new -O3 -mmlir -allow-assumed-rank -o saturated-mixing-ratio saturated-mixing-ratio.F90'
echo 'nagfor -fpp -O4 -o saturated-mixing-ratio saturated-mixing-ratio.F90'
echo 'ifx -O3 -o saturated-mixing-ratio saturated-mixing-ratio.F90'

rm single-file.F90

echo "-------------------------------------------------------------------------"
