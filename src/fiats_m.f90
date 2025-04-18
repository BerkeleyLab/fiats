! Copyright (c) 2023-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module fiats_m
 !! Specify the user-facing modules, derived types, and type parameters
 use double_precision_file_m, only : double_precision_file_t
 use double_precision_string_m, only : double_precision_string_t
 use hyperparameters_m, only : hyperparameters_t
 use input_output_pair_m, only : input_output_pair_t, shuffle, write_to_stdout
 use kind_parameters_m, only : default_real, double_precision
 use metadata_m, only : metadata_t
 use mini_batch_m, only : mini_batch_t
 use neural_network_m, only : neural_network_t, unmapped_network_t
 use network_configuration_m, only : network_configuration_t
 use tensor_m, only : tensor_t
 use tensor_map_m, only : tensor_map_t
 use tensor_names_m, only : tensor_names_t
 use trainable_network_m, only : trainable_network_t
 use training_configuration_m, only : training_configuration_t
 use training_data_files_m, only : training_data_files_t
 implicit none
end module fiats_m
