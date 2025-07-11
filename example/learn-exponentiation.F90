! Copyright (c) 2023-2025, The Regents of the University of California
! Terms of use are as specified in LICENSE.txt
module exponentiation_m
  !! Define a function that produces the desired network output for a given network input
  use fiats_m, only : tensor_t
  use assert_m, only : assert
  implicit none

contains
  elemental function y(x_tensor) result(a_tensor)
    type(tensor_t), intent(in) :: x_tensor
    type(tensor_t) a_tensor
    associate(x => x_tensor%values())
      call assert(ubound(x,1)>=7 .and. lbound(x,1)<=2,"y(x) :: sufficient input")
      a_tensor = tensor_t([x(1)**2, x(2)**3, x(3)**4, x(4)**4, x(5)**3, x(6)**2])
    end associate
  end function

end module

program learn_exponentiation
  !! This trains a neural network to learn the following six polynomial functions of its eight inputs.
  use fiats_m, only : neural_network_t, trainable_network_t, mini_batch_t, tensor_t, input_output_pair_t, shuffle
  use julienne_m, only : string_t, file_t, command_line_t, bin_t
  use assert_m, only : assert, intrinsic_array_t
  use exponentiation_m, only : y
  implicit none

  type(string_t) final_network_file
  type(command_line_t) command_line

  final_network_file = string_t(command_line%flag_value("--output-file"))

  if (len(final_network_file%string())==0) then
    error stop new_line('a') // new_line('a') // &
      'Usage: fpm run --example learn-exponentiation--profile release --flag "-fopenmp" -- --output-file "<file-name>"'
  end if

  block
    integer, parameter :: num_pairs = 10, num_epochs = 200000, num_mini_batches= 2 ! num_pairs =  # input/output pairs in training data

    type(mini_batch_t), allocatable :: mini_batches(:)
    type(input_output_pair_t), allocatable :: input_output_pairs(:)
    type(tensor_t), allocatable :: inputs(:), desired_outputs(:)
    type(trainable_network_t)  trainable_network
    type(bin_t), allocatable :: bins(:)
    real, allocatable :: cost(:), random_numbers(:)

    call random_init(image_distinct=.true., repeatable=.true.)
    trainable_network = perturbed_identity_network(perturbation_magnitude=0.05)
    call output(trainable_network, string_t("initial-network.json"))

    associate(num_inputs => trainable_network%num_inputs(), num_outputs => trainable_network%num_outputs())

      block
        integer i, j
        integer, allocatable :: output_sizes(:)
        inputs = [(tensor_t(real([(j*i, j = 1,num_inputs)])/(num_inputs*num_pairs)), i = 1, num_pairs)]
        desired_outputs = y(inputs)
        output_sizes = [(size(desired_outputs(i)%values()),i=1,size(desired_outputs))]
        call assert(all([num_outputs==output_sizes]), "fit-polynomials: # outputs", intrinsic_array_t([num_outputs,output_sizes]))
      end block
      input_output_pairs = input_output_pair_t(inputs, desired_outputs)
      block
        integer b
        bins = [(bin_t(num_items=num_pairs, num_bins=num_mini_batches, bin_number=b), b = 1, num_mini_batches)]
      end block

      allocate(random_numbers(2:size(input_output_pairs)))

      print *,"Cost"
      block
        integer e, b
        do e = 1,num_epochs
          call random_number(random_numbers)
          call shuffle(input_output_pairs)
          mini_batches = [(mini_batch_t(input_output_pairs(bins(b)%first():bins(b)%last())), b = 1, size(bins))]
          call trainable_network%train(mini_batches, cost, adam=.true., learning_rate=1.5)
          print *,sum(cost)/size(cost)
        end do
      end block

      block
        integer p
#if defined _CRAYFTN || __GFORTRAN__
        type(tensor_t), allocatable :: network_outputs(:)
        network_outputs = trainable_network%infer(inputs)
#else
        associate(network_outputs => trainable_network%infer(inputs))
#endif
          print "(a,69x,a)","  Outputs", "| Desired outputs"
          do p = 1, num_pairs
            print "(6G13.5, a1, 6G13.5)",network_outputs(p)%values(),       "|", desired_outputs(p)%values()
          end do
#if defined _CRAYFTN || __GFORTRAN__
#else
        end associate
#endif
      end block

   end associate

   call output(trainable_network, final_network_file)

  end block

contains

  subroutine output(neural_network, file_name)
    class(neural_network_t), intent(in) :: neural_network
    type(string_t), intent(in) :: file_name
    type(file_t) json_file
    json_file = neural_network%to_json()
    call json_file%write_lines(file_name)
  end subroutine

  pure function e(j,n) result(unit_vector)
    integer, intent(in) :: j, n
    integer k
    real, allocatable :: unit_vector(:)
    unit_vector = real([(merge(1,0,j==k),k=1,n)])
  end function

  function perturbed_identity_network(perturbation_magnitude) result(trainable_network)
    type(trainable_network_t) trainable_network
    real, intent(in) :: perturbation_magnitude
    integer, parameter :: n(*) = [8, 64, 64, 64, 6] ! nodes per layer (first layer = input, last layer = output)
    integer, parameter :: n_max = maxval(n), layers = size(n)
    integer k, l
    real, allocatable :: identity(:,:,:), w_harvest(:,:,:), b_harvest(:,:)

    identity =  reshape( [( [(e(k,n_max), k=1,n_max)], l = 1, layers-1 )], [n_max, n_max, layers-1])

    allocate(w_harvest, mold = identity)
    allocate(b_harvest(size(identity,1), size(identity,3)))

    call random_number(w_harvest)
    call random_number(b_harvest)

    associate(w => identity + perturbation_magnitude*(w_harvest-0.5)/0.5, b => perturbation_magnitude*(b_harvest-0.5)/0.5)

      trainable_network = trainable_network_t( neural_network_t( &
        nodes = n, weights = w, biases = b, metadata = &
          [string_t("Perturbed Identity"), string_t("Damian Rouson"), string_t("2023-09-23"), string_t("relu"), string_t("false")] &
      ))

    end associate
  end function

end program
