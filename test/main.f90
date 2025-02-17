  use trainable_network_test_m, only : trainable_network_test_t
  implicit none

  type(trainable_network_test_t) trainable_network_test
  integer :: passes=0, tests=0

  call trainable_network_test%report(passes, tests)
end 
