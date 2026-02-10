program main
  implicit none

  integer i
  integer, parameter :: m = 10, n = 20
  integer, target    :: raw_trunk_outputs(m,n) = reshape([(i, i=1,m*n)], [m,n])
  integer, parameter :: basis(*,*,*) = reshape([(size(raw_trunk_outputs)+i, i=1,product([m,n,n]))], [m,n,n])
  integer, allocatable :: aug_trunk(:,:,:)

  aug_trunk = reshape([basis, raw_trunk_outputs], [m,n,n+1])
  print *, "all(aug_trunk(:,:,1:n) " // merge("==", "/=", all(aug_trunk(:,:,1:n) == basis)) // " basis)" 
  print *, "all(aug_trunk(:,:,n+1) " // merge("==", "/=", all(aug_trunk(:,:,n+1) == raw_trunk_outputs)) // " trunk)"

end program
