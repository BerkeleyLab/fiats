program main
  implicit none

  integer i
  integer, parameter :: m = 10, n = 20
  integer, target    :: trunk(m,n) = reshape([(i, i=1,m*n)], [m,n])
  integer, parameter :: aug_v(*,*,*) = reshape([(size(trunk)+i, i=1,product([m,n,n]))], [m,n,n])
  integer, pointer   :: trunk_3d(:,:,:)
  integer, allocatable :: aug_trunk(:,:,:)

  trunk_3d(1:size(trunk,1), 1:size(trunk,2), 1:1) => trunk
  aug_trunk = reshape([aug_v, trunk_3d], [m,n,n+1])
  print *, "all(aug_trunk(:,:,1:n) " // merge("==", "/=", all(aug_trunk(:,:,1:n) == aug_v)) // " aug_v)" 
  print *, "all(aug_trunk(:,:,n+1) " // merge("==", "/=", all(aug_trunk(:,:,n+1) == trunk)) // " trunk)"

end program
