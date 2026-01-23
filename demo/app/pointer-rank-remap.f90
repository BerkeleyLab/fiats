program main
  implicit none
  integer aug_trunk(2,3,4), i, j, k
  integer, target    :: trunk(2,3) = reshape([(i, i=1,6)], [2,3])
  integer, parameter :: shape_aug_v(*) = [2,3,3]
  integer, parameter :: aug_v(*,*,*) = reshape([(size(trunk)+i, i=1,product(shape_aug_v))], shape_aug_v)
  integer, pointer   :: trunk_3d(:,:,:)

  print *, "trunk = "
  do i = 1, size(trunk,1)
    print *, i, ":", trunk(i,:)
  end do

  trunk_3d(1:size(trunk,1), 1:size(trunk,2), 1:1) => trunk

  aug_trunk = reshape([aug_v, trunk_3d], shape(aug_trunk))

  do i = 1, size(aug_trunk,1)
    do j = 1, size(aug_trunk,2)
      print *, i, j, ":", aug_trunk(i,j,:)
    end do
  end do

end program
