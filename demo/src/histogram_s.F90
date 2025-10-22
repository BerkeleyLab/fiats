! Copyright (c), The Regents of the University of California
! Terms of use are as specified in LICENSE.txt

#include "julienne-assert-macros.h"

submodule(histogram_m) histogram_s
  use julienne_m, only : &
     call_julienne_assert_ &
    ,operator(.all.) &
    ,operator(.cat.) &
    ,operator(.equalsExpected.) &
    ,operator(.isAtLeast.) &
    ,string_t
  implicit none

contains

  module procedure variable_name
    name = self%variable_name_
  end procedure

  module procedure bin_frequency
    frequency = real(self%bin_count_(bin))/real(self%cardinality_)
  end procedure

  module procedure to_separate_file
     file = to_common_file([histogram])
  end procedure

  module procedure to_common_file
    type(string_t), allocatable :: comments(:), columns(:)
    type(string_t) column_headings

    associate(num_histograms => size(histograms))

      allocate(comments(num_histograms))

      block
        integer line, b
        do line  = 1, size(comments)
          associate( &
            mode_frequency => string_t(int(100*maxval( &
              [( histograms(line)%bin_frequency(b), b = 1, size(histograms(line)%bin_count_) )] &
            ))), &
            range_min => string_t(histograms(line)%unmapped_min_), &
            range_max =>  string_t(histograms(line)%unmapped_max_) &
           )
             comments(line) = "# " &
               // trim(histograms(line)%variable_name_) &
               // " range: [" // trim(range_min%string()) // ", " //  trim(range_max%string()) // "]" &
               // ", mode frequency: " // trim(mode_frequency%string()) // "%"
          end associate
        end do
      end block

      associate(num_bins => size(histograms(1)%bin_value_))
        block 
          integer h, b ! histogram number, bin number

          column_headings = "    bin    " // .cat. [("    " // string_t(histograms(h)%variable_name_) // "    ", h=1,num_histograms)]

          call_julienne_assert(num_bins .isAtLeast. 1)
          call_julienne_assert(.all. (num_bins .equalsExpected. [(size(histograms(h)%bin_value_) , h=1,size(histograms))]))
            
          allocate(columns(num_bins))
          do b = 1, num_bins
            columns(b) =  string_t(histograms(1)%bin_value_(b)) // &
              .cat. [("  " // string_t(histograms(h)%bin_frequency(b)), h=1,num_histograms)]
          end do
        end block
      end associate

      file = file_t([comments, column_headings, columns])

    end associate
 
  end procedure

  pure function normalize(x, x_min, x_max) result(x_normalized)
    real, intent(in) :: x(:,:,:,:), x_min, x_max
    real, allocatable :: x_normalized(:,:,:,:)
    call_julienne_assert(x_min/=x_max)
    x_normalized = (x - x_min)/(x_max - x_min)
  end function

  module procedure construct
    histogram = construct_in_range(variable_name, v, minval(v), maxval(v), num_bins, disaggregated)
  end procedure

  module procedure construct_in_range

    integer i, j, k, n
    integer, parameter :: performance_threshold = 80
    real, parameter :: capture_maxval = 1.0001 ! ensure maxval(v_max) falls within the highest bin
    real, allocatable :: v_mapped(:,:,:,:)

    histogram%variable_name_ = variable_name
    histogram%unmapped_min_  = v_min
    histogram%unmapped_max_  = v_max

    allocate(histogram%bin_value_(num_bins))
    allocate(histogram%bin_count_(num_bins))

    histogram%cardinality_ = size(v)
    associate(v_min => (histogram%unmapped_min_), v_max => (histogram%unmapped_max_))
      if (disaggregated)  then
        v_mapped = v
      else
        v_mapped = normalize(v, v_min, v_max)
      end if
      associate(v_mapped_min => merge(v_min, 0., disaggregated), v_mapped_max => capture_maxval*merge(v_max, 1., disaggregated))
        associate(dv => (v_mapped_max - v_mapped_min)/real(num_bins))
          associate(v_bin_min => [(v_mapped_min + (i-1)*dv, i=1,num_bins)])
            associate(v_bin_max => [v_bin_min(2:), v_mapped_max])
              histogram%bin_value_ = 0.5*[v_bin_min + v_bin_max] ! switching to average yields problems likely related to roundoff
              if (num_bins < performance_threshold) then
                do concurrent(i = 1:num_bins)
                  histogram%bin_count_(i) = count(v_mapped >= v_bin_min(i) .and. v_mapped < v_bin_max(i))
                end do
              else
                histogram%bin_count_ = 0
                do i = 1,size(v_mapped,1)
                  do j = 1,size(v_mapped,2)
                    do k = 1,size(v_mapped,3)
                      do n = 1,size(v_mapped,4)
                        associate(bin => floor((v_mapped(i,j,k,n) - v_mapped_min)/dv) + 1)
                          histogram%bin_count_(bin) = histogram%bin_count_(bin) + 1
                        end associate
                      end do
                    end do
                  end do
                end do
              end if
            end associate
          end associate
        end associate
      end associate
#ifdef ASSERTIONS
      associate(binned => sum(histogram%bin_count_))
        call_julienne_assert(histogram%cardinality_ .equalsExpected. binned)
      end associate
#endif
    end associate

  end procedure

end submodule histogram_s
