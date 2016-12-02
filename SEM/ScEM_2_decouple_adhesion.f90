!Module to decouple adhesion magnitude from changes in local element density caused by differential interfacial tension
! CK Revell, November 2016

module scem_2_decouple_adhesion

  use scem_0_input
  use scem_1_types
  use omp_lib

  implicit none

contains

  subroutine scem_decouple_adhesion

    integer :: element_label,t1,t2,t3,i,j,k
    real*8  :: local_area
    real*8, dimension(3)  :: a,b,c


    !Need to decouple adhesion magnitude from changes in element density caused by differential interfacial tension

    !Refresh adhesion factors
    !$omp parallel workshare shared (ne,elements)
    FORALL(i=1:ne) elements(i)%adhesion_factor=1
    !$omp end parallel workshare

    do i=1, nc
!      !$omp parallel &
!      !$omp shared (cells,elements,i) &
!      !$omp private (element_label,j,local_area,a,b,c)
!      !$omp do
      do j=1, cells(i)%cortex_elements(0)
        element_label = cells(i)%cortex_elements(j)
        local_area = 0

        !$omp parallel &
        !$omp shared (cells,elements,element_label) &
        !$omp private (t1,t2,t3,a,b,c)
        !$omp do reduction (+:local_area)
        do k=1, cells(i)%triplet_count
          !t1,t2, and t3 are labels of elements in triplet k
          t1 = cells(i)%triplets(1,k)
          t2 = cells(i)%triplets(2,k)
          t3 = cells(i)%triplets(3,k)
          if (t1.EQ.element_label.OR.t2.EQ.element_label.OR.t3.EQ.element_label) then !If any of the three elements in this triplet is the same as element_label
            ! a and b are vectors representing two sides of the triangle formed by elements t1, t2, and t3
            a = elements(t1)%position - elements(t2)%position
            b = elements(t1)%position - elements(t3)%position
            c = cross_product(a,b)
            local_area = local_area+0.5*SQRT(DOT_PRODUCT(c,c)) !0.5*|axb|
          else
            CYCLE
          endif
        enddo
        !$omp end do
        !$omp end parallel

        !adhesion_factor for this element updated according to the local area around the element.
        elements(element_label)%adhesion_factor = local_area !Might be worth introducing a constant here so that the adhesion magnitude in scem_input can be maintained as a nice number
!        write(40,*) elements(element_label)%DIT_factor, elements(element_label)%adhesion_factor
      enddo
    enddo

  end subroutine scem_decouple_adhesion

!  function cross_product(vector1,vector2)
!    real*8, dimension(3), intent(in) :: vector1
!    real*8, dimension(3), intent(in) :: vector2
!    real*8, dimension(3) :: cross_product
!
!    cross_product(1) = vector1(2)*vector2(3)-vector1(3)*vector2(2)
!    cross_product(2) = vector1(3)*vector2(1)-vector1(1)*vector2(3)
!    cross_product(3) = vector1(1)*vector2(2)-vector1(2)*vector2(1)
!  end function cross_product

end module scem_2_decouple_adhesion
