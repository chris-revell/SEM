!Matrix lower/upper decomposition routine from Numerical Recipes in Fortran.
!Transcribed September 2015 by CK Revell

module ludcmp_module

  implicit none

  contains

    subroutine ludcmp(a,n,np,indx,d)

      integer n, np, indx(n), NMAX
      real d, a(np,np), TINY
      parameter(NMAX=500,TINY=1.0e-20)  !Largest expected n, and a small number.
        !Given a matrix a(1:n,1:n), with physical dimension np by np, this routine replaces it by
        !the LU decomposition of a rowwise permutation of itself. a and n are input. a is output,
        !arranges as in equation (2.3.14) above; indx(1:n) is an output vector that records the
        !row permutation effected by the partial pivoting; d is output as ±1 depending on whether
        !the number of row interchanges was even or odd, respectively. This routine is used in
        !combination with lubksb to solve linear equations or invert a matrix.
      integer i, imax, j, k
      real aamax, dum, sum, vv(NMAX)

      d=1

      do i=1, n
        aamax=0
        do j=1, n
          if(abs(a(i,j)).GT.aamax) aamax=abs(a(i,j))
        enddo
!        if (aamax.EQ.0) pause 'singular matrix in ludcmp' !No nonzero largest element
        vv(i)=1./aamax    !Save the scaling
      enddo

      do j=1, n   !This is the loop over columns of Crout's method.
        do i=1, j-1
          sum=a(i,j)
          do k=1, i-1
            sum=sum-a(i,k)*a(k,j)
          enddo
          a(i,j)=sum
        enddo
        aamax=0.  !Initialize for the search for largest pivot element.
        do i=j, n   !This is the i=j of equation (2.3.12) and i=j+1...N of equation 2.3.13
          sum=a(i,j)
          do k=1, j-1
            sum=sum-a(i,k)*a(k,j)
          enddo
          a(i,j)=sum
          dum=vv(i)*abs(sum)  !Figure of merit for the pivot
          if (dum.GE.aamax) then  !Is it better than the best so far?
            imax=i
            aamax=dum
          endif
        enddo
        if (j.NE.imax) then   !Do we need to interchange rows?
          do k=1, n           !Yes, do so...
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
          enddo
          d=-d                !... and change the parity of d.
          vv(imax)=vv(j)      !Also interchange the scale factor.
        end if
        indx(j)=imax
        if(a(j,j).EQ.0.) a(j,j)=TINY
        !If the poviot element is zero the matric is singular (at least to the precision of the algorithm).
        !For some applications on singular matrices, it is desirable to substitute TINY for zero.
        if(j.NE.n) then   !Now, finally, divide by the pivot element.
          dum=1./a(j,j)
          do  i=j+1,n
            a(i,j)=a(i,j)*dum
          enddo
        endif
      enddo !Go back for the next column in the reduction

      return
    end subroutine ludcmp
end module ludcmp_module
