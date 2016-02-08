!Routine for solving linear simultaneous equations from a lower/upper decomposition
!From Numerical Recipes in Fortran, transcribed in September 2015 by CK Revell

module ludksb_module

  implicit none

contains

  subroutine ludksb(a,n,np,indx,b)

    integer n, np, indx(n)
    real a(np,np), b(n)
      !Solve the set of n linear equations A.X=B. Here a is input, not as the matrix A but
      !rather as its LU decomposition, determined by the routine ludcmp. indx is input as the
      !permutation vector returned by ludcmp. b(1:n) is input as the right-hand side vector B
      !and returns eith the solution vector X. a, n, np and indx are not modified by this routine
      !and can be left in place for succssive calls with different right-hand sides b. THis routine
      !takes into account the possibility that b will begin with many zero elements, so it is efficient
      !for use in matrix inversion.
    integer i, ii, j, ll
    real sum
    ii=0      !When ii is set to a positive value, it will become the index of the first non-vanishing element of b. We now do the forward substitution, equation (2.3.6). The only new wrinkle is to unscramble the permutation as we go.

    do i=1, n
      ll=indx(i)
      sum=b(ll)
      b(ll)=b(i)
      if (ii.NE.0) then
        do j=ii, i-1
          sum=sum-a(i,j)*b(j)
        enddo
      else if (sum.NE.0.) then
        ii=i                      !A non-zero element was encountered, so from now on we will have to do the sums in the loop above
      endif
      b(i)=sum
    enddo

    do i=n, 1, -1     !Now we do the backsubstitution, equation (2.3.7)
      sum=b(i)
      do j=i+1, n
        sum=sum-a(i,j)*b(j)
      enddo
      b(i)=sum/a(i,i)   !Store a component of the solution vector X
    enddo

    return
  end subroutine ludksb
end module ludksb_module
