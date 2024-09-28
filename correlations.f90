program correlations
  use naiveran_mod  !We use our ready module for naiveran
  use drandom_mod   !We use our ready module for drandom
  implicit none     !We need to declare every type of variable
  !--------------------------------------------
  !Declaration of variables:
  integer, parameter   :: L = 10000 !We want random numbers from 0 to 9999
  integer              :: i,j       !For do loops
  integer              :: N         !Number of produced random numbers
  !We don't need to declare naiveran,drandom functions here as real(8) ::naiveran because there are declared at our module
  real(8)              :: gaussran  !We have the function gaussran
  !============================================
  !Calculations:
  seed = 348321   !Initial value x{0}
  N = 100000
  do i = 1,N
     !print *, INT(L * naiveran()), INT(L * naiveran()) !Print in a line two different numbers in (0,10000)
     !print *, INT(L * drandom()), INT(L * drandom())
     print *, gaussran()
  end do
end program correlations
!==============================================
