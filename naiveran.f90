! ----------------------------------------------------------
module naiveran_mod
 implicit none

 private
 public  :: naiveran

! ----------------------------------------------------------
contains
! ----------------------------------------------------------

!=============================================
!File: naiveran.f90
!Program to demonstrate the usage of a modulo
!generator with a bad choice of constants 
!resulting in strong pair correlations between
!generated numbers
!=============================================
 function naiveran()
  real(8)           :: naiveran
  integer,save      :: iran = 13337
  integer,parameter :: m    = 131072 ! equal to 2**17
  integer,parameter :: a    = 1277

  iran = a * iran
  iran = MOD(iran,m)

  naiveran = iran/DBLE(m) !Number in (0,1)
  
 end function naiveran

! ----------------------------------------------------------
end module naiveran_mod
! ----------------------------------------------------------

