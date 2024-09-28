! ----------------------------------------------------------
module drandom_mod
  use, intrinsic        :: iso_fortran_env
  implicit none
  integer, parameter    :: dp = real64
  integer               :: seed
  ! ----------------------------------------------------------
  private
  public                :: drandom,  seed

  ! ----------------------------------------------------------
contains
  ! ----------------------------------------------------------


!====================================================
!File: drandom.f90
!Implementation of the Schrage algorithm for a 
!portable modulo generator for 32 bit signed integers
!(from numerical recipes)
!
!returns uniformly distributed pseudorandom numbers
! 0.0 < x < 1.0 (0 and 1 excluded)
!period: 2**31-2 = 2 147 483 646 
!whole period~26-34sec CPU time (Intel Core DUO 2GHz)
!====================================================
 function drandom()
  real(dp)           :: drandom
  integer,parameter  :: a = 16807      ! a = 7**5
  integer,parameter  :: m = 2147483647 ! m = a*q+r   = 2**31-1
  integer,parameter  :: q = 127773     ! q = [m/a]
  integer,parameter  :: r = 2836       ! r = MOD(m,a)
  real(dp),parameter :: f = (1.0_dp/m)
  integer            :: p
  real(dp)           :: dr


101 continue
  p       = seed/q              !  = [seed/q]
  seed    = a*(seed- q*p) - r*p !  = a*MOD(seed,q)-r*[seed/q] = MOD(a*seed,m)
  if(seed .lt. 0) seed = seed + m
  dr      = f*seed
  !Not necessary with gfortran and ifort on linux but prudent.
  !It increases CPU time ~ 10% over the whole period (ifort, 23% gfortran)
  if( dr <= 0.0_dp .or. dr >= 1.0_dp) goto 101
  drandom = dr
 end function drandom
!===================================================
end module drandom_mod
