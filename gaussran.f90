!===================================================
!Function to produce random numbers distributed
!according to the gaussian distribution
!g(x) = 1/(sigma*sqrt(2*pi))*exp(-x**2/(2*sigma**2))
!===================================================
real(8) function gaussran()
  use drandom_mod
  implicit none
  !=================================================
  !Decleration of variables:
  real(8),parameter :: sigma = 1.0D0  !Sigma of gauss distirbution
  real(8)           :: r,phi          !Radius and angle for polar coordinates we used
  logical,save      :: new   = .TRUE. !Useful boolean that is saved as long as our program runs, to distinguish if we have x{i-1} or x{i}
  real(8),save      :: x              !The value that is saved as x{i-1} for the next run
  real(8),parameter :: PI2   = 6.28318530717958648D0
  !=================================================
  !Calculations:
  if(new)then
     new      = .FALSE.      !Next run is going to be x{i}
     r        =     drandom()
     phi      = PI2*drandom()
     r        = sigma*sqrt(-2.0D0*log(r))
     x        = r*cos(phi)
     gaussran = r*sin(phi)
  else
     new      = .TRUE.
     gaussran = x
  endif
  !=================================================
end function gaussran
!===================================================

