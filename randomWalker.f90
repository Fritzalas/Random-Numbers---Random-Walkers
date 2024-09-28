program randomWalker
  use drandom_mod   !We use our ready module for drandom
  implicit none     !We need to declare all the variables.
  !===================================
  !Declaration of variables:
  integer    :: Nwalk, Nstep    !We have Nwalk total random walks and with Nstep each random walker
  integer    :: ir              !Takes values 0,1,2,3 to determine the way the random walker goes
  integer(8) :: iwalk,istep     !iwalk is for the random walkers do loop.Same with istep
  integer(8) :: n               !Counts how many times we passed from the same point
  integer(8) :: x,y             !The coordinates of random walker that are LONG integers
  real(8)    :: xr,yr           !The real coordinates of the random walker
  !===================================
  !Calculations:
  call init(Nwalk,Nstep)   !The init function to start the random walker
  do iwalk = 1,Nwalk
     x=0;y=0 !Our initial starting point
     n=0     !Counts how many times the random walker is visited the same point
     do istep = 1,Nstep
        ir = INT(drandom()*4)   !The INT() gives the integer part of a number
        !Switch case:
        select case(ir)
        case(0)
           x = x + 1
        case(1)
           x = x - 1
        case(2)
           y = y + 1
        case(3)
           y = y - 1
        case default
           stop 'Error happened to our program'
        end select
        write(21,'(3I18)') iwalk,x,y
        if(x==0 .and. y==0) Then
           n = n + 1 
        end if
     end do
     xr = DBLE(x);yr = DBLE(y)
     write(20,'(I12,G28.16,3I18)') iwalk, xr*xr + yr*yr , x , y, n  !We took xr*xr and yr*yr and not x*x and y*y to avoid integer overflow
  end do
  close(20)
  close(21)
end program randomWalker
!=====================================
subroutine init(Nwalk,Nstep)
  use drandom_mod
  implicit none
  !===================================
  !Declaration of variables:
  integer       :: Nwalk,Nstep
  character(20) :: arg
  !===================================
  !User Interface:
  if(iargc()/=2)then
     print *, 'Usage: rw <Nwalk> <Nstep>'
     stop
  end if
  call getarg(1,arg); read(arg,*) Nwalk
  call getarg(2,arg); read(arg,*) Nstep
  !Seed for drandom from /dev/urandom
  open(unit=13,file='/dev/urandom',access='stream',form='unformatted')
  read(13) seed
  close(13)
  open(unit=20,file='dataR.dat')
  open(unit=21,file='data.dat')
end subroutine init
