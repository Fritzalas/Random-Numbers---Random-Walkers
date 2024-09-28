program test_ranlux
  implicit none    !We need to declare all types of variables
  !===========================
  !Declaration of variables:
  integer           :: i  !for do loops
  real(8)           :: r  !random number
  integer,parameter :: NSEEDS = 25
  integer           :: seeds(NSEEDS)
  integer           :: seed
  integer           :: ranlux_level
  integer,parameter :: NR=25
  real(8)           :: randoms(NR)
  !===========================
  !Calculations:
  seed = 5432778
  !==========================================
  !If you want a random seeds every time:
  open(unit =13,file='/dev/urandom',access='stream',form='unformatted')
  read(13) seed
  seed = ABS(seed)  !Good practise but not mandatory
  print '(A,100I12)','# Seed = ',seed
  close(13)
  !==========================================
  ranlux_level = 2
  call RLUXGO(ranlux_level,seed,0,0)
  do i =1,10
     !call RANLUX(r,1)
     print *, r
  end do
  call RANLUX(randoms,NR)
  print '(A,1000G28.16)','#  random numbers = ',randoms
  !CheckPoint:
  !Save State:
  open(unit =11,file='ranlux.seed')
  call RLUXUT(seeds)
  write(11,'(25I20)')seeds
  close(11)
  call RANLUX(randoms,NR)
  print '(A,1000G28.16)','#old  random numbers = ',randoms
  open(unit=11,file='ranlux.seed')
  read(11,*)seeds
  call RLUXIN(seeds)
  close(11)
  call RANLUX(randoms,NR)
  print '(A,1000G28.16)','#new  random numbers = ',randoms
end program test_ranlux
!=============================
