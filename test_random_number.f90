program test_random_number
  implicit none  !We need to declare all variable types
  !==========================================
  !Declaration of variables:
  integer, parameter  :: NR =10
  real(8) :: r                       !Our random number
  integer :: i                       !for do loops
  integer :: NSEEDS                  !Size of seed array
  integer,allocatable :: seeds(:)
  integer             :: seed        !Seed that user gives, so we have the same random number
  real(8)             :: randoms(NR)
  !Calculations:
  !First use of random number intrinsic function:
  !do i = 1,10
   !  call RANDOM_NUMBER(r)
    ! print *, r
  ! end do
  !But we need to seeding because in older versions of gfortran we get the same random number every time:
  seed = 5453457
  call RANDOM_SEED(size = NSEEDS)
  print '(A,100I12)',"# NSEEDS = ", NSEEDS
  ALLOCATE(seeds(NSEEDS))
  seeds = seed +37 * [(i-1,i=1,NSEEDS)]
  print '(A,100I12)','# Seeds = ',seeds
  call RANDOM_SEED(put = seeds) !If we forget we take different random numbers every time
  !==========================================
  !If you want a random seeds every time:
  open(unit =13,file='/dev/urandom',access='stream',form='unformatted')
  read(13) seeds
  seeds = ABS(seeds)  !Good practise but not mandatory
  print '(A,100I12)','# Seeds = ',seeds
  close(13)
  call RANDOM_SEED(PUT = seeds)
  !==========================================
  do i = 1,10
     call RANDOM_NUMBER(r)
     print *, r
  end do
  call RANDOM_NUMBER(randoms)
  print '(A,1000G28.16)','#  random numbers = ',randoms
  !==========================================
  !Now we are going to see checkpoints:
  !Save state of random number:
  open(unit=11,file='ran.seed')
  call RANDOM_SEED(get = seeds)
  write(11,'(1000I20)') seeds
  close(11)
  !After the get we have the same state
  call RANDOM_NUMBER(randoms)
  print '(A,1000G28.16)','# Old random numbers = ',randoms
  !Now let's continue the random number:
  open(unit=11,file = 'ran.seed')
  read(11,*) seeds
  call RANDOM_SEED(put = seeds)
  close(11)
  call RANDOM_NUMBER(randoms)
  print '(A,1000G28.16)','# New random numbers = ',randoms
end program test_random_number
!============================================
