      program tst
      implicit none 
      integer :: num, i, r1, c1, m1, pl,in
      CHARACTER(len=32) :: rt, ct, mt
      
      num = IARGC()
      pl = (num/3) -1 
      in = 0 
      
      do i = 1,num,3
         CALL GETARG(i, rt)
         CALL GETARG(i+1, ct)
         CALL GETARG(i+2, mt)
         read(rt, *) r1
         read(ct, *) c1
         read(mt, *) m1

         CALL rnd(r1,c1,m1)
         if(pl>in)then
            write(*,*) "  "
            pl =pl-1
         endif
         
      end do  
      end program tst


      subroutine rnd(r2, c2, m2)
      implicit none
      integer :: r2, c2, m2, r_rand, c_rand
      integer, allocatable :: seed(:)
      integer :: size
      real :: u 

      !generating random numbers 
      call random_seed(size=size)
      allocate(seed(size))
      call random_seed(put=seed)
      CALL random_number(u)

      r_rand = 1 + FLOOR((r2-1)*u) 
      c_rand = 1 + FLOOR((c2-1)*u)
      call matrx(r_rand, c_rand,m2)
      
      return
      end subroutine rnd

      subroutine matrx(r,c,m)
      implicit none
      integer :: r,c,m,i,j
      real :: A(r,c), B(c,r), rslt(r,r) 

      call random_number(A)
      call random_number(B)

      A = (anint(m*A*1000))/1000.0
      B = (anint(m*B*1000))/1000.0
      rslt = matmul(A,B)

      do i=1,r
         write(*,*) ( rslt(i,j), j=1,r )
      end do
      
      return
      end subroutine matrx
