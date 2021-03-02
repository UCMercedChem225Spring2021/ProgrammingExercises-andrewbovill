      program matrix01
!
!     USAGE:
!       ./matrix01.exe <n>
!
!     ABOUT:
!     This program reads a size n from the command line, fills two (n x n)
!     matrices with random numbers, and evaluates the matrix product using
!     explicit loops. The program tracks the time taken for the
!     multiplication steps and reports is at the end.
!
!
!     AUTHOR:
!     H. P. Hratchian, 2020.
!
!
!     Variable Declarations
!
      implicit none
      integer::n,i,j,k
      integer,parameter::iOut=6
      real::tStart,tEnd
      real,dimension(:,:),allocatable::A,B,C
      character(len=256)::commandLineArg
      logical::fail=.false.
!
 1000 Format('n = ',I10,'  Job Time: ',F10.3,' s.')
 9000 Format('Failure reading command line arguments...incorrect number.')
 9999 Format('The program FAILED!')
!
!     Read the user-specified matrix dimension, n, from the command line.
!
      if(COMMAND_ARGUMENT_COUNT().ne.1) then
        write(iOut,9000)
        fail = .true.
        goto 999
      endIf
      call GET_COMMAND_ARGUMENT(1,commandLineArg)
      read(commandLineArg,*) n
!
!     Allocate matrices A, B, and C. Then, fill A and B with random numbers.
!
      Allocate(A(n,n),B(n,n),C(n,n))
      call random_number(A)
      call random_number(B)
      C = 0
!
!     Carry out matrix multiplication using explicit nested loops.
!
      call CPU_TIME(tStart)

      do i = 1,n
        do j = 1,n
          do k = 1,n
            C(i,j) = A(i,k)*B(k,j) 
          enddo
        enddo
      enddo


      call CPU_TIME(tEnd)
      write(iOut,1000) n,tEnd-tStart
!
!     Write out matrix C
!

      call Print_Matrix_Full_Real(C,n,n)
 999  if(fail) write(iOut,9999) 
      end program matrix01
    
      Subroutine Print_Matrix_Full_Real(AMat,M,N)
!
!     This subroutine prints a real matrix that is fully dimension - i.e.,
!     not stored in packed form. AMat is the matrix, which is dimensioned
!     (M,N).
!
!     The output of this routine is sent to unit number 6 (set by the local
!     parameter integer IOut).
!
!
!     Variable Declarations
!
      implicit none
      integer,intent(in)::M,N
      real,dimension(M,N),intent(in)::AMat
!
!     Local variables
      integer,parameter::IOut=6,NColumns=5
      integer::i,j,IFirst,ILast
!
 1000 Format(1x,A)
 2000 Format(5x,5(7x,I7))
 2010 Format(1x,I7,5F14.6)
!
      Do IFirst = 1,N,NColumns
        ILast = Min(IFirst+NColumns-1,N)
        write(IOut,2000) (i,i=IFirst,ILast)
        Do i = 1,M
          write(IOut,2010) i,(AMat(i,j),j=IFirst,ILast)
        endDo
      endDo
!
      Return
      End Subroutine Print_Matrix_Full_Real
