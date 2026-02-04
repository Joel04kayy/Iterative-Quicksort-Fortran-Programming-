!=============================================================
! File: iqsort.f03
! Purpose: Iterative (non-recursive) Quicksort for integers.
!          Reads integers from a file, sorts them using an
!          iterative Quicksort with a stack ADT, and writes the
!          sorted output to a file.
! Standard: Fortran 2003
!
! Author: Joel Kayyalakam
! Student ID: 1225520
! Course: CIS*3190
! Date:   Jan 30, 2026
!=============================================================
program iqsort
   use intIO
   use stackADT
   implicit none

   integer, allocatable :: numbers(:)
   integer :: n
   integer :: initialCapacity
   real    :: t1, t2

   call readUnsorted(numbers, n)

   if (n <= 1) then
      write(*,'(a)') "Nothing to sort (0 or 1 integer)."
      call writeSorted(numbers, n)
      if (allocated(numbers)) deallocate(numbers)
      stop
   end if

   ! Initialize the stack with a small capacity
   ! The ADT will grow automatically if needed

   initialCapacity = max(16, int(log(real(n)) / log(2.0)) + 4)
   call initStack(initialCapacity)
   call clear()

   call cpu_time(t1)
   call iterativeQsort(numbers, n)
   call cpu_time(t2)

   write(*,'(a,f10.6,a)') "Iterative Quicksort time: ", (t2 - t1), " seconds"

   call writeSorted(numbers, n)

   if (allocated(numbers)) deallocate(numbers)

contains


   ! iterativeQsort
   ! Sorts the integer array a(1:n) using an iterative Quicksort
   ! Uses a stack of (left, right) partition requests
   ! Requires parameter passing (no global array access)

   subroutine iterativeQsort(a, n)
      integer, intent(inout) :: a(:)
      integer, intent(in)    :: n

      integer :: left, right
      integer :: i, j
      integer :: pivot, tmp
      integer :: mid

      if (n <= 1) then
         return
      end if

      call push(1, n)

      do while (.not. isEmpty())

         call pop(left, right)

         do while (left < right)

            i = left
            j = right
            mid = (left + right) / 2
            pivot = a(mid)

            do
               do while (i <= right .and. a(i) < pivot)
                  i = i + 1
               end do

               do while (j >= left .and. pivot < a(j))
                  j = j - 1
               end do

               if (i <= j) then
                  tmp = a(i)
                  a(i) = a(j)
                  a(j) = tmp
                  i = i + 1
                  j = j - 1
               end if

               if (i > j) exit
            end do

            ! Decide which partition to handle immediately and which to stack
            if ((j - left) < (right - i)) then

               if (i < right) then
                  call push(i, right)
               end if

               right = j

            else

               if (left < j) then
                  call push(left, j)
               end if

               left = i

            end if

         end do

      end do

   end subroutine iterativeQsort

end program iqsort