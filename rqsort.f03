!=============================================================
! File: rqsort.f03
! Purpose: Recursive Quicksort for integers.
!          Reads integers from a file, sorts them using a
!          recursive Quicksort, and writes the sorted output
!          to a file.
! Standard: Fortran 2003
!
! Author: Joel Kayyalakam
! Student ID: 1225520
! Course: CIS*3190
! Date:   Jan 30, 2026
!=============================================================
program rqsort
   use intIO
   implicit none

   integer, allocatable :: numbers(:)
   integer :: n
   real    :: t1, t2

   call readUnsorted(numbers, n)

   if (n <= 1) then
      write(*,'(a)') "Nothing to sort (0 or 1 integer)."
      call writeSorted(numbers, n)
      if (allocated(numbers)) deallocate(numbers)
      stop
   end if

   call cpu_time(t1)
   call recursiveQsort(numbers, 1, n)
   call cpu_time(t2)

   write(*,'(a,f10.6,a)') "Recursive Quicksort time: ", (t2 - t1), " seconds"

   call writeSorted(numbers, n)

   if (allocated(numbers)) deallocate(numbers)

contains


   ! recursiveQsort
   ! Sorts a(left:right) in place using recursive Quicksort
   ! Uses the same partition logic as the iterative version

   recursive subroutine recursiveQsort(a, left, right)
      integer, intent(inout) :: a(:)
      integer, intent(in)    :: left, right

      integer :: i, j
      integer :: pivot, tmp
      integer :: mid

      if (left >= right) then
         return
      end if

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

      if (left < j) then
         call recursiveQsort(a, left, j)
      end if

      if (i < right) then
         call recursiveQsort(a, i, right)
      end if

   end subroutine recursiveQsort

end program rqsort