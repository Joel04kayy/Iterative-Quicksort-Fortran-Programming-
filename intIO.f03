!=============================================================
! File: intIO.f03
! Purpose: Integer file input/output utilities for Quicksort.
!          - Reads unsorted integers from a user-specified file
!          - Writes sorted integers to an output file
! Standard: Fortran 2003
!
! Author: Joel Kayyalakam
! Student ID: 1225520
! Course: CIS*3190
! Date:   Jan 30, 2026
!=============================================================
module intIO
   implicit none
   private

   ! Public I/O operations
   
   public :: readUnsorted, writeSorted

contains
   
   ! readUnsorted
   ! Prompts the user for an input filename, validates the file,
   ! and reads integers (one per line) into a dynamically
   ! allocated array.
   
   subroutine readUnsorted(numbers, n)
      integer, allocatable, intent(out) :: numbers(:)
      integer, intent(out)              :: n

      character(len=512) :: filename
      integer            :: unit, ios, value
      integer            :: count, i
      logical            :: exists

      n = 0
      if (allocated(numbers)) deallocate(numbers)

      do
         write(*,'(a)', advance='no') "Enter input filename: "
         read(*,'(a)') filename
         filename = adjustl(filename)

         if (len_trim(filename) == 0) then
            write(*,'(a)') "Filename cannot be empty."
            cycle
         end if

         inquire(file=trim(filename), exist=exists)
         if (.not. exists) then
            write(*,'(a)') "File not found. Please try again."
            cycle
         end if

         open(newunit=unit, file=trim(filename), status='old', &
              action='read', iostat=ios)

         if (ios /= 0) then
            write(*,'(a)') "Unable to open file for reading."
            cycle
         end if

         exit
      end do

      ! First pass: count integers
      count = 0
      do
         read(unit, *, iostat=ios) value
         if (ios < 0) then
            exit
         else if (ios > 0) then
            close(unit)
            stop "readUnsorted: invalid data in input file"
         else
            count = count + 1
         end if
      end do

      if (count == 0) then
         close(unit)
         allocate(numbers(0))
         n = 0
         write(*,'(a)') "Warning: input file contains no integers."
         return
      end if

      rewind(unit)
      allocate(numbers(count))

      ! Second pass: store integers
      i = 0
      do
         read(unit, *, iostat=ios) value
         if (ios < 0) then
            exit
         else if (ios > 0) then
            close(unit)
            stop "readUnsorted: read error during second pass"
         else
            i = i + 1
            numbers(i) = value
         end if
      end do

      close(unit)
      n = i

      write(*,'(a,i0,a)') "Successfully read ", n, " integers."
   end subroutine readUnsorted

   ! writeSorted
   ! Writes integers to an output file (one per line).
   ! Default output filename is 'sortedNUM.txt'.
   ! Prompts the user before overwriting an existing file.
   
   subroutine writeSorted(numbers, n)
      integer, intent(in) :: numbers(:)
      integer, intent(in) :: n

      character(len=512) :: outname
      character(len=16)  :: response
      integer            :: unit, ios, i
      logical            :: exists, proceed

      outname = "sortedNUM.txt"

      do
         inquire(file=trim(outname), exist=exists)

         if (.not. exists) then
            proceed = .true.
         else
            write(*,'(a)') "File '" // trim(outname) // "' already exists."
            write(*,'(a)', advance='no') "Overwrite? (Y/N): "
            read(*,'(a)') response
            response = adjustl(response)

            if (response(1:1) == 'Y' .or. response(1:1) == 'y') then
               proceed = .true.
            else if (response(1:1) == 'N' .or. response(1:1) == 'n') then
               proceed = .false.
            else
               write(*,'(a)') "Please enter Y or N."
               cycle
            end if
         end if

         if (proceed) then
            exit
         else
            write(*,'(a)', advance='no') "Enter a new output filename (or press Enter to cancel): "
            read(*,'(a)') outname
            outname = adjustl(outname)

            if (len_trim(outname) == 0) then
               write(*,'(a)') "Output cancelled."
               return
            end if
         end if
      end do

      open(newunit=unit, file=trim(outname), status='replace', &
           action='write', iostat=ios)

      if (ios /= 0) then
         stop "writeSorted: unable to open output file"
      end if

      do i = 1, n
         write(unit,'(i0)') numbers(i)
      end do

      close(unit)

      write(*,'(a)') "Sorted output written to '" // trim(outname) // "'."
   end subroutine writeSorted

end module intIO