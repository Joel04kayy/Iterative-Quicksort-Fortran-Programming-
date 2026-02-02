!=============================================================
! File: stackADT.f03
! Purpose: Stack ADT for iterative Quicksort partition requests.
!          Stores (left, right) integer index bounds in LIFO order.
! Standard: Fortran 2003
!
! Author: Joel Kayyalakam
! Student ID: 1225520
! Course: CIS*3190 
! Date:   Jan 30, 2026
!=============================================================
module stackADT
   implicit none
   private

   ! Public stack operations
   
   public :: initStack, clear, push, pop, isEmpty

   ! A single partition request [l, r]
   type :: stackItem
      integer :: l = 0
      integer :: r = 0
   end type stackItem

   type(stackItem), allocatable :: data(:)
   integer :: top = 0          ! Number of elements currently stored
   integer :: capacity = 0     ! Allocated size of data(:)

contains

   
   ! initStack
   ! Allocates internal storage and resets the stack
   ! initialCapacity should be a small positive integer
   
   subroutine initStack(initialCapacity)
      integer, intent(in) :: initialCapacity

      if (initialCapacity <= 0) then
         stop "initStack: initialCapacity must be > 0"
      end if

      if (allocated(data)) then
         deallocate(data)
      end if

      allocate(data(initialCapacity))
      capacity = initialCapacity
      top = 0
   end subroutine initStack

   
   ! clear
   ! Removes all elements (keeps allocated storage)
   
   subroutine clear()
      top = 0
   end subroutine clear

   
   ! isEmpty
   ! Returns .true. if stack has no elements
   
   logical function isEmpty()
      isEmpty = (top == 0)
   end function isEmpty

   ! push
   ! Adds a new (l, r) request to the top of the stack
   ! Grows storage automatically if needed

   subroutine push(l, r)
      integer, intent(in) :: l, r

      if (.not. allocated(data)) then
         stop "push: stack not initialized (call initStack first)"
      end if

      if (top >= capacity) then
         call grow()
      end if

      top = top + 1
      data(top)%l = l
      data(top)%r = r
   end subroutine push

   ! pop
   ! Removes the top request from the stack and returns (l, r)
   
   subroutine pop(l, r)
      integer, intent(out) :: l, r

      if (.not. allocated(data)) then
         stop "pop: stack not initialized (call initStack first)"
      end if

      if (top == 0) then
         stop "pop: attempted to pop from an empty stack"
      end if

      l = data(top)%l
      r = data(top)%r
      top = top - 1
   end subroutine pop

   ! grow (private)
   ! Doubles the internal capacity while preserving contents
   
   subroutine grow()
      type(stackItem), allocatable :: tmp(:)
      integer :: newCapacity

      newCapacity = max(1, 2 * capacity)
      allocate(tmp(newCapacity))

      if (top > 0) then
         tmp(1:top) = data(1:top)
      end if

      call move_alloc(tmp, data)
      capacity = newCapacity
   end subroutine grow

end module stackADT