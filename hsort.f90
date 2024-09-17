program heap_ordered_tree
  implicit none

  integer, parameter :: max_size = 10
  integer :: heap(max_size)
  integer :: n, i, new_element

  ! Initialize heap size
  n = 0

  ! Insert elements into the heap
  print *, "Enter elements (-1 to stop):"

  do
    read *, new_element
    if (new_element == -1) exit
    call insert(heap, n, new_element)
  end do

  ! Print the heap
  print *, "Heap-ordered tree:"
  do i = 1, n
    print *, heap(i)
  end do

contains

  ! Subroutine to insert a new element into the heap
  subroutine insert(heap, n, element)
    integer, intent(inout) :: heap(:)
    integer, intent(inout) :: n
    integer, intent(in) :: element
    integer :: i, parent

    ! Increment the heap size
    n = n + 1
    i = n

    ! Percolate up to maintain heap order
    do while (i > 1)
      parent = i / 2
      if (heap(parent) >= element) exit
      heap(i) = heap(parent)
      i = parent
    end do

    heap(i) = element
  end subroutine insert

end program heap_ordered_tree

