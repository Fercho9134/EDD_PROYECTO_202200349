module priority_queue_m
    implicit none

    type :: node
        integer :: vertex
        real :: distance
    end type node

    type :: priority_queue
        type(node), allocatable :: heap(:)
    contains
        procedure :: push
        procedure :: pop
        procedure :: top
        procedure :: size_
    end type priority_queue

contains
    subroutine push(self, n)
        class(priority_queue), intent(inout) :: self
        type(node), intent(in) :: n
        integer :: i, parent

        if (.not. allocated(self%heap)) then
            allocate(self%heap(1))
            self%heap(1) = n
        else
            i = size(self%heap) + 1
            call extend(self%heap, i)
            self%heap(i) = n

            do while (i > 1)
                parent = i / 2
                if (self%heap(parent)%distance <= self%heap(i)%distance) exit
                call swap(self%heap(parent), self%heap(i))
                i = parent
            end do
        end if
    end subroutine push

    subroutine pop(self)
        class(priority_queue), intent(inout) :: self
        integer :: i, child

        if (size(self%heap) == 1) then
            deallocate(self%heap)
        else
            self%heap(1) = self%heap(size(self%heap))
            call shrink(self%heap, size(self%heap) - 1)

            i = 1
            do while (2 * i <= size(self%heap))
                child = 2 * i
                if (child < size(self%heap) .and. self%heap(child)%distance > self%heap(child + 1)%distance) child = child + 1
                if (self%heap(i)%distance <= self%heap(child)%distance) exit
                call swap(self%heap(i), self%heap(child))
                i = child
            end do
        end if
    end subroutine pop

    function top(self) result(n)
        class(priority_queue), intent(in) :: self
        type(node) :: n

        if (allocated(self%heap)) then
            n = self%heap(1)
        end if
    end function top

    function size_(self) result(s)
        class(priority_queue), intent(in) :: self
        integer :: s

        if (allocated(self%heap)) then
            s = size(self%heap)
        else
            s = 0
        end if
    end function size_

    subroutine swap(a, b)
        type(node), intent(inout) :: a, b
        type(node) :: temp

        temp = a
        a = b
        b = temp
    end subroutine swap

    subroutine extend(arr, n)
        type(node), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: n
        type(node), allocatable :: temp(:)

        if (n > size(arr)) then
            allocate(temp(n))
            temp(1:size(arr)) = arr
            call move_alloc(temp, arr)
        end if
    end subroutine extend

    subroutine shrink(arr, n)
        type(node), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: n
        type(node), allocatable :: temp(:)

        if (n < size(arr)) then
            allocate(temp(n))
            temp = arr(1:n)
            call move_alloc(temp, arr)
        end if
    end subroutine shrink
end module priority_queue_m
