program BeTran

    use typeStack
    use befunger

    implicit none

    character(len=1)  :: char
    integer           :: u, length, io, height, i, clock, n
    character(len=80) :: line
    logical           :: exitflag, debug

    integer,            allocatable :: seed(:)
    character(len=256), allocatable :: arg(:)
    character(len=:),   allocatable :: filename

    !set seed
    call random_seed(size = n)
    allocate(seed(n))
    call system_clock(COUNT=clock)
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(PUT = seed)


    !get file name
    n = command_argument_count()
    allocate(arg(n))
    debug = .false.

    do i = 1, n
        call get_command_argument(i, arg(i))
        if(index(arg(i), '.bf') > 0)then
            filename = trim(adjustl(arg(i)))
        end if
        if(trim(arg(i)) == "debug")debug=.true.
    end do
    if(.not.allocated(filename))stop "need .bf file"

    open(newunit=u, file=filename, iostat=io)
    height = 0
    ! read in whole program
    do
        read(u,"(A)", iostat=io)line
        if(io /= 0)exit
        height = height + 1
        length = len(trim(line))
        prog(:, height) = [(line(i:i), i=1, 80)]
    end do
    close(u)

    !interpret .bf program
    do
        char = prog(xpos, ypos)             !   get char
        if(debug)print*,iachar(char),char,xpos,ypos,st%peek(),achar(st%peek())
        call interpret(char, exitflag)

        if(exitflag)exit
        if(xpos >= 81)xpos = 1              !   wrap around in x direction
        if(ypos >= 26)ypos = 1              !   wrap around in y direction

        if(.not. flag)then                  !   skip update position if bridge has been used
            xpos = xpos + dx    
            ypos = ypos + dy
        end if
        flag = .false.
    end do
print*," "
end program BeTran