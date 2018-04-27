module befunger

    use typeStack

    implicit none

    !state variables
    integer          :: xpos=1, ypos=1, dx=1, dy=0
    type(stack)      :: st
    logical          :: flag
    character(len=1) :: prog(80, 25)

    contains

        subroutine interpret(char, exitflag)

            use typestack

            implicit none

            character(len=1), intent(IN)  :: char
            logical,          intent(OUT) :: exitflag
            character(len=1) :: tmp
            integer          :: ascii, tmp1, tmp2, j
            real             :: ran
            
            ascii = iachar(char)                    !   convert to integer so can store on stack if needed
            exitflag = .false.

            select case (ascii)
                case (48)                           !                       
                    call st%push(ascii - 48)        !
                case (49)                           !
                    call st%push(ascii - 48)        !
                case (50)                           !
                    call st%push(ascii - 48)        !
                case (51)                           !
                    call st%push(ascii - 48)        !   add number 0-9 to stack
                case (52)                           !
                    call st%push(ascii - 48)        !
                case (53)                           !
                    call st%push(ascii - 48)        !
                case (54)                           !
                    call st%push(ascii - 48)        !
                case (55)                           !
                    call st%push(ascii - 48)        !
                case (56)                           !
                    call st%push(ascii - 48)        !
                case (57)                           !
                    call st%push(ascii - 48)        !

                case (43)!+                         !   pop 2 values and add sum of values to stack
                     tmp1 = st%pop()
                     tmp2 = st%pop()
                    call st%push(tmp1+tmp2)
                case (45)!-                         !   pop 2 values a,b and add (b - a) to stack
                    tmp1 = st%pop()
                    tmp2 = st%pop()
                    call st%push(tmp2 - tmp1)
                case (42)!*                         !   pop 2 values and add product of values to stack
                    tmp1 = st%pop()
                    tmp2 = st%pop()
                    call st%push(tmp1 * tmp2)
                case (47)!/                         !   pop 2 values and add floor(b/a) to stack
                    tmp1 = st%pop()
                    tmp2 = st%pop()
                    call st%push(int(tmp2 / tmp1))
                case(96)!`                          !   Greater than: Pop two values a and b, then push 1 if b>a, otherwise zero.
                    tmp1 = st%pop()
                    tmp2 = st%pop()
                    if(tmp2 > tmp1)then
                        call st%push(1)
                    else
                        call st%push(0)
                    end if
                case (33)!!                         !   pop a value. If the value is 0, push 1; otherwise, push 0.
                    if(st%pop() == 0)then
                        call st%push(1)
                    else
                        call st%push(0)
                    end if
                case (37)!%                         !   Modulo: Pop two values a and b, then push the remainder of the integer division of b/a.
                    tmp1 = st%pop()
                    tmp2 = st%pop()
                    call st%push(int(mod(tmp1, tmp2)))
                    xpos = xpos + dx
                    ypos = ypos + dy

                case (34)!"                         !   String mode. push all values as ascii to stack up to matching "
                    xpos = xpos + dx
                    ypos = ypos + dy
                    do
                        if(prog(xpos, ypos) == '"')exit
                        ascii = iachar(prog(xpos, ypos))
                        call st%push(ascii)
                        xpos = xpos + dx
                        ypos = ypos + dy
                    end do

                case (62)!>                         !   Move right
                    dy = 0
                    dx = 1
                case (60)!<                         !   Move left
                    dy = 0
                    dx = -1
                case (118)!v                        !   Move down
                    dy = 1
                    dx = 0
                case (94)!^                         !   Move up
                    dy = -1
                    dx = 0
                case (63)!?                         !   move in random direction
                    call random_number(ran)
                    j = 1 + floor((4) * ran)
                    if(j == 1)then
                        dx = 1
                        dy = 0
                    elseif(j == 2)then
                        dx = -1
                        dy = 0
                    elseif(j == 3)then
                        dx = 0
                        dy = -1
                    else
                        dx = 0
                        dy = 1
                    end if

                case (58)!:                         !   duplicate top of stack value and push it
                    call st%push(st%peek())
                case(92)!\                          !   Swap top two stack values
                    tmp1 = st%pop()
                    tmp2 = st%pop()
                    call st%push(tmp1)
                    call st%push(tmp2)
                case(36)!$                          !   pop value and discard
                    tmp1 = st%pop()

                case (35)!#                         !   skip next instruction (bridge over it)
                    xpos = xpos + dx * 2 
                    ypos = ypos + dy * 2
                    flag = .true.

                case (44)!,                         !   pop value and output as integer
                    write(*,"(A)", advance='no')achar(st%pop())
                case (46)!.                         !   pop value and output as character
                    write(*,"(I10.1)", advance='no')st%pop()

                case (95)!_                         !   horizontal if. pop a value; set direction to right if value=0, set to left otherwise
                    dy = 0
                    if(st%pop() == 0)then
                        dx = 1
                    else
                        dx = -1
                    end if
                case(124)!|                         !   vertical if. pop a value; set direction to right if value=0, set to left otherwise
                    dx = 0
                    if(st%pop() == 0)then
                        dy = 1
                    else
                        dy = -1
                    end if
                case (64)!@                         !   exit program
                    exitflag = .true.
                    return
                case (32)!                          !   space so continue
                    return
                case(103)!g                         !   get. Pop two values y and x, then push the ASCII value of the character at that position in the program. If (x,y) is out of bounds, push 0
                    tmp1 = st%pop() + 1
                    tmp2 = st%pop() + 1
                    if((tmp1 > 80 .or. tmp1 < 1) .or. (tmp2 > 25 .or. tmp2 < 1))then
                        call st%push(0)
                    else
                        call st%push(iachar(prog(tmp2, tmp1)))
                    end if
                case(112)!p                         !   put. Pop three values y, x and v, then change the character at the position (x,y) in the program to the character with ASCII value v
                    tmp1 = st%pop() + 1
                    tmp2 = st%pop() + 1
                    prog(tmp2, tmp1) = achar(st%pop())

                case(38)!&                          !   Get integer from user and push it
                    print*," "
                    write(*,"(A)",advance='no')"enter int:"
                    read(*,"(I3.1)")tmp1
                    print*," "
                    call st%push(tmp1)
                case(126)!~                         !   Get character from user and push it
                    print*," "
                    write(*,"(A)",advance='no')"enter char:"
                    read(*,"(A)")tmp
                    print*," "
                    call st%push(iachar(tmp))
                case default
                    print*,'error',char,xpos,ypos,st%peek(),achar(st%peek())
            end select
        end subroutine interpret
end module befunger