! https://www.codeeval.com/open_challenges/9/

program stack_implementation
  implicit none

  type stack
    integer(8) :: values(128)
    integer(4) :: length = 0
  end type stack

  character(256) :: line
  type(stack) :: s

  call openfile()
  do
    line = readline()
    select case (line)
    case (char(0))
      exit
    case ("")
      continue
    case default
      call parseline(line, s)
      call printstack(s)
    end select
  end do
  close(9)
contains
  subroutine push(s, v)
    type(stack), intent(inout) :: s
    integer(8), intent(in) :: v

    s%values(s%length + 1) = v
    s%length = s%length + 1
  end subroutine push

  subroutine pop(s, v)
    type(stack), intent(inout) :: s
    integer(8), intent(out) :: v

    if (s%length == 0) return
    v = s%values(s%length)
    s%length = s%length - 1
  end subroutine pop

  subroutine delete(s)
    type(stack), intent(inout) :: s
    integer(8) :: temp

    if (s%length == 0) return
    call pop(s, temp)
  end subroutine delete

  subroutine openfile()
    character(256) :: path
    call getarg(1, path)
    open(9, file=trim(path))
  end subroutine openfile

  function readline() result(line)
    character(256) :: line
    integer(1) :: io

    read(9, "(a)", iostat=io) line
    if (io > 0) then
      print *, "ERR: Read Failed"
    else if (io < 0) then
      line = char(0)
    end if
  end function readline

  subroutine popint(first, rest)
    integer(8), intent(out) :: first
    character(256), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), " ")
    if (index > 0) then
      read(rest(1:index - 1), "(i8)") first
      rest = rest(index + 1:)
    else
      read(rest, "(i8)") first
      rest = ""
    end if
  end subroutine popint

  subroutine parseline(line, s)
    character(256), intent(inout) :: line
    type(stack), intent(inout) :: s
    integer(8) :: temp

    do
      if (trim(line) == "") exit
      call popint(temp, line)
      call push(s, temp)
    end do
  end subroutine parseline

  subroutine printstack(s)
    type(stack), intent(inout) :: s
    integer(8) :: temp
    character(64) :: tempstring
    character(256) :: line

    line = ""
    do
      if (s%length == 0) exit
      call pop(s, temp)
      write(tempstring, "(i8)") temp
      line = trim(line) // " " // trim(adjustl(tempstring))
      call delete(s)
    end do
    print "(a)", trim(adjustl(line))
    s%length = 0
  end subroutine printstack
end program stack_implementation
