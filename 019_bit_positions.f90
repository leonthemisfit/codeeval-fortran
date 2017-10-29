! https://www.codeeval.com/open_challenges/19/

program bit_positions
  implicit none

  character(256) :: line

  call openfile()
  do
    line = readline()
    select case (line)
    case (char(0))
      exit
    case ("")
      continue
    case default
      call handleline(line)
    end select
  end do
  close(9)
contains
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

  function shift(n, p) result(x)
    integer(8) :: n, p, x
    x = and(rshift(n, p - 1), 1)
  end function shift

  subroutine popint(first, rest)
    integer(8), intent(out) :: first
    character(256), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), ",")
    if (index > 0) then
      read(rest(1:index - 1), "(i8)") first
      rest = rest(index + 1:)
    else
      read(rest, "(i8)") first
      rest = ""
    end if
  end subroutine popint

  subroutine handleline(line)
    character(256), intent(inout) :: line
    integer(8) :: n, x, y

    call popint(n, line)
    call popint(x, line)
    call popint(y, line)
    if (shift(n, x) == shift(n, y)) then
      print "(a)", "true"
    else
      print "(a)", "false"
    end if
  end
end program bit_positions
