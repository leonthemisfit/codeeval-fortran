! https://www.codeeval.com/open_challenges/18/

program multiples_of_a_number
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
      call parseline(line)
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

  subroutine popint(first, rest)
    integer(4), intent(out) :: first
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

  subroutine parseline(line)
    character(256), intent(inout) :: line
    integer(4) :: n, x, d

    call popint(n, line)
    call popint(x, line)
    if (n > x) then
      d = (n / x)
      if (x * d == n) then
        x = n
      else
        x = x * (d + 1)
      end if
    end if
    print "(i0)", x
  end subroutine parseline
end program multiples_of_a_number
