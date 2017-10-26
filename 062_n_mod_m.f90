! https://www.codeeval.com/open_challenges/62/

program n_mod_m
  character(256) :: line
  integer(4) :: x, y

  call openfile()
  do
    line = readline()
    select case (line)
    case ("EOF")
      exit
    case ("")
      continue
    case default
      call popint(x, line)
      call popint(y, line)
      print "(i0)", mymod(x, y)
    end select
  end do
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
      line = "EOF"
    end if
  end function readline

  function mymod(x, y) result(res)
    integer(4) :: x, y, res
    res = x - ((x / y) * y)
  end function mymod

  subroutine popint(first, rest)
    integer(4), intent(out) :: first
    character(256), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), ",")
    if (index > 0) then
      read(rest(1:index - 1), "(i4)") first
      rest = rest(index + 1:)
    else
      read(rest, "(i4)") first
      rest = ""
    end if
  end subroutine popint
end program n_mod_m
