program fizz_buzz
  implicit none

  character(256) :: line
  integer(4) :: f, b, n

  call openfile()
  do
    line = readline()
    select case (line)
    case ("EOF")
      exit
    case ("")
      continue
    case default
      call popint(f, line)
      call popint(b, line)
      call popint(n, line)
      print "(a)", trim(fbloop(f, b, n))
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
      line = "EOF"
    end if
  end function readline

  subroutine popint(first, rest)
    integer(4), intent(out) :: first
    character(256), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), " ")
    if (index > 0) then
      read(rest(1:index - 1), "(i4)") first
      rest = rest(index + 1:)
    else
      read(rest, "(i4)") first
      rest = ""
    end if
  end subroutine popint

  function fb(f, b, n) result(str)
    integer(4) :: f, b, n
    character(32) :: str

    str = ""

    if (mod(n, f) == 0) str = trim(str) // "F"
    if (mod(n, b) == 0) str = trim(str) // "B"
    if (trim(str) == "") write(str, "(i0)") n
  end function fb

  function fbloop(f, b, n) result(str)
    integer(4) :: f, b, n, i
    character(512) :: str

    str = ""
    do i = 1, n, 1
      str = trim(str) // " " // fb(f, b, i)
    end do
    str = adjustl(str)
  end function fbloop
end program fizz_buzz
