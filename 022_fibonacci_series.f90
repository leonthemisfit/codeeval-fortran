! https://www.codeeval.com/open_challenges/22/

program fibonacci_series
  character(256) :: line
  integer(4) :: n

  call openfile()
  do
      line = readline()
      select case (line)
      case (char(0))
        exit
      case ("")
        continue
      case default
        read(line, "(i4)") n
        print "(i0)", fib(n)
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

  function fib(n) result(x)
    integer(4) :: n, x, y, z, i

    x = 0
    y = 1
    do i = 1, n, 1
      z = y
      y = x + y
      x = z
    end do
  end function fib
end program fibonacci_series
