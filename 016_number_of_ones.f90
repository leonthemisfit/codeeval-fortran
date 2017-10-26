! https://www.codeeval.com/open_challenges/16/

program number_of_ones
  implicit none

  character(20) :: output
  integer(8) :: n
  integer(1) :: io

  call openfile()
  do
    output = readline()
    select case (trim(output))
    case ("EOF")
      exit
    case ("")
      continue
    case default
      read(output, "(i8)", iostat=io) n
      if (io == 0) print "(i0)", count(n)
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
    character(20) :: line
    integer(1) :: io

    read(9, "(a)", iostat=io) line
    if (io > 0) then
      print *, "ERR: Read Failed"
    else if (io < 0) then
      line = "EOF"
    end if
  end function readline

  subroutine bit(n, remainder)
    integer(8), intent(inout) :: n
    integer(1), intent(out) :: remainder

    remainder = mod(n, 2)
    n = n / 2
  end subroutine bit

  function count(n) result(cnt)
    integer(8), intent(in) :: n
    integer(8) :: quotient
    integer(1) :: remainder
    integer(2) :: cnt
    character(64) :: res = ""
    character :: r

    quotient = n
    remainder = 0
    cnt = 0
    do
      if (quotient == 0) exit
      call bit(quotient, remainder)
      if (remainder == 1) cnt = cnt + 1
      write(r, "(i1)") remainder
      res = r // trim(res)
    end do
  end function count
end program number_of_ones
