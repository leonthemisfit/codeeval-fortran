! https://www.codeeval.com/open_challenges/24/

program sum_of_integers_from_file
  character(256) :: line
  integer(4) :: n, sum

  sum = 0
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
      sum = sum + n
    end select
  end do
  print "(i0)", sum
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
end program sum_of_integers_from_file
