! https://www.codeeval.com/open_challenges/21/

program sum_of_digits
  character(20) :: output

  call openfile()
  do
    output = readline()
    select case (trim(output))
    case ("EOF")
      exit
    case ("")
      continue
    case default
      print "(i0)", sumstring(trim(output))
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

  function sumstring(str) result(sum)
    character(20) :: str
    integer(4) :: sum
    integer(1) :: n
    integer(1) :: io

    sum = 0
    do i = 1, len_trim(str), 1
      read(str(i:i), "(i1)", iostat=io) n
      if (io == 0) sum = sum + n
    end do
  end function sumstring
end program sum_of_digits
