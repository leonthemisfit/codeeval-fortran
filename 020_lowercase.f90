! https://www.codeeval.com/open_challenges/20/

program lowercase
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
      call tolower(line)
      print "(a)", trim(line)
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

  subroutine tolower(str)
    character(256), intent(inout) :: str
    integer(4) :: i, c

    do i = 1, len_trim(str), 1
      c = ichar(str(i:i))
      if (c >= 65 .and. c <= 90) then
        c = c + 32
      end if
      str(i:i) = char(c)
    end do
  end
end program lowercase
