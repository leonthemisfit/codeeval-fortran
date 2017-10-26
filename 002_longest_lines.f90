! https://www.codeeval.com/open_challenges/2/

program longest_lines
  implicit none
  integer(4) :: count, length, i
  character(256) :: lines(256)
  character(256) :: line, longest

  call openfile()
  line = readline()
  read(line, "(i4)") count
  call readall(lines, length)
  do i = 1, count, 1
    call findlongest(lines, length, longest)
    print "(a)", trim(longest)
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
      line = char(0)
    end if
  end function readline

  subroutine readall(array, length)
    character(256), intent(inout) :: array(256)
    integer(4), intent(out) :: length
    character(256) :: line

    length = 0
    do
      line = readline()
      select case (line)
      case (char(0))
        exit
      case ("")
        continue
      case default
        length = length + 1
        array(length) = line
      end select
    end do
    close(9)
  end subroutine readall

  subroutine findlongest(array, length, val)
    character(256), intent(inout) :: array(256)
    integer(4), intent(in) :: length
    character(256), intent(out) :: val
    integer(4) :: index, max, i

    index = 0
    max = 0
    do i = 1, length, 1
      if (len_trim(array(i)) > max) then
        index = i
        max = len_trim(array(i))
      end if
    end do
    if (index > 0) then
      val = array(index)
      array(index) = ""
    else
      val = ""
    end if
  end subroutine findlongest
end program longest_lines
