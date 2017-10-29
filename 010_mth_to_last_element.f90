! https://www.codeeval.com/open_challenges/10/

program mth_to_last_element
  implicit none

  character(512) :: line

  call openfile()
  do
    line = readline()
    select case (line)
    case (char(0))
      exit
    case ("")
      cycle
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

  subroutine popint(last, rest)
    integer(4), intent(out) :: last
    character(256), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), " ", .true.)
    if (index > 0) then
      read(rest(index + 1:), "(i4)") last
      rest = rest(1:index - 1)
    else
      read(rest, "(i4)") last
      rest = ""
    end if
  end subroutine popint

  subroutine popstr(last, rest)
    character(256), intent(out) :: last
    character(256), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), " ", .true.)
    if (index > 0) then
      last = rest(index + 1:)
      rest = rest(1:index - 1)
    else
      last = rest
      rest = ""
    end if
  end subroutine popstr

  subroutine parseline(line)
    character(512), intent(inout) :: line
    character :: chars(512)
    integer(4) :: length
    integer(4) :: n

    length = 1
    call popint(n, line)
    do
      if (line == "") exit
      call popstr(chars(length), line)
      length = length + 1
    end do

    if (n <= length) then
      print "(a)", chars(n)
    end if
  end subroutine parseline
end program mth_to_last_element
