! https://www.codeeval.com/open_challenges/97/

program find_a_writer
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)
  character, parameter :: DELIM = " "

  character(STRING_LENGTH) :: line, chars, res
  integer(INT_LENGTH) :: n, i

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      res = ""
      i = 0
      call split(chars, line, "|")
      line = adjustl(line)
      do while (line /= "")
        i = i + 1
        call popint(n, line)
        res(i:i) = chars(n:n)
      end do
      print FMT_STR, trim(res)
    end select
  end do
  close(FILE_UNIT)
contains
  subroutine openfile()
    character(PATH_LENGTH) :: path

    call getarg(1, path)
    open(FILE_UNIT, file=trim(path))
  end subroutine openfile

  function readline() result(line)
    character(STRING_LENGTH) :: line
    integer(1) :: io

    read(FILE_UNIT, FMT_STR, iostat=io) line
    if (io > 0) then
      print FMT_STR, "ERR: Read Failed"
    else if (io < 0) then
      line = EOF
    end if
  end function readline

  subroutine popint(first, rest)
    integer(INT_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), DELIM)
    if (index > 0) then
      read(rest(1:index - 1), FMT_RINT) first
      rest = rest(index + 1:)
    else
      read(rest, FMT_RINT) first
      rest = ""
    end if
  end subroutine popint

  subroutine split(first, rest, delim)
    character(STRING_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    character, intent(in) :: delim
    integer(INT_LENGTH) :: index

    index = scan(trim(rest), delim)
    if (index > 0) then
      first = rest(1:index - 1)
      rest = rest(index + 1:)
    else
      first = rest
      rest = ""
    end if
  end subroutine split
end program find_a_writer
