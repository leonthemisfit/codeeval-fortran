! https://www.codeeval.com/open_challenges/32/

program trailing_string
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_INT = "(i0)"
  character(*), parameter :: FMT_STR = "(a)"
  ! character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)
  character, parameter :: DELIM = ","

  character(STRING_LENGTH) :: line, str, patt, sub
  integer(INT_LENGTH) :: slength, plength

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      call popstring(str, line)
      call popstring(patt, line)
      slength = len_trim(str)
      plength = len_trim(patt)
      sub = str(slength - plength + 1:)
      if (trim(sub) == trim(patt)) then
        print FMT_INT, 1
      else
        print FMT_INT, 0
      end if
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

  subroutine popstring(first, rest)
    character(STRING_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), DELIM)
    if (index > 0) then
      first = rest(1:index - 1)
      rest = rest(index + 1:)
    else
      first = rest
      rest = ""
    end if
  end subroutine popstring
end program trailing_string
