! https://www.codeeval.com/open_challenges/92/

program penultimate_word
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character, parameter :: EOF = char(0)
  character, parameter :: DELIM = " "

  character(STRING_LENGTH) :: line, last, curr

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      last = ""
      curr = ""
      do
        last = curr
        call popstring(curr, line)
        if (line == "") exit
      end do
      if (last == "") last = curr
      print FMT_STR, trim(last)
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
end program penultimate_word
