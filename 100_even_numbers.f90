! https://www.codeeval.com/open_challenges/100/

program even_numbers
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_INT = "(i0)"
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)

  character(STRING_LENGTH) :: line
  integer(INT_LENGTH) :: n

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      read(line, FMT_RINT) n
      if (mod(n, 2) == 0) then
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
end program even_numbers
