! https://www.codeeval.com/browse/82/

program armstrong_numbers
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_INT = "(i0)"
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)
  integer(4), parameter :: ARR_LENGTH = 64

  character(STRING_LENGTH) :: line

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      if (sumstr(line) == line) then
        print FMT_STR, "True"
      else
        print FMT_STR, "False"
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

  function getarray(nstring) result (arr)
    character(STRING_LENGTH) :: nstring
    integer(INT_LENGTH) :: arr(ARR_LENGTH), i

    do i = 1, len_trim(nstring)
      read(nstring(i:i), FMT_RINT) arr(i)
    end do
  end function getarray

  function sumstr(nstring) result (res)
    character(STRING_LENGTH) :: nstring, res
    integer(INT_LENGTH) :: n, arr(ARR_LENGTH), l, i

    n = 0
    l = len_trim(nstring)
    arr = getarray(nstring)
    do i = 1, l
      n = n + (arr(i) ** l)
    end do

    write(res, FMT_INT) n
  end function sumstr
end program armstrong_numbers
