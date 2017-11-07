! https://www.codeeval.com/open_challenges/106/

program roman_numerals
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)

  integer(4), parameter :: ARR_LENGTH = 13

  integer(INT_LENGTH), parameter :: CARDINAL(ARR_LENGTH) = (/ &
  1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1/)

  character(2), parameter :: ROMAN(ARR_LENGTH) = (/ &
  "M ", "CM", "D ", "CD", "C ", "XC", "L ", "XL", "X ", "IX", "V ", "IV", "I "/)

  character(STRING_LENGTH) :: line, numerals
  integer(INT_LENGTH) :: n, i, cnt, j

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      numerals = ""
      read(line, FMT_RINT) n
      do i = 1, ARR_LENGTH
        cnt = n / CARDINAL(i)
        n = mod(n, CARDINAL(i))
        do j = 1, cnt
          numerals = trim(numerals) // trim(ROMAN(i))
        end do
      end do
      print FMT_STR, trim(numerals)
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
end program roman_numerals
