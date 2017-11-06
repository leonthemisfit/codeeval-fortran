! https://www.codeeval.com/open_challenges/74/

program minimum_coins
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_INT = "(i0)"
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)

  integer(INT_LENGTH), parameter :: COINS(3) = (/ 5, 3, 1 /)

  character(STRING_LENGTH) :: line
  integer(INT_LENGTH) :: total

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      read(line, FMT_RINT) total
      print FMT_INT, countcoins(total)
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

  function countcoins(val) result(count)
    integer(INT_LENGTH) :: val, total, count, i, c

    total = val
    count = 0
    do i = 1, 3
      c = COINS(i)
      if (total >= c) then
        count = count + (total / c)
        total = mod(total, c)
      end if
    end do
  end function countcoins
end program minimum_coins
