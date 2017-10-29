! https://www.codeeval.com/open_challenges/54/

program cash_register
  implicit none

  integer(4), parameter :: STRING_LENGTH = 512
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: REAL_LENGTH = 8
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RREAL = "(f3.2)"
  character, parameter :: EOF = char(0)
  character, parameter :: DELIM = ";"

  character(11) :: NAMES(12) = (/ &
    "ONE HUNDRED", &
    "FIFTY      ", &
    "TWENTY     ", &
    "TEN        ", &
    "FIVE       ", &
    "TWO        ", &
    "ONE        ", &
    "HALF DOLLAR", &
    "QUARTER    ", &
    "DIME       ", &
    "NICKEL     ", &
    "PENNY      "  &
  /)

  real(REAL_LENGTH) :: VALUES(12) = (/ &
    100.00, &
     50.00, &
     20.00, &
     10.00, &
      5.00, &
      2.00, &
      1.00, &
      0.50, &
      0.25, &
      0.10, &
      0.05, &
      0.01  &
  /)

  character(STRING_LENGTH) :: line, str
  real(REAL_LENGTH) :: total, cash, change, count
  integer(INT_LENGTH) :: i, j

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      ! str = ""
      call popreal(total, line)
      call popreal(cash, line)
      if (total > cash) then
        print FMT_STR, "ERROR"
        cycle
      else if (total == cash) then
        print FMT_STR, "ZERO"
        cycle
      else
        print "(a)", trim(makechange(total, cash))
      end if
    end select
  end do
  close(FILE_UNIT)
contains
  subroutine append(str, count, name)
    character(STRING_LENGTH), intent(inout) :: str
    character(STRING_LENGTH), intent(in) :: name
    integer(INT_LENGTH), intent(in) :: count

    do i = 1, count, 1
      if (str /= "") str = trim(str) // ","
      str = trim(str) // name
    end do
  end subroutine append

  function makechange(total, cash) result(str)
    real(REAL_LENGTH), intent(in) :: total, cash
    character(STRING_LENGTH) :: str
    character(STRING_LENGTH) :: coin_name
    real(REAL_LENGTH) :: change, coin
    integer(INT_LENGTH) :: i, count

    str = ""
    change = round( round(cash) - round(total) )
    do i = 1, 12, 1
      coin = round(VALUES(i))
      coin_name = NAMES(i)
      if (change >= coin) then
        count = floor(change / coin)
        call append(str, count, coin_name)
        change = change - (count * coin)
        change = round(change)
      end if
    end do
  end function makechange

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

  subroutine popreal(first, rest)
    real(REAL_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), DELIM)
    if (index > 0) then
      read(rest(1:index - 1), *) first
      rest = rest(index + 1:)
    else
      read(rest, *) first
      rest = ""
    end if
  end subroutine popreal

  function round(n) result(r)
    real(REAL_LENGTH) :: n, r

    r = idnint(n * 100)
    r = r / 100
  end function round
end program cash_register
