! https://www.codeeval.com/open_challenges/27/

program decimal_to_binary
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: FILE_UNIT = 99
  integer(4), parameter :: INT_LENGTH = 8
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i8)"
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
      print FMT_STR, trim(tobin(n))
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

  subroutine bit(n, r)
    integer(INT_LENGTH), intent(inout) :: n
    integer(1), intent(out) :: r

    r = mod(n, 2)
    n = n / 2
  end subroutine bit

  function tobin(n) result(str)
    integer(INT_LENGTH) :: n
    character(STRING_LENGTH) :: str
    integer(1) :: r
    character(INT_LENGTH) :: c

    c = ""
    str = ""

    do
      if (n == 0) exit
      call bit(n, r)
      write(c, FMT_RINT) r
      str = trim(adjustl(c)) // trim(str)
    end do
    if (str == "") str = "0"
  end function tobin
end program decimal_to_binary
