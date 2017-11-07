! https://www.codeeval.com/open_challenges/112/

program swap_elements
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)
  integer(4), parameter :: ARR_LENGTH = 64

  interface split
    procedure splitint, splitstr
  end interface split

  character(STRING_LENGTH) :: line, vals, swapstr, arr(ARR_LENGTH)
  integer(INT_LENGTH) :: x, y, n

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      call split(vals, line, ":")
      call getarray(vals, arr, n)
      do while (line /= "")
        call split(swapstr, line, ",")
        call split(x, swapstr, "-")
        call split(y, swapstr, "-")
        call swap(arr, x + 1, y + 1)
      end do
      print FMT_STR, trim(join(arr, n))
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

  subroutine splitint(first, rest, delim)
    integer(INT_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    character, intent(in) :: delim
    integer(2) :: index

    index = scan(trim(rest), delim)
    if (index > 0) then
      read(rest(1:index - 1), FMT_RINT) first
      rest = rest(index + 1:)
    else
      read(rest, FMT_RINT) first
      rest = ""
    end if
  end subroutine splitint

  subroutine splitstr(first, rest, delim)
    character(STRING_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    character, intent(in) :: delim
    integer(2) :: index

    index = scan(trim(rest), DELIM)
    if (index > 0) then
      first = rest(1:index - 1)
      rest = rest(index + 1:)
    else
      first = rest
      rest = ""
    end if
  end subroutine splitstr

  subroutine getarray(line, arr, n)
    character(STRING_LENGTH), intent(inout) :: line
    character(STRING_LENGTH), intent(out) :: arr(ARR_LENGTH)
    integer(INT_LENGTH), intent(out) :: n

    n = 0
    do while (line /= "")
      n = n + 1
      call split(arr(n), line, " ")
    end do
  end subroutine getarray

  subroutine swap(arr, x, y)
    integer(4), intent(in) :: x, y
    character(STRING_LENGTH), intent(inout) :: arr(ARR_LENGTH)
    character(STRING_LENGTH) :: temp

    temp = arr(x)
    arr(x) = arr(y)
    arr(y) = temp
  end subroutine swap

  function join(arr, n) result(str)
    integer(INT_LENGTH) :: n, i
    character(STRING_LENGTH) :: str, arr(ARR_LENGTH), v

    str = ""
    do i = 1, n
      v = arr(i)
      str = trim(str) // " " // trim(v)
    end do
    str = adjustl(str)
  end function join
end program swap_elements
