! https://www.codeeval.com/open_challenges/91/

program simple_sorting
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  integer(4), parameter :: REAL_LENGTH = 4
  character(*), parameter :: FMT_REAL = "(i0.1,f0.3)"
  character(*), parameter :: FMT_RREAL = "(f10.3)"
  character, parameter :: EOF = char(0)
  character, parameter :: DELIM = " "
  integer(4), parameter :: ARR_LENGTH = 64

  character(STRING_LENGTH) :: line
  integer(INT_LENGTH) :: n
  real(REAL_LENGTH) :: arr(ARR_LENGTH)

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      call getarray(line, arr, n)
      call sort(arr, n)
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

  subroutine popreal(first, rest)
    real(REAL_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), DELIM)
    if (index > 0) then
      read(rest(1:index - 1), FMT_RREAL) first
      rest = rest(index + 1:)
    else
      read(rest, FMT_RREAL) first
      rest = ""
    end if
  end subroutine popreal

  subroutine getarray(line, arr, n)
    character(STRING_LENGTH), intent(inout) :: line
    real(REAL_LENGTH), intent(out) :: arr(ARR_LENGTH)
    integer(INT_LENGTH), intent(out) :: n

    n = 0
    do while (line /= "")
      n = n + 1
      call popreal(arr(n), line)
    end do
  end subroutine getarray

  subroutine swap(arr, x, y)
    integer(4), intent(in) :: x, y
    real(REAL_LENGTH), intent(inout) :: arr(ARR_LENGTH)
    real(REAL_LENGTH) :: temp

    temp = arr(x)
    arr(x) = arr(y)
    arr(y) = temp
  end subroutine swap

  subroutine sort(arr, n)
    real(REAL_LENGTH), intent(inout) :: arr(ARR_LENGTH)
    integer(INT_LENGTH), intent(in) :: n
    integer(4) :: lower, upper, i
    logical :: sorted

    lower = 1
    upper = n
    sorted = .false.
    do while (lower < upper .and. .not. sorted)
      sorted = .true.

      do i = lower, upper - 1
        if (arr(i) > arr(i + 1)) then
          call swap(arr, i, i + 1)
          sorted = .false.
        end if
      end do
      upper = upper - 1

      do i = upper, lower + 1, -1
        if (arr(i) < arr(i - 1)) then
          call swap(arr, i, i - 1)
          sorted = .false.
        end if
      end do
      lower = lower + 1
    end do
  end subroutine sort

  function join(arr, n) result(str)
    real(REAL_LENGTH) :: arr(ARR_LENGTH), v
    integer(INT_LENGTH) :: n, i
    character(STRING_LENGTH) :: str, tmp

    str = ""
    do i = 1, n
      v = arr(i)
      write(tmp, FMT_REAL) int(v), abs(v - int(v))
      str = trim(str) // DELIM // trim(tmp)
    end do
    str = adjustl(str)
  end function join
end program simple_sorting
