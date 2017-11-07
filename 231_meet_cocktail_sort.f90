! https://www.codeeval.com/open_challenges/231/

program meet_cocktail_sort
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

  character(STRING_LENGTH) :: line, ints
  integer(INT_LENGTH) :: arr(ARR_LENGTH), maxiter, n

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      call split(ints, line, "|")
      read(line, FMT_RINT) maxiter
      call getarray(ints, arr, n)
      call sort(arr, n, maxiter)
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

  subroutine popint(first, rest, delim)
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
  end subroutine popint

  subroutine getarray(line, arr, n)
    character(STRING_LENGTH), intent(inout) :: line
    integer(INT_LENGTH), intent(out) :: arr(ARR_LENGTH)
    integer(INT_LENGTH), intent(out) :: n

    n = 0
    do while (line /= "")
      n = n + 1
      call popint(arr(n), line, " ")
    end do
  end subroutine getarray

  subroutine swap(arr, x, y)
    integer(4), intent(in) :: x, y
    integer(INT_LENGTH), intent(inout) :: arr(ARR_LENGTH)
    integer(INT_LENGTH) :: temp

    temp = arr(x)
    arr(x) = arr(y)
    arr(y) = temp
  end subroutine swap

  subroutine sort(arr, n, maxiter)
    integer(INT_LENGTH), intent(inout) :: arr(ARR_LENGTH)
    integer(INT_LENGTH), intent(in) :: n, maxiter
    integer(4) :: lower, upper, i, iters
    logical :: sorted

    iters = 0
    lower = 1
    upper = n
    sorted = .false.
    do while (lower < upper .and. .not. sorted .and. iters < maxiter)
      iters = iters + 1
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

  subroutine split(first, rest, delim)
    character(STRING_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    character, intent(in) :: delim
    integer(INT_LENGTH) :: index

    index = scan(trim(rest), delim)
    if (index > 0) then
      first = rest(1:index - 1)
      rest = rest(index + 1:)
    else
      first = rest
      rest = ""
    end if
  end subroutine split

  function join(arr, n) result(str)
    integer(INT_LENGTH) :: arr(ARR_LENGTH), v, n, i
    character(STRING_LENGTH) :: str, tmp

    str = ""
    do i = 1, n
      v = arr(i)
      write(tmp, FMT_INT) v
      str = trim(str) // " " // trim(tmp)
    end do
    str = adjustl(str)
  end function join
end program meet_cocktail_sort
