! https://www.codeeval.com/open_challenges/81/

program sum_to_zero
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_INT = "(i0)"
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)
  character, parameter :: DELIM = ","
  integer(4), parameter :: ARR_LENGTH = 128

  character(STRING_LENGTH) :: line
  integer(INT_LENGTH) :: length, arr(ARR_LENGTH)

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      call getarray(arr, length, line)
      print FMT_INT, count(arr, length)
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

  subroutine popint(first, rest)
    integer(INT_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), DELIM)
    if (index > 0) then
      read(rest(1:index - 1), FMT_RINT) first
      rest = rest(index + 1:)
    else
      read(rest, FMT_RINT) first
      rest = ""
    end if
  end subroutine popint

  subroutine getarray(a, n, line)
    character(STRING_LENGTH), intent(inout) :: line
    integer(INT_LENGTH), intent(out) :: a(ARR_LENGTH), n

    n = 0
    do while (line /= "")
      n = n + 1
      call popint(a(n), line)
    end do
  end subroutine getarray

  function count(arr, length) result (n)
    integer(INT_LENGTH) :: arr(ARR_LENGTH), n, length, i, j, k, l

    n = 0
    do i = 1, length - 3
      do j = i + 1, length - 2
        do k = j + 1, length - 1
          do l = k + 1, length
            if (arr(i) + arr(j) + arr(k) + arr(l) == 0) n = n + 1
          end do
        end do
      end do
    end do
  end function count
end program sum_to_zero
