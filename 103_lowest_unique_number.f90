! https://www.codeeval.com/open_challenges/103/

program lowest_unique_number
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_INT = "(i0)"
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: EOF = char(0)
  character, parameter :: DELIM = " "
  integer(4), parameter :: ARR_LENGTH = 64

  character(STRING_LENGTH) :: line
  integer(INT_LENGTH) :: n, players(ARR_LENGTH), i, unique, player
  integer(INT_LENGTH) :: arr(9)

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      arr = (/0,0,0,0,0,0,0,0,0/)
      call getarray(line, players, n)
      do i = 1, n
        arr(players(i)) = arr(players(i)) + 1
      end do
      do i = 1, 9
        if (arr(i) == 1) then
          unique = i
          exit
        end if
      end do
      player = 0
      do i = 1, n
        if (players(i) == unique) then
          player = i
          exit
        end if
      end do
      print FMT_INT, player
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
end program lowest_unique_number
