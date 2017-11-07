! https://www.codeeval.com/open_challenges/93/

program capitalize_words
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character, parameter :: EOF = char(0)
  character, parameter :: DELIM = " "
  integer(4), parameter :: ARR_LENGTH = 64

  character(STRING_LENGTH) :: line, arr(ARR_LENGTH), word
  integer(INT_LENGTH) :: n, c

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      n = 0
      do while (line /= "")
        n = n + 1
        call popstring(word, line)
        c = iachar(word(1:1))
        if (c >= 97 .and. c <= 122) c = c - 32
        word(1:1) = char(c)
        arr(n) = word
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

  subroutine popstring(first, rest)
    character(STRING_LENGTH), intent(out) :: first
    character(STRING_LENGTH), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), DELIM)
    if (index > 0) then
      first = rest(1:index - 1)
      rest = rest(index + 1:)
    else
      first = rest
      rest = ""
    end if
  end subroutine popstring

  function join(arr, n) result(str)
    integer(INT_LENGTH) :: n, i
    character(STRING_LENGTH) :: arr(ARR_LENGTH), str, v

    str = ""
    do i = 1, n
      v = arr(i)
      str = trim(str) // " " // trim(arr(i))
    end do
    str = adjustl(str)
  end function join
end program capitalize_words
