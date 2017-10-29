! https://www.codeeval.com/open_challenges/37/

program pangrams
  implicit none

  integer(4), parameter :: STRING_LENGTH = 512
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character, parameter :: EOF = char(0)
  character(26), parameter :: alpha = "abcdefghijklmnopqrstuvwxyz"

  character(STRING_LENGTH) :: line, curr, unused
  integer(INT_LENGTH) :: i, index, length

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      curr = alpha
      call tolower(line)
      do i = 1, len_trim(line), 1
        index = scan(curr, line(i:i))
        if (index > 0) curr(index:index) = ""
      end do
      length = 0
      unused = ""
      do i = 1, 26, 1
        if (curr(i:i) /= "") then
          length = length + 1
          unused(length:length) = curr(i:i)
        end if
      end do
      if (unused /= "") then
        print FMT_STR, trim(unused)
      else
        print FMT_STR, "NULL"
      end if
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

  subroutine tolower(str)
    character(STRING_LENGTH), intent(inout) :: str
    integer(INT_LENGTH) :: i, c

    do i = 1, len_trim(str), 1
      c = ichar(str(i:i))
      if (c >= 65 .and. c <= 90) then
        c = c + 32
      end if
      str(i:i) = char(c)
    end do
  end
end program pangrams
