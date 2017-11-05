! https://www.codeeval.com/open_challenges/67/

program hex_to_decimal
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_INT = "(i0)"
  character(*), parameter :: FMT_STR = "(a)"
  character, parameter :: EOF = char(0)

  character(16), parameter :: HEX = "0123456789abcdef"

  character(STRING_LENGTH) :: line
  integer(INT_LENGTH) :: length, i, n, index, cpos, pow
  character :: c

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      length = len_trim(line)
      n = 0
      do i = 1, length, 1
        cpos = length - i + 1
        c = line(cpos:cpos)
        index = scan(HEX, c) - 1
        pow = 16 ** (i - 1)
        n = n + (index * pow)
      end do
      print FMT_INT, n
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
end program hex_to_decimal
