! https://www.codeeval.com/open_challenges/45/

program reverse_and_add
  implicit none

  integer(4), parameter :: STRING_LENGTH = 512
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 8
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i10)"
  character, parameter :: EOF = char(0)

  character(STRING_LENGTH) :: line, sum
  integer(INT_LENGTH) :: f, b, cycles, p
  logical :: isp

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      cycles = 0
      sum = line
      do
        if (ispalindrome(sum)) exit
        cycles = cycles + 1
        f = toint(sum)
        b = toint(reverse(sum))
        sum = tostr(f + b)
      end do
      print " (i0,1x,a)", cycles, trim(sum)
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

  function ispalindrome(str) result(res)
    character(STRING_LENGTH) :: str
    logical :: res
    integer(INT_LENGTH) :: length, max, l, r

    res = .true.
    length = len_trim(str)
    max = (length / 2) + 1
    do l = 1, max, 1
      r = length - l + 1
      if (str(l:l) /= str(r:r)) then
        res = .false.
        exit
      end if
    end do
  end function ispalindrome

  function reverse(str) result(rev)
    character(STRING_LENGTH) :: str, rev
    integer(INT_LENGTH) :: l, r, length

    rev = ""
    length = len_trim(str)
    do l = 1, length, 1
      r = length - l + 1
      rev(r:r) = str(l:l)
    end do
  end function reverse

  function toint(str) result(i)
    character(STRING_LENGTH) :: str
    integer(INT_LENGTH) :: i

    read(str, FMT_RINT) i
  end function toint

  function tostr(i) result(str)
    character(STRING_LENGTH) :: str
    integer(INT_LENGTH) :: i

    write(str, FMT_RINT) i
    str = adjustl(str)
  end function tostr
end program reverse_and_add
