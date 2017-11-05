! https://www.codeeval.com/open_challenges/68/

program valid_parentheses
  implicit none

  integer(4), parameter :: STRING_LENGTH = 256
  integer(4), parameter :: PATH_LENGTH = 128
  integer(4), parameter :: INT_LENGTH = 4
  integer(4), parameter :: FILE_UNIT = 99
  character(*), parameter :: FMT_STR = "(a)"
  character, parameter :: EOF = char(0)

  character(STRING_LENGTH) :: line, t

  call openfile()
  do
    line = readline()
    select case (line)
    case (EOF)
      exit
    case ("")
      cycle
    case default
      if (validate(line)) then
        print FMT_STR, "True"
      else
        print FMT_STR, "False"
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

  function isopen(c) result(bool)
    character :: c
    logical :: bool

    select case (c)
    case ("(", "{", "[")
      bool = .true.
    case default
      bool = .false.
    end select
  end function isopen

  function isclosed(c) result(bool)
    character :: c
    logical :: bool

    select case (c)
    case (")", "}", "]")
      bool = .true.
    case default
      bool = .false.
    end select
  end function isclosed

  function inverted(c) result(r)
    character :: c, r
    select case (c)
    case ("(")
      r = ")"
    case ("{")
      r = "}"
    case ("[")
      r = "]"
    case (")")
      r = "("
    case ("}")
      r = "{"
    case ("]")
      r = "["
    end select
  end function inverted

  recursive function validate(str) result(bool)
    character(STRING_LENGTH) :: str
    integer(INT_LENGTH) :: i
    logical :: bool

    if (str == "" .or. len_trim(str) == 0) then
      bool = .true.
      return
    else if (isclosed(str(1:1))) then
      bool = .false.
      return
    else
      if (isclosed(str(2:2))) then
        bool = str(2:2) == inverted(str(1:1))
      else
        bool = validate(str(2:))
      end if
    end if
  end function validate
end program valid_parentheses
