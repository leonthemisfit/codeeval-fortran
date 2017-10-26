! https://www.codeeval.com/open_challenges/170/

program guess_the_number
  character(256) :: line

  call openfile()
  do
    line = readline()
    select case (trim(line))
    case ("EOF")
      exit
    case ("")
      continue
    case default
      call game(line)
    end select
  end do

  close(9)
contains
  subroutine openfile()
    character(256) :: path
    call getarg(1, path)
    open(9, file=trim(path))
  end subroutine openfile

  function readline() result(line)
    character(256) :: line
    integer(1) :: io

    read(9, "(a)", iostat=io) line
    if (io > 0) then
      print *, "ERR: Read Failed"
    else if (io < 0) then
      line = "EOF"
    end if
  end function readline

  subroutine popstring(first, rest)
    character(256), intent(out) :: first
    character(256), intent(inout) :: rest
    integer(2) :: index

    index = scan(trim(rest), " ")
    if (index > 0) then
      first = rest(1:index - 1)
      rest = rest(index + 1:)
    else
      first = rest
      rest = ""
    end if
  end subroutine popstring

  subroutine game(line)
    character(256), intent(in) :: line
    character(256) :: startstr, first, rest
    real(4) :: lower, upper, guess

    rest = line
    lower = 0
    call popstring(startstr, rest)
    read(startstr, "(f4.0)") upper
    do
      guess = ceiling((lower + upper) / 2)
      call popstring(first, rest)
      select case (trim(first))
      case ("Lower")
        upper = max(lower, guess - 1)
      case ("Higher")
        lower = min(upper, guess + 1)
      case ("Yay!")
        print "(i0)", int(guess)
        exit
      end select
    end do
  end subroutine game
end program guess_the_number
