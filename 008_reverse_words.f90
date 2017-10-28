! https://www.codeeval.com/open_challenges/8/

program reverse_words
  implicit none
  
  character(256) :: line

  call openfile()
  do
    line = readline()
    select case (line)
    case (char(0))
      exit
    case ("")
      print "(a)", ""
    case default
      call reverse(line)
      print "(a)", trim(line)
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
      line = char(0)
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

  subroutine reverse(line)
    character(256), intent(inout) :: line
    character(256) :: newline
    character(256) :: word

    newline = ""
    word = ""
    do
      if (trim(line) == "") exit
      call popstring(word, line)
      newline = " " // trim(word) // trim(newline)
    end do
    line = adjustl(newline)
  end subroutine reverse
end program reverse_words
