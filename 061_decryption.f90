! https://www.codeeval.com/open_challenges/61/

program decryption
  implicit none

  integer(4), parameter :: STRING_LENGTH = 512
  integer(4), parameter :: INT_LENGTH = 4
  character(*), parameter :: FMT_STR = "(a)"
  character(*), parameter :: FMT_RINT = "(i4)"
  character, parameter :: DELIM = " "

  character(26), parameter :: KEY = "BHISOECRTMGWYVALUZDNFJKPQX"
  character(26), parameter :: ALPHA = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  character(STRING_LENGTH) :: message = "012222 1114142503 0313012513 &
  03141418192102 0113 2419182119021713 06131715070119"

  print FMT_STR, trim(decrypt(message))
contains
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

  subroutine popint(n, rest)
    integer(INT_LENGTH), intent(out) :: n
    character(STRING_LENGTH), intent(inout) :: rest
    character(2) :: str

    select case (len_trim(rest))
    case (:0)
      n = -1
      return
    case (2)
      str = rest(1:2)
      rest = ""
    case default
      str = rest(1:2)
      rest = rest(3:)
    end select
    read(str, FMT_RINT) n
  end subroutine popint

  function decryptint(n) result (c)
    integer(INT_LENGTH) :: n, ikey
    character :: c, ckey

    n = n + 1
    ckey = ALPHA(n:n)
    ikey = scan(KEY, ckey)
    c = ALPHA(ikey:ikey)
  end function decryptint

  function decryptword(word) result(str)
    character(STRING_LENGTH) :: word, str
    integer(INT_LENGTH) :: n

    str = ""
    do
      call popint(n, word)
      if (n == -1) exit
      str = trim(str) // decryptint(n)
    end do
  end function decryptword

  function decrypt(msg) result(str)
    character(STRING_LENGTH) :: msg, str, word

    str = ""
    do
      call popstring(word, msg)
      if (word == "") exit
      str = trim(str) // " " // decryptword(word)
    end do
    str = adjustl(str)
  end function decrypt
end program decryption
