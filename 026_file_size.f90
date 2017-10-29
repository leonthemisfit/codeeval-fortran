! https://www.codeeval.com/open_challenges/26/

program file_size
  implicit none

  character(128) :: path
  integer(4) :: size

  call getarg(1, path)
  inquire(file=path, size=size)
  print "(i0)", size
end program file_size
