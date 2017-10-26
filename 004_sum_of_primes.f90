! https://www.codeeval.com/open_challenges/4/

program sum_of_primes
  integer(4) :: primes(1000)
  integer(4) :: last, sum, i
  logical :: r

  primes(1) = 2
  i = 3
  last = 1
  sum = 2
  do
    call isprime(i, last, primes, r)
    if (r) sum = sum + i
    if (last == 1000) exit
    i = i + 1
  end do
  print "(i0)", sum
contains
  subroutine isprime(n, last, array, r)
    integer(4), intent(in) :: n
    integer(4), intent(inout) :: last
    integer(4), intent(inout) :: array(1000)
    logical, intent(out) :: r
    integer(4) :: i

    r = .true.
    do i = 1, last, 1
      if (mod(n, array(i)) == 0) then
        r = .false.
        exit
      end if
    end do

    if (r) then
      last = last + 1
      array(last) = n
    end if
  end subroutine isprime
end program sum_of_primes
