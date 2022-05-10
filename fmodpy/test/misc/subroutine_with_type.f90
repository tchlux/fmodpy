
subroutine one_up(person) bind(c)
  use iso_c_binding, only: c_double, c_int
  type, bind(c) :: fancy
     real(c_double) :: shirt
     integer(c_int) :: pants
  end type fancy
  type(fancy), intent(inout) :: person
  person%shirt = person%shirt + 1
  person%pants = person%pants + 2
end subroutine one_up
