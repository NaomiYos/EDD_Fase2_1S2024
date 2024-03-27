module fase2
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fase2!"
  end subroutine say_hello
end module fase2
