module consts
    implicit none

    integer, parameter  :: dp = selected_real_kind(15, 300)

    real(kind = dp), parameter  :: pi = 3.1415926
    complex(kind = dp), parameter   :: i = cmplx(0.0_dp, 1.0_dp, kind=dp)

    type :: vector
        real(kind = dp) :: i
        real(kind = dp) :: j
        real(kind = dp) :: k
    end type

    interface operator(-)
        procedure vec_subt
    end interface
    
    interface operator(+)
        procedure vec_add
    end interface

    interface vec_div
        procedure vec_div_int
        procedure vec_div_real
    end interface

    contains
    
        pure function vec_subt(a, b) result(c)
            type(vector), intent(in)    :: a, b
            type(vector)                :: c
            
            c%i = a%i - b%i
            c%j = a%j - b%j
            c%k = a%k - b%k
        end function

        pure function vec_add(a, b) result(c)
            type(vector), intent(in)    :: a, b
            type(vector)                :: c
            
            c%i = a%i + b%i
            c%j = a%j + b%j
            c%k = a%k + b%k
        end function

        pure function vec_div_int(a, b) result(c)
            type(vector), intent(in)    :: a
            integer, intent(in)         :: b
            type(vector)                :: c
            
            real(kind = dp)             :: b_real

        b_real = real(b, kind = dp)

            c%i = a%i / b_real
            c%j = a%j / b_real
            c%k = a%k / b_real
        end function

        pure function vec_div_real(a, b) result(c)
            type(vector), intent(in)    :: a
            real(kind = dp), intent(in) :: b
            type(vector)                :: c

            c%i = a%i / b
            c%j = a%j / b
            c%k = a%k / b
        end function

        pure function dot_p(a, b) result(c)
            type(vector), intent(in)    :: a, b
            real(kind = dp)             :: c

            c = (a%i * b%i) +(a%j * b%j) + (a%k * b%k)
        end function

        pure function pythagoras(a) result(b)
            type(vector), intent(in)    :: a
            real(kind = dp)             :: b

            b = a%i**2 + a%j**2 + a%k**2
            b = sqrt(b)
        end function
end module