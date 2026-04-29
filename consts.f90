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

    contains
    
        pure function vec_subt(a, b) result(c)
            type(vector), intent(in)    :: a, b
            type(vector)                :: c

            real(kind = dp)             :: l, m, n
            
            l = a%i - b%i
            m = a%j - b%j
            n = a%k - b%k

            c = vector(l, m, n)
        end function

        pure function vec_add(a, b) result(c)
            type(vector), intent(in)    :: a, b
            type(vector)                :: c

            real(kind = dp)             :: l, m, n
            
            l = a%i + b%i
            m = a%j + b%j
            n = a%k + b%k

            c = vector(l, m, n)
        end function   
end module