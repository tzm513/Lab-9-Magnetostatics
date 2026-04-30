program magnetostatics
    use consts
    implicit none

        ! Create variable type to store details of the bar magnet inducing the force
    type :: t_magnet
            ! Charge difference over each position
        type(vector), allocatable   :: charges(:, :, :)
            ! Centre coordinate of the magnet
        type(vector)                :: centre
            ! Distance between each point of the magnet
        real(kind = dp)             :: dx
    end type

    type(t_magnet) :: bar

        ! Set default values for centred 2x2x10 bar magnet with uniform charge (0, 0, 1)
    allocate(bar%charges(2, 2, 10))
    bar%charges = vector(0, 0, 1)
    bar%centre = vector(0, 0, 0)
    bar%dx = 1.0_dp



    contains

        pure function B(magnet, pos)
            type(t_magnet), intent(in)  :: magnet
            type(vector), intent(in)    :: pos

            real(kind = dp)             :: B

            type(vector)                :: dist
            integer                     :: x, y, z

            dist = magnet%centre - pos

            B = 0.0_dp

            do i = 0, size(magnet%charges, 1)
                do j = 0, size(magnet%charges, 2)
                    do k = 0, size(magnet%charges, 3)
                        
                    end do
                end do
            end do
        end function
end program