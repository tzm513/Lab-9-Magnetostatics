    program magnetostatics
    use consts
    implicit none

            ! ########################
            ! Derived Type Definitions
            ! ########################

    type :: magnet_face
            ! Vertices of the plane
        type(vector)    :: vertex(4)
            ! Normal vector to the plane
        type(vector)    :: normal
            ! Magnitude of the magnetisation perpendicular to the surface
        real(kind = dp) :: perp_magnetisation
            ! Resolution constant to determine how many points should be considered
        integer         :: resolution(2)
    end type

        ! Create variable type to store details of the bar magnet inducing the force
    type :: t_magnet
            ! Charge difference over each position
        type(magnet_face), allocatable  :: faces(:)
            ! Centre coordinate of the magnet
        type(vector)                    :: centre
            ! Distance between each discrete position (will undergo nint to esnure integer number of evenly spaced positions) 
        real(kind = dp)                 :: dp
    end type

        ! #####################
        ! Variable Declarations
        ! #####################

    type(t_magnet)  :: bar

    integer         :: count
    

        ! Set default values for centred 2x2x10 bar magnet with uniform charge (0, 0, 1)
    allocate(bar%faces(6))
    bar%centre = vector(0, 0, 0)
    bar%dp = 0.01_dp

    do count = 0, size(bar%faces)
        call surface_magnetisation(bar%faces(count), vector(0, 0, 1))
        bar%faces(count)%resolution(1) = nint(pythagoras(bar%faces(count)%vertex(1) - bar%faces(count)%vertex(2)) / bar%dp)
        bar%faces(count)%resolution(2) = nint(pythagoras(bar%faces(count)%vertex(2) - bar%faces(count)%vertex(3)) / bar%dp)
    end do


    contains

        pure function B(face, pos)
            type(magnet_face), intent(in)   :: face
            type(vector), intent(in)        :: pos
            
            integer                         :: cx, cy
            type(vector)                    :: dx, dy
            type(vector)                    :: surface_pos

            type(vector)                    :: dist
            type(vector)                    :: B

            B = vector(0, 0, 0)

            dx = (face%vertex(2) - face%vertex(1)) / face%resolution(1)
            dy = (face%vertex(3) - face%vertex(2)) / face%resolution(2)


            do cx = 0, face%resolution(1)
                do cy = 0, face%resolution(2)
                    surface_pos = face%vertex(1) + cx*dx + cy*dy
                    dist = pos - surface_pos

                    B = B + ((10.0_dp**(-7)) * (dist * face%perp_magnetisation) &
                    & / (pythagoras(dist)**3))
                end do
            end do
        end function

        pure subroutine surface_magnetisation(self, magnetisation)
            class(magnet_face), intent(inout)   :: self
            type(vector), intent(in)            :: magnetisation

            integer                             :: cell_count

            cell_count = self%resolution(1) * self%resolution(2)

            self%perp_magnetisation = dot_p(self%normal, magnetisation) / cell_count
        end subroutine
end program