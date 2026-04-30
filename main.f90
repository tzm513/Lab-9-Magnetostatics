    program magnetostatics
    use consts
    implicit none

            ! ########################
            ! Derived Type Definitions
            ! ########################

    type :: magnet_face
            ! Vertices of the plane
        type(vector)    :: vertices(4)
            ! Normal vector to the plane
        type(vector)    :: normal
            ! Magnitude of the magnetisation perpendicular to the surface
        real(kind = dp) :: perp_magnetisation
            ! Resolution constant to determine how many points should be considered
        integer         :: resolution(2)
            ! Surface area of face
        real(kind = dp) :: surface_area
    end type

        ! Create variable type to store details of the bar magnet inducing the force
    type :: t_magnet
            ! Charge difference over each position
        type(magnet_face), allocatable  :: faces(:)
            ! Centre coordinate of the magnet
        type(vector)                    :: centre
            ! Distance between each discrete position (will undergo nint to ensure integer number of evenly spaced positions) 
        real(kind = dp)                 :: dp
    end type

    type(t_magnet)  :: bar

    integer         :: count


    

        ! Set default values for centred 2x2x10 bar magnet with uniform charge (0, 0, 1)
    allocate(bar%faces(6))
    bar%centre = vector(0, 0, 0)
    bar%dp = 0.01_dp

        ! Vertices(1) = bottom left, Vertices(2) = bottom right, Vertices(3) = top left, Vertices(4) = to right
        ! Looking from +x, +y, +z

    bar%faces(1)%vertices(1) = vector(-2, -2, -5)
    bar%faces(1)%vertices(2) = vector(-2,  2, -5)
    bar%faces(1)%vertices(3) = vector(-2, -2,  5)
    bar%faces(1)%vertices(4) = vector(-2,  2,  5)

    bar%faces(2)%vertices(1) = vector(2,  -2, -5)
    bar%faces(2)%vertices(2) = vector(2,   2, -5)
    bar%faces(2)%vertices(3) = vector(2,  -2,  5)
    bar%faces(2)%vertices(4) = vector(2,   2,  5)

    bar%faces(3)%vertices(1) = vector(-2, -2, -5)
    bar%faces(3)%vertices(2) = vector( 2, -2, -5)
    bar%faces(3)%vertices(3) = vector(-2, -2,  5)
    bar%faces(3)%vertices(4) = vector( 2, -2,  5)

    bar%faces(4)%vertices(1) = vector(-2,  2, -5)
    bar%faces(4)%vertices(2) = vector( 2,  2, -5)
    bar%faces(4)%vertices(3) = vector(-2,  2,  5)
    bar%faces(4)%vertices(4) = vector( 2,  2,  5)

    bar%faces(5)%vertices(1) = vector(-2, -2, -5)
    bar%faces(5)%vertices(2) = vector( 2,  2, -5)
    bar%faces(5)%vertices(3) = vector( 2, -2, -5)
    bar%faces(5)%vertices(4) = vector(-2,  2, -5)

    bar%faces(6)%vertices(1) = vector(-2, -2,  5)
    bar%faces(6)%vertices(2) = vector( 2,  2,  5)
    bar%faces(6)%vertices(3) = vector( 2, -2,  5)
    bar%faces(6)%vertices(4) = vector(-2,  2,  5)

    do count = 1, size(bar%faces)
        bar%faces(count)%normal = cross_p(bar%faces(count)%vertices(1) - bar%faces(count)%vertices(2), &
        & bar%faces(count)%vertices(2) - bar%faces(count)%vertices(3))

        call surface_magnetisation(bar%faces(count), vector(0, 0, 1))
        bar%faces(count)%resolution(1) = nint(pythagoras(bar%faces(count)%vertices(1) - bar%faces(count)%vertices(2)) / bar%dp)
        bar%faces(count)%resolution(2) = nint(pythagoras(bar%faces(count)%vertices(2) - bar%faces(count)%vertices(3)) / bar%dp)
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

                ! Calculate the size of the distance step over the plane
            dx = (face%vertices(2) - face%vertices(1)) / face%resolution(1)
            dy = (face%vertices(3) - face%vertices(2)) / face%resolution(2)

                ! Nested loops to step over the whole plane which is being considered
            do cx = 0, face%resolution(1)
                do cy = 0, face%resolution(2)
                        ! Calculate position of currently considered point on the plane
                    surface_pos = face%vertices(1) + cx*dx + cy*dy
                        ! Distance from field point to plane point
                    dist = pos - surface_pos

                        ! Sum field strength from each point on the surface
                    B = B + ((10.0_dp**(-7)) * (dist * face%perp_magnetisation) &
                    & / (pythagoras(dist)**3))
                end do
            end do
        end function

        pure subroutine surface_magnetisation(self, magnetisation)
            class(magnet_face), intent(inout)   :: self
            type(vector), intent(in)            :: magnetisation

            integer                             :: cell_count

                ! Number of cells on plane
            cell_count = self%resolution(1) * self%resolution(2)

                ! Magnitude of magnetisation perpendicular to the plane per consideration cell
            self%perp_magnetisation = dot_p(self%normal, magnetisation) * self%surface_area / cell_count
        end subroutine
end program