    program magnetostatics
    use consts
    implicit none

            ! ########################
            ! Derived Type Definitions
            ! ########################

    type :: magnet_face
            ! Vertices of the plane, relative to the centre of the magnet
        real(kind = dp) :: vertices(4, 3)
            ! Normal vector to the plane
        real(kind = dp) :: normal(3)
            ! Magnitude of the magnetisation perpendicular to the surface
        real(kind = dp) :: perp_magnetisation
            ! Resolution constant to determine how many points should be considered
        integer         :: resolution(2)
            ! Surface area of face
        real(kind = dp) :: surface_area
    end type

        ! Create variable type to store details of tarning: Unused dummy argument ‘magnetisation’ at (1) [-Wunused-dummy-argumehe bar magnet inducing the force
    type :: t_magnet
            ! Charge difference over each position
        type(magnet_face)   :: faces(6)
            ! Centre coordinate of the magnet
        real(kind = dp)     :: centre(3)
            ! Distance between each discrete position (will undergo nint to ensure integer number of evenly spaced positions) 
        real(kind = dp)     :: dp
    end type

    type(t_magnet)  :: bar

    integer         :: count
    integer         :: temp

    real(kind = dp) :: field(3), field_dir(3)
    real(kind = dp) :: position(3)


    integer             :: unit
    integer             :: iostat
    character(len=50)   :: iomsg = ''
    
    position = (/2.0_dp, 0.0_dp, 5.01_dp/)

        ! Set default values for centred 2x2x10 bar magnet with uniform charge (0, 0, 1)
    bar%centre = (/0, 0, 0/)
    bar%dp = 0.01_dp
    unit = 11

    ! Vertices of the magnet
    bar%faces(5)%vertices(2, :) = (/-2.0_dp, -2.0_dp, -5.0_dp/)
    bar%faces(2)%vertices(1, :) = (/-2.0_dp, -2.0_dp, -5.0_dp/)
    bar%faces(6)%vertices(1, :) = (/-2.0_dp, -2.0_dp, -5.0_dp/)

    bar%faces(2)%vertices(2, :) = (/-2.0_dp,  2.0_dp, -5.0_dp/)
    bar%faces(3)%vertices(1, :) = (/-2.0_dp,  2.0_dp, -5.0_dp/)
    bar%faces(6)%vertices(2, :) = (/-2.0_dp,  2.0_dp, -5.0_dp/)

    bar%faces(3)%vertices(2, :) = (/ 2.0_dp,  2.0_dp, -5.0_dp/)
    bar%faces(4)%vertices(1, :) = (/ 2.0_dp,  2.0_dp, -5.0_dp/)
    bar%faces(6)%vertices(3, :) = (/ 2.0_dp,  2.0_dp, -5.0_dp/)

    bar%faces(4)%vertices(2, :) = (/ 2.0_dp, -2.0_dp, -5.0_dp/)
    bar%faces(5)%vertices(1, :) = (/ 2.0_dp, -2.0_dp, -5.0_dp/)
    bar%faces(6)%vertices(4, :) = (/ 2.0_dp, -2.0_dp, -5.0_dp/)


    bar%faces(5)%vertices(3, :) = (/-2.0_dp, -2.0_dp, 5.0_dp/)
    bar%faces(2)%vertices(4, :) = (/-2.0_dp, -2.0_dp, 5.0_dp/)
    bar%faces(1)%vertices(1, :) = (/-2.0_dp, -2.0_dp, 5.0_dp/)

    bar%faces(2)%vertices(3, :) = (/-2.0_dp,  2.0_dp, 5.0_dp/)
    bar%faces(3)%vertices(4, :) = (/-2.0_dp,  2.0_dp, 5.0_dp/)
    bar%faces(1)%vertices(2, :) = (/-2.0_dp,  2.0_dp, 5.0_dp/)

    bar%faces(3)%vertices(3, :) = (/ 2.0_dp,  2.0_dp, 5.0_dp/)
    bar%faces(4)%vertices(4, :) = (/ 2.0_dp,  2.0_dp, 5.0_dp/)
    bar%faces(1)%vertices(3, :) = (/ 2.0_dp,  2.0_dp, 5.0_dp/)

    bar%faces(4)%vertices(3, :) = (/ 2.0_dp, -2.0_dp, 5.0_dp/)
    bar%faces(5)%vertices(4, :) = (/ 2.0_dp, -2.0_dp, 5.0_dp/)
    bar%faces(1)%vertices(4, :) = (/ 2.0_dp, -2.0_dp, 5.0_dp/)


    do count = 1, size(bar%faces)
        
    end do

    open(unit, file = "3D_plot.txt", action='write', iostat=iostat, iomsg=iomsg)
    if (iostat .ne. 0) then
        write(*,*) trim(iomsg)
        stop
    end if

    do temp=0, 100
        field = (/0.0_dp, 0.0_dp, 0.0_dp/)
        do count = 1, size(bar%faces)
            field = field + B(bar%faces(count), position)
        end do

        field_dir = field / length(field)

        write(unit,*) position, field_dir, length(field)

        position = position - (0.1_dp * field_dir)

        if((field_dir(3) > 0.9_dp) .and. (position(3) < 0)) exit
    end do

    close(unit)

    contains

        pure function B(face, pos)
            type(magnet_face), intent(in)   :: face
            real(kind = dp), intent(in)     :: pos(3)
            
            integer                         :: cx, cy
            real(kind = dp)                 :: dx(3), dy(3)
            real(kind = dp)                 :: surface_pos(3)

            real(kind = dp)                 :: dist(3)
            real(kind = dp)                 :: B(3)

            B = (/0.0_dp, 0.0_dp, 0.0_dp/)

                ! Calculate the size of the distance step over the plane
            dx = (face%vertices(1, :) - face%vertices(3, :)) / face%resolution(1)
            dy = (face%vertices(1, :) - face%vertices(2, :)) / face%resolution(2)


                ! Nested loops to step over the whole plane which is being considered
            do cx = 0, face%resolution(1)
                do cy = 0, face%resolution(2)

                        ! Calculate position of currently considered point on the plane
                    surface_pos = face%vertices(1, :) + cx*dx + cy*dy
                        ! Distance from field point to plane point
                    dist = pos - surface_pos

                        ! Sum field strength from each point on the surface
                    B = B + (10.0_dp**(-7))*((dist * face%perp_magnetisation) &
                    & / (length(dist)**3))
                end do
            end do
        end function

        pure subroutine surface_magnetisation(self, magnetisation)
            class(magnet_face), intent(inout)   :: self
            real(kind = dp), intent(in)         :: magnetisation(3)

            integer                             :: cell_count

                ! Number of cells on plane
            cell_count = self%resolution(1) * self%resolution(2)

                ! Magnitude of magnetisation perpendicular to the plane per consideration cell
            self%perp_magnetisation = dot_product(self%normal, magnetisation) * self%surface_area / real(cell_count, kind = dp)
        end subroutine

        pure function length(vector)
            real(kind = dp), intent(in) :: vector(:)

            real(kind = dp) :: sum
            integer         :: c

            real(kind = dp) :: length

            sum = 0.0_dp
            do c = 1, size(vector)
                sum = sum + vector(c)**2
            end do

            length = sqrt(sum)
        end function
end program