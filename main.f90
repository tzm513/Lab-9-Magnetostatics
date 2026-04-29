program magnetostatics
    use consts
    implicit none

        ! Create variable type to store details of the bar magnet inducing the force
    type :: magnet(xlen, ylen, zlen)
        integer, len :: xlen, ylen, zlen

            ! Charge difference over each position
        type(vector), dimension(xlen, ylen, zlen)   :: charges
            ! Centre coordinate of the magnet
        type(vector)                :: centre(3)
            ! Distance between each point of the magnet
        real(kind = dp)             :: dx

    end type
    
    type(magnet(2, 2, 10)) :: bar
end program