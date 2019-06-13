module time_keeper

  implicit none
  integer    :: start(8), now(8)

contains

  subroutine startclock( )
    implicit none
    call date_and_time(values=start)
  end subroutine startclock


  subroutine elapsedtime_s( et_s )
    implicit none
    integer             :: diffs(8)=0
    real   , intent(out):: et_s       ! in seconds

    call date_and_time(values=now)

    ! - Find the difference in times
    diffs = now - start

    ! - This works only when the time is measured in a specific month
    if (diffs(3) > 0) then
       diffs(5) = 24*diffs(3) + diffs(5)
    endif

    et_s = diffs(5) * 3600 + diffs(6) * 60 + diffs(7) + 1e-3 * diffs(8)

  end subroutine elapsedtime_s

end module time_keeper




program sandpile
use time_keeper


        !Variables
        integer :: MAXH, FPH, toohigh, i, j, k, l, dimen
        integer, dimension(2000,2000) :: G_SIZE
        integer, dimension(2) :: MPC, highest
        integer, dimension(:,:), allocatable :: maxlocation, field
        real :: et_s

        ! Initialize var
        MAXH = 4
        FPH  = 10000
        i    = 0
        MPC(1) = 100
        MPC(2) = 200
        dimen = 500

        allocate(maxlocation(FPH/MAXH,2))
        allocate(field(dimen,dimen))
       

        !Make field full of zeros
        do j = 1, dimen
                do k = 1, dimen
                        field(j,k) = 0
                end do
        end do
        
        field(249,225) = FPH
        field(249,275) = FPH
        field(249,249) = FPH/2
        
        call startclock()

        !While loop
        do while (maxval(field) >= MAXH)
                l = 1
                do j = 1, FPH/MAXH
                        maxlocation(j,1) = 0
                        maxlocation(j,2) = 0
                end do
                ! Find where field >= MAXH
                do j = 1, dimen
                        do k = 1, dimen
                                if (field(j,k) >= MAXH) then
                                        
                                        maxlocation(l,1) = j
                                        maxlocation(l,2) = k
                                        l = l+1
                                end if
                        end do
                end do

                !print *, maxlocation(1,:)
                
                !Increase pile where maxlocation is not empty
                do j = 1, FPH/MAXH
                        if (maxlocation(j,1) /= 0 .and. maxlocation(j,2) /=0) then
                                
                                if (field(maxlocation(j,1),maxlocation(j,2)) >= MAXH ) then
                                        
                                        field(maxlocation(j,1),maxlocation(j,2)) = &
                                                field(maxlocation(j,1),maxlocation(j,2)) - MAXH
                                        field(maxlocation(j,1)+1,maxlocation(j,2)) = &
                                                field(maxlocation(j,1)+1,maxlocation(j,2)) + MAXH/4
                                        field(maxlocation(j,1)-1,maxlocation(j,2)) = &
                                                field(maxlocation(j,1)-1,maxlocation(j,2)) + MAXH/4
                                        field(maxlocation(j,1),maxlocation(j,2)+1) = &
                                                field(maxlocation(j,1),maxlocation(j,2)+1) + MAXH/4
                                        field(maxlocation(j,1),maxlocation(j,2)-1) = &
                                                field(maxlocation(j,1),maxlocation(j,2)-1) + MAXH/4
                                end if
                        end if
                       
                end do
                
                i = i+1
               
        end do
        
        print *, i
        
        call elapsedtime_s(et_s)
        write(*,*) et_s

        !Save data
        open(12, file = "data.csv")
        do j = 1,dimen
        do k = 1, dimen
        write(12,*) field(j,k)
        end do
        end do
        close(12)
end program sandpile
