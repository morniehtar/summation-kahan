#define _pr 8

program output
    implicit none

    real(_pr) :: dres, kres

    procedure(term), pointer :: fptr

    fptr => term
    dres = dsum(fptr, 1000000000)
    print "(/)"
    kres = ksum(fptr, 1000000000)

    print "(/)"
    print *, 'Dumb:    ', dres
    print *, "Kahan's: ", kres


contains
    function term(int)
        integer, intent(in) :: int
        real(_pr) :: term
        term = 0.000000001
    end function term

    function dsum(fptr, lim)
        integer, intent(in) :: lim
        procedure(term), pointer, intent(in) :: fptr
        real(_pr) :: res, dsum
        integer :: i

        res = 0
        dsum = 0

        do i = 1, lim
            dsum = dsum + fptr(i)
            if (mod(i, 10000000)==0) call progress(i, lim)
        end do

    end function dsum

    function ksum(fptr, lim)
        integer, intent(in) :: lim
        procedure(term), pointer, intent(in) :: fptr
        real(_pr) :: ksum, st, t, y
        integer :: i

        ksum = 0
        st = 0
        do i = 1, lim
            y = fptr(i) - st
            t = ksum + y
            st = (t - ksum) - y
            ksum = t
            if (mod(i, 10000000)==0) call progress(i, lim)
        end do

    end function ksum

    subroutine progress(j, lim)
        integer, intent(in) :: j, lim
        integer :: k, l
        character(len=71) :: bar

        bar="\rCalculation: ???% [                                                  ]"

        l=(100.d0*j)/lim
        write(unit=bar(15:17), fmt="(i3)") l
        l=l/2
        do k = 1, l
            bar(20+k:20+k)="="
        end do

        write(*,'(a)',advance='no') bar
        write(*, fmt="(i10)", advance='no') j
    end subroutine progress

end program output

