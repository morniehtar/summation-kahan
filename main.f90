#define _pr 8

module store
    implicit none
    public
    integer :: perc
end module store

program output
    use omp_lib
    use store
    implicit none

    real(_pr) :: dres, kres

    procedure(term), pointer :: fptr

    fptr => term
    dres = dsum(fptr, 10**9)
    print "(/)"
    perc = 0
    kres = ksum(fptr, 10**9)

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
        real(_pr) :: dsum
        integer :: i

        dsum = 0
        !$omp parallel default(none) shared(dsum, lim) shared(fptr)
            !$omp do schedule(static) reduction(+: dsum)
            do i = 1, lim
                dsum = dsum + fptr(i)
                if (mod(i, 10000000)==0) call progress(i, lim)
            end do
            !$omp end do
        !$omp end parallel
    end function dsum

    function ksum(fptr, lim)
        integer, intent(in) :: lim
        procedure(term), pointer, intent(in) :: fptr
        real(_pr) :: st, t, y, ksum
        integer :: i

        ksum = 0
        !$omp parallel default(none) shared(ksum, lim, fptr) private(st, y, t)
            st = 0
            !$omp do schedule(static) reduction(+: ksum)
            do i = 1, lim
                y = fptr(i) - st
                t = ksum + y
                st = (t - ksum) - y
                ksum = t
                if (mod(i, 10000000)==0) call progress(i, lim)
            end do
            !$omp end do
        !$omp end parallel
    end function ksum

    subroutine progress(j, lim)
        use store
        integer, intent(in) :: j, lim
        integer :: k, l, p
        character(len=71) :: bar
        common p

        bar="\rCalculation: ???% [                                                  ]"

        l=(100.d0*j)/lim
        if (perc .lt. l) perc = l

        write(unit=bar(15:17), fmt="(i3)") perc
        do k = 1, perc/2
            bar(20+k:20+k)="="
        end do

        write(*,'(a)', advance='no') bar
    end subroutine progress

end program output
