    module fft_m
        use kind_m
        implicit none
        private
        public :: fft_window
        integer , parameter :: np2 = 9, nn = 2**np2, nn_2 = nn / 2   ! 2^9 = 512
        real(kd), parameter :: pi = 4 * atan(1.0_kd), pi2 = 2 * pi
        real(kd), parameter :: pi2_n = pi2 / nn
        integer, save :: indx(nn) 
        integer, private :: i_
        complex(kd), parameter :: omega(0:*) = [(exp(cmplx(0.0_kd, pi2_n * i_, kind = kd)), i_ = 0, nn - 1)] 
        real(kd)   , parameter :: hann_window(*) = [(0.5_kd * (1.0_kd - cos(pi2_n * i_))  , i_ = 0, nn - 1)]
    contains
        subroutine fft_initialize()
            integer :: i, j2, k, n
            do i = 1, nn
                n = 0
                k = i - 1
                do j2 = np2 - 1, 0, -1
                    n = n + mod(k, 2) * 2**j2
                    k = k / 2
                end do
                indx(i) = n + 1 ! indx = 1..n
            end do
        end subroutine fft_initialize
 
        subroutine fft_window(x, y)
            real    (kd), intent(in ) :: x(:)
            complex (kd), intent(out) :: y(:)
            logical :: qfirst = .true.
            if (qfirst) then
                qfirst = .false.
                call fft_initialize()
            end if
            y = hann_window * x / nn
            call fft2(y)
        end subroutine fft_window

        subroutine fft2(fft) ! fft_2^np2
            complex (kd), intent(in out) :: fft(:)
            complex (kd) :: tmp1, tmp2, c1
            integer :: i, j, k2, m1, m2, iphase, kn2
            fft = fft(indx)
            do k2 = 1, np2
                kn2 = 2**(k2 - 1)
                do i = 1, nn, 2* kn2
                    do j = 1, kn2
                        iphase = 2**(np2 - k2)*(j - 1)
                        c1 = omega( mod(iphase, nn) )
                        m1 = i + j - 1
                        m2 = m1 + kn2
                        tmp1 =      fft(m1)
                        tmp2 = c1 * fft(m2)
                        fft(m1) = tmp1 + tmp2 ! 1 = x^2 ; x = 1 or -1
                        fft(m2) = tmp1 - tmp2
                    end do
                end do
            end do
        end subroutine fft2
    end module fft_m
