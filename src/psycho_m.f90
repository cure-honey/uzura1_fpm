    module psycho_m
        use kind_m
        use fft_m
        implicit none
        private
        public :: psychoacoustics
        real(kd), save :: freq_fft(256), pseud_bark(256), ath_fft(256)
        real(kd), save :: crbw_fft(256), cbwl_fft(256)
        real(kd), save :: sp(256, 256) ! spreading function for masking
        real(kd), save :: scale
        real(kd), parameter :: alpha = 0.27_kd ! non-linear factor
    contains
        function psychoacoustics(pcm, isample_rate) result(smr) ! impure
            real (kd), intent(in) :: pcm(:, :) 
            integer  , intent(in) :: isample_rate
            real (kd) :: smr(32, size(pcm, 2))
            complex(kd) :: cfft(512)
            integer :: ichannel, i0, i1
            logical, save :: qfirst = .true.
            if (qfirst) then 
                qfirst = .false.
                call init_absolute_threshold(isample_rate)
            end if
            i0 = 161 
            i1 = i0 + 512 - 1
            do ichannel = 1, size(pcm, 2)
                call fft_window(pcm(i0:i1, ichannel), cfft)
                call calc_smr(cfft, smr(:, ichannel))
            end do
        end function psychoacoustics
 
        subroutine init_absolute_threshold(isample_rate)
            integer, intent(in) :: isample_rate
            real (kd) :: freq, tmp
            integer :: i, m
            do i = 1, size(freq_fft)
                freq = real(isample_rate, kind = kd) / 2.0_kd / 1000.0_kd &
                     * real(i - 1, kind = kd) / real(size(freq_fft), kind = kd)
                ath_fft(i) = 3.64_kd * freq**(-0.8_kd)  & ! 
                           - 6.5_kd * exp(-0.6_kd * (freq - 3.3_kd)**2) + 0.001_kd * freq**4.0_kd   
                freq = freq * 1000.0_kd ! khz -> hz
                freq_fft(i) = freq
            end do
            scale =  real(isample_rate, kind = kd) / 2 / 256 ! freq to fft-line scale
            crbw_fft = critical_band_width( freq_fft )       ! critical band width in hz
            cbwl_fft = decibel( crbw_fft )                   ! critical band width in log
            tmp = 0.0_kd ! pseud bark: integrate critical band  
            do m = 1, 256
                tmp = tmp + 1.0_kd / crbw_fft(m) ! integration
                pseud_bark(m) = tmp * scale 
            end do
            ! spreading function
            forall(i = 1:256, m = 1:256) sp(i, m) = spreading( pseud_bark( i ) - pseud_bark( m ) ) ! top normalized to 1.0
        end subroutine init_absolute_threshold

        pure elemental real(kd) function spreading(z)
            real(kd), intent(in) :: z ! pseud-bark
            if ( z > 0.0_kd ) then
                spreading = -25.0_kd * z 
            else
                spreading =  75.0_kd * z 
            end if
            spreading = max(-160.0_kd, spreading)
        end function spreading

        pure elemental real(kd) function critical_band_width(f)
            real(kd), intent(in) :: f ! hz
            real(kd), parameter :: gamma = 0.69_kd
            critical_band_width = 25.0_kd + 75.0_kd * ( 1.0_kd + 1.4_kd * (f / 1000.0_kd)**2 )**gamma 
            !  critical_band_width = 100.0_kd * ( 1.0_kd + 1.0_kd * (f / 1000.0_kd)**2 )**gamma 
        end function critical_band_width

        pure elemental real(kd) function decibel(x) 
            real (kd), intent(in) :: x 
            decibel = 10.0e0_kd * log10( max(x, 1.0e-100_kd) )
        end function decibel
      
        subroutine calc_smr(cfft, smr)
            complex(kd), intent(in) :: cfft(:)
            real   (kd), intent(out) :: smr(:)
            real   (kd) :: snr(32), amnr(32)
            real   (kd) :: xa(256), ya(256), za(256)
            integer :: iband, i, m, i0, i1
            xa = 2 * decibel( abs(cfft(:256)) )
            xa(1) = 0.0_kd ! dc cut
            ! convolution of spreading function
            ya = 0.0_kd
            do i = 1, 256
                do m = 1, 256 ! i maskee, m masker 
                    ya(i) = ya(i) + 10.0_kd**( ((sp(i, m) + xa(m) - ath_fft(m)) * alpha - cbwl_fft(m)) / 10.0_kd ) ! non-linear sum
                end do
            end do
            ya = max(decibel(ya * scale) / alpha - 11.5_kd, ath_fft - 90.3_kd) ! 11.5 mask factor, 90.3dB = 2^15  ATH shift empirical 
            ! effective spl
            do i = 1, 256
                m = nint( crbw_fft(i) / scale )
                i0 = max(i  - m / 2,   1)       !f0 - bw(f0) / 2 
                i1 = min(i0 + m - 1, 256)       !f0 + bw(f0) / 2
                za(i) = sum( 10.0_kd**( (xa(i0:i1) * alpha  - cbwl_fft(i)) / 10.0_kd ) ) 
            end do
            za = decibel(za * scale) / alpha
            ! smr = snr' - mnr'
            do iband = 1, 32
                m = (iband - 1) * 8 + 1
                i0 = m - nint( crbw_fft(m) / 2 / scale ) ! fl - bw(fl) / 2  ; subband [fl..fh] 
                i0 = max(i0,   1)
                m = m + 7
                i1 = m + nint( crbw_fft(m) / 2 / scale ) ! fh + bw(fh) / 2 
                i1 = min(i1, 256)
                snr(iband)  = maxval( za(i0:i1) )
                amnr(iband) = minval( ya(i0:i1) ) 
            end do
            smr = snr - amnr 
        end subroutine calc_smr
    end module psycho_m
