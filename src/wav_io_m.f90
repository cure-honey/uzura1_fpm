    module wav_io_m
        use kind_m
        use file_io_m
        implicit none
        private
        public :: wavfile_t, open_wav_file
        type :: fmt_t
            sequence
            character(4):: chunk_id
            integer(int32) :: chunk_size
            integer(int16) :: format_id, channels
            integer(int32) :: sampling_rate
            integer(int32) :: bytes_per_sec
            integer(int16) :: block_size, bits_per_sample
        end type fmt_t

        type :: data_t
            sequence
            character(4) :: chunk_id
            integer(int32)  :: chunk_size
            !! integer(int16), allocatable :: pcm16(:)
        end type data_t

        type :: riffwav_t
            sequence
            character(4)  :: chunk_id
            integer(int32):: chunk_size
            character(4)  :: formattag
            type (fmt_t ) :: fmt
            type (data_t) :: dat
        end type riffwav_t

        type, extends(file_t) :: wavfile_t
            private
            integer :: iunit 
            integer :: ipos
            character(:), allocatable :: fn
            type (riffwav_t) :: riff
            integer(int16), allocatable :: pcm16(:) ! <- data of data_chunk
        contains
            procedure, public :: open_file  => open_wav
            procedure, public :: close_file => close_wav
            final             :: destroy_file
            procedure, public :: pcm1frame
            procedure, public :: get_channel
            procedure, public :: get_sampling_rate
            procedure, public :: get_data_size
            procedure, public :: get_play_time
        end type wavfile_t
    contains
        subroutine open_wav_file(this, fn)
            type(wavfile_t), intent(out), allocatable :: this 
            character(len = *), intent(in) :: fn
            allocate(this)
            call this%open_file(fn)
        end subroutine open_wav_file
 
        subroutine open_wav(this, fn)
            class(wavfile_t), intent(in out) :: this
            character(len = *), intent(in) :: fn
            integer :: io
            this%fn = fn
            open(newunit = this%iunit, file = trim(this%fn), access = 'stream', iostat = io, status = 'old', form = 'unformatted')
            if (io /= 0) then
                write(*, *) 'i/o error ', io, ' occuerred. file =', this%iunit, 'file name ', fn
                stop 'check input file!'
            end if
            ! read wave file little endian assumed
            associate (riff => this%riff, fmt => this%riff%fmt, dat => this%riff%dat)  
                ! riff-wave chunk
                read(this%iunit) riff 
                if ( riff%chunk_id  /= 'RIFF' ) stop 'this is not RIFF file'
                if ( riff%formattag /= 'WAVE' ) stop 'this RIFF file is not in WAVE format'
                ! fmt chunk
                if ( fmt%chunk_id   /= 'fmt ' ) stop 'fmt chunk not found'
                if ( fmt%format_id  /=  1     ) stop 'unknown wave format' ! 1 linear pcm
                if ( fmt%bits_per_sample /= 16) stop 'not 16bit data'
                select case ( fmt%channels )
                case (1)
                    write(*, '(a, i3, a, i6, a)') 'monoral', fmt%bits_per_sample, 'bit sampling rate', fmt%sampling_rate, 'hz '
                case (2)
                    write(*, '(a, i3, a, i6, a)') 'stereo' , fmt%bits_per_sample, 'bit sampling rate', fmt%sampling_rate, 'hz '
                case default
                    stop 'wave channels must be 1 or 2'
                end select
                ! data chunk
                if (dat%chunk_id /= 'data') then
                    do      
                        inquire(this%iunit, pos = this%ipos)
                        this%ipos = this%ipos + dat%chunk_size  ! skip non-data chunk
                        read(this%iunit, pos = this%ipos, iostat = io) dat
                        if (io == -1) stop 'end of file encounterd while searching for a data chunk'
                        if (dat%chunk_id == 'data') exit
                    end do
                end if
                ! now file position is at the beginning of pcm data
                allocate( this%pcm16( dat%chunk_size / 2 ) )
                ! read whole pcm data
                read(this%iunit) this%pcm16 
            end associate
        end subroutine open_wav 

        subroutine close_wav(this)           
            class(wavfile_t), intent(in) :: this
            close(this%iunit)
        end subroutine close_wav
  
        subroutine destroy_file(this) ! finalization routine
            type(wavfile_t), intent(in) :: this
            call this%close_file()
        end subroutine destroy_file  
  
        subroutine pcm1frame(this, pcm) ! copy from array
            class(wavfile_t), intent(in) :: this
            real(kd), intent(in out) :: pcm(:, :)
            integer, save :: ipos = 1
            pcm = eoshift(pcm, -384, 0.0_kd, 1)
            select case (this%riff%fmt%channels)
            case (1) !mono
                pcm(384:1:-1, 1) = real( this%pcm16(ipos:ipos + 384 - 1), kind = kd ) / 2**15 
                ipos = ipos + 384
            case (2) !stereo
                pcm(384:1:-1, 1) = real( this%pcm16(ipos    :ipos + 2 * 384 - 1:2), kind = kd ) / 2**15 ! L 16bit int
                pcm(384:1:-1, 2) = real( this%pcm16(ipos + 1:ipos + 2 * 384 - 1:2), kind = kd ) / 2**15 ! R
                ipos = ipos + 2 * 384
            case default
                stop 'ichannel must be 1 or 2: subroutine wav_get' 
            end select
        end subroutine pcm1frame
        ! getters
        integer function get_channel(this)
            class(wavfile_t), intent(in) :: this
            get_channel = this%riff%fmt%channels ! mono:1 stereo:2
        end function get_channel
 
        integer function get_sampling_rate(this)
            class(wavfile_t), intent(in) :: this
            get_sampling_rate = this%riff%fmt%sampling_rate ! in Hz
        end function get_sampling_rate
 
        integer function get_data_size(this)
            class(wavfile_t), intent(in) :: this
            get_data_size = this%riff%dat%chunk_size ! in bytes
        end function get_data_size
    
        integer function get_play_time(this) result(isec)
            class(wavfile_t), intent(in) :: this
            isec = this%riff%dat%chunk_size / this%riff%fmt%bytes_per_sec ! play time in sec
        end function get_play_time
    end module wav_io_m
