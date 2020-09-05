! File: c-graph.F90
!
! GNU C-Graph Version 2.0
! October 2011
! 
! Copyright (c) 1982, 1983, 1996, 2008, 2009, 2010, 2011 Adrienne Gaye
! Thompson, Sole Author. GNU C-Graph version 2.0. Derived from
! BSc. dissertation "Interactive Computer Package Demonstrating:
! Sampling Convolution and the FFT", University of Aberdeen, Scotland
! (1983). For the code from the dissertation, visit
! <http://codeartnow.com/law-project>.
!
! This file is part of GNU C-Graph.
!
! GNU C-Graph is free software licensed under the terms of the GNU
! General Public License (the GNU GPL) version 3, or (at your option)
! any later version. Under the GPL section 5 and as an additional
! requirement under section 7, all modified copies of this code must
! include in the interactive user interface and their Appropriate
! Legal Notices, the following text:
!
!   Based on GNU C-Graph version 2.0.1 by Adrienne Gaye Thompson, Sole Author.
!
! GNU C-Graph is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. Details are
! in file "COPYING" in the source package.
!
!

program cgraphv2
  !===============
  ! GNU C-Graph is a tool for visualizing convolution.
  use get_cgopt    
  use fftmod, only: fft, pi
  implicit none
  integer :: n, pow, badinput, kount 
  real :: nn
  real, allocatable :: f1(:), f2(:), con(:)
  complex, allocatable :: fft1(:), fft2(:), ifft12(:)
  character :: gpl, nchar*8
  character(*), parameter :: gpldir = PKGDATADIR
  character(100) :: displayer*200, filetodisplay
  character(100) :: logodisplayer*200, logodisplay

  ! Get command-line options.
  kount = command_argument_count()
  if (kount /= 0) call display_option(kount)


  ! Load pseudo splash screen.
  if (splash) then
     call system("clear")
     logodisplay = gpldir // '/' // 'logo/c-graph.gif'
     write(logodisplayer, '(2a)' )  'animate -pause 6 -loop 1 &
          &-title "GNU C-Graph" -geometry -0-0 ', logodisplay
     call system(logodisplayer)
  end if


  ! Introductory Messages
  call system ("clear")
  write(*,"(//,a,/)") 'This is GNU C-Graph version 2.0.1 - &
       &a tool for visualizing convolution.'
  write(*,"(t2,a,/)") 'Dedicated to Eliezer Regnier and all victims of &
       &apartheid.'
  call copyright()

  write(*, '(5(a,/))') 'GNU C-Graph is free software licensed under &
       &the terms of the GNU General',&
       'Public License (the GNU GPL) version 3, or &
       &(at your option) any later',&
       'version. Under the GPL section 5 and as an &
       &additional requirement under',&
       'section 7, all modified copies of this code &
       &must include in the interactive',&
       'user interface and their Appropriate Legal &
       &Notices, the following text:'

  write(*, '(t3,a,/)') 'Based on GNU C-Graph version 2.0.1 &
       &by Adrienne Gaye Thompson, Sole Author.'

  write(*, '(3(a,/))') 'GNU C-Graph is distributed in the hope &
       &that it will be useful, but WITHOUT',&
       'ANY WARRANTY; without even the implied warranty of &
       &MERCHANTABILITY or FITNESS',&
       'FOR A PARTICULAR PURPOSE. Details are in file "COPYING" &
       &in the source package.'

  write(*, "(a)") 'Press <g> then <ENTER> to display the GPL, &
       & or just press <ENTER> to continue.'
  call regnier(gpl)
  if (gpl=='g'.or. gpl=='G') then
     filetodisplay = gpldir // '/' //  'COPYING'
     write(displayer, '(2a)')   'more ', filetodisplay
     write(displayer, '(2a)' )  'less -P "GPL - <SPC> to continue, &
          & "b" to scroll back, "q" to quit." ', filetodisplay
     call system(displayer)
  end if

  call system ("clear")         ! Introduce C-Graph.
  write(*,'(a,//)') '~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~'
  write(*,'(a)') 'THIS IS GNU C-Graph - a tool for visualizing convolution.'
  write(*,"(//,a,/)") 'Compare the linear convolution of two&
       & signals with their circular convolution.'

  call signal()                 ! Display signals menu.
  write(*,"(//,a,/)") 'Generate 2 signals from the above menu &
       &with up to 5 parameters: '
  write(*,'(/a,t5,a)') '1.', 'Number of samples "N" ',&
       '2.', 'Signal code "A" to "H" ',&
       '3.', 'Whether the signal, if periodic, is a wave "w" & 
       &or a pulse "p" ',&
       '4.', 'Frequency "f" if periodic ',&
       '5.', 'Scaling coefficient "sc" '

  ! Begin Session
  write(*,"(/,a)") 'Choose a value for "N" between 64 and 1024.'
  call regnier(nchar,nn)
  if (nn < 64 .or. nn > 1024)  nn=512  !Set default number of samples.
  pow=nint(log(nn)/log(2.)) 
  n=2**pow
  write(*, '(a,i6)') 'The number of samples "N" is:', n
  allocate (f1(0:n-1), f2(0:n-1), con(0:2*n-1),&
       fft1(0:n-1), fft2(0:n-1), ifft12(0:n-1))
  call afun(f1, f2)   ! Choose signals and parameters
  call convo()        ! Compute the linear convolution
  call transformer()  ! Compute the circular convolution
  call replicant()    ! Plot the graphs with Gnuplot
  call uen4059()   ! Save graphs, command files and data
  deallocate (f1, f2, con, fft1, fft2, ifft12)
  write(*,'(a,/,a,/)')"Exiting GNU C-Graph ...", "Bye."
  call copyright()


CONTAINS

  integer function periodic(s)
    !=========================== 
    character (*) :: s
    character(*), parameter :: aperiodic='fghFGH'
    periodic=INDEX(aperiodic, s)
  end function periodic


  subroutine signal()
    !==================
    write(*,"(//)")
    write(*,"(a,t20,a)") 'Signal', 'Code',&
         & '======','====',&
         &'Sine', 'A', 'Cosine', 'B',&
         & 'Triangle', 'C', 'Square', 'D',&
         &'Sawtooth', 'E', 'Exponential', 'F',&
         &'Ramp', 'G', 'Step', 'H'
  end subroutine signal

  subroutine afun(fa, fb)
    !====================== 
    character (1) :: s, wp    
    integer :: t
    real :: sc
    real, intent(out) :: fa(0:n-1), fb(0:n-1)

    call signal() 
    write(*,"(/, a)") 'Enter code for first signal'
    call getparam (s,wp,t,sc)   
    call sigselect(fa,s,wp,t,sc)
    call signal()
    write(*,"(/, a)") 'Enter code for second signal'
    call getparam (s,wp,t,sc)
    call sigselect(fb,s,wp,t,sc,'defsig')
  end subroutine afun


  subroutine getparam (s,wp,t,sc)
    !============================== 
    character(len=*) :: s, wp
    character (8) :: numchar
    integer :: k, t, scc
    real :: fp, sc

    call regnier(s)
    k = periodic(s)
    if (k == 0) then
       write(*,'(/,a)') 'Is the signal periodic or is it a pulse?'
       write(*,'(a)') 'Type "w" for periodic wave, or "p" for pulse.'

       call regnier(wp)
       write(*,'(/,a)') 'Select the frequency "f" of this signal.'
       call regnier(numchar, fp); fp=abs(fp)
       if (fp < 0.5 .or. fp >n/4) fp=1 ! Set default frequency.
       t=4* nint(real(n)/(4*fp))
       write(*,'(/,a,f6.2,a)') "The frequency of this signal &
            &is " ,real(n)/t ,"Hz."
    end if
    write(*,'(3(/,a))') "Do you wish to scale this signal?", &
         "Enter a value for the scaling coefficient 'sc'.",&
         "A coefficient of 1 will give the unit function."
    scc = eliezer(s)
    call regnier(numchar, sc)

    if(scc==0) then
       if(abs(sc) > 1) sc =1
    else if (abs(sc) >n) then
       sc=1
    end if
  end subroutine getparam


  recursive subroutine sigselect(f,s,wp,t,sc, defsig)
    !================================================== 
    integer, intent(in) :: t
    real :: sc
    real, intent(out) :: f(0:n-1)
    character (1), intent(inout) :: s
    character (1), intent(in) :: wp
    character(*), intent(in), optional :: defsig

    select case(s)
    case('a','A')
       call cgsin(f,wp,t,sc)
    case('b','B')
       call cgcosin(f,wp,t,sc)
    case('c','C')
       call tri(f,wp,t,sc)
    case('d','D')
       call square(f,wp,t,sc)
    case('e','E')
       call sawtooth(f,wp,t,sc)
    case('f','F')
       call exponential(f,sc)
    case('g','G')
       call ramp(f,sc)
    case('h','H')
       call step(f,sc)
    case default
       if(present(defsig)) then
          write(*,'(/,a)') 'You selected a square signal by default.'
          s='d'
       else
          write(*,'(/,a)') 'You selected a triangle signal by default.'
          s='c'
       end if
       call sigselect(f,s,wp,t,sc)
    end select
  end subroutine sigselect


  subroutine cgsin(f,wp,t,sc)
    !==========================
    character :: wp
    integer :: i,t
    real, intent(out) :: f(0:n-1)
    real :: pulse(0:t/2), sc

    if (wp /='w') then          ! Generate pulse by default.
       do i= 0, t/2
          pulse(i) =  sin(2*pi*real(i)/t)  
       end do
       f(0:t/2) = sc * pulse
       f(t/2 + 1 : n-1) = 0     ! Zero-pad. 
    else
       do i= 0, n-1
          f(i)= sc * sin(2*pi*real(i)/t)  
       end do
    end if
  end subroutine cgsin


  subroutine cgcosin(f,wp,t,sc)
    !============================ 
    character :: wp
    integer :: i,t
    real, intent(out) :: f(0:n-1)
    real :: pulse(0:t/2), sc

    if (wp /='w') then          ! Generate pulse by default.
       do i= 0, t/2
          pulse(i) = abs(cos(2*pi*real(i)/t))  
       end do
       f(0:t/2) = sc * pulse
       f(t/2 + 1 : n-1) = 0     ! Zero-pad. 
    else
       do i= 0, n-1
          f(i)= sc * cos(2*pi*real(i)/t)  
       end do
    end if
  end subroutine cgcosin


  subroutine tri(f,wp,t,sc)
    !========================
    character :: wp
    integer :: i,j,t          
    real, intent(out) :: f(0:n-1)
    real :: pulse(0:t/2), sc

    if (wp /='w') then          ! Generate pulse by default.
       do i = 0, (t/2)
          if(i <= t/4) then
             pulse(i) = real(i)
          else
             pulse(i) = real (-i + t/2.0)
          end if
       end do
       f(0:t/2) = sc * pulse
       f(t/2 + 1 : n-1) = 0     ! Zero-pad. 
    else                        ! Generate wave.
       do i = 0, n, t
          do j=0, t-1
             if((i+j) <= n) then
                if(j <= t/2) then 
                   f(i+j) = sc * real(j - t/4.0)
                else
                   f(i+j) = sc * real(-j + .75*t)
                end if
             end if
          end do
       end do
    end if
  end subroutine tri


  subroutine square(f,wp,t,sc)
    !=========================== 
    character :: wp
    integer :: i,j,t
    real, intent(out) :: f(0:n-1)
    real :: sc

    if (wp /='w') then          ! Generate pulse by default.
       f(0:t/2) = sc * real(t/2.0)
       f(t/2 + 1 : n-1) = 0     ! Zero-pad. 
    else                        ! Generate wave.
       do i = 1, n, t
          do j=0, t-1
             if((i+j) < n) then
                if(j <= t/2-1) then
                   f(i+j) = sc * real(t/2)
                else
                   f(i+j) = sc * real(-t/2) 
                end if
             end if
          end do
       end do
    end if
  end subroutine square


  subroutine sawtooth(f,wp,t,sc)
    !=========================== 
    character :: wp
    integer :: i,j,t          
    real, intent(out) :: f(0:n-1)
    real :: pulse(0:t/2), sc

    if (wp /='w') then          ! Generate pulse by default.
       do i = 0, (t/2)
          pulse(i) = real(i)
       end do
       f(0:t/2) = sc * pulse
       f(t/2 + 1: n-1) = 0      ! Zero-pad. 
    else                        ! Generate wave.
       do i = 0, n, t
          do j=0, t-1
             if((i+j) <= n) then
                f(i+j) = sc * real(j - (t-1)/2.0)
             end if
          end do
       end do
    end if
  end subroutine sawtooth


  subroutine exponential(f,sc)
    !=========================== 
    character :: wp
    integer :: i
    real :: sqrtn, sc
    real, intent(out) :: f(0:n-1) 

    sqrtn=sqrt(real(n))
    do i= 0, n-1
       f(i)= sc * exp(real(-i/sqrtn))
    end do
  end subroutine exponential


  subroutine ramp(f,sc)
    !====================
    character :: wp
    integer :: i,t
    real :: sc
    real, intent(out) :: f(0:n-1) 

    do i= 0, n-1
       f(i)= sc * i
    end do
  end subroutine ramp


  subroutine step(f,sc)
    !====================
    character :: wp
    integer :: t
    real :: sc
    real, intent(out) :: f(0:n-1)

    f=sc
  end subroutine step


  subroutine convo()
    !=================
    integer :: i, j
    real, allocatable :: fa(:), fb(:)
    allocate (fa(0:2*n-1), fb(0:2*n-1))

    fa(0:n-1) = f1
    fb(0:n-1) = f2
    open (10, file='time.dat')
    do i=0, 2*n-1
       do j=0,i-1
          con(i)= con(i) + (fa(i-j+1)) * (fb(j))
       end do
       write(10, '(i6, 1x,f10.4,1x,f10.4,1x,f14.4)') i, fa(i), fb(i), &
            con(i)/n
    end do
    close(10)
  end subroutine convo


  subroutine transformer()
    !=======================
    integer :: i

    ! Convert arrays for input to FFT.
    fft1=cmplx(f1)
    fft2=cmplx(f2)

    ! Transform both functions.
    call fft(fft1,n,1)
    call fft(fft2,n,1)

    ! Compute the convolution.
    ifft12 = fft1*fft2
    call fft(ifft12,n,-1)
    open(11, file='trans.dat')
    do i= 0, n-1
       write(11, '(i6, 1x, 2(f10.4,1x),1x, f14.4)') i, abs(fft1(i))/n, &
            & abs(fft2(i))/n, real(ifft12(i))/n
    end do
    close(11)
  end subroutine transformer


  subroutine regnier(law,justice)
    !============================== 
    ! In memory of Eliezer Regnier
    character(len=*) :: law
    integer :: rights
    real, optional :: justice

    haiti: do rights=0,1
       write(*, "(a)", advance='no') 'C-Graph:>> '
       read(*,'(a)') law
       if(present(justice)) then
          read(law, *, iostat=badinput) justice
          if(badinput /=0) then
             if(rights==1) then
                justice=1
             else
                write(*,'(/,a)') "That was not a number. Try again!"
             end if
          else 
             exit haiti
          end if
       else 
          exit haiti 
       end if
    end do haiti
  end subroutine regnier


  integer function eliezer (s)
    !=========================== 
    ! In memory of Eliezer Regnier
    ! Define scaling protocol.
    character (*) :: s
    character(*), parameter :: scale='abfhABFH'
    eliezer=INDEX(scale, s)
  end function eliezer


  subroutine pauser (message)
    !==========================
    character(len=*), optional :: message
    if(present(message)) then
       write(*, '(/,a)',advance='no') message
    else
       write(*, '(/,a)', advance='no') 'Hit <Enter> to continue :>> '
    end if
    read(*,*)
  end subroutine pauser


  subroutine uen4059()
    !======================
    ! Make directories to store images, command, and data files.
    !
    ! "uen4059" is the user id for Adrienne's account on the Honeywell 
    ! mainframe at the University of Aberdeen, where she authored the 
    ! original code in 1982-1983 for the dissertation that became "C-Graph".
    call system("mkdir -p c-graphs/graphs c-graphs/coms")
    call system("mv -f sig*.png trans*.png convo*.png c-graphs/graphs")
    call system("mv -f sig*.cg trans*.cg convo*.cg c-graphs/coms")
    call system("mv -f time.dat trans.dat c-graphs/coms")
  end subroutine uen4059


  subroutine copyright()
    write(*, "(4(t2,a,/))") &
         'Copyright (c) 1982, 1983, 1996, 2009, 2010, 2011 &
         &Adrienne Gaye Thompson,',&
         'Sole Author. GNU C-Graph version 2.0. Derived from &
         &BSc. dissertation',&
         '"Interactive Computer Package Demonstrating: Sampling &
         &Convolution and',&
         'the FFT", University of Aberdeen, Scotland (1983).'
  end subroutine copyright


  subroutine replicant()
    !=====================
    !=====================
    ! Invoke Gnuplot for graphs.  
    integer :: i
    real :: maxf1, maxf2, maxcon, maxfft1, maxfft2, maxifft12,maxf12
    maxf1=maxval(abs(f1))
    maxf2=maxval(abs(f2))
    maxcon=maxval(abs(con)/n)
    maxfft1=maxval(abs(fft1)/n)
    maxfft2=maxval(abs(fft2)/n)
    maxifft12=maxval(abs(ifft12))/n
    maxf12=max(maxf1,maxf2)

    ! Display Signals in the Time Domain.
    ! ===================================
    call pauser ('Press <Enter> to see the signals in the time domain:>> ')
    open (3, file='signals.cg')
    write(3,*) 'set term x11'
    write(3,*) 'set object 1 rect from screen 0,0 to screen 1,1 behind&
         & fc rgb"#d3d3d3" '
    write(3,*) 'set key left top vertical reverse'
    write(3,*) 'set zeroaxis lt -1'
    write(3,*) 'set xrange [0:' ,n-1, ']'
    write(3,*) 'set yrange [' ,-1.4*maxf12 ,':' ,1.4*maxf12 ,']'
    write(3, *) 'set title "TIME DOMAIN"; set xlabel "Sample Number";&
         & set ylabel "Amplitude" '
    write(3, *) 'plot "time.dat" using 1:2 with filledcurves y1=0 &
         & lc rgb "#447744" t "Signal 1" '
    write(3, *) 'set label "GNU C-Graph" at screen .01, screen .023 &
         &font "serif, 14, bold" tc rgb "#447744" '
    write(3,*) 'pause 1.5'
    write(3, *) 'replot "time.dat" using 1:3 with lines &
         & lc rgb "#cc9900" lw 3 t "Signal 2" '
    write(3,*) 'set term png ; set out "signals.png"; &
         & replot; set term pop'
    write(3,*) 'pause -1 "Hit <Enter> to continue:>> " '
    write(3, *) 'reset'
    close(3)
    call system ('gnuplot signals.cg')
    write(*,'(/,a)') "View the frequency-domain representation of the &
         & signals."
    call pauser ('Press <Enter> to see their FFTs:>> ')

    ! Display Fourier Transforms.
    ! ===========================
    open (4, file='transforms.cg')
    write(4,*) 'set term x11'
    png_reset1: do i=0,1
       if (i == 1)  write(4,*) 'set term png font "arial, 10"; &
            & set out "transforms.png"'
       ! Set up bounding boxes
       write(4,*) 'set size 1,1'
       write(4,*) 'set origin 0,0'
       write(4,*) 'set multiplot'

       ! Plot fourier transform of first signal.
       write(4,*) 'set size .5,1'
       write(4,*) 'set origin 0,0'
       write(4,*) 'set object 1 rect from graph 0,0 to graph 1,1 behind &
            & fc rgb"#d3d3d3" '
       write(4,*) 'set xrange [0:' ,(n/2) -1, ']'
       write(4,*) 'set yrange [0:' ,1.3*maxfft1 ,']'
       write(4,*) 'set xtics nomirror; set ytics nomirror'
       write(4, *) 'set title "FFT SIGNAL 1";&
            & set xlabel "Frequency in hz"; set ylabel "Amplitude/N" '
       write(4,*) 'plot "trans.dat" using 1:2 with impulses &
            & lc rgb "#447744" lw 3 notitle ; reset'

       ! Plot time data as inset.
       write(4,*) 'set size .2,.23'
       write(4,*) 'set origin .3,.65'
       write(4,*) 'set object 2 rect from graph 0,0 to graph 1,1 back &
            & fc rgb "white" '
       write(4,*) 'set zeroaxis lt -1'
       write(4,*) 'set xrange [0:' ,n-1, ']; set noxtics'
       write(4,*) 'set yrange [' ,-1.2*maxf1 ,':' ,1.2*maxf1 ,']; set noytics'
       write(4,*) 'set nokey'
       write(4, *) 'plot "time.dat" using 1:2 notitle with filledcurves y1=0 &
            & lc rgb "#447744" '
       write(4,*) 'reset'

       ! Plot Transform of second signal.
       write(4,*) 'set size .5,1'
       write(4,*) 'set origin .5,0'
       write(4,*) 'set object 3 rect from graph 0,0 to graph 1,1 behind &
            & fc rgb"#d3d3d3" '
       write(4,*) 'set xrange [0:' ,(n/2) -1, ']'
       write(4,*) 'set yrange [0:' ,1.3*maxfft2 ,']'
       write(4,*) 'set xtics nomirror; set ytics nomirror'
       write(4, *) 'set title "FFT SIGNAL 2";&
            & set xlabel "Frequency in hz"; set ylabel "Amplitude/N" '
       write(4,*) 'plot "trans.dat" using 1:3 with impulses lc rgb "#cc9900" &
            & lw 3 notitle'
       write(4,*) 'reset '

       ! Plot time data as inset.
       write(4,*) 'set size .2,.23'
       write(4,*) 'set origin .8,.65'
       write(4,*) 'set object 4 rect from graph 0,0 to graph 1,1 behind &
            & fc rgb "white" '
       write(4,*) 'set zeroaxis lt -1'
       write(4,*) 'set xrange [0:' ,n-1, ']; set noxtics'
       write(4,*) 'set yrange [' ,-1.2*maxf2 ,':' ,1.2*maxf2 ,']; set noytics'
       write(4,*) 'set nokey'
       write(4, *) 'set label "GNU C-Graph" at screen .01, screen .023 &
            &font "serif, 12, bold" tc rgb "#447744" '
       write(4, *) 'plot "time.dat" using 1:3 notitle with filledcurves y1=0 &
            & lc rgb "#cc9900" '
       write(4,*) 'reset'
       write(4,*) 'unset multiplot'
       if (i == 1) write(4,*) 'set term pop'
    end do png_reset1
    write(4,*) 'pause -1 "Hit <Enter> to continue:>> " '
    close(4)
    call system ('gnuplot transforms.cg')
    write(*,*) ' '
    call pauser("Press <Enter> to compare linear and circular convolution:>> ")

    ! Display Linear & Circular Convolution.
    ! ======================================
    open (7, file='convolutions.cg')

    ! Plot Linear Convolution.  
    write(7,*) 'set term x11'
    png_reset2: do i=0,1
       if (i == 1)  write(7,*) 'set term png font "arial, 10"; &
            & set out "convolutions.png"'
       write(7,*) 'set size 1,.5'
       write(7,*) 'set origin 0,.5'
       write(7,*) 'set multiplot'
       write(7,*) 'set object 1 rect from graph 0,0 to graph 1,1 behind &
            & fc rgb"#d3d3d3" '
       write(7,*) 'set zeroaxis lt -1'
       write(7,*) 'set xrange [0:' ,(2*n) -1, ']'
       write(7,*) 'set yrange [' ,-1.4*maxcon ,':' ,1.4*maxcon , ']'
       write(7,*) 'set xtics nomirror; set ytics nomirror'
       write(7, *) 'set title "LINEAR CONVOLUTION" '
       WRITE(7,*) ' set xlabel "Sample Number"; set ylabel "Amplitude" '
       write(7,*) 'plot "time.dat" using 1:4 with filledcurves y1=0 &
            & lc rgb "#447744" lw 3 notitle'

       ! Plot Circular Convolution.
       write(7,*) 'set size 1,.5'
       write(7,*) 'set origin 0,0'
       write(7,*) 'set object 3 rect from graph 0,0 to graph 1,1 behind &
            & fc rgb"#d3d3d3" '
       write(7,*) 'set zeroaxis lt -1'
       write(7,*) 'set xrange [0:' ,(2*n) -1, ']'
       write(7,*) 'set yrange [' ,-1.4*maxifft12 ,':' ,1.4*maxifft12 , ']'
       write(7,*) 'set xtics nomirror; set ytics nomirror'
       write(7, *) 'set title "CIRCULAR CONVOLUTION" '
       write(7,*) ' set xlabel "Sample Number"; set ylabel "Amplitude" '
       write(7, *) 'set label "GNU C-Graph" at screen .01, screen .023 &
            &font "serif, 14, bold" tc rgb "#447744" '
       write(7,*) 'plot "trans.dat" using 1:4 with filledcurves y1=0 &
            & lc rgb "#cc9900" lw 3 notitle '
       write(7,*) 'unset multiplot'
       if (i == 1) write(7,*) 'set term pop'
    end do png_reset2

    write(7,*) 'pause -1 "Hit <Enter> to continue:>> " '
    close(7) 
    call system ('gnuplot convolutions.cg')
  end subroutine replicant

end program cgraphv2


