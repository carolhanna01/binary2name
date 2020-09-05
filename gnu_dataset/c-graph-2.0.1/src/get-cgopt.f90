! File: get-cgopt.f90
!
! GNU C-Graph Version 2.0
! October 2011
! 
! Copyright (c) 2011 Adrienne Gaye Thompson
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

module get_cgopt
  implicit none
  logical :: splash = .true.


CONTAINS

  subroutine display_option(count)
    integer :: opcount, count
    character(12) :: option
!    real :: version=2.0
    character(6) :: version = "2.0.1"


    do opcount = 1, count
       call get_command_argument(opcount, option)
       select case (option)
       case ('-n', '--no-splash')
          splash = .false. ; return
       case ('-d', '--dedicate')
          call get_dedicate()
       case ('-h', '--help')
          call get_help()
       case ('-v', '--version')
          call get_version(version)
       case default
          write(*, '(a,a,//)') option , "is an invalid option."
       end select
    end do
    stop
  end subroutine display_option

  
  subroutine get_dedicate() 
    write(*,'(4(/,a),//,t20,a)') 'GNU C-Graph is dedicated to the victims &
         &of apartheid wherever they may be,', &
         'and is devoted to the friendship and memory of Haitian attorney &
         &and human rights', 'advocate Eliezer Regnier who died on 27 &
         &February 2010, as a result of', 'the struggle for justice. &
         &May his soul rest in peace.'

    write(*, '(/,T12,a)') "for  ALL the VICTIMS of APARTHEID STRUGGLING &
         &to be FREE"
    write(*, '(T37,a)') "and"
    write(*, '(T34,a,/)') "to REGNIER"
    write(*, '(T12,a)') "You're sending me discrete signals from across &
         &the room,"
    write(*, '(T12,a)') "I respond on impulse, reflecting on the sampling &
         &of events"
    write(*, '(T10,a)') "That were a dichotomy from the day you left your &
         &mother's womb"
    write(*, '(T8,a)') "Multiplied in frequency, integrated in time, a &
         &weighted confluence"
    write(*, '(a)') "Of sliding shifting trains of thought, alternative &
         &messages under transformation"
    write(*, '(T7, a)') "Counterpoint, duality, involution, &
         &contradistinction without confusion"
    write(*, '(T8, a,/)') "Independence in summation. Silence - this is &
         &convoluted conversation"
    write(*, '(T27,a,/)') "-- Adrienne Gaye Thompson"
  end subroutine get_dedicate


  subroutine get_version(vers)
!    real :: vers
    character(*) :: vers

!    write(*, '(/,a,f3.1)') 'This is GNU C-Graph version ' ,vers
    write(*, '(/,2a)') 'This is GNU C-Graph version ' ,vers
    write(*, '(a,/)') 'Authored by Adrienne Gaye Thompson (agt@codeartnow.com)'
    write(*, '(a)') 'Copyright (c) 1982, 1983, 1996, 2008, 2009, 2010, 2011 &
         &Adrienne Gaye Thompson. See the file COPYING for the full notice.'
    write(*, '(a)') 'C-Graph is licensed under the GPL version 3 or later'
    write(*, '(a,/)') '<http://gnu.org/licenses/gpl.html>'
    write(*, '(a)') 'This is free software; you are free to modify&
         & and distribute it.'
    write(*, '(a,/)') 'There is NO WARRANTY, to the extent permitted by law.'
  end subroutine get_version


  subroutine get_help()
    write(*,'(/,a,/)' ) 'Usage: c-graph [OPTION]...'
    write(*,'(a,t20,a,/)') '-d, --dedicate','print the dedication and exit'
    write(*,'(a,t20,a)') '-h, --help' ,'print these help options and exit'
    write(*,'(a,t20,a,/)') '-n, --no-splash','invoke GNU C-Graph &
         &with no splash screen'
    write(*,'(a,t20,a)') '-v, --version','print this version of GNU &
         &C-Graph and exit'
    write(*,'(/,a,/)' ) 'Report bugs to <agt@codeartnow.com>.'
  end subroutine get_help
end module get_cgopt
