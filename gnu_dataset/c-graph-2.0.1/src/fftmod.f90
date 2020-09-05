! File: fftmod.f90
!
! GNU C-Graph Version 2.0
! October 2011
!
! PUBLIC DOMAIN
! Authors:
! 1) Arthur Wouk: The original F77 code.
! 2) Alan Miller:  Modifications converting the code to F90
! <http://jblevins.org/mirror/amiller/fft_simple.f90>
!
! GNU C-Graph author, Adrienne Thompson has trivially modified this 
! code for use with single precision data, and to include it in a 
! module sharing the variable "PI" used in the main program cgraphv2.f90. 
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

module fftmod
  implicit none
  real, parameter :: pi=3.14159

contains


  !  It runs and computes correctly although it is not in any sense
  !  sophisticated.  However, it is free, and useful if you are in a hurry
  !  and don't want to think things out for yourself.

  !  Arthur Wouk (wouk@brl-vgr)

  !  *******************
  !  Fast Fourier Transform
  !  *******************

  ! Code converted using TO_F90 by Alan Miller
  ! Date: 2003-07-09  Time: 17:02:48

  SUBROUTINE fft(x, n, k)
    !    FFT COMPUTES THE (FAST) FOURIER TRANSFORM OF THE VECTOR X
    !    (A COMPLEX ARRAY OF DIMENSION N).
    !    SOURCE: Joel H. Ferziger; Numerical methods for engineering 
    !    applications.
    !    2nd edition, Wiley (1998).

    !    X = DATA TO BE TRANSFORMED; ON RETURN IT CONTAINS THE TRANSFORM.
    !    N = SIZE OF VECTOR.  MUST BE A POWER OF 2 (<32769).
    !    K = 1 FOR FORWARD TRANSFORM.
    !    K = -1 FOR INVERSE TRANSFORM.

    IMPLICIT NONE

    INTEGER, INTENT(IN)           :: n
    COMPLEX,  INTENT(IN OUT)  :: x(n)
    INTEGER, INTENT(IN)           :: k

    INTEGER       :: sby2, s
    REAL          :: ang, re, im
    COMPLEX       :: xtemp, t, u(16), v, w
    LOGICAL       :: NEW
    INTEGER       :: i, INDEX, itemp, j, jndex, l, l2n, p, q, stage
    REAL          ::  pi2 = 2*pi, gain = 1.0
    INTEGER       :: no = 0, ko = 0

    !        TEST FIRST CALL?

    NEW = (no /= n)
    IF (NEW) THEN

       !        IF FIRST CALL COMPUTE LOG2 (N).

       l2n = 0
       no = 1
10     l2n = l2n + 1
       no = no + no
       IF (no < n) GO TO 10
       gain = 1. / n
       ang = pi2 * gain
       re = COS(ang)
       im = SIN(ang)
    END IF

    !        COMPUTE COMPLEX EXPONENTIALS IF NOT FIRST CALL

    IF (.NOT.(.NOT.NEW .AND. k*ko >= 1)) THEN
       u(1) = CMPLX(re, -SIGN(im, real(k)))
       DO  i = 2, l2n
          u(i) = u(i-1) * u(i-1)
       END DO
       !  The code from netlib cntained the line:
       !  K0 = K
       ko = k
    END IF

    !        MAIN LOOP

    sby2 = n
    DO  stage = 1, l2n
       v = u(stage)
       w = (1.0, 0.0)
       s = sby2
       sby2 = s / 2
       DO  l = 1, sby2
          DO  i = 1, n, s
             p = i + l - 1
             q = p + sby2
             t = x(p) + x(q)
             x(q) = (x(p)-x(q)) * w
             x(p) = t
          END DO
          w = w * v
       END DO
    END DO

    !        REORDER THE ELEMENTS BY BIT REVERSAL

    DO  i = 1, n
       INDEX = i - 1
       jndex = 0
       DO  j = 1, l2n
          jndex = jndex + jndex
          itemp = INDEX / 2
          IF (itemp+itemp /= INDEX) jndex = jndex + 1
          INDEX = itemp
       END DO
       j = jndex + 1
       IF (j >= i) THEN
          xtemp = x(j)
          x(j) = x(i)
          x(i) = xtemp
       END IF
    END DO

    !        FORWARD TRANSFORM DONE

    IF (k > 0) RETURN

    !        INVERSE TRANSFORM

    x(1:n) = x(1:n) * gain
    RETURN
  END SUBROUTINE fft

end module fftmod
