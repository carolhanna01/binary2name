*  ------------------------>  Fortran - sourcefile  <-----------------------  *
*  Copyright (C) 199x by International Computer Science Institute             *
*  This file is part of the GNU Sather package. It is free software; you may  *
*  redistribute  and/or modify it under the terms of the  GNU General Public  *
*  License (GPL)  as  published  by the  Free  Software  Foundation;  either  *
*  version 3 of the license, or (at your option) any later version.           *
*  This  program  is distributed  in the  hope that it will  be  useful, but  *
*  WITHOUT ANY WARRANTY without even the implied warranty of MERCHANTABILITY  *
*  or FITNESS FOR A PARTICULAR PURPOSE. See Doc/GPL for more details.         *
*  The license text is also available from:  Free Software Foundation, Inc.,  *
*  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA                      *
*  ----------->  Please email comments to <bug-sather@gnu.org>  <-----------  *

      SUBROUTINE XERBLA( SRNAME, INFO )
*
*  -- LAPACK auxiliary routine (preliminary version) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*6        SRNAME
      INTEGER            INFO
*     ..
*
*  Purpose
*  =======
*
*  XERBLA  is an error handler for the LAPACK routines.
*  It is called by an LAPACK routine if an input parameter has an
*  invalid value.  A message is printed and execution stops.
*
*  Installers may consider modifying the STOP statement in order to
*  call system-specific exception-handling facilities.
*
*  Arguments
*  =========
*
*  SRNAME  (input) CHARACTER*6
*          The name of the routine which called XERBLA.
*
*  INFO    (input) INTEGER
*          The position of the invalid parameter in the parameter list
*          of the calling routine.
*
*
      WRITE( *, FMT = 9999 )SRNAME, INFO
*
      STOP
*
 9999 FORMAT( ' ** On entry to ', A6, ' parameter number ', I2, ' had ',
     $      'an illegal value' )
*
*     End of XERBLA
*
      END
