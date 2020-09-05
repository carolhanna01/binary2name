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

*     ----------------------- CHARACTER TESTS ------------------------

      CHARACTER FUNCTION  CHAR_FUNCTION()
      CHAR_FUNCTION = 'A'
      END

      CHARACTER FUNCTION CHAR_FUNCTION_CHAR(X)
      CHARACTER X
      CHAR_FUNCTION_CHAR = CHAR(ICHAR(X)+1)
      END

      CHARACTER FUNCTION CHAR_FUNCTION_CHAR_CHAR(X, Y)
      CHARACTER X, Y
      CHAR_FUNCTION_CHAR_CHAR = CHAR((ICHAR(X)+ICHAR(Y))/2)
      END
      
      SUBROUTINE SUBROUTINE_OUT_CHAR(X)
      CHARACTER X
      X = 'D'
      END

      SUBROUTINE SUBROUTINE_INOUT_CHAR(X)
      CHARACTER X
      X = CHAR(ICHAR(X)+1)
      END

      SUBROUTINE SUBROUTINE_CHAR_CHAR_OUT_CHAR(X,Y,Z)
      CHARACTER X,Y,Z
      Z = CHAR((ICHAR(X)+ICHAR(Y))/2)
      END

      SUBROUTINE SUBROUTINE_INOUT_CHAR_INOUT_CHAR(X,Y)
      CHARACTER X,Y
      CHARACTER T
      T = X
      X = Y
      Y = T
      END

      SUBROUTINE SUBROUTINE_CHAR_ARR(X)
      CHARACTER*(*) X
      X = "XXXX"
      END
      
      SUBROUTINE SUBROUTINE_CHAR_ARR_CHAR_ARR(SRC, DEST)
      CHARACTER*(*) SRC,DEST
      DEST = SRC
      END


      SUBROUTINE SUBROUTINE_CHAR_ARR_CHAR_3(SRC, DEST)
      CHARACTER*(*) SRC
      CHARACTER*3, DEST
      DEST = SRC
      END

      SUBROUTINE SUBROUTINE_CHAR_3_CHAR_3(SRC, DEST)
      CHARACTER*3, SRC, DEST
      DEST = SRC
      END

      CHARACTER*(*) FUNCTION char_arr_function()
      char_arr_function = "XXXX"
      END

      CHARACTER*(*) FUNCTION CHAR_ARR_FUNCTION_CHAR_ARR(X)
      CHARACTER*(*) X
      PRINT*, "X: ", X
      CHAR_ARR_FUNCTION_CHAR_ARR = X
      END
      
*     -------------------------- INTEGER TESTS ---------------------

      INTEGER FUNCTION F_INT_FUNCTION()
      F_INT_FUNCTION = 99
      END

      INTEGER FUNCTION F_INT_FUNCTION_F_INT(X)
      INTEGER X
      F_INT_FUNCTION_F_INT = X
      END

      INTEGER FUNCTION F_INT_FUNCTION_F_INT_F_INT(X,Y)
      INTEGER X,Y
      F_INT_FUNCTION_F_INT_F_INT = X+Y
      END

      INTEGER FUNCTION INT_FUNC_INOUT_INT_INOUT_INT(X,Y)
      INTEGER X,Y
      INTEGER I
      I=X
      X=Y
      Y=I
      INT_FUNC_INOUT_INT_INOUT_INT = X+Y
      END

      
      SUBROUTINE SUB_INOUT_INT_INOUT_INT(X,Y)
      INTEGER X,Y
      INTEGER I
      I=X
      X=Y
      Y=I
      END

      SUBROUTINE SUB_INT_INT_OUT_INT(X,Y,Z)
      INTEGER X,Y,Z
      Z = X + Y
      END


*     ------------------------ REAL TESTS ----------------------

      REAL FUNCTION F_REAL_FUNCTION()
      F_REAL_FUNCTION = 99.5
      END

      REAL FUNCTION F_REAL_FUNCTION_F_REAL(X)
      REAL X
      F_REAL_FUNCTION_F_REAL = X
      END

      REAL FUNCTION F_REAL_FUNCTION_F_REAL_F_REAL(X,Y)
      REAL X,Y
      F_REAL_FUNCTION_F_REAL_F_REAL = X+Y
      END

      REAL FUNCTION REAL_FUNC_INOUT_REAL_INOUT_REAL(X,Y)
      REAL X,Y
      REAL I
      I=X
      X=Y
      Y=I
      REAL_FUNC_INOUT_REAL_INOUT_REAL = X+Y
      END

      SUBROUTINE SUB_INOUT_REAL_INOUT_REAL(X,Y)
      REAL X,Y
      REAL I
      I=X
      X=Y
      Y=I
      END

      SUBROUTINE SUB_REAL_REAL_OUT_REAL(X,Y,Z)
      REAL X,Y,Z
      Z = X + Y
      END      

*     ------------------ DOUBLE PRECISION TESTS ---------------------------

      DOUBLE PRECISION FUNCTION F_D_FUNCTION()
      F_D_FUNCTION = 99.5
      END

      DOUBLE PRECISION FUNCTION F_D_FUNCTION_F_D(X)
      DOUBLE PRECISION X
      F_D_FUNCTION_F_D = X
      END

      DOUBLE PRECISION FUNCTION F_D_FUNCTION_F_D_F_D(X,Y)
      DOUBLE PRECISION X,Y
      F_D_FUNCTION_F_D_F_D = X+Y
      END

      DOUBLE PRECISION FUNCTION F_D_FUNC_INOUT_F_D_INOUT_F_D(X,Y)
      DOUBLE PRECISION X,Y
      DOUBLE PRECISION I
      I=X
      X=Y
      Y=I
      F_D_FUNC_INOUT_F_D_INOUT_F_D = X+Y
      END

      SUBROUTINE SUB_INOUT_F_D_INOUT_F_D(X,Y)
      DOUBLE PRECISION X,Y
      DOUBLE PRECISION I
      I=X
      X=Y
      Y=I
      END

      SUBROUTINE SUB_F_D_F_D_OUT_F_D(X,Y,Z)
      DOUBLE PRECISION X,Y,Z
      Z = X + Y
      END      


*     ------------------------ LOGICAL TESTS  --------------------

      LOGICAL FUNCTION F_L_FUNCTION()
      F_L_FUNCTION = .TRUE.
      END

      LOGICAL FUNCTION F_L_FUNCTION_F_L(X)
      LOGICAL X
      F_L_FUNCTION_F_L = X
      END

      LOGICAL FUNCTION F_L_FUNCTION_F_L_F_L(X,Y)
      LOGICAL X,Y
      F_L_FUNCTION_F_L_F_L = X .OR. Y
      END

      LOGICAL FUNCTION F_L_FUNC_INOUT_F_L_INOUT_F_L(X,Y)
      LOGICAL X,Y
      LOGICAL I
      I=X
      X=Y
      Y=I
      F_L_FUNC_INOUT_F_L_INOUT_F_L = X .OR. Y
      END

      SUBROUTINE SUB_INOUT_F_L_INOUT_F_L(X,Y)
      LOGICAL X,Y
      LOGICAL I
      I=X
      X=Y
      Y=I
      END

      SUBROUTINE SUB_F_L_F_L_OUT_F_L(X,Y,Z)
      LOGICAL X,Y,Z
      Z = X .AND. Y
      END      

*     ------------------ COMPLEX TESTS ---------------------------

      COMPLEX FUNCTION F_C_FUNCTION()
      F_C_FUNCTION = (99.5,99.5)
      END

      COMPLEX FUNCTION F_C_FUNCTION_F_C(X)
      COMPLEX X
      F_C_FUNCTION_F_C = X
      END

      COMPLEX FUNCTION F_C_FUNCTION_F_C_F_C(X,Y)
      COMPLEX X,Y
      F_C_FUNCTION_F_C_F_C = X+Y
      END

      COMPLEX FUNCTION F_C_FUNC_INOUT_F_C_INOUT_F_C(X,Y)
      COMPLEX X,Y
      COMPLEX I
      I=X
      X=Y
      Y=I
      F_C_FUNC_INOUT_F_C_INOUT_F_C = X+Y
      END

      SUBROUTINE SUB_INOUT_F_C_INOUT_F_C(X,Y)
      COMPLEX X,Y
      COMPLEX I
      I=X
      X=Y
      Y=I
      END

      SUBROUTINE SUB_F_C_F_C_OUT_F_C(X,Y,Z)
      COMPLEX X,Y,Z
      Z = X + Y
      END      

*     --------------- DOUBLE PRECISION COMPLEX TESTS -----------------------

      DOUBLE COMPLEX FUNCTION F_DC_FUNCTION()
      F_DC_FUNCTION = (99.5,99.5)
      END

      DOUBLE COMPLEX FUNCTION F_DC_FUNCTION_F_DC(X)
      DOUBLE COMPLEX X
      F_DC_FUNCTION_F_DC = X
      END

      DOUBLE COMPLEX FUNCTION F_DC_FUNCTION_F_DC_F_DC(X,Y)
      DOUBLE COMPLEX X,Y
      F_DC_FUNCTION_F_DC_F_DC = X+Y
      END

      DOUBLE COMPLEX FUNCTION F_DC_FUNC_INOUT_F_DC_INOUT_F_DC(X,Y)
      DOUBLE COMPLEX X,Y
      DOUBLE COMPLEX I
      I=X
      X=Y
      Y=I
      F_DC_FUNC_INOUT_F_DC_INOUT_F_DC = X+Y
      END

      SUBROUTINE SUB_INOUT_F_DC_INOUT_F_DC(X,Y)
      DOUBLE COMPLEX X,Y
      DOUBLE COMPLEX I
      I=X
      X=Y
      Y=I
      END

      SUBROUTINE SUB_F_DC_F_DC_OUT_F_DC(X,Y,Z)
      DOUBLE COMPLEX X,Y,Z
      Z = X + Y
      END      


*     ------------ Alternate Subroutine Returns/Exception handling -----------

      SUBROUTINE MAY_TRIGGER_EXCEPTIONS(I,*,*)
      IF (I .EQ. 0) RETURN
      IF (I .EQ. 1) RETURN 1
      IF (I .EQ. 2) RETURN 2
      END


*     ------- Callbacks to Sather tests (i.e. Fortran->Sather interface) -----

      SUBROUTINE SUB_CALLBACK1(I,J,K)
      INTEGER I,J,K
      INTEGER SATHER_ADD1
      K=SATHER_ADD1(I,J)
      END

      SUBROUTINE SUB_CALLBACK2(RI,RJ,RK)
      REAL RI,RJ,RK
      REAL SATHER_ADD2
      RK=SATHER_ADD2(RI,RJ)
      END

      SUBROUTINE SUB_CALLBACK3(I,J)
      INTEGER I, J
      CALL SATHER_SWAP3(I,J)
      END

      SUBROUTINE SUB_CALLBACK4(DI,DJ)
      DOUBLE PRECISION DI, DJ
      CALL SATHER_SWAP4(DI,DJ)
      END
      
      SUBROUTINE SUB_CALLBACK5(CP)
      COMPLEX CP
      COMPLEX SATHER_RETURN_COMPLEX
      CP = SATHER_RETURN_COMPLEX(CP)
      END

      SUBROUTINE SUB_CALLBACK6(DCP)
      DOUBLE COMPLEX DCP
      DOUBLE COMPLEX SATHER_RETURN_DOUBLE_COMPLEX
      DCP = SATHER_RETURN_DOUBLE_COMPLEX(DCP)
      END

      CHARACTER FUNCTION FUNC_CALLBACK1()
      CHARACTER SATHER_RETURN_CHAR
      CHARACTER CH
      CH = SATHER_RETURN_CHAR()
      FUNC_CALLBACK1 = CH
      END

      CHARACTER*4 FUNCTION FUNC_CALLBACK2()
      CHARACTER*4 SATHER_RETURN_CHAR4
      CHARACTER*4 CH
      CH = SATHER_RETURN_CHAR4()
      FUNC_CALLBACK2 = CH
      END


      INTEGER FUNCTION FUNC_TEST_F_ROUT(EXTFUNC,A,B)
      EXTERNAL EXTFUNC
      INTEGER A,B
      INTEGER SUM
      
      CALL EXTFUNC(A,B,SUM)
      FUNC_TEST_F_ROUT = SUM
      END

      SUBROUTINE SUB_TEST_F_ROUT(EXTFUNC,A,B,SUM)
      EXTERNAL EXTFUNC
      INTEGER A,B
      INTEGER SUM
      
      CALL EXTFUNC(A,B,SUM)
      END
      
      
