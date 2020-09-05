/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/


#include "mavlib_kernel.h"
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#if !defined(WIN32) && !defined(macintosh)
#include <sys/time.h>
#endif
#include <time.h>
#ifdef macintosh
#include <Quickdraw.h>
#else
#ifndef MAV_SUNOS5
#ifndef MAV_LINUX
#ifndef WIN32
int gettimeofday (struct timeval *__restrict __tv,
				  __timezone_ptr_t __tz);
/*int gettimeofday(struct timeval *tp, ...);*/
#endif
#else
double drand48(void);
void srand48(long int seedval);
#endif
#endif
#endif

#ifdef WIN32
#ifdef __MWERKS__
#include <Windows.h>
#include <Winbase.h>
#endif
#endif

#if defined(machintosh)
#elif defined(WIN32) && !defined(__CYGWIN__)
#include <process.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif

int mav_opt_fixedRnd= MAV_FALSE;



/* Routine to normalise a vector */

MAV_vector mav_vectorNormalize(MAV_vector v)
{
  MAV_vector rv;
  double divisor= sqrt(v.x*v.x+v.y*v.y+v.z*v.z);

  if (divisor > 0.0000001) 
  {
    rv= mav_vectorScalar(v, 1.0/divisor);
  } 
  else 
  {
    /*    if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: vector (%f %f %f) too small to normalise. Dot Product %f\n", v.x, v.y, v.z, divisor);*/
    rv= MAV_NULL_VECTOR;
  }

  return rv;
}



/* Routine to calculate the magnitude of a vector */

float mav_vectorMag(MAV_vector v)
{
  return sqrt(v.x*v.x+v.y*v.y+v.z*v.z);
}



/* Routine to calculate the dot product of 2 vectors */

float mav_vectorDotProduct(MAV_vector v1, MAV_vector v2)
{
  return (v1.x*v2.x + v1.y*v2.y + v1.z*v2.z);
}



/* Routine to calculate the cross product of 2 vectors */

MAV_vector mav_vectorCrossProduct(MAV_vector v1, MAV_vector v2)
{
  MAV_vector rv;

  rv.x= v1.y*v2.z - v1.z*v2.y;
  rv.y= v1.z*v2.x - v1.x*v2.z;
  rv.z= v1.x*v2.y - v1.y*v2.x;

  return rv;
}



/* Routine to calculate the sum of 2 vectors */

MAV_vector mav_vectorAdd(MAV_vector v1, MAV_vector v2)
{
  MAV_vector rv;

  rv.x= v1.x+v2.x;
  rv.y= v1.y+v2.y;
  rv.z= v1.z+v2.z;

  return rv;
}



/* Routine to calculate the difference of 2 vectors */

MAV_vector mav_vectorSub(MAV_vector v1, MAV_vector v2)
{
  MAV_vector rv;

  rv.x= v1.x-v2.x;
  rv.y= v1.y-v2.y;
  rv.z= v1.z-v2.z;

  return rv;
}



/* Routine to multiply a vector by a scaler */

MAV_vector mav_vectorScalar(MAV_vector v1, float f)
{
  MAV_vector rv;

  rv.x= v1.x*f;
  rv.y= v1.y*f;
  rv.z= v1.z*f;

  return rv;
}



/* 
   Routine to define a matrix with a given orientation and position
   Where roll, defined in degrees, is a rotation around the Z-axis,
   pitch around the X-axis, and yaw around the Y-axis.
 */

MAV_matrix mav_matrixSet(float roll, float pitch, float yaw, float xpos, float ypos, float zpos)
{
  MAV_matrix mat;
  float cosroll,sinroll,cospitch,sinpitch,cosyaw,sinyaw;
  float sinyawcosroll,sinyawsinroll;
  
/* Convert to radians */

  roll*=MAV_PI_OVER_180;
  pitch*=MAV_PI_OVER_180;
  yaw*=MAV_PI_OVER_180;

/* Calculate trig functions */
  cosroll=cos(roll);
  sinroll=sin(roll);
  cospitch=cos(pitch);
  sinpitch=sin(pitch);
  cosyaw=cos(yaw);
  sinyaw=sin(yaw);
  sinyawcosroll=sinyaw*cosroll;
  sinyawsinroll=sinyaw*sinroll;
  
/* Form the matrix */

  mat.mat[0][0] = cosyaw*cosroll;
  mat.mat[0][1] = sinpitch*sinyawcosroll - cospitch*sinroll;
  mat.mat[0][2] = cospitch*sinyawcosroll + sinpitch*sinroll;
  mat.mat[0][3] = xpos;

  mat.mat[1][0] = cosyaw*sinroll;
  mat.mat[1][1] = sinpitch*sinyawsinroll + cospitch*cosroll;
  mat.mat[1][2] = cospitch*sinyawsinroll - sinpitch*cosroll;
  mat.mat[1][3] = ypos;

  mat.mat[2][0] = -sinyaw;
  mat.mat[2][1] = cosyaw*sinpitch;
  mat.mat[2][2] = cosyaw*cospitch;
  mat.mat[2][3] = zpos;

  mat.mat[3][0] = 0.0;
  mat.mat[3][1] = 0.0;
  mat.mat[3][2] = 0.0;
  mat.mat[3][3] = 1.0;

  return mat;
}



/* As above but using old (wrong) convention. Kept for back compatibility sake, do not use */

MAV_matrix mav_matrixSetOld(float roll, float pitch, float yaw, float xpos, float ypos, float zpos)
{
  MAV_matrix mat;
  float cosroll,sinroll,cospitch,sinpitch,cosyaw,sinyaw;
  float sinyawcosroll,sinyawsinroll;
  
/* Convert to radians */

  roll*=MAV_PI_OVER_180;
  pitch*=-MAV_PI_OVER_180;
  yaw*=MAV_PI_OVER_180;

/* Calculate trig functions */
  cosroll=cos(roll);
  sinroll=sin(roll);
  cospitch=cos(pitch);
  sinpitch=sin(pitch);
  cosyaw=cos(yaw);
  sinyaw=sin(yaw);
  sinyawcosroll=sinyaw*cosroll;
  sinyawsinroll=sinyaw*sinroll;
  
/* Form the matrix */

  mat.mat[0][0] = cosyaw*cosroll;
  mat.mat[1][0] = sinpitch*sinyawcosroll - cospitch*sinroll;
  mat.mat[2][0] = cospitch*sinyawcosroll + sinpitch*sinroll;
  mat.mat[3][0] = 0.0;

  mat.mat[0][1] = cosyaw*sinroll;
  mat.mat[1][1] = sinpitch*sinyawsinroll + cospitch*cosroll;
  mat.mat[2][1] = cospitch*sinyawsinroll - sinpitch*cosroll;
  mat.mat[3][1] = 0.0;

  mat.mat[0][2] = -sinyaw;
  mat.mat[1][2] = cosyaw*sinpitch;
  mat.mat[2][2] = cosyaw*cospitch;
  mat.mat[3][2] = 0.0;

  mat.mat[0][3] = xpos;
  mat.mat[1][3] = ypos;
  mat.mat[2][3] = zpos;
  mat.mat[3][3] = 1.0;

  return mat;  
}



/* Routine to define the orientation of a matrix */

MAV_matrix mav_matrixRPYSet(MAV_matrix m, float roll, float pitch, float yaw)
{
  MAV_matrix mat;
  float cosroll,sinroll,cospitch,sinpitch,cosyaw,sinyaw;
  float sinyawcosroll,sinyawsinroll;
  float sc;

/* Get scale */

  sc= mav_matrixScaleGet(m);

/* Convert to radians */

  roll*=MAV_PI_OVER_180;
  pitch*=MAV_PI_OVER_180;
  yaw*=MAV_PI_OVER_180;

/* Calculate trig functions */
  cosroll=cos(roll);
  sinroll=sin(roll);
  cospitch=cos(pitch);
  sinpitch=sin(pitch);
  cosyaw=cos(yaw);
  sinyaw=sin(yaw);
  sinyawcosroll=sinyaw*cosroll;
  sinyawsinroll=sinyaw*sinroll;

/* Form the matrix */

  mat.mat[0][0] = cosyaw*cosroll;
  mat.mat[0][1] = sinpitch*sinyawcosroll - cospitch*sinroll;
  mat.mat[0][2] = cospitch*sinyawcosroll + sinpitch*sinroll;
  mat.mat[0][3] = m.mat[0][3];

  mat.mat[1][0] = cosyaw*sinroll;
  mat.mat[1][1] = sinpitch*sinyawsinroll + cospitch*cosroll;
  mat.mat[1][2] = cospitch*sinyawsinroll - sinpitch*cosroll;
  mat.mat[1][3] = m.mat[1][3];

  mat.mat[2][0] = -sinyaw;
  mat.mat[2][1] = cosyaw*sinpitch;
  mat.mat[2][2] = cosyaw*cospitch;
  mat.mat[2][3] = m.mat[2][3];

  mat.mat[3][0] = 0.0;
  mat.mat[3][1] = 0.0;
  mat.mat[3][2] = 0.0;
  mat.mat[3][3] = 1.0;

/* Preserve scale */

  mav_matrixScaleSet(m, sc);

  return mat;
}



/* Routine to print the contents of a matrix*/

void mav_vectorPrint(char *s, MAV_vector v)
{
  printf("%s", s);
  printf("%f %f %f\n", v.x, v.y, v.z);
}



/* Routine to print the contents of a texture coordinate */

void mav_texCoordPrint(char *s, MAV_texCoord t)
{
  printf("%s", s);
  printf("%f %f\n", t.s, t.t);
}



/* 
   Routine to scale a matrix - does not affect position 
   Each dimension is scaled equally
 */

MAV_matrix mav_matrixScaleSet(MAV_matrix mat, float scale)
{
  MAV_matrix rv;

  rv= mat;

  rv.mat[0][0] *= scale;
  rv.mat[0][1] *= scale;
  rv.mat[0][2] *= scale;

  rv.mat[1][0] *= scale;
  rv.mat[1][1] *= scale;
  rv.mat[1][2] *= scale;

  rv.mat[2][0] *= scale;
  rv.mat[2][1] *= scale;
  rv.mat[2][2] *= scale;

  return rv;
}



/* Routine to multiple a vector by a 4x3 matrix */

MAV_vector mav_vectorMult(MAV_vector in, MAV_matrix matrix)
{
  MAV_vector out;

  out.x = in.x*matrix.mat[0][0] + in.y*matrix.mat[0][1] + in.z*matrix.mat[0][2] + matrix.mat[0][3];
  out.y = in.x*matrix.mat[1][0] + in.y*matrix.mat[1][1] + in.z*matrix.mat[1][2] + matrix.mat[1][3];
  out.z = in.x*matrix.mat[2][0] + in.y*matrix.mat[2][1] + in.z*matrix.mat[2][2] + matrix.mat[2][3];
  
  return out;
}



/* Routine to multiple a vector by a 3x3 matrix */

MAV_vector mav_vectorMult3x3(MAV_vector in, MAV_matrix matrix)
{
  MAV_vector out;

  out.x = in.x*matrix.mat[0][0] + in.y*matrix.mat[0][1] + in.z*matrix.mat[0][2];
  out.y = in.x*matrix.mat[1][0] + in.y*matrix.mat[1][1] + in.z*matrix.mat[1][2];
  out.z = in.x*matrix.mat[2][0] + in.y*matrix.mat[2][1] + in.z*matrix.mat[2][2];
  
  return out;
}



/* Routine to multiple a vector by a 4x4 matrix */

MAV_vector mav_vectorMult4x4(MAV_vector in, MAV_matrix matrix)
{
  MAV_vector out;
  float w;
  float iw;
  
  out.x = in.x*matrix.mat[0][0] + in.y*matrix.mat[0][1] + in.z*matrix.mat[0][2] + matrix.mat[0][3];
  out.y = in.x*matrix.mat[1][0] + in.y*matrix.mat[1][1] + in.z*matrix.mat[1][2] + matrix.mat[1][3];
  out.z = in.x*matrix.mat[2][0] + in.y*matrix.mat[2][1] + in.z*matrix.mat[2][2] + matrix.mat[2][3];
  w     = in.x*matrix.mat[3][0] + in.y*matrix.mat[3][1] + in.z*matrix.mat[3][2] + matrix.mat[3][3];

  iw=1.0/w;
  out.x *= iw;
  out.y *= iw;
  out.z *= iw;
  
  return out;
}


/* Routines to rotate a vector about an arbitrary axis */

MAV_vector mavlib_rotX(MAV_vector vect, float ang) 
{
  MAV_vector result;
  float cosang,sinang;
  
  cosang=cos(ang);
  sinang=sin(ang);
    
  result.x= vect.x;
  result.y= vect.y * cosang - vect.z * sinang;
  result.z= vect.y * sinang + vect.z * cosang;
  
  return result;
}

MAV_vector mavlib_rotY(MAV_vector vect, float ang) 
{
  MAV_vector result;
  float cosang,sinang;
  
  cosang=cos(ang);
  sinang=sin(ang);
  
  result.x= vect.z * sinang + vect.x * cosang;
  result.y= vect.y;
  result.z= vect.z * cosang - vect.x * sinang;
  
  return result;
}

MAV_vector mavlib_rotZ(MAV_vector vect, float ang) 
{
  MAV_vector result;
  float cosang,sinang;
  
  cosang=cos(ang);
  sinang=sin(ang);
 
  result.x= vect.x * cosang - vect.y * sinang;
  result.y= vect.x * sinang + vect.y * cosang;
  result.z= vect.z;
  
  return result;
}

MAV_vector mav_vectorRotate(MAV_vector vect, MAV_vector ax, float ang)
{
  MAV_vector result;
  float roty, rotz;

  /* rotate vector about axis from origin */

  if (ax.z != 0 || ax.x != 0) roty= atan2(ax.z, ax.x);
  else roty= 0;
  
  rotz= -asin(ax.y/mav_vectorMag(ax));

  result= mavlib_rotY(vect, roty);
  result= mavlib_rotZ(result, rotz);
  
  result= mavlib_rotX(result, ang);
  
  result= mavlib_rotZ(result, -rotz);
  result= mavlib_rotY(result, -roty);
  
  return result;
}



/* Routine to calculate the scale of a matrix - assumes equal XYZ scaling */

float mav_matrixScaleGet(MAV_matrix mat)
{
/* Find the scale of the x axis */

  return mav_vectorMag(mav_matrixXAxisGet(mat));
}



/* Routines to get and set the various axes of a matrix */

MAV_vector mav_matrixXAxisGet(MAV_matrix m)
{
  MAV_vector rv;
  
  rv.x= m.mat[MAV_MATRIX_XAXIS_X];
  rv.y= m.mat[MAV_MATRIX_XAXIS_Y];
  rv.z= m.mat[MAV_MATRIX_XAXIS_Z];

  return rv;
}

MAV_vector mav_matrixYAxisGet(MAV_matrix m)
{
  MAV_vector rv;
  
  rv.x= m.mat[MAV_MATRIX_YAXIS_X];
  rv.y= m.mat[MAV_MATRIX_YAXIS_Y];
  rv.z= m.mat[MAV_MATRIX_YAXIS_Z];

  return rv;
}

MAV_vector mav_matrixZAxisGet(MAV_matrix m)
{
  MAV_vector rv;
  
  rv.x= m.mat[MAV_MATRIX_ZAXIS_X];
  rv.y= m.mat[MAV_MATRIX_ZAXIS_Y];
  rv.z= m.mat[MAV_MATRIX_ZAXIS_Z];

  return rv;
}

MAV_matrix mav_matrixXAxisSet(MAV_matrix m, MAV_vector v)
{
  MAV_matrix rv= m;

  rv.mat[MAV_MATRIX_XAXIS_X]= v.x;
  rv.mat[MAV_MATRIX_XAXIS_Y]= v.y;
  rv.mat[MAV_MATRIX_XAXIS_Z]= v.z;

  return rv;
}

MAV_matrix mav_matrixYAxisSet(MAV_matrix m, MAV_vector v)
{
  MAV_matrix rv= m;

  rv.mat[MAV_MATRIX_YAXIS_X]= v.x;
  rv.mat[MAV_MATRIX_YAXIS_Y]= v.y;
  rv.mat[MAV_MATRIX_YAXIS_Z]= v.z;

  return rv;
}

MAV_matrix mav_matrixZAxisSet(MAV_matrix m, MAV_vector v)
{
  MAV_matrix rv= m;

  rv.mat[MAV_MATRIX_ZAXIS_X]= v.x;
  rv.mat[MAV_MATRIX_ZAXIS_Y]= v.y;
  rv.mat[MAV_MATRIX_ZAXIS_Z]= v.z;

  return rv;
}



/* Routine to print the contents of a matrix */

void mav_matrixPrint(char *s, MAV_matrix m)
{
  printf("%s", s);
  printf("%f %f %f %f\n", m.mat[0][0], m.mat[0][1], m.mat[0][2], m.mat[0][3]);
  printf("%f %f %f %f\n", m.mat[1][0], m.mat[1][1], m.mat[1][2], m.mat[1][3]);
  printf("%f %f %f %f\n", m.mat[2][0], m.mat[2][1], m.mat[2][2], m.mat[2][3]);
  printf("%f %f %f %f\n", m.mat[3][0], m.mat[3][1], m.mat[3][2], m.mat[3][3]);
}



/* Routine to calculate the screen coordinates (in NDC) of a world-frame vector */

MAV_vector mav_vectorScrnPos(MAV_vector in)
{
  return mav_vectorMult4x4(in, mav_win_current->pdvMat);
}



/* Routine to calculate the world-frame position of a NDC-frame vector */

MAV_vector mav_vectorWorldPos(MAV_vector in)
{
  return mav_vectorMult4x4(in, mav_matrixInverse(mav_win_current->pdvMat));
}



/* Routine to multiply two matrices */

MAV_matrix mav_matrixMult(MAV_matrix a, MAV_matrix b)
{
  MAV_matrix res;
  int i,j,k;
  
  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      res.mat[i][j]= 0.0;
      for (k=0; k<4; k++) res.mat[i][j]+= a.mat[i][k]*b.mat[k][j];
    }
  }

  return res;
}



/* Routine to define a matrix */

MAV_vector mav_vectorSet(float x, float y, float z)
{
  MAV_vector rv;
  
  rv.x= x;
  rv.y= y;
  rv.z= z;

  return rv;
}



/* 
   Routine to calculate the roll, pitch and yaw of a matrix.
   Can't be done uniquely and the results are only meaningful if used in their
   entirety with mav_matrixSet. Avoid this routine if at all possible
*/

void mav_matrixRPYGet(MAV_matrix mat, float *roll, float *pitch, float *yaw)
{
  float cr, sr, cp, sp, cy, sy;
  float icy;
  float xsc, ysc, zsc;

  /* Make sure we have a unit scale matrix */
  xsc= 1.0/mav_vectorMag(mav_matrixXAxisGet(mat));
  ysc= 1.0/mav_vectorMag(mav_matrixYAxisGet(mat));
  zsc= 1.0/mav_vectorMag(mav_matrixZAxisGet(mat));

  mat.mat[MAV_MATRIX_XAXIS_X]*= xsc;
  mat.mat[MAV_MATRIX_XAXIS_Y]*= xsc;
  mat.mat[MAV_MATRIX_XAXIS_Z]*= xsc;

  mat.mat[MAV_MATRIX_YAXIS_X]*= ysc;
  mat.mat[MAV_MATRIX_YAXIS_Y]*= ysc;
  mat.mat[MAV_MATRIX_YAXIS_Z]*= ysc;

  mat.mat[MAV_MATRIX_ZAXIS_X]*= zsc;
  mat.mat[MAV_MATRIX_ZAXIS_Y]*= zsc;
  mat.mat[MAV_MATRIX_ZAXIS_Z]*= zsc;

#define M(X,Y) mat.mat[Y-1][X-1]

  sy = -M(1,3);
  cy = 1-(sy*sy);

  if (cy != 0.0) 
  {
    cy = sqrt(cy);
    icy = 1.0/cy;
      
    sp = M(2,3)*icy;
    cp = M(3,3)*icy;
      
    sr = M(1,2)*icy;
    cr = M(1,1)*icy;
  }
  else 
  {
    sp= -M(3,2);
    cp= M(2,2);
        
    sr= 0.0;
    cr= 1.0;
  }

#undef M

  *pitch = atan2(sp, cp);
  *yaw = atan2(sy, cy);
  *roll = atan2(sr, cr);

  /* convert to degrees */

  *pitch *= MAV_180_OVER_PI;
  *yaw *= MAV_180_OVER_PI;
  *roll *= MAV_180_OVER_PI;
}


/* The following routine to invert a 4x4 matrix taken from Graphics Gems I */

/*
Matrix Inversion
by Richard Carling
from "Graphics Gems", Academic Press, 1990
*/

typedef struct {  /* 4-by-4 matrix */
  float element[4][4];
} MAVLIB_Matrix4;

#define SMALL_NUMBER    1.e-8
void mavlib_inverse(MAVLIB_Matrix4 *in, MAVLIB_Matrix4 *out);
void mavlib_adjoint(MAVLIB_Matrix4 *in, MAVLIB_Matrix4 *out);
float mavlib_det4x4(MAVLIB_Matrix4 *m);
float mavlib_det3x3(float a1, float a2, float a3, float b1, float b2, float b3, float c1, float c2, float c3);
float mavlib_det2x2(float a, float b, float c, float d);

MAV_matrix mav_matrixInverse(MAV_matrix matrix)
{
  MAV_matrix res;
  MAVLIB_Matrix4 in, out;
  int i,j;

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      in.element[i][j]=matrix.mat[i][j];
    }
  }

  mavlib_inverse(&in, &out);

  for (i=0; i<4; i++) {
    for (j=0; j<4; j++) {
      res.mat[i][j]=out.element[i][j];
    }
  }

  return res;
}


/* 
 *   inverse( original_matrix, inverse_matrix )
 * 
 *    calculate the inverse of a 4x4 matrix
 *
 *     -1     
 *     A  = ___1__ adjoint A
 *         det A
 */

void mavlib_inverse(MAVLIB_Matrix4 *in, MAVLIB_Matrix4 *out)
{
    int i, j;
    float det;

    /* calculate the adjoint matrix */

    mavlib_adjoint( in, out );

    /*  calculate the 4x4 determinant
     *  if the determinant is zero, 
     *  then the inverse matrix is not unique.
     */

    det = mavlib_det4x4( in );

    if ( fabs( det ) < SMALL_NUMBER) {
        fprintf(stderr, "Warning: Non-singular matrix, no inverse!\n");
        return;
    }

    /* scale the adjoint matrix to get the inverse */

    for (i=0; i<4; i++)
        for(j=0; j<4; j++)
            out->element[i][j] = out->element[i][j] / det;
}


/* 
 *   adjoint( original_matrix, inverse_matrix )
 * 
 *     calculate the adjoint of a 4x4 matrix
 *
 *      Let  a   denote the minor determinant of matrix A obtained by
 *           ij
 *
 *      deleting the ith row and jth column from A.
 *
 *                    i+j
 *     Let  b   = (-1)    a
 *          ij            ji
 *
 *    The matrix B = (b  ) is the adjoint of A
 *                     ij
 */

void mavlib_adjoint(MAVLIB_Matrix4 *in, MAVLIB_Matrix4 *out)
{
    float a1, a2, a3, a4, b1, b2, b3, b4;
    float c1, c2, c3, c4, d1, d2, d3, d4;

    /* assign to individual variable names to aid  */
    /* selecting correct values  */

        a1 = in->element[0][0]; b1 = in->element[0][1]; 
        c1 = in->element[0][2]; d1 = in->element[0][3];

        a2 = in->element[1][0]; b2 = in->element[1][1]; 
        c2 = in->element[1][2]; d2 = in->element[1][3];

        a3 = in->element[2][0]; b3 = in->element[2][1];
        c3 = in->element[2][2]; d3 = in->element[2][3];

        a4 = in->element[3][0]; b4 = in->element[3][1]; 
        c4 = in->element[3][2]; d4 = in->element[3][3];


    /* row column labeling reversed since we transpose rows & columns */

    out->element[0][0]  =   mavlib_det3x3( b2, b3, b4, c2, c3, c4, d2, d3, d4);
    out->element[1][0]  = - mavlib_det3x3( a2, a3, a4, c2, c3, c4, d2, d3, d4);
    out->element[2][0]  =   mavlib_det3x3( a2, a3, a4, b2, b3, b4, d2, d3, d4);
    out->element[3][0]  = - mavlib_det3x3( a2, a3, a4, b2, b3, b4, c2, c3, c4);
        
    out->element[0][1]  = - mavlib_det3x3( b1, b3, b4, c1, c3, c4, d1, d3, d4);
    out->element[1][1]  =   mavlib_det3x3( a1, a3, a4, c1, c3, c4, d1, d3, d4);
    out->element[2][1]  = - mavlib_det3x3( a1, a3, a4, b1, b3, b4, d1, d3, d4);
    out->element[3][1]  =   mavlib_det3x3( a1, a3, a4, b1, b3, b4, c1, c3, c4);
        
    out->element[0][2]  =   mavlib_det3x3( b1, b2, b4, c1, c2, c4, d1, d2, d4);
    out->element[1][2]  = - mavlib_det3x3( a1, a2, a4, c1, c2, c4, d1, d2, d4);
    out->element[2][2]  =   mavlib_det3x3( a1, a2, a4, b1, b2, b4, d1, d2, d4);
    out->element[3][2]  = - mavlib_det3x3( a1, a2, a4, b1, b2, b4, c1, c2, c4);
        
    out->element[0][3]  = - mavlib_det3x3( b1, b2, b3, c1, c2, c3, d1, d2, d3);
    out->element[1][3]  =   mavlib_det3x3( a1, a2, a3, c1, c2, c3, d1, d2, d3);
    out->element[2][3]  = - mavlib_det3x3( a1, a2, a3, b1, b2, b3, d1, d2, d3);
    out->element[3][3]  =   mavlib_det3x3( a1, a2, a3, b1, b2, b3, c1, c2, c3);
}
/*
 * float = det4x4( matrix )
 * 
 * calculate the determinant of a 4x4 matrix.
 */
float mavlib_det4x4( MAVLIB_Matrix4 *m )
{
    float ans;
    float a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4;

    /* assign to individual variable names to aid selecting */
        /*  correct elements */

        a1 = m->element[0][0]; b1 = m->element[0][1]; 
        c1 = m->element[0][2]; d1 = m->element[0][3];

        a2 = m->element[1][0]; b2 = m->element[1][1]; 
        c2 = m->element[1][2]; d2 = m->element[1][3];

        a3 = m->element[2][0]; b3 = m->element[2][1]; 
        c3 = m->element[2][2]; d3 = m->element[2][3];

        a4 = m->element[3][0]; b4 = m->element[3][1]; 
        c4 = m->element[3][2]; d4 = m->element[3][3];

    ans = a1 * mavlib_det3x3( b2, b3, b4, c2, c3, c4, d2, d3, d4)
        - b1 * mavlib_det3x3( a2, a3, a4, c2, c3, c4, d2, d3, d4)
        + c1 * mavlib_det3x3( a2, a3, a4, b2, b3, b4, d2, d3, d4)
        - d1 * mavlib_det3x3( a2, a3, a4, b2, b3, b4, c2, c3, c4);
    return ans;
}



/*
 * float = det3x3(  a1, a2, a3, b1, b2, b3, c1, c2, c3 )
 * 
 * calculate the determinant of a 3x3 matrix
 * in the form
 *
 *     | a1,  b1,  c1 |
 *     | a2,  b2,  c2 |
 *     | a3,  b3,  c3 |
 */

float mavlib_det3x3( float a1, float a2, float a3, float b1, float b2, float b3, float c1, float c2, float c3 )
{
    float ans;

    ans = a1 * mavlib_det2x2( b2, b3, c2, c3 )
        - b1 * mavlib_det2x2( a2, a3, c2, c3 )
        + c1 * mavlib_det2x2( a2, a3, b2, b3 );
    return ans;
}

/*
 * float = det2x2( float a, float b, float c, float d )
 * 
 * calculate the determinant of a 2x2 matrix.
 */

float mavlib_det2x2( float a, float b, float c, float d)
{
    float ans;
    ans = a * d - b * c;
    return ans;
}




/*
  The spherical linear interpolation routine below, for interpolating
  quaternion is taken from Graphics Gems III
*/

#define GG_EPSILON 1.0E-6               /* a tiny number */
#define GG_TRUE  1  
#define GG_FALSE 0

void mavlib_slerp(float alpha, MAV_quaternion *a, MAV_quaternion *b, MAV_quaternion *q, int spin)
{
  float beta;                    /* complementary interp parameter */
  float theta;                   /* angle between A and B */
  float sin_t, cos_t;            /* sine, cosine of theta */
  float phi;                     /* theta plus spins */
  int bflip;                      /* use negation of B? */

  /* cosine theta = dot product of A and B */
  cos_t = a->x*b->x + a->y*b->y + a->z*b->z + a->w*b->w;
  
  /* if B is on opposite hemisphere from A, use -B instead */
  if (cos_t < 0.0) {
    cos_t = -cos_t;
    bflip = GG_TRUE;
  } else
    bflip = GG_FALSE;
  
  /* if B is (within precision limits) the same as A,
   * just linear interpolate between A and B.
   * Can't do spins, since we don't know what direction to spin.
   */
  if (1.0 - cos_t < GG_EPSILON) {
    beta = 1.0 - alpha;
  } else {                                /* normal case */
    theta = acos(cos_t);
    phi = theta + spin * MAV_PI;
    sin_t = sin(theta);
    beta = sin(theta - alpha*phi) / sin_t;
    alpha = sin(alpha*phi) / sin_t;
  }
  
  if (bflip)
    alpha = -alpha;
  
  /* interpolate */
  q->x = beta*a->x + alpha*b->x;
  q->y = beta*a->y + alpha*b->y;
  q->z = beta*a->z + alpha*b->z;
  q->w = beta*a->w + alpha*b->w;
}



/* 
   The interpolation routine below is really a wrapper to the spherical linear
   quaternion interpolation routine above. 

   I/P - MAV_quaternion start, finish - the two orientations quaternions to interpolate between
         float alpha                  - interpolation constant in the range [0,1]

   Return value -  the interpolated quaternion

   interpolated = start    at alpha = 0.0
   interpolated = finish   at alpha = 1.0
*/

MAV_quaternion mav_quaternionInterpolate(MAV_quaternion start, MAV_quaternion finish, float alpha)
{
  MAV_quaternion interpolated;
  
/* Spherical linear interpolate quaternions. Extra spin value of 0, i.e. normal interpolation */

  mavlib_slerp(alpha, &start, &finish, &interpolated, 0);

  return interpolated;
}    



/* Convert a 4x4 transformation matrix into a quaternion, and vice versa. See SIGGRAPH 85 pg 253 */

MAV_quaternion mav_quaternionMatrixConvert(MAV_matrix mat)
{
  MAV_quaternion quaternion;
  float a, b, c, d, norm;
  float i4a, i2b, inorm;

#define M(X,Y) mat.mat[Y-1][X-1]

  a = 0.25*(1.0 + M(1,1) + M(2,2) + M(3,3));

  if (a > 0.0) 
  {
    a = sqrt(a);
    i4a=1.0/(4.0*a);
    b = (M(2,3) - M(3,2))*i4a;
    c = (M(3,1) - M(1,3))*i4a;
    d = (M(1,2) - M(2,1))*i4a;
  }
  else
  {
    a = 0.0;
    b = -0.5*(M(2,2) + M(3,3));
      
    if (b > 0.0)
    {
      b = sqrt(b);
      i2b = 1.0/(2.0*b);
      c = M(1,2)*i2b;
      d = M(1,3)*i2b;
    }
    else
    {
      b = 0.0;
      c = 0.5*(1.0 - M(3,3));
	  
      if (c > 0.0) 
      {
	c = sqrt(c);
	d = M(2,3)/(2.0*c);
      }
      else
      {
	c = 0.0;
	d = 1.0;
      }
    }
  }

#undef M
  
  /* Normalise the quaternion */

  norm = sqrt(a*a + b*b + c*c + d*d);
  
  inorm=1.0/norm;
  a *= inorm;
  b *= inorm;
  c *= inorm;
  d *= inorm;
  
  quaternion.w = a;
  quaternion.x = b;
  quaternion.y = c;
  quaternion.z = d;

  return quaternion;
}

MAV_matrix mav_matrixQuaternionConvert(MAV_quaternion q)
{
  MAV_matrix m;

  m.mat[0][0]=1.0-2.0*(q.y*q.y+q.z*q.z);
  m.mat[0][1]=2.0*(q.x*q.y-q.w*q.z);
  m.mat[0][2]=2.0*(q.x*q.z+q.w*q.y);
  m.mat[0][3]=0.0;

  m.mat[1][0]=2.0*(q.x*q.y+q.w*q.z);
  m.mat[1][1]=1.0-2.0*(q.x*q.x+q.z*q.z);
  m.mat[1][2]=2.0*(q.y*q.z-q.w*q.x);
  m.mat[1][3]=0.0;

  m.mat[2][0]=2.0*(q.x*q.z-q.w*q.y);
  m.mat[2][1]=2.0*(q.y*q.z+q.w*q.x);
  m.mat[2][2]=1.0-2.0*(q.x*q.x+q.y*q.y);
  m.mat[2][3]=0.0;

  m.mat[3][0]=0.0;
  m.mat[3][1]=0.0;
  m.mat[3][2]=0.0;
  m.mat[3][3]=1.0;

  return m;
}



/* Routine to define a quaternion */

MAV_quaternion mav_quaternionSet(MAV_vector ax, float ang)
{
  MAV_quaternion q;
  float sa, ca;

/* Convert to radians */

  ang*=MAV_PI_OVER_180;

/* Form the quaternion */

  sa= sin(ang*0.5);
  ca= cos(ang*0.5);

  q.x= ax.x*sa;
  q.y= ax.y*sa;
  q.z= ax.z*sa;
  q.w= ca;

  return q;
}



/* Routine to print a quaternion */

void mav_quaternionPrint(char *s, MAV_quaternion q)
{
  printf("%s", s);
  printf("%f %f %f %f\n", q.w, q.x, q.y, q.z);  
}



/* Routine to define just the position of a matrix */

MAV_matrix mav_matrixXYZSet(MAV_matrix m, MAV_vector v)
{
  MAV_matrix rv;

  rv=m;
  rv.mat[MAV_MATRIX_XCOMP]=v.x;
  rv.mat[MAV_MATRIX_YCOMP]=v.y;
  rv.mat[MAV_MATRIX_ZCOMP]=v.z;
  
  return rv;
}



/* Routine to obtain just the position of a matrix */

MAV_vector mav_matrixXYZGet(MAV_matrix m)
{
  MAV_vector rv;
  
  rv.x= m.mat[MAV_MATRIX_XCOMP];
  rv.y= m.mat[MAV_MATRIX_YCOMP];
  rv.z= m.mat[MAV_MATRIX_ZCOMP];

  return rv;
}



/* Routines to implement a matrix stack */

typedef struct MAVLIB_MATRIXSTACKELEM {
  MAV_matrix matrix;
  struct MAVLIB_MATRIXSTACKELEM *prev;
} MAVLIB_matrixStackElem;

MAVLIB_matrixStackElem *mavlib_currentMatrixStackElem= NULL;

void mav_matrixStackPush(MAV_matrix matrix)
{
  MAVLIB_matrixStackElem *elem= (MAVLIB_matrixStackElem *) mav_malloc(sizeof(MAVLIB_matrixStackElem));

  elem->matrix=matrix;
  elem->prev= mavlib_currentMatrixStackElem;

  mavlib_currentMatrixStackElem= elem;
}

MAV_matrix mav_matrixStackGet(void)
{
  if (mavlib_currentMatrixStackElem)
  {
    return mavlib_currentMatrixStackElem->matrix;
  }
  else
  {
    fprintf(stderr, "Matrix stack empty\n");    
    return MAV_ID_MATRIX;
  }
}

void mav_matrixStackPop(void)
{
  if (mavlib_currentMatrixStackElem) 
  {
    MAVLIB_matrixStackElem *prev= mavlib_currentMatrixStackElem->prev;
    mav_free(mavlib_currentMatrixStackElem);
    mavlib_currentMatrixStackElem= prev;
  }
  else
  {
    fprintf(stderr, "Matrix stack empty\n");    
  }
}



/* Routine to print the contents of surface parameters */

void mav_surfaceParamsPrint(char *s, MAV_surfaceParams sp)
{
  printf("%s", s);
  if (sp.mode==MAV_COLOUR) printf("mode: MAV_COLOUR\n");
  if (sp.mode==MAV_MATERIAL) printf("mode: MAV_MATERIAL\n");
  if (sp.mode==MAV_TEXTURE) printf("mode: MAV_TEXTURE\n");
  if (sp.mode==MAV_LIT_TEXTURE) printf("mode: MAV_LIT_TEXTURE\n");
  if (sp.mode==MAV_BLENDED_TEXTURE) printf("mode: MAV_BLENDED_TEXTURE\n");
  printf("colour: %i\n", sp.colour);
  printf("material: %i\n", sp.material);
  printf("texture: %i\n", sp.texture);
}



/* Routine to get the time */

#ifdef WIN32
#ifndef __MWERKS__
#include <sys/timeb.h>
#endif
#endif

MAV_time mav_timeGet(void)
{
  MAV_time rv;

  /* get wallclock time */
#ifdef WIN32
#ifdef __MWERKS__
  SYSTEMTIME systime;
  GetSystemTime (&systime);
  rv.sec = systime.wSecond;
  rv.usec = systime.wMilliseconds*1000;
#else
  struct timeb tv;

  ftime(&tv);
  rv.sec= tv.time;
  rv.usec= tv.millitm*1000;
#endif
#elif defined(macintosh)
  time_t tt;
  struct tm *tv;
  time (&tt);
  tv = gmtime (&tt);
  rv.sec = tv->tm_sec + tv->tm_min*60 + tv->tm_hour*3600;
  rv.usec = 0;
#else
  struct timeval tv;

  gettimeofday(&tv, NULL);
  rv.sec= tv.tv_sec;
  rv.usec= tv.tv_usec;
#endif

  /* get cpu time */
  rv.cpu= clock();

  return rv;
}



/* Routine to start a timer */

void mav_timerStart(MAV_timer *timer)
{
  /* get time */
  timer->start= mav_timeGet();
}



/* Routine to end a timer */

void mav_timerStop(MAV_timer *timer)
{
  /* get time */
  timer->end= mav_timeGet();

  /* calculate elapsed time */
  timer->elapsed.sec= timer->end.sec-timer->start.sec;
  timer->elapsed.usec= timer->end.usec-timer->start.usec;
  timer->elapsed.cpu= timer->end.cpu-timer->start.cpu;

  timer->wall= timer->elapsed.sec+timer->elapsed.usec/1.0E+6;

#ifdef MAV_SUNOS4
  timer->cpu= -1;
#else
  timer->cpu= ((float) timer->elapsed.cpu)/CLOCKS_PER_SEC;
#endif
}



/* Routine to print a time structure */

void mav_timePrint(char *s, MAV_time time)
{
  printf("%s", s);
  printf("sec %d usec %d cpu %d\n", time.sec, time.usec, time.cpu);
}



/* Routine to print a timer structure */
  
void mav_timerPrint(char *s, MAV_timer timer)
{
  printf("%s", s);
  printf("Wallclock time: %f CPU time: %f\n", timer.wall, timer.cpu);
}



/* 5000 fixed random numbers (good for cross platform work) */

float mavlib_fixedRnd[]={0.396465, 0.840485, 0.353336, 0.446583, 0.318693, 0.886428, 0.015583, 0.584090, 0.159369, 0.383716, 0.691004, 0.058859, 0.899854, 0.163546, 0.159072, 0.533065, 0.604144, 0.582699, 0.269971, 0.390478, 0.293401,
 0.742377, 0.298526, 0.075538, 0.404983, 0.857378, 0.941968, 0.662831, 0.846476, 0.002755, 0.462379, 0.532596, 0.787877, 0.265612, 0.982752, 0.306785, 0.600855, 0.608716, 0.212439, 0.885895, 0.304657, 0.151860, 0.337662, 0.387477,
 0.643610, 0.753553, 0.603616, 0.531628, 0.459360, 0.652488, 0.327181, 0.946370, 0.368040, 0.943890, 0.007428, 0.516600, 0.272771, 0.024299, 0.591955, 0.204964, 0.877693, 0.059369, 0.260843, 0.302829, 0.891495, 0.498198, 0.710026,
 0.286414, 0.864924, 0.675541, 0.458490, 0.959636, 0.774675, 0.376551, 0.228639, 0.354534, 0.300318, 0.669766, 0.718967, 0.565955, 0.824465, 0.390612, 0.818766, 0.844008, 0.180468, 0.943396, 0.424887, 0.520666, 0.065644, 0.913508,
 0.882585, 0.761364, 0.398923, 0.688257, 0.761548, 0.405009, 0.125251, 0.484634, 0.222463, 0.873121, 0.528822, 0.001414, 0.860514, 0.018697, 0.814892, 0.242884, 0.314572, 0.965733, 0.935560, 0.809432, 0.492109, 0.220136, 0.576354,
 0.289029, 0.321067, 0.261323, 0.173988, 0.001817, 0.044784, 0.241175, 0.415451, 0.701625, 0.221846, 0.503910, 0.067030, 0.393063, 0.479477, 0.218142, 0.219511, 0.916203, 0.350222, 0.192694, 0.211235, 0.633682, 0.053565, 0.783411,
 0.030674, 0.444097, 0.176413, 0.932180, 0.909648, 0.472845, 0.871695, 0.695567, 0.930190, 0.455091, 0.398567, 0.893304, 0.693547, 0.838734, 0.739699, 0.651270, 0.678154, 0.577212, 0.273118, 0.935388, 0.661939, 0.047413, 0.373038,
 0.618195, 0.148628, 0.377306, 0.644591, 0.025688, 0.841381, 0.077019, 0.742634, 0.255986, 0.901844, 0.377648, 0.319530, 0.211435, 0.648533, 0.251315, 0.228644, 0.250922, 0.943221, 0.136715, 0.270061, 0.548707, 0.324021, 0.865080,
 0.296703, 0.680059, 0.833147, 0.876307, 0.649665, 0.073115, 0.898547, 0.253581, 0.611319, 0.841890, 0.832019, 0.372587, 0.757048, 0.109215, 0.850812, 0.559329, 0.857580, 0.343097, 0.691770, 0.345197, 0.893585, 0.958708, 0.121735,
 0.981544, 0.055025, 0.614776, 0.038127, 0.376473, 0.525605, 0.281924, 0.560538, 0.607243, 0.816440, 0.446535, 0.027112, 0.471854, 0.284898, 0.292724, 0.195680, 0.017579, 0.829599, 0.573165, 0.104781, 0.732976, 0.119098, 0.223888,
 0.947253, 0.739052, 0.821357, 0.826132, 0.250980, 0.256496, 0.338193, 0.388222, 0.527318, 0.266409, 0.401221, 0.870895, 0.045892, 0.294574, 0.394247, 0.560002, 0.310821, 0.822577, 0.475481, 0.091144, 0.262401, 0.917044, 0.978436,
 0.332093, 0.902074, 0.240635, 0.373164, 0.752286, 0.457827, 0.900628, 0.422895, 0.565924, 0.970462, 0.353871, 0.431507, 0.179091, 0.215319, 0.337177, 0.454368, 0.044726, 0.683882, 0.062149, 0.551375, 0.316171, 0.268218, 0.500766,
 0.062537, 0.964827, 0.729576, 0.806316, 0.151974, 0.705018, 0.729957, 0.842566, 0.617929, 0.955448, 0.479388, 0.527085, 0.029387, 0.607612, 0.047569, 0.592770, 0.402647, 0.904394, 0.330131, 0.310837, 0.854945, 0.734150, 0.098627,
 0.340459, 0.401740, 0.985167, 0.629595, 0.831962, 0.437120, 0.791227, 0.059462, 0.135135, 0.543824, 0.930452, 0.884899, 0.389895, 0.414558, 0.856863, 0.286759, 0.107757, 0.576676, 0.777602, 0.142251, 0.808677, 0.748094, 0.410025,
 0.475116, 0.344460, 0.416799, 0.023648, 0.525811, 0.717892, 0.236023, 0.694865, 0.779951, 0.762750, 0.121563, 0.370518, 0.928251, 0.766006, 0.289580, 0.983661, 0.428107, 0.037813, 0.598326, 0.015596, 0.756724, 0.130679, 0.374567,
 0.217496, 0.577797, 0.205663, 0.787021, 0.748369, 0.455336, 0.285268, 0.850220, 0.450008, 0.835012, 0.430246, 0.330996, 0.950872, 0.248873, 0.356794, 0.417871, 0.497053, 0.885293, 0.563165, 0.685500, 0.967759, 0.182369, 0.567805,
 0.029665, 0.203962, 0.157765, 0.547594, 0.203991, 0.211287, 0.067294, 0.193097, 0.901522, 0.782049, 0.302590, 0.422008, 0.989525, 0.797121, 0.387472, 0.181940, 0.224731, 0.737383, 0.533507, 0.664736, 0.201831, 0.468824, 0.566497,
 0.097464, 0.277024, 0.017312, 0.563884, 0.886940, 0.070190, 0.062021, 0.253307, 0.307065, 0.896640, 0.134073, 0.947710, 0.429455, 0.078126, 0.783667, 0.443670, 0.421182, 0.701886, 0.189821, 0.126220, 0.248059, 0.657928, 0.137505,
 0.322776, 0.822514, 0.013841, 0.923707, 0.376353, 0.582069, 0.261373, 0.413709, 0.687697, 0.753721, 0.171328, 0.129204, 0.158990, 0.292758, 0.857593, 0.278877, 0.688424, 0.838890, 0.814977, 0.048614, 0.072942, 0.430979, 0.749826,
 0.151954, 0.412267, 0.102516, 0.761123, 0.641786, 0.734910, 0.547414, 0.099882, 0.732324, 0.528830, 0.519475, 0.286500, 0.947320, 0.426137, 0.494295, 0.702475, 0.258223, 0.811368, 0.880436, 0.001128, 0.107099, 0.273252, 0.983986,
 0.361123, 0.816799, 0.855262, 0.289413, 0.399486, 0.008759, 0.063785, 0.967694, 0.683162, 0.301556, 0.725423, 0.923585, 0.649014, 0.915768, 0.536122, 0.976677, 0.233476, 0.418559, 0.581879, 0.780298, 0.200170, 0.901052, 0.129137,
 0.928666, 0.764864, 0.479741, 0.556377, 0.012088, 0.383736, 0.377665, 0.728552, 0.543807, 0.677556, 0.589766, 0.282764, 0.107827, 0.298733, 0.528771, 0.905013, 0.437309, 0.951172, 0.232344, 0.519998, 0.616537, 0.188277, 0.979256,
 0.503197, 0.160059, 0.539533, 0.192562, 0.249064, 0.330188, 0.150881, 0.622583, 0.211723, 0.943823, 0.622012, 0.213676, 0.473675, 0.842723, 0.441184, 0.119022, 0.920418, 0.674266, 0.164721, 0.780024, 0.709523, 0.184820, 0.776733,
 0.215191, 0.254506, 0.662861, 0.960615, 0.942083, 0.808005, 0.797347, 0.986451, 0.699889, 0.664521, 0.117867, 0.689696, 0.155395, 0.327687, 0.451219, 0.411565, 0.507467, 0.846274, 0.824898, 0.568791, 0.691978, 0.140441, 0.847326,
 0.701252, 0.207550, 0.066953, 0.439221, 0.844976, 0.301014, 0.339061, 0.537427, 0.032927, 0.878691, 0.181444, 0.964765, 0.727520, 0.445014, 0.799276, 0.495390, 0.331074, 0.153593, 0.737218, 0.448911, 0.350289, 0.618333, 0.941504,
 0.137884, 0.758674, 0.044803, 0.586638, 0.657517, 0.009904, 0.966803, 0.336267, 0.844593, 0.611781, 0.012352, 0.852300, 0.511202, 0.234461, 0.670445, 0.802138, 0.060274, 0.760440, 0.618569, 0.924027, 0.361776, 0.203505, 0.325599,
 0.556151, 0.874418, 0.948130, 0.030367, 0.405383, 0.766816, 0.661961, 0.868516, 0.794053, 0.569046, 0.587080, 0.575568, 0.573114, 0.280716, 0.749100, 0.419331, 0.801361, 0.001114, 0.168272, 0.537376, 0.964634, 0.922168, 0.098130,
 0.793530, 0.326097, 0.843240, 0.042622, 0.379912, 0.166723, 0.400849, 0.152887, 0.327416, 0.480203, 0.644941, 0.123787, 0.637737, 0.742841, 0.890233, 0.895116, 0.094762, 0.879013, 0.396302, 0.013252, 0.773602, 0.506976, 0.118753,
 0.079173, 0.553739, 0.076790, 0.776958, 0.154755, 0.411190, 0.028689, 0.818182, 0.630572, 0.156373, 0.611236, 0.555519, 0.887865, 0.483415, 0.337057, 0.115914, 0.080570, 0.477215, 0.130999, 0.668024, 0.689416, 0.001860, 0.280439,
 0.286966, 0.781115, 0.973539, 0.147590, 0.348749, 0.466720, 0.349256, 0.894516, 0.391834, 0.172739, 0.576338, 0.926752, 0.312669, 0.497947, 0.224416, 0.432815, 0.717982, 0.562607, 0.929520, 0.914212, 0.396091, 0.502711, 0.627847,
 0.421923, 0.615550, 0.710707, 0.244126, 0.876882, 0.250477, 0.369880, 0.566283, 0.217864, 0.271997, 0.019268, 0.415080, 0.632939, 0.654157, 0.269817, 0.475689, 0.060628, 0.205744, 0.273791, 0.259858, 0.842878, 0.757662, 0.234694,
 0.714819, 0.550568, 0.775468, 0.693666, 0.481654, 0.838478, 0.110598, 0.803228, 0.865183, 0.803430, 0.799570, 0.516156, 0.234027, 0.877791, 0.631462, 0.995121, 0.988361, 0.280004, 0.459426, 0.459604, 0.969674, 0.531614, 0.776546,
 0.705702, 0.437826, 0.431918, 0.020383, 0.190044, 0.592893, 0.130567, 0.040034, 0.592684, 0.300050, 0.856824, 0.546083, 0.349950, 0.803461, 0.331749, 0.574624, 0.664677, 0.048388, 0.614279, 0.704508, 0.798706, 0.919066, 0.900113,
 0.160498, 0.075013, 0.533332, 0.717828, 0.968303, 0.461421, 0.394094, 0.742052, 0.970176, 0.074664, 0.003969, 0.589347, 0.997209, 0.586054, 0.714503, 0.077351, 0.577772, 0.684452, 0.458052, 0.438435, 0.757113, 0.268373, 0.341643,
 0.390500, 0.443388, 0.409461, 0.251855, 0.112673, 0.978020, 0.820092, 0.902341, 0.463562, 0.666881, 0.237508, 0.566043, 0.953803, 0.882386, 0.927616, 0.878197, 0.360088, 0.149935, 0.238711, 0.020065, 0.590406, 0.725585, 0.577121,
 0.633969, 0.703291, 0.431371, 0.314658, 0.455114, 0.399408, 0.581885, 0.406285, 0.743083, 0.971498, 0.367835, 0.033346, 0.664506, 0.576755, 0.982178, 0.483788, 0.217734, 0.820093, 0.520502, 0.030053, 0.640343, 0.371842, 0.463787,
 0.514803, 0.431669, 0.631498, 0.578140, 0.154435, 0.726635, 0.060821, 0.828036, 0.123973, 0.235709, 0.593408, 0.397863, 0.400651, 0.921588, 0.660621, 0.917874, 0.176973, 0.009183, 0.610359, 0.277141, 0.669975, 0.455543, 0.218579,
 0.025687, 0.067579, 0.164903, 0.264842, 0.884078, 0.584299, 0.686116, 0.751784, 0.620533, 0.519206, 0.930932, 0.861573, 0.867738, 0.894160, 0.298352, 0.555788, 0.944409, 0.728229, 0.069346, 0.312138, 0.546245, 0.506315, 0.775256,
 0.654062, 0.168717, 0.610952, 0.560339, 0.247143, 0.227617, 0.793650, 0.112856, 0.617613, 0.224993, 0.506919, 0.179430, 0.279107, 0.656781, 0.644081, 0.793987, 0.848401, 0.128037, 0.938332, 0.326204, 0.781735, 0.801920, 0.655375,
 0.106279, 0.690743, 0.594944, 0.928650, 0.424934, 0.899825, 0.694314, 0.989192, 0.992361, 0.411612, 0.030129, 0.943027, 0.710585, 0.758975, 0.318025, 0.660410, 0.754092, 0.173774, 0.494306, 0.729068, 0.238123, 0.005347, 0.609284,
 0.456686, 0.097674, 0.614943, 0.104462, 0.208434, 0.508720, 0.873166, 0.470136, 0.258443, 0.202764, 0.541660, 0.422241, 0.098161, 0.193340, 0.951244, 0.326915, 0.260888, 0.726285, 0.548345, 0.779732, 0.125142, 0.052387, 0.699704,
 0.171297, 0.487379, 0.910375, 0.079989, 0.164529, 0.538009, 0.299481, 0.554226, 0.210203, 0.027399, 0.253996, 0.571208, 0.240009, 0.117743, 0.338105, 0.147487, 0.220519, 0.632595, 0.528987, 0.814882, 0.312149, 0.041664, 0.011782,
 0.776149, 0.455588, 0.660762, 0.273892, 0.640369, 0.570786, 0.765012, 0.251597, 0.746583, 0.482660, 0.870398, 0.282683, 0.314292, 0.731087, 0.550148, 0.279303, 0.643675, 0.643953, 0.237334, 0.020548, 0.816145, 0.280093, 0.395495,
 0.329903, 0.884452, 0.928713, 0.790200, 0.345172, 0.286481, 0.893544, 0.894628, 0.194525, 0.141865, 0.919624, 0.442896, 0.389521, 0.497198, 0.229824, 0.990528, 0.752562, 0.353989, 0.134171, 0.939943, 0.970885, 0.151455, 0.230671,
 0.680794, 0.061468, 0.069999, 0.520005, 0.324538, 0.431015, 0.184992, 0.262439, 0.476724, 0.503265, 0.826684, 0.083110, 0.789324, 0.383444, 0.011152, 0.917027, 0.822457, 0.948730, 0.097894, 0.628528, 0.376342, 0.446640, 0.828647,
 0.058200, 0.739954, 0.893387, 0.114171, 0.375656, 0.755318, 0.675430, 0.694141, 0.746879, 0.681422, 0.043860, 0.511001, 0.773553, 0.228391, 0.068160, 0.050941, 0.008081, 0.942853, 0.888794, 0.331443, 0.186491, 0.184566, 0.223349,
 0.800637, 0.012523, 0.020645, 0.375463, 0.382570, 0.245888, 0.458656, 0.947150, 0.806270, 0.137537, 0.977895, 0.931357, 0.325030, 0.572051, 0.437131, 0.371695, 0.923597, 0.544773, 0.227718, 0.719454, 0.161151, 0.257582, 0.297267,
 0.934453, 0.271332, 0.366259, 0.138049, 0.196541, 0.979411, 0.974074, 0.466172, 0.390032, 0.471682, 0.325781, 0.515710, 0.755857, 0.911732, 0.814646, 0.242712, 0.878354, 0.019317, 0.639087, 0.337058, 0.478516, 0.264930, 0.017157,
 0.538055, 0.956986, 0.769874, 0.334333, 0.058398, 0.792559, 0.698774, 0.526352, 0.859498, 0.420371, 0.778388, 0.713236, 0.461375, 0.144161, 0.140528, 0.947970, 0.987438, 0.675065, 0.363519, 0.533791, 0.266908, 0.622431, 0.849438,
 0.823783, 0.276793, 0.417574, 0.325234, 0.636988, 0.685779, 0.847397, 0.674538, 0.313796, 0.151864, 0.370201, 0.701581, 0.162825, 0.664818, 0.330211, 0.085279, 0.310275, 0.157599, 0.870338, 0.314472, 0.205008, 0.834709, 0.945227,
 0.996847, 0.728041, 0.880385, 0.179936, 0.624889, 0.739099, 0.068730, 0.115567, 0.760698, 0.216406, 0.157966, 0.484427, 0.094685, 0.925439, 0.121345, 0.806916, 0.945699, 0.820376, 0.260756, 0.684142, 0.335163, 0.436068, 0.031363,
 0.448733, 0.696412, 0.182209, 0.145792, 0.246335, 0.811875, 0.283932, 0.846460, 0.457696, 0.522419, 0.719168, 0.459726, 0.582194, 0.770587, 0.743032, 0.754181, 0.125142, 0.346252, 0.839238, 0.698742, 0.640762, 0.351110, 0.339772,
 0.889092, 0.248416, 0.517341, 0.663857, 0.820956, 0.670490, 0.563723, 0.568616, 0.926607, 0.035442, 0.285545, 0.915157, 0.650800, 0.389162, 0.917146, 0.939249, 0.071047, 0.492534, 0.920218, 0.594927, 0.615103, 0.124244, 0.007545,
 0.197825, 0.198834, 0.717318, 0.259373, 0.833520, 0.707011, 0.838244, 0.304813, 0.081436, 0.727281, 0.261868, 0.978408, 0.819135, 0.086592, 0.337431, 0.705364, 0.762835, 0.245273, 0.005516, 0.278994, 0.031365, 0.395921, 0.377350,
 0.159937, 0.221524, 0.064177, 0.782496, 0.016091, 0.097661, 0.691614, 0.824812, 0.830442, 0.137663, 0.625676, 0.346481, 0.504696, 0.467318, 0.879261, 0.513602, 0.781583, 0.546881, 0.137762, 0.122233, 0.377831, 0.754815, 0.671870,
 0.939940, 0.171064, 0.416898, 0.900954, 0.594014, 0.903371, 0.042831, 0.388396, 0.483739, 0.266397, 0.510982, 0.533889, 0.111065, 0.109738, 0.654974, 0.358617, 0.413047, 0.386849, 0.388618, 0.710727, 0.352349, 0.919090, 0.909232,
 0.307080, 0.531533, 0.045840, 0.160360, 0.082412, 0.685974, 0.241524, 0.145405, 0.851511, 0.962724, 0.152090, 0.661678, 0.769633, 0.183062, 0.644961, 0.883226, 0.170043, 0.229707, 0.117079, 0.039759, 0.187393, 0.101539, 0.782731,
 0.312618, 0.966816, 0.485300, 0.111899, 0.357502, 0.989440, 0.242222, 0.234549, 0.443519, 0.096936, 0.475254, 0.518324, 0.710618, 0.605362, 0.697729, 0.796414, 0.149914, 0.599481, 0.575820, 0.826934, 0.526551, 0.183970, 0.164180,
 0.541138, 0.031209, 0.431182, 0.037055, 0.849396, 0.168056, 0.753677, 0.166290, 0.163555, 0.483791, 0.893629, 0.384541, 0.132497, 0.699619, 0.063889, 0.258403, 0.223123, 0.626611, 0.603241, 0.784721, 0.888301, 0.691511, 0.880996,
 0.507139, 0.901500, 0.756845, 0.865998, 0.062841, 0.587784, 0.234104, 0.478380, 0.335778, 0.803806, 0.825159, 0.487840, 0.977548, 0.604594, 0.914195, 0.681908, 0.116160, 0.149685, 0.014844, 0.376837, 0.302446, 0.941054, 0.365428,
 0.110732, 0.730761, 0.423878, 0.885054, 0.774692, 0.242498, 0.120312, 0.146509, 0.187743, 0.695826, 0.185948, 0.695610, 0.592824, 0.721895, 0.786527, 0.123747, 0.084887, 0.335805, 0.274877, 0.469307, 0.122231, 0.120116, 0.419264,
 0.238588, 0.015603, 0.930028, 0.086695, 0.067389, 0.336511, 0.104131, 0.838754, 0.526112, 0.189957, 0.714460, 0.951885, 0.054838, 0.113900, 0.307282, 0.855447, 0.517120, 0.307636, 0.114543, 0.894213, 0.414339, 0.966833, 0.739143,
 0.500282, 0.704240, 0.679764, 0.609549, 0.213772, 0.231277, 0.286525, 0.975708, 0.761384, 0.581609, 0.942589, 0.184527, 0.404398, 0.316818, 0.685227, 0.375546, 0.648356, 0.878048, 0.854412, 0.412839, 0.889240, 0.362143, 0.750295,
 0.795089, 0.707152, 0.995217, 0.851668, 0.830459, 0.938193, 0.624103, 0.411863, 0.535025, 0.816468, 0.335112, 0.880946, 0.412599, 0.916966, 0.331033, 0.169895, 0.539804, 0.210890, 0.155372, 0.520741, 0.643917, 0.710011, 0.967223,
 0.626556, 0.193529, 0.196521, 0.826192, 0.560108, 0.885455, 0.758122, 0.345493, 0.126312, 0.408090, 0.951567, 0.879264, 0.010913, 0.484620, 0.656417, 0.909538, 0.818850, 0.083138, 0.137393, 0.037002, 0.996234, 0.348961, 0.669549,
 0.021229, 0.128846, 0.432577, 0.784782, 0.489416, 0.079501, 0.510118, 0.838364, 0.082731, 0.441840, 0.118505, 0.065676, 0.046360, 0.377318, 0.541271, 0.575871, 0.219303, 0.796865, 0.611107, 0.112396, 0.434460, 0.233444, 0.559555,
 0.780257, 0.905878, 0.514240, 0.701769, 0.004387, 0.930892, 0.322275, 0.944334, 0.528828, 0.358973, 0.980823, 0.636726, 0.406756, 0.730296, 0.797180, 0.832895, 0.359125, 0.735232, 0.265244, 0.909858, 0.115407, 0.926426, 0.502439,
 0.917013, 0.992108, 0.178849, 0.842090, 0.355769, 0.940307, 0.756778, 0.327019, 0.057037, 0.107183, 0.542395, 0.466690, 0.818085, 0.941459, 0.806258, 0.494320, 0.754533, 0.439963, 0.168989, 0.161101, 0.332944, 0.021372, 0.195081,
 0.954040, 0.580200, 0.041070, 0.468526, 0.347093, 0.463940, 0.273380, 0.386417, 0.070653, 0.240671, 0.007882, 0.146632, 0.393382, 0.152311, 0.286865, 0.140919, 0.487556, 0.086010, 0.381037, 0.852243, 0.232038, 0.204982, 0.818486,
 0.318117, 0.386774, 0.509080, 0.183621, 0.698187, 0.242961, 0.333066, 0.852751, 0.678756, 0.361157, 0.868674, 0.773374, 0.578872, 0.305494, 0.474753, 0.095800, 0.577519, 0.769353, 0.280169, 0.776844, 0.460157, 0.772708, 0.989047,
 0.372993, 0.452169, 0.753388, 0.860663, 0.879347, 0.848710, 0.136566, 0.190589, 0.233934, 0.312990, 0.064514, 0.789940, 0.655679, 0.461645, 0.321138, 0.610851, 0.307805, 0.015533, 0.465066, 0.854300, 0.443940, 0.711939, 0.866225,
 0.317278, 0.597395, 0.959990, 0.752366, 0.977179, 0.988459, 0.001107, 0.745196, 0.599485, 0.951967, 0.002158, 0.385297, 0.608665, 0.207668, 0.967872, 0.198536, 0.340423, 0.417111, 0.783764, 0.284998, 0.745424, 0.978939, 0.779990,
 0.758562, 0.430438, 0.023995, 0.394067, 0.628189, 0.783217, 0.274756, 0.276165, 0.590925, 0.653462, 0.850937, 0.259976, 0.354942, 0.366099, 0.333248, 0.265937, 0.385014, 0.577106, 0.866945, 0.428534, 0.672331, 0.889553, 0.800955,
 0.577663, 0.288014, 0.112210, 0.149949, 0.953462, 0.051153, 0.339239, 0.946891, 0.508189, 0.796368, 0.572446, 0.561626, 0.926578, 0.085037, 0.702687, 0.114283, 0.172532, 0.093550, 0.260863, 0.856285, 0.805236, 0.109536, 0.278187,
 0.550054, 0.877864, 0.054528, 0.837795, 0.503018, 0.078535, 0.663403, 0.823282, 0.631968, 0.053339, 0.024969, 0.983351, 0.208321, 0.987923, 0.715040, 0.634701, 0.299500, 0.455746, 0.527196, 0.157736, 0.240662, 0.971881, 0.074205,
 0.332218, 0.186876, 0.340729, 0.245114, 0.583949, 0.178972, 0.739769, 0.562366, 0.330845, 0.555352, 0.040267, 0.996736, 0.929933, 0.633804, 0.396470, 0.324669, 0.728922, 0.626707, 0.919358, 0.915655, 0.551057, 0.822885, 0.837380,
 0.132306, 0.614099, 0.331021, 0.998305, 0.731769, 0.568831, 0.622088, 0.712318, 0.646139, 0.594847, 0.799235, 0.621739, 0.866719, 0.508054, 0.858315, 0.617355, 0.390753, 0.702068, 0.154579, 0.431493, 0.040988, 0.692138, 0.162563,
 0.815113, 0.622688, 0.673522, 0.286880, 0.557660, 0.227282, 0.105128, 0.659461, 0.706958, 0.367413, 0.033220, 0.329851, 0.380657, 0.078754, 0.968985, 0.133792, 0.484601, 0.646684, 0.308574, 0.503888, 0.436821, 0.410851, 0.855993,
 0.085052, 0.873996, 0.321241, 0.190041, 0.835501, 0.041070, 0.921988, 0.758673, 0.257850, 0.650990, 0.033364, 0.431121, 0.019402, 0.982358, 0.100952, 0.173397, 0.818942, 0.656059, 0.449650, 0.811025, 0.350410, 0.211158, 0.187899,
 0.918918, 0.340979, 0.364558, 0.376422, 0.537957, 0.219706, 0.782902, 0.130975, 0.107328, 0.148526, 0.679263, 0.348502, 0.057899, 0.143608, 0.447324, 0.712073, 0.412496, 0.383696, 0.503583, 0.976670, 0.351921, 0.672922, 0.180831,
 0.402894, 0.539824, 0.356730, 0.826674, 0.679930, 0.454488, 0.817054, 0.647548, 0.056460, 0.591195, 0.361121, 0.505789, 0.425781, 0.105862, 0.809445, 0.044376, 0.239398, 0.840293, 0.153380, 0.666552, 0.755018, 0.163638, 0.139543,
 0.036406, 0.866780, 0.285980, 0.724320, 0.658436, 0.677153, 0.641613, 0.781387, 0.521227, 0.813967, 0.235251, 0.821710, 0.800584, 0.988359, 0.018585, 0.460017, 0.774182, 0.348899, 0.082277, 0.923435, 0.732224, 0.185931, 0.765365,
 0.614326, 0.121309, 0.553416, 0.396379, 0.005803, 0.042248, 0.431546, 0.875189, 0.438564, 0.662719, 0.383683, 0.409283, 0.503379, 0.994168, 0.950213, 0.550864, 0.014630, 0.726369, 0.657451, 0.100812, 0.314750, 0.585480, 0.842595,
 0.101441, 0.083388, 0.670662, 0.049874, 0.322210, 0.572894, 0.884589, 0.535462, 0.531819, 0.445640, 0.374047, 0.129749, 0.267566, 0.169291, 0.573788, 0.998366, 0.270636, 0.332029, 0.250760, 0.574524, 0.106369, 0.697734, 0.893393,
 0.414179, 0.974951, 0.335998, 0.914069, 0.965118, 0.723998, 0.566738, 0.729978, 0.206983, 0.374636, 0.665114, 0.671875, 0.980502, 0.081989, 0.518766, 0.420778, 0.202840, 0.675066, 0.533390, 0.884235, 0.387246, 0.706223, 0.689948,
 0.956639, 0.227307, 0.123607, 0.490921, 0.838358, 0.950620, 0.238700, 0.645765, 0.657543, 0.628151, 0.963565, 0.367002, 0.203847, 0.281584, 0.347378, 0.536140, 0.067552, 0.052477, 0.148665, 0.289040, 0.319444, 0.677906, 0.578562,
 0.627688, 0.770134, 0.304641, 0.871815, 0.931956, 0.532240, 0.217884, 0.057651, 0.991319, 0.060176, 0.264558, 0.173806, 0.751403, 0.561329, 0.721546, 0.296566, 0.050206, 0.925604, 0.655628, 0.640756, 0.459007, 0.390062, 0.706940,
 0.749977, 0.026106, 0.589363, 0.917333, 0.473552, 0.408977, 0.504578, 0.138043, 0.177499, 0.061272, 0.941630, 0.559581, 0.018174, 0.882801, 0.390979, 0.586037, 0.315297, 0.938747, 0.552469, 0.154254, 0.182708, 0.482396, 0.079528,
 0.407107, 0.838087, 0.875516, 0.596749, 0.553925, 0.035044, 0.971946, 0.181438, 0.677668, 0.101313, 0.821085, 0.946000, 0.018597, 0.274628, 0.944803, 0.302333, 0.921817, 0.693319, 0.058236, 0.885964, 0.678307, 0.202333, 0.634204,
 0.936158, 0.116031, 0.398610, 0.673269, 0.403974, 0.162850, 0.274415, 0.035202, 0.944314, 0.062588, 0.775861, 0.639776, 0.756885, 0.289358, 0.319687, 0.260069, 0.507006, 0.985839, 0.188085, 0.842994, 0.078126, 0.414361, 0.023451,
 0.710590, 0.997274, 0.124464, 0.787464, 0.361070, 0.111553, 0.428985, 0.337407, 0.218020, 0.572907, 0.167985, 0.653564, 0.715040, 0.770136, 0.404276, 0.061657, 0.775145, 0.407416, 0.617284, 0.011273, 0.601125, 0.109104, 0.578219,
 0.963100, 0.864724, 0.388449, 0.536583, 0.213523, 0.826903, 0.476028, 0.716810, 0.443902, 0.674770, 0.800550, 0.708231, 0.963940, 0.787238, 0.054438, 0.878659, 0.954440, 0.683478, 0.978837, 0.901921, 0.075829, 0.836287, 0.752890,
 0.265867, 0.056960, 0.195251, 0.171209, 0.826318, 0.916459, 0.338720, 0.408037, 0.153532, 0.289428, 0.414164, 0.878854, 0.343853, 0.569386, 0.016255, 0.568080, 0.856046, 0.753419, 0.552329, 0.758369, 0.698324, 0.662607, 0.444904,
 0.702095, 0.310404, 0.655556, 0.288006, 0.763487, 0.830169, 0.756608, 0.203797, 0.541073, 0.135077, 0.868960, 0.611958, 0.580394, 0.335726, 0.163927, 0.228233, 0.398119, 0.957585, 0.949289, 0.730396, 0.567866, 0.264464, 0.080626,
 0.377621, 0.738101, 0.394691, 0.939395, 0.229030, 0.351886, 0.742144, 0.950127, 0.093192, 0.971149, 0.316241, 0.727271, 0.145091, 0.944187, 0.358045, 0.051120, 0.290931, 0.429744, 0.926982, 0.683885, 0.942854, 0.755087, 0.360841,
 0.403832, 0.311876, 0.755759, 0.760252, 0.800655, 0.643045, 0.153256, 0.970409, 0.222703, 0.834981, 0.471632, 0.893276, 0.570736, 0.517668, 0.288489, 0.531804, 0.308569, 0.146976, 0.338997, 0.565853, 0.413964, 0.829165, 0.738327,
 0.812810, 0.969744, 0.059180, 0.218407, 0.912549, 0.532184, 0.066454, 0.165310, 0.477943, 0.288911, 0.062956, 0.535413, 0.703084, 0.699903, 0.010268, 0.578887, 0.659466, 0.811554, 0.023540, 0.131108, 0.826411, 0.644570, 0.746361,
 0.984496, 0.589926, 0.021672, 0.826206, 0.229338, 0.751071, 0.351499, 0.437527, 0.172029, 0.460916, 0.291822, 0.909238, 0.488137, 0.347703, 0.840316, 0.263995, 0.146892, 0.029202, 0.347071, 0.544964, 0.619567, 0.269193, 0.703016,
 0.446682, 0.239094, 0.004078, 0.666226, 0.063076, 0.859945, 0.549730, 0.968109, 0.678867, 0.239260, 0.918719, 0.658760, 0.949759, 0.651604, 0.206140, 0.661006, 0.609294, 0.005222, 0.013526, 0.902113, 0.901620, 0.257624, 0.403160,
 0.767693, 0.243279, 0.858776, 0.640892, 0.201633, 0.465196, 0.672624, 0.178911, 0.176942, 0.258335, 0.749464, 0.838519, 0.744156, 0.850811, 0.115527, 0.817625, 0.575679, 0.266931, 0.600679, 0.012336, 0.156314, 0.873824, 0.594307,
 0.703776, 0.202816, 0.001383, 0.473134, 0.120321, 0.849905, 0.746800, 0.518108, 0.895053, 0.828783, 0.244530, 0.181727, 0.354897, 0.639236, 0.622902, 0.098702, 0.711232, 0.321110, 0.084706, 0.784026, 0.084337, 0.249114, 0.373826,
 0.331103, 0.351759, 0.680891, 0.734671, 0.922106, 0.732521, 0.997208, 0.074820, 0.672280, 0.926994, 0.490559, 0.721526, 0.344182, 0.351589, 0.630785, 0.064886, 0.147235, 0.316334, 0.300365, 0.728297, 0.058457, 0.873486, 0.025393,
 0.251970, 0.503328, 0.951035, 0.599097, 0.747729, 0.478844, 0.414084, 0.211799, 0.540998, 0.206588, 0.960355, 0.663984, 0.494987, 0.610676, 0.547524, 0.987587, 0.876843, 0.527836, 0.116725, 0.765407, 0.218315, 0.794266, 0.168585,
 0.191066, 0.414828, 0.701805, 0.668955, 0.913946, 0.145380, 0.775143, 0.417380, 0.567708, 0.758134, 0.159049, 0.462086, 0.580461, 0.807859, 0.902021, 0.153324, 0.043092, 0.034470, 0.128272, 0.708274, 0.594311, 0.398004, 0.067031,
 0.634311, 0.131415, 0.823641, 0.485823, 0.395206, 0.730484, 0.128277, 0.597480, 0.382188, 0.154864, 0.593152, 0.589439, 0.596836, 0.333325, 0.288789, 0.684363, 0.697095, 0.926742, 0.350726, 0.974946, 0.885827, 0.692720, 0.327161,
 0.090104, 0.920917, 0.559968, 0.680655, 0.217436, 0.637870, 0.787989, 0.811552, 0.816455, 0.344043, 0.102671, 0.195132, 0.962593, 0.265161, 0.445426, 0.579983, 0.133216, 0.599825, 0.773839, 0.702228, 0.119144, 0.319612, 0.917324,
 0.727733, 0.869428, 0.497569, 0.674543, 0.068943, 0.470049, 0.965940, 0.383494, 0.339435, 0.150900, 0.507597, 0.984057, 0.840769, 0.507870, 0.540624, 0.900701, 0.033665, 0.831413, 0.530845, 0.815206, 0.168438, 0.128875, 0.784755,
 0.881100, 0.455188, 0.709687, 0.160683, 0.099585, 0.488070, 0.930787, 0.867155, 0.267668, 0.823646, 0.250133, 0.376064, 0.100827, 0.702662, 0.250666, 0.498696, 0.192802, 0.527368, 0.932693, 0.492818, 0.412869, 0.320893, 0.752299,
 0.279569, 0.055660, 0.537123, 0.545500, 0.243853, 0.590249, 0.123459, 0.326551, 0.918335, 0.634477, 0.106999, 0.703845, 0.059206, 0.878687, 0.447666, 0.303476, 0.114891, 0.062022, 0.828235, 0.629898, 0.973358, 0.866818, 0.107773,
 0.491612, 0.709083, 0.235513, 0.606725, 0.759873, 0.099891, 0.263674, 0.301575, 0.357723, 0.162482, 0.506721, 0.546021, 0.058790, 0.282541, 0.421580, 0.592981, 0.739343, 0.562946, 0.303662, 0.035075, 0.017712, 0.356862, 0.670419,
 0.205841, 0.842845, 0.119884, 0.672188, 0.563567, 0.184329, 0.438211, 0.006746, 0.416051, 0.921350, 0.552718, 0.593954, 0.917559, 0.734969, 0.789396, 0.190032, 0.786951, 0.949648, 0.865868, 0.336658, 0.444365, 0.034600, 0.115331,
 0.838351, 0.357304, 0.817402, 0.750636, 0.023085, 0.017866, 0.351118, 0.582204, 0.687111, 0.661645, 0.047249, 0.257708, 0.067454, 0.184183, 0.891180, 0.465119, 0.776991, 0.639554, 0.811032, 0.443538, 0.120678, 0.722466, 0.107082,
 0.090878, 0.970986, 0.073407, 0.065238, 0.409723, 0.690587, 0.516707, 0.787650, 0.661487, 0.865564, 0.183415, 0.955807, 0.526538, 0.658836, 0.201870, 0.790840, 0.619201, 0.857170, 0.404283, 0.252860, 0.660104, 0.503102, 0.521249,
 0.401721, 0.445342, 0.852900, 0.169557, 0.467453, 0.907272, 0.151379, 0.711859, 0.206739, 0.172824, 0.872910, 0.737766, 0.475362, 0.683076, 0.044560, 0.520804, 0.063639, 0.436282, 0.582211, 0.708682, 0.926968, 0.002728, 0.164175,
 0.647932, 0.216151, 0.583861, 0.610749, 0.898566, 0.457674, 0.453408, 0.494073, 0.508727, 0.853511, 0.231217, 0.373244, 0.670015, 0.729903, 0.660697, 0.627979, 0.216283, 0.447137, 0.650922, 0.984008, 0.645014, 0.635559, 0.020314,
 0.009297, 0.592316, 0.010221, 0.559453, 0.777062, 0.969082, 0.566937, 0.087879, 0.595177, 0.834568, 0.020773, 0.189123, 0.888084, 0.939903, 0.524422, 0.297770, 0.573781, 0.191589, 0.124581, 0.013754, 0.332342, 0.865558, 0.970935,
 0.616673, 0.131850, 0.625754, 0.891801, 0.134597, 0.021481, 0.475544, 0.765895, 0.328285, 0.169090, 0.981525, 0.839386, 0.126938, 0.202116, 0.071965, 0.215620, 0.392679, 0.286551, 0.618326, 0.651242, 0.098042, 0.870477, 0.878810,
 0.262388, 0.727149, 0.338096, 0.816058, 0.029018, 0.813510, 0.819839, 0.724012, 0.101029, 0.314956, 0.327215, 0.516136, 0.888653, 0.011836, 0.111332, 0.271949, 0.059738, 0.264191, 0.455856, 0.122098, 0.681257, 0.445891, 0.923707,
 0.513339, 0.410905, 0.811042, 0.687604, 0.151173, 0.461668, 0.765250, 0.933943, 0.114967, 0.888131, 0.790089, 0.274471, 0.501875, 0.953964, 0.084746, 0.608132, 0.533463, 0.077175, 0.278801, 0.314686, 0.982540, 0.270477, 0.972288,
 0.040850, 0.505330, 0.521435, 0.964248, 0.558343, 0.531867, 0.129281, 0.632975, 0.271261, 0.639699, 0.336687, 0.277096, 0.358368, 0.254913, 0.384914, 0.266695, 0.325509, 0.360796, 0.365118, 0.491283, 0.876231, 0.996608, 0.438011,
 0.932864, 0.587469, 0.373840, 0.415344, 0.797340, 0.394971, 0.679765, 0.621796, 0.768646, 0.483728, 0.161142, 0.932894, 0.823144, 0.866505, 0.045654, 0.976833, 0.075949, 0.883716, 0.754377, 0.524802, 0.197952, 0.625878, 0.752642,
 0.920174, 0.617668, 0.062171, 0.174570, 0.122258, 0.533906, 0.542021, 0.011983, 0.191008, 0.445125, 0.548685, 0.483038, 0.206885, 0.633740, 0.742730, 0.069762, 0.112157, 0.716311, 0.697852, 0.344576, 0.449073, 0.824263, 0.451014,
 0.880117, 0.645227, 0.258517, 0.353595, 0.368428, 0.310873, 0.062257, 0.400611, 0.257229, 0.936956, 0.079107, 0.552504, 0.263061, 0.651346, 0.862743, 0.760403, 0.642247, 0.055272, 0.634654, 0.813609, 0.017562, 0.138818, 0.159146,
 0.490592, 0.376135, 0.395815, 0.431341, 0.195744, 0.905464, 0.322378, 0.920700, 0.577947, 0.030632, 0.803128, 0.448478, 0.968735, 0.096046, 0.544051, 0.724522, 0.653078, 0.916832, 0.795476, 0.952181, 0.440934, 0.053707, 0.483411,
 0.508086, 0.079615, 0.364570, 0.408211, 0.497715, 0.213123, 0.707719, 0.018027, 0.412404, 0.723325, 0.745665, 0.077835, 0.306673, 0.887652, 0.582718, 0.255963, 0.967055, 0.258619, 0.500206, 0.677377, 0.749142, 0.051676, 0.694512,
 0.050672, 0.865243, 0.050572, 0.617437, 0.283456, 0.361161, 0.174662, 0.036325, 0.043972, 0.230422, 0.362482, 0.121470, 0.506344, 0.684278, 0.912871, 0.907322, 0.299378, 0.585567, 0.882162, 0.116880, 0.246585, 0.226636, 0.197311,
 0.168302, 0.885939, 0.702005, 0.334573, 0.860420, 0.555216, 0.014037, 0.396698, 0.748672, 0.351817, 0.126787, 0.254077, 0.142442, 0.869329, 0.524568, 0.793243, 0.374341, 0.887198, 0.465895, 0.826490, 0.985272, 0.270732, 0.281863,
 0.586631, 0.537940, 0.117721, 0.435176, 0.079288, 0.295637, 0.572651, 0.019736, 0.650346, 0.190737, 0.892489, 0.281662, 0.597729, 0.644404, 0.977924, 0.124892, 0.304524, 0.499946, 0.134779, 0.425073, 0.375326, 0.789262, 0.771847,
 0.014599, 0.440828, 0.285564, 0.808977, 0.072556, 0.369008, 0.904601, 0.013818, 0.212661, 0.437347, 0.427669, 0.323819, 0.119330, 0.822478, 0.737752, 0.474145, 0.149728, 0.435341, 0.618129, 0.239887, 0.863708, 0.445688, 0.071844,
 0.125856, 0.132903, 0.714672, 0.319875, 0.978429, 0.937032, 0.094020, 0.942964, 0.346100, 0.504843, 0.101053, 0.692478, 0.338891, 0.377386, 0.677670, 0.032901, 0.607670, 0.743948, 0.565803, 0.544446, 0.024165, 0.960534, 0.649891,
 0.538653, 0.848428, 0.144970, 0.437563, 0.080737, 0.045428, 0.742315, 0.572191, 0.606018, 0.894890, 0.763281, 0.935468, 0.328185, 0.832505, 0.263837, 0.935206, 0.106944, 0.285940, 0.506271, 0.054593, 0.642078, 0.111091, 0.024109,
 0.625917, 0.138986, 0.593626, 0.611765, 0.186970, 0.623878, 0.124719, 0.634327, 0.175028, 0.130064, 0.870236, 0.266054, 0.245333, 0.113071, 0.577257, 0.533842, 0.192739, 0.842864, 0.164281, 0.783079, 0.471713, 0.655397, 0.750788,
 0.269931, 0.206801, 0.890622, 0.142807, 0.591512, 0.559366, 0.995230, 0.614091, 0.121780, 0.453687, 0.599632, 0.733359, 0.257892, 0.193997, 0.691620, 0.378545, 0.283692, 0.041835, 0.183723, 0.356211, 0.911718, 0.629068, 0.915513,
 0.881113, 0.913320, 0.305359, 0.232082, 0.755917, 0.732303, 0.450003, 0.849959, 0.378195, 0.097824, 0.097283, 0.454180, 0.660968, 0.607403, 0.253396, 0.885564, 0.819454, 0.194156, 0.233270, 0.474185, 0.501327, 0.680094, 0.569862,
 0.980268, 0.437353, 0.059249, 0.289971, 0.222132, 0.195842, 0.263048, 0.992967, 0.135482, 0.535973, 0.401709, 0.486540, 0.130264, 0.584987, 0.518302, 0.141715, 0.929914, 0.691780, 0.858676, 0.433534, 0.910322, 0.429156, 0.375831,
 0.780333, 0.447895, 0.189393, 0.429849, 0.120312, 0.161210, 0.018655, 0.395660, 0.146683, 0.740797, 0.406952, 0.585227, 0.630227, 0.436335, 0.348807, 0.665948, 0.299362, 0.168119, 0.038601, 0.703944, 0.722315, 0.686057, 0.502052,
 0.373438, 0.083637, 0.579096, 0.495634, 0.498661, 0.598839, 0.183071, 0.348622, 0.382846, 0.137626, 0.316183, 0.563278, 0.733261, 0.889979, 0.137171, 0.928397, 0.018205, 0.212682, 0.448521, 0.406606, 0.851217, 0.130106, 0.210728,
 0.908728, 0.251901, 0.965715, 0.476510, 0.317011, 0.097760, 0.774501, 0.057354, 0.326521, 0.676046, 0.296264, 0.987860, 0.169697, 0.341110, 0.111038, 0.823594, 0.417583, 0.956063, 0.247357, 0.391018, 0.423045, 0.076298, 0.258277,
 0.277136, 0.712226, 0.947775, 0.466800, 0.903590, 0.632295, 0.029711, 0.809555, 0.854903, 0.733325, 0.297011, 0.234284, 0.431439, 0.872830, 0.373295, 0.582100, 0.460524, 0.571538, 0.134662, 0.099744, 0.838343, 0.172470, 0.804441,
 0.360845, 0.283063, 0.332091, 0.430340, 0.249723, 0.376634, 0.037516, 0.650867, 0.488530, 0.257522, 0.948517, 0.674214, 0.820983, 0.726010, 0.423887, 0.719415, 0.646855, 0.042519, 0.140980, 0.819317, 0.514434, 0.170671, 0.052566,
 0.823481, 0.197851, 0.551663, 0.360270, 0.835540, 0.027486, 0.934250, 0.797644, 0.795861, 0.010308, 0.142540, 0.403291, 0.801737, 0.019189, 0.587466, 0.985620, 0.322151, 0.783007, 0.540865, 0.011840, 0.193824, 0.550104, 0.745206,
 0.836213, 0.114349, 0.494667, 0.457209, 0.173710, 0.914402, 0.197613, 0.977763, 0.947790, 0.785207, 0.759940, 0.639882, 0.033605, 0.653451, 0.337418, 0.975185, 0.684622, 0.280860, 0.551440, 0.799899, 0.291100, 0.369766, 0.403279,
 0.524222, 0.558525, 0.882671, 0.959716, 0.442621, 0.713863, 0.039303, 0.289561, 0.784108, 0.214046, 0.598726, 0.406237, 0.145514, 0.926541, 0.329271, 0.444975, 0.680416, 0.424214, 0.231273, 0.998868, 0.232629, 0.949252, 0.525763,
 0.620904, 0.975495, 0.582957, 0.528475, 0.524993, 0.113778, 0.054287, 0.867906, 0.684037, 0.719500, 0.540673, 0.607128, 0.914343, 0.780004, 0.385655, 0.422963, 0.380518, 0.337769, 0.270993, 0.267609, 0.871748, 0.076851, 0.443291,
 0.774517, 0.447669, 0.568347, 0.779683, 0.913120, 0.779471, 0.413037, 0.972255, 0.089796, 0.009321, 0.827664, 0.870695, 0.532346, 0.899402, 0.504559, 0.016953, 0.582411, 0.762139, 0.352927, 0.869725, 0.608016, 0.367357, 0.602848,
 0.901398, 0.993231, 0.062808, 0.646518, 0.172284, 0.873359, 0.464844, 0.410510, 0.678756, 0.474767, 0.659777, 0.330703, 0.531074, 0.554338, 0.178102, 0.549304, 0.156904, 0.183464, 0.478283, 0.510225, 0.326616, 0.107546, 0.109872,
 0.012386, 0.951428, 0.959401, 0.563661, 0.135839, 0.675409, 0.492033, 0.396266, 0.506505, 0.143506, 0.704428, 0.015488, 0.663639, 0.121480, 0.697853, 0.399293, 0.969686, 0.027310, 0.457408, 0.503914, 0.512994, 0.830091, 0.594965,
 0.594966, 0.836106, 0.120056, 0.709201, 0.749535, 0.440205, 0.656396, 0.163195, 0.257630, 0.812207, 0.077566, 0.200220, 0.169505, 0.965479, 0.197271, 0.817263, 0.253031, 0.026774, 0.409154, 0.299837, 0.745059, 0.417588, 0.951071,
 0.850489, 0.495820, 0.422538, 0.917903, 0.079597, 0.049476, 0.993569, 0.007978, 0.986558, 0.401607, 0.168042, 0.605869, 0.361161, 0.290487, 0.470109, 0.272659, 0.650853, 0.969331, 0.685852, 0.378452, 0.345095, 0.167479, 0.848739,
 0.081344, 0.851138, 0.966631, 0.616859, 0.258199, 0.908042, 0.321331, 0.268734, 0.216995, 0.491973, 0.059087, 0.670279, 0.975469, 0.293545, 0.163555, 0.979616, 0.749133, 0.538049, 0.623563, 0.845105, 0.741927, 0.715701, 0.911418,
 0.029427, 0.828948, 0.688700, 0.416618, 0.951138, 0.732511, 0.345056, 0.727036, 0.595424, 0.555710, 0.129185, 0.230798, 0.706360, 0.302528, 0.252555, 0.960952, 0.560634, 0.297056, 0.483884, 0.164835, 0.942305, 0.248740, 0.150095,
 0.634501, 0.022697, 0.393628, 0.312381, 0.158162, 0.610195, 0.486789, 0.185153, 0.460327, 0.910676, 0.007595, 0.361489, 0.995841, 0.348012, 0.362023, 0.622156, 0.896667, 0.781054, 0.984974, 0.314557, 0.076888, 0.342294, 0.436793,
 0.974722, 0.958588, 0.165845, 0.993246, 0.964021, 0.857179, 0.098343, 0.004466, 0.040568, 0.768145, 0.553809, 0.868637, 0.573931, 0.027489, 0.518649, 0.768928, 0.334863, 0.115448, 0.199973, 0.061502, 0.696288, 0.167931, 0.381719,
 0.098052, 0.617957, 0.623950, 0.479194, 0.303649, 0.858340, 0.831929, 0.912149, 0.010568, 0.569913, 0.465769, 0.426258, 0.137984, 0.347538, 0.756062, 0.637643, 0.588040, 0.176901, 0.956678, 0.437987, 0.748766, 0.786702, 0.166499,
 0.139814, 0.817091, 0.063526, 0.775414, 0.852136, 0.609821, 0.313997, 0.958431, 0.623003, 0.961767, 0.830308, 0.998259, 0.263314, 0.828434, 0.960641, 0.861809, 0.448969, 0.441896, 0.436987, 0.617376, 0.987966, 0.704244, 0.253846,
 0.574862, 0.915190, 0.850532, 0.810053, 0.720184, 0.048422, 0.945298, 0.163311, 0.246290, 0.590644, 0.182051, 0.146794, 0.047082, 0.410323, 0.187551, 0.256651, 0.617684, 0.465340, 0.562186, 0.838396, 0.216124, 0.978183, 0.949417,
 0.332461, 0.599630, 0.018466, 0.772260, 0.541047, 0.505798, 0.030669, 0.919609, 0.859309, 0.048156, 0.064954, 0.642128, 0.686712, 0.720832, 0.360128, 0.524335, 0.264747, 0.032533, 0.901336, 0.949778, 0.644165, 0.331012, 0.370195,
 0.049032, 0.490501, 0.941170, 0.839202, 0.810312, 0.816085, 0.769252, 0.780791, 0.200165, 0.258349, 0.359216, 0.688170, 0.662948, 0.189467, 0.502737, 0.391603, 0.884214, 0.175720, 0.769645, 0.130171, 0.871973, 0.244262, 0.249722,
 0.563127, 0.153848, 0.150912, 0.779054, 0.212985, 0.646007, 0.669154, 0.504603, 0.688618, 0.725349, 0.584878, 0.750015, 0.383354, 0.449683, 0.033534, 0.355198, 0.950809, 0.206733, 0.459834, 0.433771, 0.578607, 0.332824, 0.421208,
 0.428676, 0.084005, 0.192609, 0.754547, 0.440227, 0.900652, 0.634425, 0.044047, 0.686387, 0.695169, 0.679642, 0.581953, 0.156582, 0.307285, 0.332217, 0.022753, 0.702887, 0.936884, 0.835640, 0.066838, 0.031638, 0.573176, 0.903326,
 0.847093, 0.010704, 0.970571, 0.015576, 0.259641, 0.979706, 0.496504, 0.937515, 0.509409, 0.069994, 0.710105, 0.059514, 0.316977, 0.026234, 0.957125, 0.901709, 0.942906, 0.430957, 0.793989, 0.330214, 0.770359, 0.216640, 0.968250,
 0.393015, 0.190158, 0.696973, 0.598480, 0.652316, 0.951589, 0.240566, 0.306026, 0.937120, 0.677485, 0.967676, 0.374592, 0.300364, 0.546685, 0.185610, 0.650037, 0.889993, 0.077267, 0.543747, 0.268652, 0.449498, 0.809728, 0.327819,
 0.422611, 0.282985, 0.502997, 0.808969, 0.069086, 0.050526, 0.183855, 0.890602, 0.675968, 0.514926, 0.200790, 0.370159, 0.011372, 0.920182, 0.948694, 0.259404, 0.443624, 0.639558, 0.476266, 0.731838, 0.868622, 0.947710, 0.041852,
 0.226382, 0.946096, 0.622575, 0.327187, 0.010098, 0.856975, 0.710545, 0.386646, 0.457505, 0.920824, 0.349813, 0.139898, 0.448843, 0.593039, 0.464684, 0.899066, 0.523903, 0.132393, 0.356592, 0.851587, 0.791684, 0.454832, 0.429462,
 0.844065, 0.519956, 0.811477, 0.217926, 0.180567, 0.884026, 0.218220, 0.521795, 0.364557, 0.502521, 0.369589, 0.290313, 0.883226, 0.232409, 0.767564, 0.211543, 0.816087, 0.728885, 0.241611, 0.028032, 0.508989, 0.632006, 0.012353,
 0.967519, 0.005967, 0.945406, 0.119164, 0.410370, 0.793590, 0.388155, 0.880790, 0.842740, 0.557732, 0.784506, 0.155325, 0.110877, 0.656122, 0.104144, 0.406869, 0.107908, 0.400083, 0.664572, 0.944781, 0.915808, 0.746807, 0.484984,
 0.479197, 0.483667, 0.814077, 0.455483, 0.213416, 0.556046, 0.630117, 0.115683, 0.777839, 0.445075, 0.563116, 0.571922, 0.187447, 0.555343, 0.981796, 0.712611, 0.789718, 0.275795, 0.377388, 0.087258, 0.554628, 0.917419, 0.628565,
 0.917782, 0.563619, 0.034887, 0.467701, 0.922722, 0.778654, 0.475421, 0.865724, 0.308469, 0.042584, 0.365675, 0.803251, 0.159766, 0.282781, 0.509504, 0.250542, 0.928331, 0.394516, 0.124009, 0.603664, 0.047894, 0.679826, 0.801739,
 0.988299, 0.311260, 0.751049, 0.132352, 0.867730, 0.335924, 0.793080, 0.642307, 0.103023, 0.665477, 0.160259, 0.976470, 0.789577, 0.141562, 0.686306, 0.426887, 0.816418, 0.612916, 0.360553, 0.653111, 0.597581, 0.692318, 0.300873,
 0.392402, 0.946162, 0.637740, 0.191058, 0.049891, 0.692147, 0.920226, 0.584208, 0.726748, 0.678717, 0.187862, 0.068122, 0.692734, 0.955565, 0.073478, 0.186149, 0.757842, 0.771983, 0.194647, 0.978026, 0.931547, 0.760451, 0.219997,
 0.898379, 0.178344, 0.099545, 0.628491, 0.862763, 0.751406, 0.512873, 0.761164, 0.017046, 0.294584, 0.461022, 0.591783, 0.108759, 0.694409, 0.532064, 0.848280, 0.719773, 0.101750, 0.751893, 0.365769, 0.087662, 0.956425, 0.690710,
 0.900057, 0.365660, 0.582073, 0.911435, 0.774163, 0.343836, 0.583324, 0.518105, 0.256319, 0.350517, 0.998762, 0.548817, 0.203848, 0.074945, 0.832896, 0.403059, 0.371259, 0.779434, 0.115645, 0.785107, 0.631830, 0.010039, 0.191490,
 0.124231, 0.849019, 0.754942, 0.210952, 0.642877, 0.047629, 0.653791, 0.238530, 0.182665, 0.055028, 0.521400, 0.473408, 0.265308, 0.731593, 0.136057, 0.537389, 0.314610, 0.998429, 0.054860, 0.869781, 0.353127, 0.952152, 0.582682,
 0.997739, 0.122261, 0.674123, 0.942571, 0.855248, 0.470405, 0.778808, 0.006476, 0.702316, 0.975915, 0.077317, 0.557387, 0.361425, 0.289194, 0.872735, 0.206566, 0.020153, 0.213073, 0.659200, 0.599753, 0.341633, 0.418179, 0.313096,
 0.059807, 0.938686, 0.188350, 0.079222, 0.053561, 0.778955, 0.191958, 0.744766, 0.889713, 0.938741, 0.912638, 0.727637, 0.482052, 0.469845, 0.685266, 0.391929, 0.306370, 0.409251, 0.860759, 0.641410, 0.376070, 0.084553, 0.260393,
 0.772042, 0.025054, 0.552672, 0.023955, 0.117205, 0.959378, 0.975332, 0.156265, 0.265966, 0.691781, 0.967397, 0.436835, 0.537122, 0.454342, 0.954398, 0.984069, 0.052587, 0.670772, 0.610087, 0.218016, 0.666000, 0.316209, 0.795800,
 0.507030, 0.300062, 0.081842, 0.638677, 0.629474, 0.953524, 0.355306, 0.528885, 0.750872, 0.920828, 0.114937, 0.715926, 0.781731, 0.180023, 0.983903, 0.685722, 0.316696, 0.643844, 0.794615, 0.713726, 0.390287, 0.121780, 0.048078,
 0.540844, 0.749462, 0.635045, 0.918958, 0.399980, 0.423298, 0.779833, 0.834838, 0.038922, 0.952945, 0.405773, 0.855455, 0.612731, 0.571324, 0.115636, 0.880870, 0.544394, 0.935991, 0.289020, 0.240643, 0.630306, 0.217025, 0.167669,
 0.151664, 0.766878, 0.625073, 0.334581, 0.400782, 0.931593, 0.986016, 0.884926, 0.008410, 0.217110, 0.084636, 0.076750, 0.487634, 0.307184, 0.019659, 0.006447, 0.134942, 0.330408, 0.249360, 0.262011, 0.906382, 0.329953, 0.826389,
 0.840414, 0.258122, 0.958142, 0.191458, 0.289177, 0.602016, 0.522750, 0.940240, 0.852041, 0.062462, 0.272389, 0.021301, 0.810750, 0.166010, 0.171232, 0.643567, 0.556370, 0.173395, 0.254957, 0.751119, 0.671802, 0.526861, 0.621476,
 0.695513, 0.330261, 0.463509, 0.849181, 0.839203, 0.121459, 0.196084, 0.074836, 0.348603, 0.407291, 0.222172, 0.541350, 0.676006, 0.022120, 0.655511, 0.346648, 0.932928, 0.499915, 0.921366, 0.608392, 0.973707, 0.602828, 0.492241,
 0.790697, 0.940660, 0.205170, 0.409184, 0.519376, 0.877274, 0.155756, 0.314359, 0.608000, 0.853270, 0.593383, 0.367329, 0.010648, 0.627738, 0.958534, 0.313542, 0.056285, 0.919586, 0.153718, 0.780074, 0.167880, 0.777345, 0.620229,
 0.579925, 0.655654, 0.714931, 0.576100, 0.979531, 0.443036, 0.281440, 0.995027, 0.920116, 0.346272, 0.589900, 0.685952, 0.555726, 0.910257, 0.273402, 0.785241, 0.981003, 0.695534, 0.051875, 0.216342, 0.049830, 0.221853, 0.966607,
 0.964048, 0.940002, 0.674981, 0.713240, 0.001552, 0.885049, 0.664115, 0.921395, 0.188391, 0.886656, 0.231398, 0.687310, 0.606753, 0.432065, 0.432340, 0.865152, 0.443776, 0.290995, 0.861792, 0.216276, 0.370383, 0.454518, 0.745486,
 0.360110, 0.265238, 0.630848, 0.668654, 0.875117, 0.328977, 0.434953, 0.703043, 0.876146, 0.012523, 0.333261, 0.919654, 0.911148, 0.518614, 0.442744, 0.850251, 0.244804, 0.842601, 0.674491, 0.965110, 0.151047, 0.702844, 0.272908,
 0.262459, 0.045953, 0.218871, 0.178505, 0.476862, 0.435438, 0.663805, 0.558365, 0.585431, 0.209971, 0.024011, 0.355686, 0.363815, 0.725733, 0.811482, 0.438060, 0.875635, 0.005992, 0.602164, 0.331481, 0.392256, 0.868729, 0.015402,
 0.149516, 0.272995, 0.630742, 0.346226, 0.028937, 0.493151, 0.501541, 0.243783, 0.606761, 0.508356, 0.812662, 0.767269, 0.089617, 0.973991, 0.732756, 0.807023, 0.623193, 0.998849, 0.202463, 0.335698, 0.644133, 0.369329, 0.045287,
 0.745615, 0.943863, 0.816597, 0.833962, 0.767486, 0.376536, 0.462048, 0.332200, 0.176953, 0.662754, 0.129663, 0.607984, 0.843923, 0.066064, 0.086445, 0.098871, 0.221198, 0.918031, 0.808884, 0.577325, 0.299551, 0.935264, 0.684626,
 0.643158, 0.463249, 0.981877, 0.770184, 0.792184, 0.790554, 0.685252, 0.229254, 0.459826, 0.776047, 0.991398, 0.279226, 0.603218, 0.464599, 0.056889, 0.048587, 0.206297, 0.729865, 0.603225, 0.883695, 0.613266, 0.193210, 0.120898,
 0.293819, 0.313851, 0.742874, 0.796818, 0.858270, 0.919020, 0.099029, 0.270478, 0.274097, 0.731755, 0.590202, 0.900112, 0.048285, 0.890355, 0.612857, 0.066699, 0.651188, 0.351326, 0.881885, 0.666020, 0.780339, 0.497717, 0.929148,
 0.670000, 0.533625, 0.372792, 0.644483, 0.889458, 0.071566, 0.116927, 0.393699, 0.580336, 0.839254, 0.249862, 0.196844, 0.621926, 0.714007, 0.815948, 0.053519, 0.804068, 0.905463, 0.875242, 0.097370, 0.514302, 0.936521, 0.359601,
 0.326459, 0.367407, 0.217968, 0.354911, 0.058339, 0.198430, 0.253869, 0.555930, 0.506911, 0.304263, 0.301906, 0.059785, 0.372224, 0.904157, 0.801109, 0.111830, 0.613977, 0.860588, 0.273036, 0.640462, 0.575638, 0.700909, 0.921154,
 0.865793, 0.909762, 0.677823, 0.282269, 0.344080, 0.788415, 0.851496, 0.024309, 0.122014, 0.268008, 0.144800, 0.103282, 0.281862, 0.408550, 0.770743, 0.770861, 0.822856, 0.457052, 0.940484, 0.410878, 0.261779, 0.618079, 0.992642,
 0.441244, 0.465569, 0.316154, 0.029165, 0.876816, 0.422787, 0.460692, 0.367148, 0.953037, 0.994377, 0.750508, 0.015250, 0.070208, 0.185464, 0.088750, 0.431343, 0.360042, 0.380494, 0.447811, 0.816536, 0.291174, 0.968399, 0.145574,
 0.529572, 0.825888, 0.240310, 0.228029, 0.072875, 0.889308, 0.710534, 0.148252, 0.793323, 0.003108, 0.635948, 0.978057, 0.571379, 0.322577, 0.486720, 0.166906, 0.533960, 0.468361, 0.152589, 0.194253, 0.239839, 0.197686, 0.407068,
 0.058263, 0.950081, 0.365427, 0.453382, 0.258367, 0.936783, 0.799471, 0.550248, 0.004524, 0.306156, 0.689856, 0.775492, 0.706161, 0.349594, 0.164799, 0.896707, 0.132969, 0.900213, 0.712601, 0.249753, 0.913086, 0.801392, 0.585506,
 0.099854, 0.547364, 0.205732, 0.884728, 0.501592, 0.817633, 0.662868, 0.535300, 0.461572, 0.189195, 0.546563, 0.191229, 0.722356, 0.530544, 0.367027, 0.407103, 0.662989, 0.564861, 0.543514, 0.329229, 0.287841, 0.936851, 0.732806,
 0.120220, 0.642266, 0.524436, 0.357455, 0.274328, 0.044793, 0.687728, 0.417786, 0.610772, 0.508227, 0.958208, 0.446098, 0.768558, 0.586319, 0.300472, 0.840727, 0.454213, 0.531971, 0.489183, 0.150307, 0.014419, 0.193627, 0.437424,
 0.841408, 0.385336, 0.068186, 0.982852, 0.930630, 0.391656, 0.634167, 0.818461, 0.678280, 0.279040, 0.723694, 0.969957, 0.485900, 0.326954, 0.675234, 0.636720, 0.703665, 0.234211, 0.212844, 0.883667, 0.303135, 0.541423, 0.997248,
 0.793493, 0.201781, 0.763444, 0.117667, 0.349454, 0.411583, 0.776733, 0.172404, 0.745793, 0.426590, 0.732911, 0.205297, 0.634174, 0.243670, 0.757999, 0.512063, 0.268848, 0.834515, 0.619564, 0.045966, 0.760207, 0.541702, 0.589963,
 0.880204, 0.975872, 0.871292, 0.856919, 0.421976, 0.721245, 0.191596, 0.002774, 0.020452, 0.008675, 0.442768, 0.526060, 0.421338, 0.698174, 0.713524, 0.864630, 0.550647, 0.915295, 0.376165, 0.415089, 0.169365, 0.166681, 0.894231,
 0.160775, 0.541606, 0.364558, 0.761401, 0.518083, 0.867313, 0.312657, 0.706948, 0.201821, 0.515688, 0.012553, 0.051255, 0.373743, 0.179244, 0.227067, 0.232179, 0.169092, 0.775940, 0.532276, 0.194715, 0.254274, 0.036591, 0.938955,
 0.766276, 0.230665, 0.208094, 0.545243, 0.879579, 0.106598, 0.614492, 0.463328, 0.684618, 0.369001, 0.085329, 0.952667, 0.168408, 0.320921, 0.574766, 0.690163, 0.912326, 0.749550, 0.949696, 0.953821, 0.426164, 0.855696, 0.667482,
 0.121372, 0.047816, 0.929431, 0.344272, 0.860505, 0.458399, 0.983450, 0.436398, 0.174520, 0.283176, 0.456261, 0.026579, 0.669076, 0.205600, 0.938493, 0.083093, 0.015242, 0.270162, 0.439444, 0.306481, 0.235287, 0.172304, 0.045397,
 0.510477, 0.654709, 0.991811, 0.489706, 0.320720, 0.812216, 0.008879, 0.486254, 0.188381, 0.354173, 0.517002, 0.017373, 0.381458, 0.657854, 0.309333, 0.725018, 0.762306, 0.215523, 0.238959, 0.255888, 0.017622, 0.396485, 0.740309,
 0.748917, 0.260930, 0.204962, 0.828551, 0.534279, 0.965565, 0.826727, 0.074411, 0.709691, 0.348828, 0.756299, 0.669397, 0.260761, 0.159465, 0.909210, 0.565273, 0.251580, 0.317917, 0.531067, 0.984155, 0.799388, 0.722137, 0.738090,
 0.372281, 0.798284, 0.414441, 0.874523, 0.210003, 0.922654, 0.063308, 0.403188, 0.419629, 0.337771, 0.657929, 0.949824, 0.436213, 0.805153, 0.455160, 0.783307, 0.157386, 0.791610, 0.524318, 0.957017, 0.877848, 0.586894, 0.873870,
 0.476314, 0.181519, 0.884646, 0.737554, 0.099204, 0.427000, 0.379605, 0.471043, 0.267891, 0.565309, 0.541163, 0.942278, 0.045115, 0.296595, 0.215298, 0.770437, 0.768364, 0.056379, 0.225290, 0.214780, 0.555509, 0.651715, 0.843653,
 0.573025, 0.105828, 0.001538, 0.316266, 0.210227, 0.290231, 0.909489, 0.473189, 0.597399, 0.853833, 0.975917, 0.563258, 0.241239, 0.093412, 0.080993, 0.520978, 0.082739, 0.693153, 0.613366, 0.899801, 0.736206, 0.066222, 0.127237,
 0.186587, 0.937971, 0.659738, 0.061863, 0.414524, 0.335315, 0.229748, 0.806202, 0.198187, 0.533474, 0.214704, 0.820671, 0.072664, 0.348072, 0.054966, 0.899022, 0.392236, 0.900624, 0.787287, 0.455244, 0.861993, 0.418166, 0.042474,
 0.020771, 0.882874, 0.929649, 0.887941, 0.831394, 0.397118, 0.019861, 0.871538, 0.444980, 0.453123, 0.137745, 0.366331, 0.061991, 0.657726, 0.844667, 0.506177, 0.500862, 0.457260, 0.717052, 0.897745, 0.317464, 0.459186, 0.227832,
 0.045422, 0.138376, 0.962141, 0.866082, 0.321486, 0.015641, 0.458040, 0.511329, 0.035569, 0.198635, 0.075049, 0.858358, 0.026105, 0.676973, 0.980246, 0.643257, 0.424127, 0.775791, 0.406557, 0.908472, 0.178860, 0.907927, 0.726644,
 0.373211, 0.328351, 0.811424, 0.399168, 0.658469, 0.230595, 0.558376, 0.457988, 0.118963, 0.315654, 0.723590, 0.815940, 0.026418, 0.792257, 0.088029, 0.090948, 0.951857, 0.948863, 0.440064, 0.028048, 0.234198, 0.675549, 0.572491,
 0.246879, 0.176678, 0.936183, 0.678947, 0.866204, 0.063020, 0.617795, 0.581273, 0.186727, 0.666294, 0.343921, 0.699689, 0.708091, 0.772835, 0.969835, 0.368090, 0.238304, 0.452190, 0.979411, 0.033723, 0.162423, 0.665805, 0.415829,
 0.428056, 0.276276, 0.069122, 0.232257, 0.896258, 0.179998, 0.974120, 0.615152, 0.496855, 0.550530, 0.805371, 0.373940, 0.079463, 0.673072, 0.608980, 0.509076, 0.493780, 0.175225, 0.319727, 0.558955, 0.132092, 0.756594, 0.320165,
 0.918729, 0.743482, 0.888654, 0.052083, 0.545119, 0.541800, 0.538358, 0.984648, 0.975878, 0.050575, 0.273159, 0.476639, 0.456296, 0.453719, 0.979817, 0.067838, 0.874355, 0.846721, 0.484691, 0.525005, 0.747745, 0.182553, 0.725288,
 0.782507, 0.407478, 0.681594, 0.683209, 0.441083, 0.902425, 0.405290, 0.954533, 0.108166, 0.765638, 0.787121, 0.777956, 0.920410, 0.286038, 0.457786, 0.252131, 0.627572, 0.108227, 0.308658, 0.805768, 0.633739, 0.019228, 0.600856,
 0.206818, 0.087260, 0.495394, 0.067269, 0.245900, 0.137920, 0.687283, 0.470122, 0.954940, 0.695446, 0.867642};

int mavlib_fixedRndIndex=0;



/* Routine to return a random number */

float mav_random(void)
{
  float rv;

  if (mav_opt_fixedRnd)
  {
    rv= mavlib_fixedRnd[mavlib_fixedRndIndex++];
    if (mavlib_fixedRndIndex>=5000) {
      if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Run out of fixed random numbers, repeating\n");
      mavlib_fixedRndIndex=0;
    }
  }
  else
  {
#ifdef macintosh
    rv= (unsigned) (Random () / 65536);
#elif defined(WIN32)
    rv= ((float) rand())/RAND_MAX;
#else
    rv= ((float) drand48());
#endif
  }

  return rv;
}



/* Routine to seed the random number generator */

void mav_randomSeed(long seed)
{
  if (seed>=0)
  {
    if (mav_opt_fixedRnd)
    {
      if (seed>=5000) 
      {
	if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: Seed values greater than 5000 for fixed random number generation, using 0\n");
	seed=0;
      }

      mavlib_fixedRndIndex=seed;
    }
    else
    {
#ifdef macintosh
      qd.randSeed = seed;
#elif defined(WIN32)
      srand((unsigned int) seed);
#else
      srand48(seed);
#endif
    }
  }
  else
  { 
    /* seed from time */
    MAV_time t= mav_timeGet();
#ifdef macintosh
    qd.randSeed = t.usec;
#elif defined(WIN32)
    srand((unsigned int) t.usec);
#else
    srand48(t.usec);
#endif
  }
}



char *mav_getTempDir(void)
{
#if defined(__CYGWIN__)
  return "/tmp/";
#elif defined(WIN32)
  return getenv("TEMP");
#elif defined(macintosh)
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: guessing at temporary Mac directory\n");
  return ".";
#else
  return "/tmp/";
#endif  
}

int mav_getPID(void)
{
#if defined(__CYGWIN__)
  return getpid();
#elif defined(WIN32)
  return _getpid();
#elif defined(macintosh)
  if (mav_opt_output==MAV_VERBOSE) fprintf(stderr, "Warning: guessing at Mac process PID\n");
  return 1;
#else
  return getpid();
#endif  
}
