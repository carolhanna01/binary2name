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


#include "mavlib_tdm.h"

void mavlib_TDM_vp(MAV_window *w, int i)
{
  MAV_matrix mat;

  mav_viewParamsFixed(w);

  mat= mavlib_TDM_calcPos(i, mav_TDM_pos[i], mavlib_TDM_iv());

  w->vp->trans_right.x= mat.mat[0][0];
  w->vp->trans_right.y= mat.mat[1][0];
  w->vp->trans_right.z= mat.mat[2][0];

  w->vp->trans_up.x= mat.mat[0][1];
  w->vp->trans_up.y= mat.mat[1][1];
  w->vp->trans_up.z= mat.mat[2][1];

  w->vp->trans_view.x= -mat.mat[0][2];
  w->vp->trans_view.y= -mat.mat[1][2];
  w->vp->trans_view.z= -mat.mat[2][2];

  w->vp->trans_eye= mav_matrixXYZGet(mat);
}

void mavlib_TDM_vp0(MAV_window *w)
{
  mavlib_TDM_vp(w, 0);
}

void mavlib_TDM_vp1(MAV_window *w)
{
  mavlib_TDM_vp(w, 1);
}

void mavlib_TDM_vp2(MAV_window *w)
{
  mavlib_TDM_vp(w, 2);
}

void mavlib_TDM_vp3(MAV_window *w)
{
  mavlib_TDM_vp(w, 3);
}

void mavlib_TDM_vp4(MAV_window *w)
{
  mavlib_TDM_vp(w, 4);
}

void mavlib_TDM_vp5(MAV_window *w)
{
  mavlib_TDM_vp(w, 5);
}

void mavlib_TDM_vp6(MAV_window *w)
{
  mavlib_TDM_vp(w, 6);
}

void mavlib_TDM_vp7(MAV_window *w)
{
  mavlib_TDM_vp(w, 7);
}

void mavlib_TDM_vp8(MAV_window *w)
{
  mavlib_TDM_vp(w, 8);
}

void mavlib_TDM_vp9(MAV_window *w)
{
  mavlib_TDM_vp(w, 9);
}

MAV_viewModifierFn mav_TDM_vp[]={mavlib_TDM_vp0, mavlib_TDM_vp1, mavlib_TDM_vp2, mavlib_TDM_vp3, mavlib_TDM_vp4, mavlib_TDM_vp5, mavlib_TDM_vp6, mavlib_TDM_vp7, mavlib_TDM_vp8, mavlib_TDM_vp9};
