/*
 * // SpeedX //
 *
 *  medernac@isty-info.uvsq.fr
 *
 *  Copyright (C) 2000 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

#ifndef __SPRITE_H

#define __SPRITE_H



#include "const.h"



int vehicule = 0 ;



extern int delta_relatif[HVISION] ;

extern int ProjectiveDistance[HVISION];

extern int vmin[NVEHICULES];

extern int dx, dy;



//int num_veh;

char *v[7];

char *m_v[7];

char *number [10] ;

char *panel;



int num_v,num_l ;



#endif /* __SPRITE_H */

