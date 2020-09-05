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


#include "allobjs.h"
#include <stdio.h>
#include <stdlib.h>

MAV_object *selected=NULL;

int select_obj(MAV_object *obj, MAV_mouseEvent *event)
{
  MAV_surfaceParams **sp;

  /* unselect currently selected object */
  unselect_obj(obj, event);

  /* select object - set surface params to selected */
  selected=event->obj;
  if (mav_callbackGetSurfaceParamsExec(event->win, selected, &sp)) *sp= selectedSp;
  
  /* check if object is a cylinder and if so examine the userdef field */
  if (mav_objectClassGet(event->obj)==mav_class_cylinder) {
    MAV_cylinder *cyl= mav_objectDataGet(event->obj);
    MyStruct *s= (MyStruct *) cyl->userdef;
    printf("cylinder: %s\n", s->name);
  }

  return 1;
}

int unselect_obj(MAV_object *obj, MAV_mouseEvent *event)
{
  MAV_surfaceParams **sp;

  /* unselect selected object - set surface params to unselected */
  if (selected) {
    if (mav_callbackGetSurfaceParamsExec(event->win, selected, &sp)) *sp= unselectedSp;
    selected=NULL;
  }

  return 1;
}

int pick=0;
int bb=0;

int keyb(MAV_object *obj, MAV_keyboardEvent *event)
{
  int cv=MAV_MATERIAL;

/* set selected object spinning while the s key is held down */

  if (event->key=='s') {
    if (event->movement==MAV_PRESSED)
    {
      mav_frameFn1Add(spinner, NULL);
    }
    else
    {
      mav_frameFn1Rmv(spinner, NULL);
    }
  }

  if (event->movement==MAV_PRESSED) {    

    switch (event->key) {

/* q quits */
    case 'q':
      exit(1);

/* r restricts mouse */
    case 'r':
      mav_opt_restrictMouse= !mav_opt_restrictMouse;
      break;

/* p toggles auto mouse selection */
    case 'p':
      pick=!pick;
      if (pick) 
      {
	mav_frameFn2Add(picker, NULL);
      }
      else
      {
	mav_frameFn2Rmv(picker, NULL);
      }
      break;

/* b toggles bounding box draw */
    case 'b':
      bb=!bb;
      if (bb) 
      {
	mav_frameFn2Add(boxer, NULL);
      }
      else
      {
	mav_frameFn2Rmv(boxer, NULL);
      }
      break;

/* set surface params to colour, material or texture */
    case 'c':            
    case 'm':            
    case 't':

      if (event->key=='c') cv=MAV_COLOUR;
      if (event->key=='m') cv=MAV_MATERIAL;
      if (event->key=='t') cv=MAV_TEXTURE;

      selectedSp->mode= cv;
      unselectedSp->mode= cv;
      
      break;

/* no view parameter modifier */
    case 'f':
      event->win->vp->mod= NULL;
      break;

/* lookabout view parameter modifier */
    case 'l':
      event->win->vp->mod= lookabout;
      break;
    }
  }

  return 1;
}


/* callbacks to change the size of objects */

int inc_box(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_box *box=(MAV_box *) mav_objectDataGet(obj);
  
  select_obj(obj, event);

  box->size.x+=0.2;

  return 1;
}

int dec_box(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_box *box=(MAV_box *) mav_objectDataGet(obj);
  
  select_obj(obj, event);
  box->size.x-=0.2;

  return 1;
}

int inc_pyr(MAV_object *obj, MAV_mouseEvent *event)
{
  MAV_pyramid *pyr=(MAV_pyramid *) mav_objectDataGet(obj);

  select_obj(obj, event);
  pyr->top_size_x+=0.2;
  pyr->top_size_y+=0.2;

  return 1;
}

int dec_pyr(MAV_object *obj, MAV_mouseEvent *event)
{
  MAV_pyramid *pyr=(MAV_pyramid *) mav_objectDataGet(obj);

  select_obj(obj, event);
  pyr->top_size_x-=0.2;
  pyr->top_size_y-=0.2;

  return 1;
}

int inc_cyl(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_cylinder *cyl=(MAV_cylinder *) mav_objectDataGet(obj);

  select_obj(obj, event);
  cyl->radius+=0.2;

  return 1;
}

int dec_cyl(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_cylinder *cyl=(MAV_cylinder *) mav_objectDataGet(obj);
  
  select_obj(obj, event);
  cyl->radius-=0.2;

  return 1;
}

int inc_cone(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_cone *cone=(MAV_cone *) mav_objectDataGet(obj);

  select_obj(obj, event);
  cone->rt+=0.3;

  return 1;
}

int dec_cone(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_cone *cone=(MAV_cone *) mav_objectDataGet(obj);

  select_obj(obj, event);
  cone->rt-=0.3;

  return 1;
}

int inc_hsph(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_hsphere *hsph=(MAV_hsphere *) mav_objectDataGet(obj);

  select_obj(obj, event);
  hsph->endcap=MAV_TRUE;

  return 1;
}

int dec_hsph(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_hsphere *hsph=(MAV_hsphere *) mav_objectDataGet(obj);

  select_obj(obj, event);
  hsph->endcap=MAV_FALSE;

  return 1;
}

int inc_ct(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_ctorus *ct=(MAV_ctorus *) mav_objectDataGet(obj);

  select_obj(obj, event);
  ct->angle+=0.1;

  return 1;
}

int dec_ct(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_ctorus *ct=(MAV_ctorus *) mav_objectDataGet(obj);

  select_obj(obj, event);
  ct->angle-=0.1;

  return 1;
}

int inc_rt(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_rtorus *rt=(MAV_rtorus *) mav_objectDataGet(obj);

  select_obj(obj, event);
  rt->angle+=0.1;

  return 1;
}

int dec_rt(MAV_object *obj, MAV_mouseEvent *event) 
{
  MAV_rtorus *rt=(MAV_rtorus *) mav_objectDataGet(obj);

  select_obj(obj, event);
  rt->angle-=0.1;

  return 1;
}

