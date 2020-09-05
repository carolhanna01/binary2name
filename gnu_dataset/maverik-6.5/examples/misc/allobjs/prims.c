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
#include <string.h>
#if !defined(WIN32) || defined(__CYGWIN__)
char *strdup (const char *s1);
#endif
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

/* Define the primitives and add them to the SMS */

void defprims(void)
{
  MAV_box *box;
  MAV_pyramid *pyr;
  MAV_cylinder *cyl; MyStruct *s;
  MAV_cone *cone;
  MAV_sphere *sph;
  MAV_hsphere *hsph;
  MAV_ellipse *elip;
  MAV_hellipse *helip;
  MAV_ctorus *ct;
  MAV_rtorus *rt;
  MAV_polygon *apoly;
  MAV_polygonGrp *polyGrp;
  MAV_facet *facet;
  MAV_rectangle *rect;
  MAV_polyline *line;
  MAV_text *text;
  MAV_composite *comp;
  MAV_teapot *teapot;

  /* define box */

  box= (MAV_box *) mav_malloc(sizeof(MAV_box));
  box->size.x=10;
  box->size.y=20;
  box->size.z=20;
  box->sp= unselectedSp;
  box->matrix= mav_matrixSet(10.0, 0.0, 0.0,  0.0, 0.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_box, box));

  /* define pyramid */

  pyr= (MAV_pyramid *) mav_malloc(sizeof(MAV_pyramid));
  pyr->top_size_x=10.0;
  pyr->top_size_y=10.0;
  pyr->bot_size_x=20.0;
  pyr->bot_size_y=20.0;
  pyr->offset_x=0;
  pyr->offset_y=0;
  pyr->height=20.0;
  pyr->sp= unselectedSp;
  pyr->matrix= mav_matrixSet(0.0, 90.0, 0.0,  10.0, 100.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_pyramid, pyr));

  /* define cylinder */

  cyl= (MAV_cylinder *) mav_malloc(sizeof(MAV_cylinder));
  cyl->radius=20.0;
  cyl->height=10.0;
  cyl->nverts=20;
  cyl->endcap=MAV_TRUE;
  cyl->sp= unselectedSp;
  cyl->matrix= mav_matrixSet(0.0, 0.0, 0.0,  -10.0, 20.0, 0.0);
  s = (MyStruct *) mav_malloc(sizeof(MyStruct));
  strcpy(s->name, "part no 234");
  cyl->userdef = (void *) s;
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_cylinder, cyl));

  /* define cone */

  cone= (MAV_cone *) mav_malloc(sizeof(MAV_cone));
  cone->rt=5.0;
  cone->rb=20.0;
  cone->height=10.0;
  cone->nverts=20;
  cone->endcap=MAV_TRUE;
  cone->sp= unselectedSp;
  cone->matrix= mav_matrixSet(90.0, 20.0, 0.0,  100.0, 20.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_cone, cone));

  /* define sphere */

  sph= (MAV_sphere *) mav_malloc(sizeof(MAV_sphere));
  sph->radius=20.0;
  sph->nchips=10;
  sph->nverts=20;
  sph->sp= unselectedSp;
  sph->matrix= mav_matrixSet(0.0, 0.0, 0.0,  0.0, 70.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_sphere, sph));

  /* define half sphere */

  hsph= (MAV_hsphere *) mav_malloc(sizeof(MAV_hsphere));
  hsph->radius=15.0;
  hsph->nchips=5;
  hsph->nverts=20;
  hsph->endcap=MAV_TRUE;
  hsph->sp= unselectedSp;
  hsph->matrix= mav_matrixSet(0.0, 90.0, 0.0,  70.0, 0.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_hsphere, hsph));

  /* define ellipse */

  elip= (MAV_ellipse *) mav_malloc(sizeof(MAV_ellipse));
  elip->radius=5.0;
  elip->height=15.0;
  elip->nverts=20;
  elip->nchips=15;
  elip->sp= unselectedSp;
  elip->matrix= mav_matrixSet(0.0, 90.0, 0.0,  70.0, 70.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_ellipse, elip));

  /* define half ellipse */

  helip= (MAV_hellipse *) mav_malloc(sizeof(MAV_hellipse));
  helip->radius=5.0;
  helip->height=15.0;
  helip->nverts=20;
  helip->nchips=10;
  helip->endcap=MAV_TRUE;
  helip->sp= unselectedSp;
  helip->matrix= mav_matrixSet(0.0, 90.0, 0.0,  70.0, 110.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_hellipse, helip));

  /* define circular torus */

  ct= (MAV_ctorus *) mav_malloc(sizeof(MAV_ctorus));
  ct->rmajor=10.0;
  ct->rminor=5.0;
  ct->angle=4.8;
  ct->nverts=15;
  ct->nchips=15;
  ct->endcap=MAV_TRUE;
  ct->sp= unselectedSp;
  ct->matrix= mav_matrixSet(0.0, 0.0, 0.0,  30.0, 50.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_ctorus, ct));

  /* define rectangular torus */

  rt= (MAV_rtorus *) mav_malloc(sizeof(MAV_rtorus));
  rt->radius=10.0;
  rt->width=2.0;
  rt->height=4.0;
  rt->angle=2.8;
  rt->nchips=15;
  rt->endcap=MAV_TRUE;
  rt->sp= unselectedSp;
  rt->matrix= mav_matrixSet(0.0, 0.0, 0.0,  30.0, 30.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rtorus, rt));

  /* define polygon */
  
  apoly= (MAV_polygon *) mav_malloc(sizeof(MAV_polygon));
  apoly->np=5;
  apoly->norm.x=0.0;
  apoly->norm.y=0.0;
  apoly->norm.z=1.0;

  apoly->vert = (MAV_vector *) mav_malloc(5*sizeof(MAV_vector));
  apoly->tex = (MAV_texCoord *) mav_malloc(5*sizeof(MAV_texCoord));

  apoly->vert[0].x = 3.0;
  apoly->vert[0].y = 1.0;
  apoly->vert[0].z = 0.0;
  apoly->tex[0].s = (3+2)/5.0;
  apoly->tex[0].t = (1+4)/8.0;

  apoly->vert[1].x = 1.0;
  apoly->vert[1].y = 4.0;
  apoly->vert[1].z = 0.0;
  apoly->tex[1].s = (1+2)/5.0;
  apoly->tex[1].t = (4+4)/8.0;

  apoly->vert[2].x = -2.0;
  apoly->vert[2].y = 3.0;
  apoly->vert[2].z = 0.0;
  apoly->tex[2].s = (-2+2)/5.0;
  apoly->tex[2].t = (3+4)/8.0;

  apoly->vert[3].x = -2.0;
  apoly->vert[3].y = -3.0;
  apoly->vert[3].z = 0.0;
  apoly->tex[3].s = (-3+2)/5.0;
  apoly->tex[3].t = (-3+4)/8.0;

  apoly->vert[4].x = 2.0;
  apoly->vert[4].y = -4.0;
  apoly->vert[4].z = 0.0;
  apoly->tex[4].s = (2+2)/5.0;
  apoly->tex[4].t = (-4+4)/8.0;

  apoly->sp= unselectedSp;
  apoly->matrix= mav_matrixSet(0.0, 0.0, 0.0,  20.0, -10.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_polygon, apoly));

  /* define polygon group */  

  polyGrp= (MAV_polygonGrp *) mav_malloc(sizeof(MAV_polygonGrp));
  polyGrp->npolys=2;

  polyGrp->np= (int *) mav_malloc(polyGrp->npolys*sizeof(int));
  polyGrp->norm= (MAV_vector *) mav_malloc(polyGrp->npolys*sizeof(MAV_vector));
  polyGrp->tex= (MAV_texCoord **) mav_malloc(polyGrp->npolys*sizeof(MAV_texCoord *));
  polyGrp->vert= (MAV_vector **) mav_malloc(polyGrp->npolys*sizeof(MAV_vector *));
  polyGrp->sp= (MAV_surfaceParams **) mav_malloc(polyGrp->npolys*sizeof(MAV_surfaceParams *));


  polyGrp->np[0]=3;
  polyGrp->sp[0]= unselectedSp;
  polyGrp->vert[0]= (MAV_vector *) mav_malloc(polyGrp->np[0]*sizeof(MAV_vector));
  polyGrp->tex[0]= (MAV_texCoord *) mav_malloc(polyGrp->np[0]*sizeof(MAV_texCoord));

  polyGrp->vert[0][0].x= 0.0;
  polyGrp->vert[0][0].y= 0.0;
  polyGrp->vert[0][0].z= 0.0;
  polyGrp->tex[0][0].s= 0;
  polyGrp->tex[0][0].t= 0;
  
  polyGrp->vert[0][1].x= 10.0;
  polyGrp->vert[0][1].y= 0.0;
  polyGrp->vert[0][1].z= 0.0;
  polyGrp->tex[0][1].s= 1;
  polyGrp->tex[0][1].t= 0;

  polyGrp->vert[0][2].x= 8.0;
  polyGrp->vert[0][2].y= 0.0;
  polyGrp->vert[0][2].z= 5.0;
  polyGrp->tex[0][2].s= 0.8;
  polyGrp->tex[0][2].t= 1;

  polyGrp->norm[0].x= 0;
  polyGrp->norm[0].y= 1;
  polyGrp->norm[0].z= 0;


  polyGrp->np[1]=4;
  polyGrp->sp[1]= unselectedSp;
  polyGrp->vert[1]= (MAV_vector *) mav_malloc(polyGrp->np[1]*sizeof(MAV_vector));
  polyGrp->tex[1]= (MAV_texCoord *) mav_malloc(polyGrp->np[1]*sizeof(MAV_texCoord));

  polyGrp->vert[1][0].x= 0.0;
  polyGrp->vert[1][0].y= 0.0;
  polyGrp->vert[1][0].z= 0.0;
  polyGrp->tex[1][0].s= 0.0;  
  polyGrp->tex[1][0].t= 0.0;  

  polyGrp->vert[1][1].x= 5.0;
  polyGrp->vert[1][1].y= 0.0;
  polyGrp->vert[1][1].z= 0.0;
  polyGrp->tex[1][1].s= 1.0;  
  polyGrp->tex[1][1].t= 0.0;  

  polyGrp->vert[1][2].x= 4.0;
  polyGrp->vert[1][2].y= 5.0;
  polyGrp->vert[1][2].z= 0.0;
  polyGrp->tex[1][2].s= 0.8;  
  polyGrp->tex[1][2].t= 1.0;  

  polyGrp->vert[1][3].x= 0.0;
  polyGrp->vert[1][3].y= 4.0;
  polyGrp->vert[1][3].z= 0.0;
  polyGrp->tex[1][3].s= 0.0;  
  polyGrp->tex[1][3].t= 0.8;  

  polyGrp->norm[1].x= 0;
  polyGrp->norm[1].y= 0;
  polyGrp->norm[1].z= 1;


  polyGrp->matrix= mav_matrixSet(0.0, 0.0, 0.0,  120.0, 40.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_polygonGrp, polyGrp));

  /* define facet */

  facet= (MAV_facet *) mav_malloc(sizeof(MAV_facet));
  facet->npolys= 1;
  facet->np= (int *) mav_malloc(facet->npolys*sizeof(int));
  facet->norm= (MAV_vector **) mav_malloc(facet->npolys*sizeof(MAV_vector *));
  facet->tex= (MAV_texCoord **) mav_malloc(facet->npolys*sizeof(MAV_texCoord *));
  facet->vert= (MAV_vector **) mav_malloc(facet->npolys*sizeof(MAV_vector *));
  facet->sp= (MAV_surfaceParams **) mav_malloc(facet->npolys*sizeof(MAV_surfaceParams *));
  
  facet->np[0]= 4;
  facet->norm[0]= (MAV_vector *) mav_malloc(facet->np[0]*sizeof(MAV_vector));
  facet->tex[0]= (MAV_texCoord *) mav_malloc(facet->np[0]*sizeof(MAV_texCoord));
  facet->vert[0]= (MAV_vector *) mav_malloc(facet->np[0]*sizeof(MAV_vector));
  facet->sp[0]= unselectedSp;

  facet->vert[0][0].x=0;
  facet->vert[0][0].y=0;
  facet->vert[0][0].z=0;
  facet->tex[0][0].s=0;
  facet->tex[0][0].t=0;
  facet->norm[0][0].x=-1.0/sqrt(3);
  facet->norm[0][0].y=-1.0/sqrt(3);
  facet->norm[0][0].z=1.0/sqrt(3);

  facet->vert[0][1].x=10;
  facet->vert[0][1].y=0;
  facet->vert[0][1].z=0;
  facet->tex[0][1].s=1;
  facet->tex[0][1].t=0;
  facet->norm[0][1].x=1.0/sqrt(3);
  facet->norm[0][1].y=-1.0/sqrt(3);
  facet->norm[0][1].z=1.0/sqrt(3);

  facet->vert[0][2].x=10;
  facet->vert[0][2].y=10;
  facet->vert[0][2].z=0;
  facet->tex[0][2].s=1;
  facet->tex[0][2].t=1;
  facet->norm[0][2].x=1.0/sqrt(3);
  facet->norm[0][2].y=1.0/sqrt(3);
  facet->norm[0][2].z=1.0/sqrt(3);

  facet->vert[0][3].x=0;
  facet->vert[0][3].y=10;
  facet->vert[0][3].z=0;
  facet->tex[0][3].s=0;
  facet->tex[0][3].t=1;
  facet->norm[0][3].x=-1.0/sqrt(3);
  facet->norm[0][3].y=1.0/sqrt(3);
  facet->norm[0][3].z=1.0/sqrt(3);

  facet->matrix= mav_matrixSet(0.0, 0.0, 0.0,  130.0, 50.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_facet, facet));

  /* define rectangle */

  rect= (MAV_rectangle *) mav_malloc(sizeof(MAV_rectangle));
  rect->width=10;
  rect->height=5;
  rect->xtile=2;
  rect->ytile=1;
  rect->sp= unselectedSp;
  rect->matrix= mav_matrixSet(0.0, 0.0, 0.0,  20.0, 20.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_rectangle, rect));

  /* define polyline */

  line= (MAV_polyline *) mav_malloc(sizeof(MAV_polyline));
  line->nlines= 2;
  line->np= (int *) mav_malloc(line->nlines*sizeof(int));
  line->closed= (int *) mav_malloc(line->nlines*sizeof(int));
  line->vert= (MAV_vector **) mav_malloc(line->nlines*sizeof(MAV_vector *));
  line->sp= (MAV_surfaceParams **) mav_malloc(line->nlines*sizeof(MAV_surfaceParams *));

  line->np[0]=3;
  line->closed[0]= MAV_TRUE;
  line->sp[0]= unselectedSp;
  line->vert[0]= (MAV_vector *) mav_malloc(line->np[0]*sizeof(MAV_vector));
  line->vert[0][0].x= 0.0;
  line->vert[0][0].y= 0.0;
  line->vert[0][0].z= 0.0;

  line->vert[0][1].x= 10.0;
  line->vert[0][1].y= 0.0;
  line->vert[0][1].z= 0.0;

  line->vert[0][2].x= 8.0;
  line->vert[0][2].y= 5.0;
  line->vert[0][2].z= 0.0;

  line->np[1]=2;
  line->sp[1]= unselectedSp;
  line->closed[1]= MAV_FALSE;
  line->vert[1]= (MAV_vector *) mav_malloc(line->np[1]*sizeof(MAV_vector));
  line->vert[1][0].x= -2.0;
  line->vert[1][0].y= 3.0;
  line->vert[1][0].z= 0.0;

  line->vert[1][1].x= 5.0;
  line->vert[1][1].y= 1.0;
  line->vert[1][1].z= 0.0;

  line->matrix= mav_matrixSet(0.0, 0.0, 0.0,  110.0, 60.0, 0.0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_polyline, line));

  /* define text */

  text= (MAV_text *) mav_malloc(sizeof(MAV_text));
  text->text= strdup("**text**");
  text->style= MAV_FILLED_FONT;
  text->justify= MAV_CENTER_JUSTIFY;
  text->sp= unselectedSp;
  text->matrix= mav_matrixScaleSet(mav_matrixSet(0.0, 0.0, 0.0, 120.0, 100.0, 0.0), 10);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_text, text));

  /* define composite */
  
  comp= (MAV_composite *) mav_malloc(sizeof(MAV_composite));
  if (mav_compositeReadAC3D("../../MPG/mavlogo.ac", comp, MAV_ID_MATRIX)==MAV_FALSE) {
    fprintf(stderr, "Error could not read composite file\n");
    exit(1);
  }
  comp->matrix= mav_matrixScaleSet(mav_matrixSet(0.0, 0.0, 0.0, 120.0, 0.0, 0.0), 3);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_composite, comp));

  /* define a teapot */

  teapot= (MAV_teapot *) mav_malloc(sizeof(MAV_teapot));
  teapot->size=20.0;
  teapot->subdivisions=3;
  teapot->sp= unselectedSp;
  teapot->matrix= mav_matrixSet(0,0,0,50,80,0);
  mav_SMSObjectAdd(sms, mav_objectNew(mav_class_teapot, teapot));
}

