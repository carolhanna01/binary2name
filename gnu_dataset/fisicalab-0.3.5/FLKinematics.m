/* 
   Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014 German A. Arias

   This file is part of FísicaLab application

   FísicaLab is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.
 
   This application is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.
 
   You should have received a copy of the GNU General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111 USA.
*/

#import <stdlib.h>
#import <stdio.h>
#import <math.h>
#import <gsl/gsl_vector.h>
#import <gsl/gsl_multiroots.h>
#import <gsl/gsl_rng.h>
#import "FLKinematics.h"


static int buildSystem (const gsl_vector *v, void *p, gsl_vector *func)
{
  int tIndex;
  int nEqu = 0;
  double tf;
  NSMutableDictionary *object;
  NSEnumerator *enumerator;
  NSNumber *type;
  NSMutableArray *dat;

  // Get properties of FL object.
  FLKinematics *FLObj = (__bridge FLKinematics *)(p);

  NSInteger varT = FLObj->varT;
  NSString *timeVar = FLObj->timeVar;
  double timeDat = FLObj->timeDat;
  NSMutableArray *vars = FLObj->vars;
  NSArray *dictionary = FLObj->dictionary;
  // ----------------------------------------
  
  if (varT == 1)
    {
      tIndex = [vars indexOfObject: timeVar];
      tf = gsl_vector_get (v, tIndex);
    }
  else
    {
      tf = timeDat;
    }
  
  enumerator = [dictionary objectEnumerator];
  
  while ((object = [enumerator nextObject]))
    {
      type = [object objectForKey: @"Type"];
      dat = [object objectForKey: @"Values"];
      
      switch ([type intValue])
	{
	case 2:
	  {
	    double xsi, ysi, xof, yof, vxs, vys, vxof, vyof;
	    double xo, yo, vxo, vyo, angx, angy;
            
	    NSString *nameObj = [dat objectAtIndex: 1];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableArray *mObj = nil, *mObjTitles = nil;
	    NSMutableDictionary *sName;
            
	    while ((sName = [search nextObject]))
	      {
		NSString *n = [[sName objectForKey: @"Values"] objectAtIndex:
								 0];
		NSString *t = [[sName objectForKey: @"Titles"] objectAtIndex:
								 0];
                
		if ([t isEqualToString: _(@"Name")] &&
		    [nameObj isEqualToString: n])
		  {
		    mObj = [sName objectForKey: @"Values"];
		    mObjTitles = [sName objectForKey: @"Titles"];
		    break;
		  }
	      }
	    
	    // Initial data xsi
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		xsi = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		xsi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data ysi
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		ysi = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		ysi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data vxs
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		vxs = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		vxs = gsl_vector_get (v, k);
	      }
	    
	    // Initial data vys
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		vys = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		vys = gsl_vector_get (v, k);
	      }
	    
	    // Final data xof
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		xof = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		xof = gsl_vector_get (v, k);
	      }
	    
	    // Final data yof
	    if (![vars containsObject: [dat objectAtIndex: 7]])
	      {
		yof = [[dat objectAtIndex: 7] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 7]];
		yof = gsl_vector_get (v, k);
	      }
	    
	    // Final data vxof
	    if (![vars containsObject: [dat objectAtIndex: 8]])
	      {
		vxof = [[dat objectAtIndex: 8] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 8]];
		vxof = gsl_vector_get (v, k);
	      }
	    
	    // Final data vyof
	    if (![vars containsObject: [dat objectAtIndex: 9]])
	      {
		vyof = [[dat objectAtIndex: 9] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 9]];
		vyof = gsl_vector_get (v, k);
	      }
	    
	    // Get data of object inside this mobile system
	    
	    // Final data x
	    if ([mObjTitles containsObject: @"xf"])
	      { 
		int xf_o = [mObjTitles indexOfObject: @"xf"];
		
		if (![vars containsObject: [mObj objectAtIndex: xf_o]])
		  {
		    xo = [[mObj objectAtIndex: xf_o] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: xf_o]];
		    xo = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		xo = 0;
	      }                   
	    
	    // Final data y
	    if ([mObjTitles containsObject: @"yf"])
	      {
		int yf_o = [mObjTitles indexOfObject: @"yf"];
		
		if (![vars containsObject: [mObj objectAtIndex: yf_o]])
		  {
		    yo = [[mObj objectAtIndex: yf_o] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: yf_o]];
		    yo = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		yo = 0;
	      }                   
	    
	    // Final data vxo
	    if ([mObjTitles containsObject: @"vxf"] ||
		[mObjTitles containsObject: @"vx"])
	      {
		int vxf_o; 
		angx = 0;
		
		if ([mObjTitles containsObject: @"vxf"])
		  {
		    vxf_o = [mObjTitles indexOfObject: @"vxf"];
		  }
		else
		  {
		    vxf_o = [mObjTitles indexOfObject: @"vx"];
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: vxf_o]])
		  {
		    vxo = [[mObj objectAtIndex: vxf_o] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: vxf_o]];
		    vxo = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		if ([mObjTitles containsObject: @"vf"])
		  {
		    int vf_o = [mObjTitles indexOfObject: @"vf"];
		    int ang_x = [mObjTitles indexOfObject: @"angf"];
		    
		    if (![vars containsObject: [mObj objectAtIndex: vf_o]])
		      {
                        vxo = [[mObj objectAtIndex: vf_o] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj objectAtIndex: vf_o]];
                        vxo = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObj objectAtIndex: ang_x]])
		      {
                        angx = [[mObj objectAtIndex: ang_x] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj objectAtIndex: ang_x]];
                        angx = gsl_vector_get (v, k);
		      }
		  }
		else
		  {
		    vxo = 0;
		    angx = 0;
		  }
	      }     
	    
	    // Final data vyo
	    if ([mObjTitles containsObject: @"vyf"] ||
		[mObjTitles containsObject: @"vy"])
	      {
		int vyf_o;
		angy = 90;
		
		if ([mObjTitles containsObject: @"vyf"])
		  {
		    vyf_o = [mObjTitles indexOfObject: @"vyf"];
		  }
		else
		  {
		    vyf_o = [mObjTitles indexOfObject: @"vy"];
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: vyf_o]])
		  {
		    vyo = [[mObj objectAtIndex: vyf_o] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: vyf_o]];
		    vyo = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		if ([mObjTitles containsObject: @"vf"])
		  {
		    int vf_o = [mObjTitles indexOfObject: @"vf"];
		    int ang_y = [mObjTitles indexOfObject: @"angf"];
		    
		    if (![vars containsObject: [mObj objectAtIndex: vf_o]])
		      {
                        vyo = [[mObj objectAtIndex: vf_o] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj objectAtIndex: vf_o]];
                        vyo = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObj objectAtIndex: ang_y]])
		      {
                        angy = [[mObj objectAtIndex: ang_y] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj objectAtIndex: ang_y]];
                        angy = gsl_vector_get (v, k);
		      }
		  }
		else
		  {
		    vyo = 0;
		    angy = 0;
		  }
	      }     
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, xsi + vxs*tf + xo - xof);
	    gsl_vector_set (func, nEqu + 1, ysi + vys*tf + yo - yof);
	    gsl_vector_set (func, nEqu + 2,
			    vxs + vxo*cos(M_PI*angx/180) - vxof);
	    gsl_vector_set (func, nEqu + 3,
			    vys + vyo*sin(M_PI*angy/180) - vyof);
            
	    nEqu += 4;
	  }
	  break;
	case 3:
	  {
	    double ti, ax, ay, xf, xi, yf, yi, vxf, vxi, vyf, vyi;
            
	    // Initial data t
	    if (![vars containsObject: [dat objectAtIndex: 7]])
	      {
		ti = [[dat objectAtIndex: 7] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 7]];
		ti = gsl_vector_get (v, k);
	      }                   
	    
	    // Data ax
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		ax = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		ax = gsl_vector_get (v, k);
	      }
	    
	    // Data ay
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		ay = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		ay = gsl_vector_get (v, k);
	      }
	    
	    // Initial data x
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		xi = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		xi = gsl_vector_get (v, k);
	      }
	    
	    // Initial dato y
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		yi = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		yi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data vx
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		vxi = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		vxi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data vy
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		vyi = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		vyi = gsl_vector_get (v, k);
	      }
	    
	    // Final data x
	    if (![vars containsObject: [dat objectAtIndex: 8]])
	      {
		xf = [[dat objectAtIndex: 8] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 8]];
		xf = gsl_vector_get (v, k);
	      }
	    
	    // Final data y
	    if (![vars containsObject: [dat objectAtIndex: 9]])
	      {
		yf = [[dat objectAtIndex: 9] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 9]];
		yf = gsl_vector_get (v, k);
	      }
	    
	    // Final data vx
	    if (![vars containsObject: [dat objectAtIndex: 10]])
	      {
		vxf = [[dat objectAtIndex: 10] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 10]];
		vxf = gsl_vector_get (v, k);
	      }
	    
	    // Final data vy
	    if (![vars containsObject: [dat objectAtIndex: 11]])
	      {
		vyf = [[dat objectAtIndex: 11] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 11]];
		vyf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu,
			    xi + vxi*(tf-ti) + 0.5*ax*(tf-ti)*(tf-ti) - xf);
	    gsl_vector_set (func, nEqu + 1,
			    yi + vyi*(tf-ti) + 0.5*ay*(tf-ti)*(tf-ti) - yf);
	    gsl_vector_set (func, nEqu + 2, vxi + ax*(tf-ti) - vxf);
	    gsl_vector_set (func, nEqu + 3, vyi + ay*(tf-ti) - vyf);  
            
	    nEqu += 4; 
	  }
	  break;
	case 4:
	  {
	    double ti, ax, ay, xi, yi, vi, angi, xf, yf, vf, angf;
            
	    // Initial data t
	    if (![vars containsObject: [dat objectAtIndex: 7]])
	      {
		ti = [[dat objectAtIndex: 7] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 7]];
		ti = gsl_vector_get (v, k);
	      }
	    
	    // Data ax
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		ax = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		ax = gsl_vector_get (v, k);
	      }
	    
	    // Data ay
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		ay = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		ay = gsl_vector_get (v, k);
	      }
	    
	    // Initial data x
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		xi = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		xi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data y
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		yi = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		yi = gsl_vector_get (v, k);
	      }
	    
	    // Final data x
	    if (![vars containsObject: [dat objectAtIndex: 8]])
	      {
		xf = [[dat objectAtIndex: 8] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 8]];
		xf = gsl_vector_get (v, k);
	      }
	    
	    // Final data y
	    if (![vars containsObject: [dat objectAtIndex: 9]])
	      {
		yf = [[dat objectAtIndex: 9] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 9]];
		yf = gsl_vector_get (v, k);
	      }
	    
	    // Initial data v
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		vi = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    // Final data v
	    if (![vars containsObject: [dat objectAtIndex: 10]])
	      {
		vf = [[dat objectAtIndex: 10] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 10]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    // Initial data angle
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		angi = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		angi = gsl_vector_get (v, k);
	      }
	    
	    // Final data angle
	    if (![vars containsObject: [dat objectAtIndex: 11]])
	      {
		angf = [[dat objectAtIndex: 11] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 11]];
		angf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, xi + vi*cos(M_PI*angi/180)*(tf-ti)
			    + 0.5*ax*(tf-ti)*(tf-ti) - xf);
	    gsl_vector_set (func, nEqu + 1, yi + vi*sin(M_PI*angi/180)*(tf-ti)
			    + 0.5*ay*(tf-ti)*(tf-ti) - yf);
	    gsl_vector_set (func, nEqu + 2, gsl_hypot( (vi*cos(M_PI*angi/180)
							+ ax*(tf-ti)),
						       (vi*sin(M_PI*angi/180)
							+ ay*(tf-ti)) ) - vf);
	    gsl_vector_set (func, nEqu + 3,
			    atan2( (vi*sin(M_PI*angi/180)
				    + ay*(tf-ti)),
				   (vi*cos(M_PI*angi/180)
				    + ax*(tf-ti)) ) - (M_PI*angf/180) );
            
	    nEqu += 4;
	  }
	  break;
	case 5:
	case 6:
	  {  
	    double ti, ax, xf, xi, vxf, vxi;
            
	    // Initial data t
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		ti = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		ti = gsl_vector_get (v, k);
	      }
	    
	    // Data a
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		ax = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		ax = gsl_vector_get (v, k);
	      }      
	    
	    // Initial data x
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		xi = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		xi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data vx
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		vxi = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		vxi = gsl_vector_get (v, k);
	      }
	    
	    // Final data x
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		xf = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		xf = gsl_vector_get (v, k);
	      }
	    
	    // Final data v
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		vxf = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		vxf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, xi + vxi*(tf-ti)
			    + 0.5*ax*(tf-ti)*(tf-ti) - xf);
	    gsl_vector_set (func, nEqu + 1, vxi + ax*(tf-ti) - vxf);
            
	    nEqu += 2;
	  }
	  break;
	case 7:
	  {
	    double xsi, xof, vxs, vxof;
	    double xo, vxo, angx;
            
	    NSString *nameObj = [dat objectAtIndex: 1];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableArray *mObj = nil, *mObjTitles = nil;
	    NSMutableDictionary *sName;
            
	    while ((sName = [search nextObject]))
	      {
		NSString *n = [[sName objectForKey: @"Values"] objectAtIndex:
								 0];
		NSString *t = [[sName objectForKey: @"Titles"] objectAtIndex:
								 0];
                
		if ([t isEqualToString: _(@"Name")] &&
		    [nameObj isEqualToString: n])
		  {
		    mObj = [sName objectForKey: @"Values"];
		    mObjTitles = [sName objectForKey: @"Titles"];
		    break;
		  }
	      }
	    
	    // Initial data xs
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		xsi = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		xsi = gsl_vector_get (v, k);
	      }
	    
	    // Data vxs
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		vxs = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		vxs = gsl_vector_get (v, k);
	      }
	    
	    // Final data xof
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		xof = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		xof = gsl_vector_get (v, k);
	      }
	    
	    // Final data vxof
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		vxof = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		vxof = gsl_vector_get (v, k);
	      }
	    
	    // Get data of object inside mobile system
	    
	    // Final data x
	    if ([mObjTitles containsObject: @"xf"])
	      { 
		int xf_o = [mObjTitles indexOfObject: @"xf"];
		
		if (![vars containsObject: [mObj objectAtIndex: xf_o]])
		  {
		    xo = [[mObj objectAtIndex: xf_o] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: xf_o]];
		    xo = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		xo = 0;
	      }
	    
	    // Final data vxo
	    if ([mObjTitles containsObject: @"vxf"] ||
		[mObjTitles containsObject: @"vx"])
	      {
		int vxf_o;
		angx = 0;
		
		if([mObjTitles containsObject: @"vxf"])
		  {
		    vxf_o = [mObjTitles indexOfObject: @"vxf"];
		  }
		else
		  {
		    vxf_o = [mObjTitles indexOfObject: @"vx"];
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: vxf_o]])
		  {
		    vxo = [[mObj objectAtIndex: vxf_o] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: vxf_o]];
		    vxo = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		if ([mObjTitles containsObject: @"vf"])
		  {
		    int vf_o = [mObjTitles indexOfObject: @"vf"];
		    int ang_x = [mObjTitles indexOfObject: @"angf"];
		    
		    if (![vars containsObject: [mObj objectAtIndex: vf_o]])
		      {
                        vxo = [[mObj objectAtIndex: vf_o] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj objectAtIndex: vf_o]];
                        vxo = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObj objectAtIndex: ang_x]])
		      {
                        angx = [[mObj objectAtIndex: ang_x] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj objectAtIndex: ang_x]];
                        angx = gsl_vector_get (v, k);
		      }
		  }
		else
		  {
		    vxo = 0;
		    angx = 0;
		  }
	      }     
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, xsi + vxs*tf + xo - xof);
	    gsl_vector_set (func, nEqu + 1,
			    vxs + vxo*cos(M_PI*angx/180) - vxof);
            
	    nEqu += 2;
	  }
	  break;
	case 8:
	  {
	    double ysi, yof, vys, vyof;
	    double yo, vyo, angy;
            
	    NSString *nameObj = [dat objectAtIndex: 1];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableArray *mObj = nil, *mObjTitles = nil;
	    NSMutableDictionary *sName;
            
	    while ((sName = [search nextObject]))
	      {
		NSString *n = [[sName objectForKey: @"Values"] objectAtIndex:
								 0];
		NSString *t = [[sName objectForKey: @"Titles"] objectAtIndex:
								 0];
                
		if ([t isEqualToString: _(@"Name")] &&
		    [nameObj isEqualToString: n])
		  {
		    mObj = [sName objectForKey: @"Values"];
		    mObjTitles = [sName objectForKey: @"Titles"];
		    break;
		  }
	      }
	    
	    // Initial data ys
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		ysi = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		ysi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data vys
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		vys = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		vys = gsl_vector_get (v, k);
	      }
	    
	    // Final data yo
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		yof = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		yof = gsl_vector_get (v, k);
	      }
	    
	    // Final data vyof
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		vyof = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		vyof = gsl_vector_get (v, k);
	      }
	    
	    // Get data of object inside mobile system
	    
	    // Final data y
	    if ([mObjTitles containsObject: @"yf"])
	      {
		int yf_o = [mObjTitles indexOfObject: @"yf"];
		
		if (![vars containsObject: [mObj objectAtIndex: yf_o]])
		  {
		    yo = [[mObj objectAtIndex: yf_o] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: yf_o]];
		    yo = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		yo = 0;
	      }
	    
	    // Final data vyo
	    if ([mObjTitles containsObject: @"vyf"] ||
		[mObjTitles containsObject: @"vy"])
	      {
		int vyf_o;
		angy = 90;
		
		if ([mObjTitles containsObject: @"vyf"])
		  {
		    vyf_o = [mObjTitles indexOfObject: @"vyf"];
		  }
		else
		  {
		    vyf_o = [mObjTitles indexOfObject: @"vy"];
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: vyf_o]])
		  {
		    vyo = [[mObj objectAtIndex: vyf_o] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: vyf_o]];
		    vyo = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		if ([mObjTitles containsObject: @"vf"])
		  {
		    int vf_o = [mObjTitles indexOfObject: @"vf"];
		    int ang_y = [mObjTitles indexOfObject: @"angf"];
		    
		    if (![vars containsObject: [mObj objectAtIndex: vf_o]])
		      {
                        vyo = [[mObj objectAtIndex: vf_o] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj objectAtIndex: vf_o]];
                        vyo = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObj objectAtIndex: ang_y]])
		      {
                        angy = [[mObj objectAtIndex: ang_y] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj objectAtIndex: ang_y]];
                        angy = gsl_vector_get (v, k);
		      }
		  }
		else
		  {
		    vyo = 0;
		    angy = 0;
		  }
	      }     
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, ysi + vys*tf + yo - yof);
	    gsl_vector_set (func, nEqu + 1,
			    vys + vyo*sin(M_PI*angy/180) - vyof);
            
	    nEqu += 2;
	  }
	  break;
	case 9:
	case 10:
	  {
	    double ti, xf, xi, vx;
	    
	    // Initial data t
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		ti = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		ti = gsl_vector_get (v, k);
	      }
	    
	    // Initial data x
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		xi = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		xi = gsl_vector_get (v, k);
	      }
	    
	    // Final data x
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		xf = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		xf = gsl_vector_get (v, k);
	      }
	    
	    // Data v
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		vx = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		vx = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, xi + vx*(tf-ti) - xf);
	    
	    nEqu += 1;
	  }
	  break;
	case 11:
	  {
	    double xf1, yf1, xf2, yf2, d;
            
	    NSString *nameObj1 = [dat objectAtIndex: 0];
	    NSString *nameObj2 = [dat objectAtIndex: 1];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableArray *mObj1 = nil, *mObjTitles1 = nil,
	      *mObj2 = nil, *mObjTitles2 = nil;
	    NSMutableDictionary *sName;
            
	    while ((sName = [search nextObject]))
	      {
		NSString *n = [[sName objectForKey: @"Values"] objectAtIndex:
								 0];
		NSString *t = [[sName objectForKey: @"Titles"] objectAtIndex:
								 0];
                
		if ([t isEqualToString: _(@"Name")])
		  {
		    if ([nameObj1 isEqualToString: n])
		      {
			mObj1 = [sName objectForKey: @"Values"];
			mObjTitles1 = [sName objectForKey: @"Titles"];
		      }
		    
		    if ([nameObj2 isEqualToString: n])
		      {
			mObj2 = [sName objectForKey: @"Values"];
			mObjTitles2 = [sName objectForKey: @"Titles"];
		      }

		    if (mObj1 != nil && mObj2 != nil)
		      {
			break;
		      }
		  }
	      }
	    
	    // Final data x for object 1
	    if ([mObjTitles1 containsObject: @"xf"])
	      { 
		int xf_uno = [mObjTitles1 indexOfObject: @"xf"];
		
		if (![vars containsObject: [mObj1 objectAtIndex: xf_uno]])
		  {
		    xf1 = [[mObj1 objectAtIndex: xf_uno] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj1 objectAtIndex: xf_uno]];
		    xf1 = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		xf1 = 0;
	      }                                     
	    
	    // Final data y for object 1
	    if ([mObjTitles1 containsObject: @"yf"])
	      { 
		int yf_uno = [mObjTitles1 indexOfObject: @"yf"];
		
		if (![vars containsObject: [mObj1 objectAtIndex: yf_uno]])
		  {
		    yf1 = [[mObj1 objectAtIndex: yf_uno] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj1 objectAtIndex: yf_uno]];
		    yf1 = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		yf1 = 0;
	      }
	    
	    // Final data x for object 2
	    if ([mObjTitles2 containsObject: @"xf"])
	      { 
		int xf_dos = [mObjTitles2 indexOfObject: @"xf"];
		
		if (![vars containsObject: [mObj2 objectAtIndex: xf_dos]])
		  {
		    xf2 = [[mObj2 objectAtIndex: xf_dos] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj2 objectAtIndex: xf_dos]];
		    xf2 = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		xf2 = 0;
	      }
	    
	    // Final data y for object 2
	    if ([mObjTitles2 containsObject: @"yf"])
	      {
		int yf_dos = [mObjTitles2 indexOfObject: @"yf"];
		
		if (![vars containsObject: [mObj2 objectAtIndex: yf_dos]])
		  {
		    yf2 = [[mObj2 objectAtIndex: yf_dos] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj2 objectAtIndex: yf_dos]];
		    yf2 = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		yf2 = 0;
	      }
	    
	    // Data d
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		d = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		d = gsl_vector_get (v, k);
	      }
	    
	    // Build equations
	    gsl_vector_set (func, nEqu, gsl_hypot( xf1 - xf2, yf1 - yf2) - d);
	    
	    nEqu += 1;
	  }
	  break;
	case 12:
	  {
	    double vxf1, vyf1, vxf2, vyf2, angx1, angy1, angx2, angy2, vr, ang;
	    
	    NSString *nameObj1 = [dat objectAtIndex: 0];
	    NSString *nameObj2 = [dat objectAtIndex: 1];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableArray *mObj1 = nil, *mObjTitles1 = nil,
	      *mObj2 = nil, *mObjTitles2 = nil;
	    NSMutableDictionary *sName;
            
	    while ((sName = [search nextObject]))
	      {
		NSString *n = [[sName objectForKey: @"Values"] objectAtIndex:
								 0];
		NSString *t = [[sName objectForKey: @"Titles"] objectAtIndex:
								 0];
                
		if ([t isEqualToString: _(@"Name")])
		  { 
		    if ([nameObj1 isEqualToString: n])
		      {
			mObj1 = [sName objectForKey: @"Values"];
			mObjTitles1 = [sName objectForKey: @"Titles"]; 
		      }
		    
		    if ([nameObj2 isEqualToString: n])
		      {
			mObj2 = [sName objectForKey: @"Values"];
			mObjTitles2 = [sName objectForKey: @"Titles"];
		      }

		    if (mObj1 != nil && mObj2 != nil)
		      {
			break;
		      }
		  }
	      }
	    
	    // Final data vx for object 1
	    if ([mObjTitles1 containsObject: @"vxf"] ||
		[mObjTitles1 containsObject: @"vx"])
	      {
		int vxf_one;
		angx1 = 0;
		
		if ([mObjTitles1 containsObject: @"vxf"])
		  {
		    vxf_one = [mObjTitles1 indexOfObject: @"vxf"];
		  }
		else
		  {
		    vxf_one = [mObjTitles1 indexOfObject: @"vx"];
		  }
		
		if (![vars containsObject: [mObj1 objectAtIndex: vxf_one]])
		  {
		    vxf1 = [[mObj1 objectAtIndex: vxf_one] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject:
				    [mObj1 objectAtIndex: vxf_one]];
		    vxf1 = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		if ([mObjTitles1 containsObject: @"vf"])
		  {
		    int vf_one = [mObjTitles1 indexOfObject: @"vf"];
		    int ang_x = [mObjTitles1 indexOfObject: @"angf"];
		    
		    if (![vars containsObject: [mObj1 objectAtIndex: vf_one]])
		      {
                        vxf1 = [[mObj1 objectAtIndex: vf_one] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj1 objectAtIndex: vf_one]];
                        vxf1 = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObj1 objectAtIndex: ang_x]])
		      {
                        angx1 = [[mObj1 objectAtIndex: ang_x] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj1 objectAtIndex: ang_x]];
                        angx1 = gsl_vector_get (v, k);
		      }
		  }
		else
		  {
		    vxf1 = 0;
		    angx1 = 0;
		  }
	      }
	    
	    // Final data vy for object 1
	    if ([mObjTitles1 containsObject: @"vyf"] ||
		[mObjTitles1 containsObject: @"vy"])
	      {
		int vyf_one;
		angy1 = 90;
		
		if ([mObjTitles1 containsObject: @"vyf"])
		  {
		    vyf_one = [mObjTitles1 indexOfObject: @"vyf"];
		  }
		else
		  {
		    vyf_one = [mObjTitles1 indexOfObject: @"vy"];
		  }
		
		if (![vars containsObject: [mObj1 objectAtIndex: vyf_one]])
		  {
		    vyf1 = [[mObj1 objectAtIndex: vyf_one] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject:
				    [mObj1 objectAtIndex: vyf_one]];
		    vyf1 = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		if ([mObjTitles1 containsObject: @"vf"])
		  {
		    int vf_one = [mObjTitles1 indexOfObject: @"vf"];
		    int ang_y = [mObjTitles1 indexOfObject: @"angf"];
		    
		    if (![vars containsObject: [mObj1 objectAtIndex: vf_one]])
		      {
                        vyf1 = [[mObj1 objectAtIndex: vf_one] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj1 objectAtIndex: vf_one]];
                        vyf1 = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObj1 objectAtIndex: ang_y]])
		      {
                        angy1 = [[mObj1 objectAtIndex: ang_y] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj1 objectAtIndex: ang_y]];
                        angy1 = gsl_vector_get (v, k);
		      }
		  }
		else
		  {
		    vyf1 = 0;
		    angy1 = 0;
		  }
	      }  
	    
	    // Final data vx for object 2
	    if ([mObjTitles2 containsObject: @"vxf"] ||
		[mObjTitles2 containsObject: @"vx"])
	      {
		int vxf_two;
		angx2 = 0;
		
		if ([mObjTitles2 containsObject: @"vxf"])
		  {
		    vxf_two = [mObjTitles2 indexOfObject: @"vxf"];
		  }
		else
		  {
		    vxf_two = [mObjTitles2 indexOfObject: @"vx"];
		  }
		
		if (![vars containsObject: [mObj2 objectAtIndex: vxf_two]])
		  {
		    vxf2 = [[mObj2 objectAtIndex: vxf_two] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject:
				    [mObj2 objectAtIndex: vxf_two]];
		    vxf2 = gsl_vector_get (v, k);
		  }
	      }
	    else
	      {
		if ([mObjTitles2 containsObject: @"vf"])
		  {
		    int vf_one = [mObjTitles2 indexOfObject: @"vf"];
		    int ang_x = [mObjTitles2 indexOfObject: @"angf"];
		    
		    if (![vars containsObject: [mObj2 objectAtIndex: vf_one]])
		      {
                        vxf2 = [[mObj2 objectAtIndex: vf_one] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj2 objectAtIndex: vf_one]];
                        vxf2 = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObj2 objectAtIndex: ang_x]])
		      {
                        angx2 = [[mObj2 objectAtIndex: ang_x] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj2 objectAtIndex: ang_x]];
                        angx2 = gsl_vector_get (v, k);
		      }
		  }
		else
		  {
		    vxf2 = 0;
		    angx2 = 0; 
		  }
	      }
	    
	    // Final data vy for object 2
	    if ([mObjTitles2 containsObject: @"vyf"] ||
		[mObjTitles2 containsObject: @"vy"])
	      {
		int vyf_dos;
		angy2 = 90;
		
		if ([mObjTitles2 containsObject: @"vyf"])
		  {
		    vyf_dos = [mObjTitles2 indexOfObject: @"vyf"];
		  }
		else
		  {
		    vyf_dos = [mObjTitles2 indexOfObject: @"vy"];
		  }
		
		if (![vars containsObject: [mObj2 objectAtIndex: vyf_dos]])
		  {
		    vyf2 = [[mObj2 objectAtIndex: vyf_dos] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject:
				    [mObj2 objectAtIndex: vyf_dos]];
		    vyf2 = gsl_vector_get (v, k);
		  }
	      }                   
	    else
	      {
		if ([mObjTitles2 containsObject: @"vf"])
		  {
		    int vf_one = [mObjTitles2 indexOfObject: @"vf"];
		    int ang_y = [mObjTitles2 indexOfObject: @"angf"];
		    
		    if (![vars containsObject: [mObj2 objectAtIndex: vf_one]])
		      {
                        vyf2 = [[mObj2 objectAtIndex: vf_one] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj2 objectAtIndex: vf_one]];
                        vyf2 = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObj2 objectAtIndex: ang_y]])
		      {
                        angy2 = [[mObj2 objectAtIndex: ang_y] doubleValue];
		      }
		    else
		      {
                        int k = [vars indexOfObject:
					[mObj2 objectAtIndex: ang_y]];
                        angy2 = gsl_vector_get (v, k);
		      }
		  }
		else
		  {
		    vyf2 = 0;
		    angy2 = 0;
		  }
	      }  
	    
	    // Data relative velocity
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		vr = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		vr = gsl_vector_get (v, k);
	      }
	    
	    // Data angle
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		ang = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		ang = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    {
	      const double cvx = (vxf1*cos(M_PI*angx1/180)) -
		(vxf2*cos(M_PI*angx2/180));
	      const double cvy = (vyf1*sin(M_PI*angy1/180)) -
		(vyf2*sin(M_PI*angy2/180)); 
	      
	      gsl_vector_set (func, nEqu, gsl_hypot( cvx, cvy ) - vr);
	      gsl_vector_set (func, nEqu + 1,
			      atan2( (vyf1*sin(M_PI*angy1/180)) -
				     (vyf2*sin(M_PI*angy2/180)),
				     (vxf1*cos(M_PI*angx1/180)) -
				     (vxf2*cos(M_PI*angx2/180)) ) -
			      (M_PI*ang/180) );
	      
	      nEqu += 2;
	    }
	  }
	  break;
	case 13:
	  {
	    double ar, angf, xi, yi, vi, ti, xf, yf, vf;
            
	    // Data radial acceleration
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		ar = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		ar = gsl_vector_get (v, k);
	      }
	    
	    // Data angle
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		angf = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		angf = gsl_vector_get (v, k);
	      }
	    
	    // Initial data x
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		xi = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		xi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data y
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		yi = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		yi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data v
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		vi = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    // Initial data t
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		ti = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		ti = gsl_vector_get (v, k);
	      }
	    
	    // Final data x
	    if (![vars containsObject: [dat objectAtIndex: 7]])
	      {
		xf = [[dat objectAtIndex: 7] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 7]];
		xf = gsl_vector_get (v, k);
	      }
	    
	    // Final data y
	    if (![vars containsObject: [dat objectAtIndex: 8]])
	      {
		yf = [[dat objectAtIndex: 8] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 8]];
		yf = gsl_vector_get (v, k);
	      }
	    
	    // Final data v
	    if (![vars containsObject: [dat objectAtIndex: 9]])
	      {
		vf = [[dat objectAtIndex: 9] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 9]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, xi + vi*cos(M_PI*angf/180)*(tf - ti) +
			    0.5*ar*cos(M_PI*angf/180)*(tf - ti)*(tf - ti) -
			    xf);
	    gsl_vector_set (func, nEqu + 1, yi +
			    vi*sin(M_PI*angf/180)*(tf - ti) +
			    0.5*ar*sin(M_PI*angf/180)*(tf - ti)*(tf - ti) -
			    yf);
	    gsl_vector_set (func, nEqu + 2, vi + ar*(tf - ti) - vf);
	    
	    nEqu += 3;
	  }
	  break;
	case 14:
	  {
	    double x1, x2, dx;
            
	    // Data x for object 1
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		x1 = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		x1 = gsl_vector_get (v, k);
	      }
	    
	    // Data x for object 2
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		x2 = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		x2 = gsl_vector_get (v, k);
	      }
	    
	    // Data dx 
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		dx = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		dx = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, x1 - x2 - dx);
	    
	    nEqu += 1;
	  }
	  break;
	}
    }

  return GSL_SUCCESS;
}


@interface FLKinematics (Private)
- (void) makeSystem;
@end

@implementation FLKinematics (Private)

- (void) makeSystem
{
  int increase = 1;
  double newValue;
  BOOL follow;
  
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;
  
  int state = 0, stateInt, k, length;
  double par;
  NSString *message;
  size_t iter;
  int countRes = 0;
  id anObj;
  NSMutableArray *results;
  NSEnumerator *varCount;
  id dataSign;
  NSNumber *typeOther;
  NSMutableArray *checkSign;
  NSEnumerator *signObj;
  
  int nvar  = [vars count];
  const size_t n = nvar;
  gsl_vector *x;
  const gsl_rng_type * Y;
  gsl_rng * r;
  
  x = gsl_vector_alloc (n);
  
  // Random numbers generator
  gsl_rng_env_setup();
  Y = gsl_rng_default;
  r = gsl_rng_alloc (Y);
  
  // Check if time is positive or if the system is stuck
  do
    {
      gsl_multiroot_function f;
      iter = 0;
      for (k = 0; k < nvar; k++)
	{
	  if (increase <= 15)
	    {
	      newValue = 1;
	    }
	  else if (increase <= 30)
	    {
	      newValue = 10;
	    }
	  else if (increase <= 60)
	    {
	      newValue = 100;
	    }
	  else
	    {
	      newValue = 1000;
	    }
	  
	  par = newValue*(gsl_rng_uniform (r));
	  gsl_vector_set (x, k, par);
	}
      
      f.f = &buildSystem;
      f.n = n;
      f.params = (__bridge void *)(self);
      T = gsl_multiroot_fsolver_hybrids;
      s = gsl_multiroot_fsolver_alloc (T, nvar);
      gsl_multiroot_fsolver_set (s, &f, x);
      
      do
	{
	  iter++;
	  stateInt = gsl_multiroot_fsolver_iterate (s);
	  
	  if(stateInt)
	    break;
	  
	  state = gsl_multiroot_test_residual (s->f, 1e-7);
	}
      while (state == GSL_CONTINUE && iter < 1000);  
      
      // Check time variables
      if ( ( (varT == 1) || (stateInt) ) && (increase < 90) )
	{
	  if (stateInt)
	    {
	      increase += 1;
	      follow = YES;
	    }
	  else
	    {
	      int r, Tindex;
	      int sign = 0;
	      double check ;
	      Tindex = [vars indexOfObject: timeVar];
	      
	      for (r = 0; r < [varsTime count]; r++)
		{
		  check = (gsl_vector_get (s->x, Tindex)) -
		    [[varsTime objectAtIndex: r] doubleValue];

		  if (check >= 0)
		    {
		      sign += 1;
		    }
		}
              
	      if (sign != [varsTime count])
		{
		  increase += 1;
		  follow = YES;
		}
	      else
		{
		  follow = NO;
		}
	    }
	}
      else
	{
	  follow = NO;
	}
      // Verification has terminated
    }
  while (follow);
  // The search of the solution has terminated
  
  // Move the results to array results
  results = [NSMutableArray array];
  varCount = [vars objectEnumerator];
  
  while ((anObj = [varCount nextObject]))
    {
      [results addObject: [NSNumber numberWithDouble:
				      gsl_vector_get (s->x, countRes)]];
      countRes += 1;
    }
  
  // Check and correct the signs
  signObj = [dictionary objectEnumerator];
  
  while ((dataSign = [signObj nextObject]))
    {
      int par1 = 0, w = 0;
      typeOther = [dataSign objectForKey: @"Type"];
      
      switch ([typeOther intValue])
	{
	case 12:
	  {
	    checkSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the velocity
	    if ( [vars containsObject: [checkSign objectAtIndex: 2]] &&
		 [vars containsObject: [checkSign objectAtIndex: 3]] )
	      {
		w = [vars indexOfObject: [checkSign objectAtIndex: 2]];
		
		if ([[results objectAtIndex: w] doubleValue] < 0)
		  {
		    double nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w withObject:
			       [NSNumber numberWithDouble: nv]];
		    par1 = 1;
		  }
	      }
	    
	    // Correct the angle
	    if ([vars containsObject: [checkSign objectAtIndex: 3]])
	      {
		double nv;
		w = [vars indexOfObject: [checkSign objectAtIndex: 3]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( par1 == 1 )
		  {
		    nv += 180;
		  }
                
                if ( nv > 360 )
		  {
		    nv -= floor(nv/360)*360;
		  }
                
                if( nv < 0 )
		  {
		    nv += (floor(-1*nv/360) + 1)*360;
		  }
                
                [results replaceObjectAtIndex: w
				   withObject: [NSNumber numberWithDouble: nv]];
	      }
	  }
	  break;
	case 13:
	  {
	    checkSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the angle
	    if ([vars containsObject: [checkSign objectAtIndex: 2]])
	      {
		double nv;
		w = [vars indexOfObject: [checkSign objectAtIndex: 2]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv > 360 )
		  {
		    nv -= floor(nv/360)*360;
		  }
                
                if ( nv < 0 )
		  {
		    nv += (floor(-1*nv/360) + 1)*360;
		  }
                
                [results replaceObjectAtIndex: w
				   withObject: [NSNumber numberWithDouble: nv]];
	      }
	  }
	  break;
	}
    }
  
  // Print the results
  [self printUnknowns: vars withResults: results];
  
  // Print the calculus state
  message = [NSString stringWithFormat: _(@"Status = %s \n"),
		      gsl_strerror (state)];
  length = [[[self viewer] textStorage] length];
  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: message];
  
  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  gsl_rng_free (r);
}

@end

@implementation FLKinematics

- (id) init
{
  NSBundle *messages;
  self = [super init];
  
  vars = [NSMutableArray new];
  varsTime = [NSMutableArray new];
  dictionary = [NSMutableArray new];
  
  // Build the array of messages
  messages = [NSBundle mainBundle];
  errors = [[NSArray alloc] initWithContentsOfFile:
			  [messages pathForResource: @"kinematicsMessages"
					     ofType: @"plist"]];
  
  return self;
}

- (void) makeEquationsForData: (NSMutableDictionary *)list
{
  int length, c, objsContained = 0, numEquations = 0;
  NSUInteger sys= 0;
  BOOL error = NO;
  BOOL errorName = NO;
  BOOL errorContent = NO;
  NSNumber *identifier;
  NSMutableDictionary *object;
  NSMutableArray *namesObjects = [NSMutableArray array];
  NSMutableArray *namesContainedObjects = [NSMutableArray array];
  NSEnumerator *enumerator;

  varT = 0;
  timeDat = 0;
  
  [dictionary setArray: [list allValues]];
  enumerator = [dictionary objectEnumerator];
  
  while ((object = [enumerator nextObject]) && !error) 
    {
      int x;
      NSString *key;
      NSArray *terms;
      NSNumber *number;
      NSArray *titles;
      NSMutableArray *info;
      NSMutableArray *values;
      identifier = [object objectForKey: @"Type"];
      titles = [object objectForKey: @"Titles"];
      info = [object objectForKey: @"Data"];
      values = [object objectForKey: @"Values"];
      [values removeAllObjects];
      
      // Count the number of variables
      for (x = 0; x < [info count]; x++)
	{
	  NSString *data = [info objectAtIndex: x];
	  NSString *title = [[titles objectAtIndex: x] description];
	  
	  if (![self isNumericDataTheString: data] &&
	      ![title isEqualToString: _(@"Object")] &&
	      ![title isEqualToString: _(@"Object 1")] &&
	      ![title isEqualToString: _(@"Object 2")] &&
	      ![title isEqualToString: _(@"Name")])
	    {
	      if ([self hasConversionTheString: data])
		{
		  terms = [data componentsSeparatedByString: @"@"];
		  key = [[terms objectAtIndex: 1] stringByTrimmingSpaces];
                  
		  if ([[[self conversions] allKeys] containsObject: key])
		    {
		      NSString *var = [[terms objectAtIndex: 0]
					stringByTrimmingSpaces];

		      if ([self isNumericDataTheString: var])
                        {
			  number = [NSNumber numberWithDouble:
					     [var doubleValue]*
					     [[[self conversions] objectForKey:
							    key] doubleValue]];
			  [values addObject: [number stringValue]];
			  
			  // If is a time variable add this to array varsTime
			  if ([title hasPrefix: @"t"] &&
			      [identifier intValue] != 1)
			    {
			      [varsTime addObject: [number stringValue]];
			    }
                        }
		      else
                        {
			  var = [var stringByAppendingString: @"@"];
			  var = [var stringByAppendingString: key];
			  
			  [values addObject: var];

			  if (![vars containsObject: var])
			    {
			      [vars addObject: var];
			    }
                        }
		    }
		  else
		    {
		      NSString *advert;
                      length = [[[self viewer] textStorage] length];
                      advert = [NSString stringWithFormat:
					   [errors objectAtIndex: 0],
					   [key cString]];
                      [[self viewer] replaceCharactersInRange:
			                           NSMakeRange(length, 0)
						   withString: advert];
                      error = YES;
		    }
		  
		}
	      else
		{
		  // Add a simple variable (without a conversion factor)
		  NSString *varFactor;
		  sys = [self system];
		  
		  // Check the type of variable
		  if ([title hasPrefix: @"x"] || [title hasPrefix: @"y"] || 
		      [title hasPrefix: @"d"])
		    {
		      if (sys == 0)
			{ varFactor = @"m"; }
		      else
			{ varFactor = @"ft"; }
		    }
		  else if ([title hasPrefix: @"t"])
		    { 
		      varFactor = @"s";
		    }
		  else if ([title hasPrefix: @"v"])
		    {
		      if (sys == 0)
			{ varFactor = @"m/s"; }
		      else
			{ varFactor = @"ft/s"; }
		    }
		  else if ([title hasPrefix: @"ang"])
		    { 
		      varFactor = [NSString stringWithString: _(@"degrees")];
		    }
		  else
		    {
		      if (sys == 0)
			{ varFactor = @"m/s2"; }
		      else
			{ varFactor = @"ft/s2"; }
		    }
		  
		  data = [data stringByAppendingString: @"@"];
		  data = [data stringByAppendingString: varFactor];
		  
		  [values addObject: data];
		  if (![vars containsObject: data])
		    {
		      [vars addObject: data];
		    }
		  
		  // If is a time variable add this to array varsTime
		  if ([[titles objectAtIndex: x] hasPrefix: @"t"] &&
		      [identifier intValue] != 1)
		    {
		      [varsTime addObject: data];
		    }
		}
	    }
          else
	    {
	      [values addObject: data];
              
	      // If is a time data add this to array varsTime
	      if ([title hasPrefix: @"t"] && [identifier intValue] != 1)
		{
		  [varsTime addObject: data];
		}
	    }
	}    
      // Here ends the count of variables
      
      if (error)
	break;
      
      // Verify the number of equations
      switch ([identifier intValue])
	{
	case 1:
	  {
	    if(![self isNumericDataTheString: [values objectAtIndex: 0]])
	      {
		timeVar = [values objectAtIndex: 0];
		varT = 1;
	      }
	    else
	      {
		timeDat = [[values objectAtIndex: 0] doubleValue];
		varT = 2;
	      }
	  }
	  break;
	case 2:
	  {
	    numEquations += 4;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		if (![namesObjects containsObject: [values objectAtIndex: 0]])
		  {
		    [namesObjects addObject: [values objectAtIndex: 0]];
		  }
		else
		  {
		    errorName = YES;
		  }
	      }
	    
	    if ( [[values objectAtIndex: 1] length] > 0 &&
		 ![[values objectAtIndex: 1] isEqualToString: @"0"] )
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }    
	  break;
	case 3:
	  {
	    numEquations += 4;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		if (![namesObjects containsObject: [values objectAtIndex: 0]])
		  {
		    [namesObjects addObject: [values objectAtIndex: 0]];
		  }
		else
		  {
		    errorName = YES;
		  }
	      }
	  }
	  break;
	case 4:
	  {
	    numEquations += 4;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		if (![namesObjects containsObject: [values objectAtIndex: 0]])
		  {
		    [namesObjects addObject: [values objectAtIndex: 0]];
		  }
		else
		  {
		    errorName = YES;
		  }
	      }
	  }
	  break;
	case 5:
	case 6:
	  {
	    numEquations += 2;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		if (![namesObjects containsObject: [values objectAtIndex: 0]])
		  {
		    [namesObjects addObject: [values objectAtIndex: 0]];
		  }
		else
		  {
		    errorName = YES;
		  }
	      }
	  }
	  break;
	case 7:
	case 8:
	  {
	    numEquations += 2;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		if (![namesObjects containsObject: [values objectAtIndex: 0]])
		  {
		    [namesObjects addObject: [values objectAtIndex: 0]];
		  }
		else
		  {
		    errorName = YES;
		  }
	      }
	    
	    if ( [[values objectAtIndex: 1] length] > 0 &&
		 ![[values objectAtIndex: 1] isEqualToString: @"0"] )
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 9:
	case 10:
	  {
	    numEquations += 1;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		if (![namesObjects containsObject: [values objectAtIndex: 0]])
		  {
		    [namesObjects addObject: [values objectAtIndex: 0]];
		  }
		else
		  {
		    errorName = YES;
		  }
	      }
	  }
	  break;
	case 11:
	  {
	    numEquations += 1;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	    
	    if ( [[values objectAtIndex: 1] length] > 0 &&
		 ![[values objectAtIndex: 1] isEqualToString: @"0"] )
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 12:
	  {
	    numEquations += 2;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	    
	    if ( [[values objectAtIndex: 1] length] > 0 &&
		 ![[values objectAtIndex: 1] isEqualToString: @"0"] )
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 13:
	  {
	    numEquations += 3;
	    
	    if ( [[values objectAtIndex: 0] length] > 0 &&
		 ![[values objectAtIndex: 0] isEqualToString: @"0"] )
	      {
		if (![namesObjects containsObject: [values objectAtIndex: 0]])
		  {
		    [namesObjects addObject: [values objectAtIndex: 0]];
		  }
		else
		  {
		    errorName = YES;
		  }
	      }
	  }
	  break;
	case 14:
	  {
	    numEquations += 1;
	  }
	  break;
	case 15:
	  {
	    [namesObjects addObject: [values objectAtIndex: 0]];
	  }
	  break;
	}
      // Here ends the verify for equations
    }
  
  // Here begins the verify of the problem
  for (c = 0; c < [namesContainedObjects count]; c++)
    {
      if ([namesObjects containsObject:
			  [namesContainedObjects objectAtIndex: c]])
        {  
	  objsContained += 1;
        }
    }
  
  if ([vars count] == numEquations && numEquations > 0 && !error &&
      !errorName && !errorContent)
    {
      if (objsContained == [namesContainedObjects count])
	{
	  [self makeSystem];
	}
      else
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 1]];
	}
    }
  else
    {
      if (errorName)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 3]];
	}
      else if (errorContent)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 4]];
	}
      else
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 2]];
	}
    }
}

- (void) dealloc
{
  [dictionary release];
  [vars release];
  [varsTime release];
  [errors release];
  [super dealloc];
}

@end
