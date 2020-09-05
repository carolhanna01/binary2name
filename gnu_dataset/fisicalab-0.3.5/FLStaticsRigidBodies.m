/* 
   Copyright (C) 2010, 2011, 2012, 2013, 2014 German A. Arias

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
#import "FLStaticsRigidBodies.h"


static int buildSystem (const gsl_vector *v, void *p, gsl_vector *func)
{
  int gIndex, codeSolidForce = 0;
  int nEqu = 0;
  double gf;
  NSNumber *object;
  NSEnumerator *enumerator;

  // Get properties of FL object.
  FLStaticsRigidBodies *FLObj = (__bridge FLStaticsRigidBodies *)(p);

  NSInteger varG = FLObj->varG;
  NSString *gravityVar = FLObj->gravityVar;
  double gravityDat = FLObj->gravityDat;
  NSMutableArray *vars = FLObj->vars;
  NSMutableArray *forceObjs = FLObj->forceObjs;
  NSMutableArray *codObjects = FLObj->codObjects;
  NSMutableArray *codSolids = FLObj->codSolids;
  NSMutableArray *typeElements = FLObj->typeElements;
  NSMutableArray *solidForceTypes = FLObj->solidForceTypes;
  NSMutableArray *codResul = FLObj->codResul;
  NSMutableArray *codOthers = FLObj->codOthers;
  NSMutableDictionary *dictionary = FLObj->dictionary;
  // ----------------------------------------
  
  if (varG == 1)
    {
      gIndex = [vars indexOfObject: gravityVar];
      gf = gsl_vector_get (v, gIndex);   
    }
  else
    {
      if (varG == 2)
	{
          gf = gravityDat;
	}
      else
	{
          gf = 0;
	}
    }
  
  
  enumerator = [codSolids objectEnumerator];
  
  while ((object = [enumerator nextObject]))
    {
      int t, typeFS;
      double forcesX = 0, forcesY = 0, momentum = 0, bmx = 0, bmy = 0,
	vang = 0, sang = 0;
      double signx, signy, ecx = 0, ecy = 0;
      NSNumber *codElements, *codForce, *typeElem;
      NSEnumerator *elementsSol;
      NSMutableArray *elemDataResul, *dataElem, *dataForces;
      
      NSNumber *type = [[dictionary objectForKey: object]
			 objectForKey: @"Type"];
      NSMutableArray *dat = [[dictionary objectForKey: object]
			      objectForKey: @"Values"];
      
      
      // Get the data of the element with the resultant, if any
      if ([codResul objectAtIndex: codeSolidForce] != [NSNull null])
	{
	  elemDataResul = [[dictionary objectForKey: [codResul objectAtIndex:
			      codeSolidForce]] objectForKey: @"Values"];
	  
	  if ([type intValue] == 276)
	    {
	      if (![vars containsObject: [elemDataResul objectAtIndex: 1]])
		{
		  bmx = [[elemDataResul objectAtIndex: 1] doubleValue];
		}
	      else
		{
		  int k = [vars indexOfObject:
				  [elemDataResul objectAtIndex: 1]];
		  bmx = gsl_vector_get (v, k);
		}
	      
	      if (![vars containsObject: [elemDataResul objectAtIndex: 2]])
		{
		  bmy = [[elemDataResul objectAtIndex: 2] doubleValue];
		}
	      else
		{
		  int k = [vars indexOfObject:
				  [elemDataResul objectAtIndex: 2]];
		  bmy = gsl_vector_get (v, k);
		}
	    }
	  else
	    {
	      if ([type intValue] == 252)
		{
		  double lcr, cang;
		  
		  if (![vars containsObject: [elemDataResul objectAtIndex: 1]])
		    {
		      lcr = [[elemDataResul objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [vars indexOfObject:
				      [elemDataResul objectAtIndex: 1]];
		      lcr = gsl_vector_get (v, k);
		    }
		  
		  if (![vars containsObject: [dat objectAtIndex: 3]])
		    {
		      cang = [[dat objectAtIndex: 3] doubleValue];
		    }
		  else
		    {
		      int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		      cang = gsl_vector_get (v, k);
		    }
		  
		  bmx = lcr*cos(M_PI*cang/180);
		  bmy = lcr*sin(M_PI*cang/180);
		}
	      else
		{
		  double xs, ys, pang;
		  
		  if (![vars containsObject: [elemDataResul objectAtIndex: 1]])
		    {
		      xs = [[elemDataResul objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [vars indexOfObject:
				      [elemDataResul objectAtIndex: 1]];
		      xs = gsl_vector_get (v, k);
		    }
		  
		  if (![vars containsObject: [elemDataResul objectAtIndex: 2]])
		    {
		      ys = [[elemDataResul objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [vars indexOfObject:
				      [elemDataResul objectAtIndex: 2]];
		      ys = gsl_vector_get (v, k);
		    }
		  
		  if (![vars containsObject: [dat objectAtIndex: 4]])
		    {
		      pang = [[dat objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		      pang = gsl_vector_get (v, k);
		    }
		  
		  bmx = xs*cos(M_PI*pang/180) - ys*sin(M_PI*pang/180);
		  bmy = xs*sin(M_PI*pang/180) + ys*cos(M_PI*pang/180);
		}
	    }
	}
      
      
      // Check the type of solid
      if ([type intValue] == 252)
	{
	  double weight, mass, lc;
	  
	  if (![vars containsObject: [dat objectAtIndex: 1]])
	    {
	      mass = [[dat objectAtIndex: 1] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [dat objectAtIndex: 1]];
	      mass = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [dat objectAtIndex: 2]])
	    {
	      lc = [[dat objectAtIndex: 2] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [dat objectAtIndex: 2]];
	      lc = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [dat objectAtIndex: 3]])
	    {
	      vang = [[dat objectAtIndex: 3] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [dat objectAtIndex: 3]];
	      vang = gsl_vector_get (v, k);
	    }
	  
	  // Add the weight to vertical forces and add its momentum
	  weight = -mass*gf;
	  forcesY += weight;
	  momentum += (lc*cos(M_PI*vang/180) - bmx)*weight;
	}
      
      if ([type intValue] == 253)
	{
	  double weight, mass, cx, cy;
	  
	  if (![vars containsObject: [dat objectAtIndex: 1]])
	    {
	      mass = [[dat objectAtIndex: 1] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [dat objectAtIndex: 1]];
	      mass = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [dat objectAtIndex: 2]])
	    {
	      cx = [[dat objectAtIndex: 2] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [dat objectAtIndex: 2]];
	      cx = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [dat objectAtIndex: 3]])
	    {
	      cy = [[dat objectAtIndex: 3] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [dat objectAtIndex: 3]];
	      cy = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [dat objectAtIndex: 4]])
	    {
	      sang = [[dat objectAtIndex: 4] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [dat objectAtIndex: 4]];
	      sang = gsl_vector_get (v, k);
	    }
	  
	  // Add the weight to vertical forces and add its momentum
	  weight = -mass*gf;
	  forcesY += weight;
	  momentum += (cx*cos(M_PI*sang/180) - cy*sin(M_PI*sang/180) - bmx)*
	    weight;
	}
      
      
      // Search the elements of the solid
      elementsSol = [[typeElements objectAtIndex: codeSolidForce]
		      objectEnumerator];
      while ((codElements = [elementsSol nextObject]))
	{
	  int numObj = [codObjects indexOfObject: codElements];
	  double forcesVigaX = 0, forcesVigaY = 0;
	  NSString *f = nil;
	  NSEnumerator *fuerDat = [[forceObjs objectAtIndex: numObj]
				    objectEnumerator];
	  dataElem = [[dictionary objectForKey: codElements]
		       objectForKey: @"Values"];
	  typeElem = [[dictionary objectForKey: codElements]
		       objectForKey: @"Type"];
	  
	  // Gets the coordinates of the element
	  if ([typeElem intValue] == 255)
	    {
	      double l;
	      
	      if (![vars containsObject: [dataElem objectAtIndex: 1]])
		{
		  l = [[dataElem objectAtIndex: 1] doubleValue];
		}
	      else
		{
		  int k = [vars indexOfObject: [dataElem objectAtIndex: 1]];
		  l = gsl_vector_get (v, k);
		}
	      
	      ecx = l*cos(M_PI*vang/180);
	      ecy = l*sin(M_PI*vang/180);
	    }
	  else if ([typeElem intValue] == 256)
	    {
	      double elx, ely;
	      
	      if (![vars containsObject: [dataElem objectAtIndex: 1]])
		{
		  elx = [[dataElem objectAtIndex: 1] doubleValue];
		}
	      else
		{
		  int k = [vars indexOfObject: [dataElem objectAtIndex: 1]];
		  elx = gsl_vector_get (v, k);
		}
	      
	      if (![vars containsObject: [dataElem objectAtIndex: 2]])
		{
		  ely = [[dataElem objectAtIndex: 2] doubleValue];
		}
	      else
		{
		  int k = [vars indexOfObject: [dataElem objectAtIndex: 2]];
		  ely = gsl_vector_get (v, k);
		}
	      
	      ecx = elx*cos(M_PI*sang/180) - ely*sin(M_PI*sang/180);
	      ecy = elx*sin(M_PI*sang/180) + ely*cos(M_PI*sang/180);
	    }
	  else if ([typeElem intValue] == 251)
	    {
	      if (![vars containsObject: [dataElem objectAtIndex: 1]])
		{
		  ecx = [[dataElem objectAtIndex: 1] doubleValue];
		}
	      else
		{
		  int k = [vars indexOfObject: [dataElem objectAtIndex: 1]];
		  ecx = gsl_vector_get (v, k);
		}
	      
	      if (![vars containsObject: [dataElem objectAtIndex: 2]])
		{
		  ecy = [[dataElem objectAtIndex: 2] doubleValue];
		}
	      else
		{
		  int k = [vars indexOfObject: [dataElem objectAtIndex: 2]];
		  ecy = gsl_vector_get (v, k);
		}
	    }
	  
	  
	  // Get the data of applied forces
	  while ((codForce = [fuerDat nextObject]))
	    {
	      t = [[[dictionary objectForKey: codForce]
		     objectForKey: @"Type"] intValue];
	      dataForces = [[dictionary objectForKey: codForce]
			     objectForKey: @"Values"];
	      f = [dataForces objectAtIndex: 0];
	      
	      switch (t)
		{
		  // Momentum
		case 254:
		  {
		    if (![vars containsObject: f])
		      {
			momentum += [f doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			momentum += gsl_vector_get (v, k);
		      }
		  }
		  break;
		  // Horizontal force and horizontal Beam 2F
		case 261:
		case 262:
		case 281:
		case 282:
		  {
		    if ( (t == 261) || (t == 281) )
		      { signx = 1; }
		    else
		      { signx = - 1; }
		    
		    if (![vars containsObject: f])
		      {
			if ([typeElem intValue] == 294)
			  {
			    forcesVigaX += signx*[f doubleValue];
			  }
			else
			  {
			    forcesX += signx*[f doubleValue];
			    momentum += signx*(bmy - ecy)*[f doubleValue];
			  }
		      }
		    else
		      {
			int k = [vars indexOfObject: f];

			if ([typeElem intValue] == 294)
			  {
			    forcesVigaX += signx*(gsl_vector_get (v, k));
			  }
			else
			  {
			    forcesX += signx*(gsl_vector_get (v, k));
			    momentum += signx*(bmy - ecy)*
			      (gsl_vector_get (v, k));
			  }
		      }
		  }
		  break;
		  // Vertical force and vertical Beam 2F
		case 263:
		case 264:
		case 283:
		case 284:
		  {
		    if ( (t == 263) || (t == 283) )
		      { signy = 1; }
		    else
		      { signy = - 1; }
		    
		    if (![vars containsObject: f])
		      {
			if ([typeElem intValue] == 294)
			  {
			    forcesVigaY += signy*[f doubleValue];
			  }
			else
			  {
			    forcesY += signy*[f doubleValue];
			    momentum += signy*(ecx - bmx)*[f doubleValue];
			  }
		      }
		    else
		      {
			int k = [vars indexOfObject: f];

			if ([typeElem intValue] == 294)
			  {
			    forcesVigaY += signy*(gsl_vector_get (v, k));
			  }
			else
			  {
			    forcesY += signy*(gsl_vector_get (v, k));
			    momentum += signy*(ecx - bmx)*
			      (gsl_vector_get (v, k));
			  }
		      }
		  }
		  break;
		  // General forces and general Beams 2F
		case 257 ... 260:
		case 277 ... 280:
		  {
		    double ang;
		    NSString *a = [dataForces objectAtIndex: 1];
                    
		    if ( (t == 257) || (t == 260) || (t == 277) || (t == 280) )
		      { signx = 1; }
		    else
		      { signx = - 1; }
		    
		    if ( (t == 257) || (t == 259) || (t == 277) || (t == 279) )
		      { signy = 1; }
		    else
		      { signy = - 1; }
		    
		    if (![vars containsObject: a])
		      {
			ang = [a doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: a];
			ang = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: f])
		      {
			if ([typeElem intValue] == 294)
			  {
			    forcesVigaX += signx*[f doubleValue]*
			      cos(M_PI*ang/180);
			    forcesVigaY += signy*[f doubleValue]*
			      sin(M_PI*ang/180);
			  }
			else
			  {
			    forcesX += signx*[f doubleValue]*cos(M_PI*ang/180);
			    forcesY += signy*[f doubleValue]*sin(M_PI*ang/180);
			    momentum += signy*(ecx - bmx)*[f doubleValue]*
			      sin(M_PI*ang/180) + signx*(bmy - ecy)*
			      [f doubleValue]*cos(M_PI*ang/180);
			  }
		      }
		    else
		      {
			int k = [vars indexOfObject: f];

			if ([typeElem intValue] == 294)
			  {
			    forcesVigaX += signx*(gsl_vector_get (v, k))*
			      cos(M_PI*ang/180);
			    forcesVigaY += signy*(gsl_vector_get (v, k))*
			      sin(M_PI*ang/180);
			  }
			else
			  {
			    forcesX += signx*(gsl_vector_get (v, k))*
			      cos(M_PI*ang/180);
			    forcesY += signy*(gsl_vector_get (v, k))*
			      sin(M_PI*ang/180);
			    momentum += signy*(ecx - bmx)*
			      (gsl_vector_get (v, k))*sin(M_PI*ang/180) +
			      signx*(bmy - ecy)*(gsl_vector_get (v, k))*
			      cos(M_PI*ang/180);
			  }
		      }
		  }
		  break;
		  // Horizontales frictions
		case 269:
		case 270:
		  {
		    double u;
		    NSString *a = [dataForces objectAtIndex: 1];
		    
		    if (t == 269)
		      { signx = 1; }
		    else
		      { signx = - 1; }
		    
		    if (![vars containsObject: a])
		      {
			u = [a doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: a];
			u = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: f])
		      {
			forcesX += u*signx*[f doubleValue];
			momentum += u*signx*(bmy - ecy)*[f doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			forcesX += u*signx*(gsl_vector_get (v, k));
			momentum += u*signx*(bmy - ecy)*(gsl_vector_get (v, k));
		      } 
		  }
		  break;
		  // Vertical frictions
		case 271:
		case 272:
		  {
		    double u;
		    NSString *a = [dataForces objectAtIndex: 1];
		    
		    if (t == 271)
		      { signy = 1; }
		    else
		      { signy = - 1; }
		    
		    if (![vars containsObject: a])
		      {
			u = [a doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: a];
			u = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: f])
		      {
			forcesY += u*signy*[f doubleValue];
			momentum += u*signy*(ecx - bmx)*[f doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			forcesY += u*signy*(gsl_vector_get (v, k));
			momentum += u*signy*(ecx - bmx)*(gsl_vector_get (v, k));
		      } 
		  }
		  break;
		  // General frictions
		case 265 ... 268:
		  {
		    double u, ang;
		    NSString *a = [dataForces objectAtIndex: 1];
		    NSString *b = [dataForces objectAtIndex: 2];
                    
		    if ( (t == 265) || (t == 268) )
		      { signx = 1; }
		    else
		      { signx = - 1; }
		    
		    if ( (t == 265) || (t == 267) )
		      { signy = 1; }
		    else
		      { signy = - 1; }
		    
		    if (![vars containsObject: b])
		      {
			u = [b doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: b];
			u = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: a])
		      {
			ang = [a doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: a];
			ang = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: f])
		      {
			forcesX += u*signx*[f doubleValue]*cos(M_PI*ang/180);
			forcesY += u*signy*[f doubleValue]*sin(M_PI*ang/180);
			momentum += signy*(ecx - bmx)*u*[f doubleValue]*
			  sin(M_PI*ang/180) + signx*(bmy - ecy)*u*
			  [f doubleValue]*cos(M_PI*ang/180);
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			forcesX += u*signx*(gsl_vector_get (v, k))*
			  cos(M_PI*ang/180);
			forcesY += u*signy*(gsl_vector_get (v, k))*
			  sin(M_PI*ang/180);
			momentum += signy*(ecx - bmx)*u*(gsl_vector_get (v, k))*
			  sin(M_PI*ang/180) + signx*(bmy - ecy)*u*
			  (gsl_vector_get (v, k))*cos(M_PI*ang/180);
		      }
		  }
		  break;
		  // Horizontal resultant
		case 274:
		  {
		    NSString *a = [dataForces objectAtIndex: 1];

		    if (![vars containsObject: f])
		      {
			momentum -= [f doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			momentum -= gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: a])
		      {
			forcesX -= [a doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: a];
			forcesX -= gsl_vector_get (v, k);
		      }
		  }
		  break;
		  // Vertical resultant
		case 275:
		  {
		    NSString *a = [dataForces objectAtIndex: 1];

		    if (![vars containsObject: f])
		      {
			momentum -= [f doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			momentum -= gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: a])
		      {
			forcesY -= [a doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: a];
			forcesY -= gsl_vector_get (v, k);
		      }
		  }
		  break;
		  // General resultant
		case 273:
		  {
		    double ang;
		    NSString *a = [dataForces objectAtIndex: 1];
		    NSString *b = [dataForces objectAtIndex: 2];
		    
		    if (![vars containsObject: f])
		      {
			momentum -= [f doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			momentum -= gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: b])
		      {
			ang = [b doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: b];
			ang = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: a])
		      {
			forcesX -= [a doubleValue]*cos(M_PI*ang/180);
			forcesY -= [a doubleValue]*sin(M_PI*ang/180);
		      }
		    else
		      {
			int k = [vars indexOfObject: a];
			forcesX -= (gsl_vector_get (v, k))*cos(M_PI*ang/180);
			forcesY -= (gsl_vector_get (v, k))*sin(M_PI*ang/180);
		      }
		  }
		  break;
		  // Truss: horizontal beam
		case 289:
		case 290:
		  {
		    if (t == 289)
		      { signx = 1; }
		    else
		      { signx = - 1; }
		    
		    if (![vars containsObject: f])
		      {
			forcesVigaX += signx*[f doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			forcesVigaX += signx*(gsl_vector_get (v, k));
		      }
		  }
		  break;
		  // Truss: vertical beam
		case 291:
		case 292:
		  {
		    if (t == 291)
		      { signy = 1; }
		    else
		      { signy = - 1; }
		    
		    if (![vars containsObject: f])
		      {
			forcesVigaY += signy*[f doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			forcesVigaY += signy*(gsl_vector_get (v, k));
		      }
		  }
		  break;
		  // Truss: general beam
		case 285 ... 288:
		  {
		    double ang;
		    NSString *a = [dataForces objectAtIndex: 1];
                    
		    if ( (t == 285) || (t == 288) )
		      { signx = 1; }
		    else
		      { signx = - 1; }
		    
		    if ( (t == 285) || (t == 287) )
		      { signy = 1; }
		    else
		      { signy = - 1; }
		    
		    if (![vars containsObject: a])
		      {
			ang = [a doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: a];
			ang = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: f])
		      {
			forcesVigaX += signx*[f doubleValue]*cos(M_PI*ang/180);
			forcesVigaY += signy*[f doubleValue]*sin(M_PI*ang/180);
		      }
		    else
		      {
			int k = [vars indexOfObject: f];
			forcesVigaX += signx*(gsl_vector_get (v, k))*
			  cos(M_PI*ang/180);
			forcesVigaY += signy*(gsl_vector_get (v, k))*
			  sin(M_PI*ang/180);
		      }
		  }
		  break;
		}
	    }

	  // Equations for joint of truss
	  if ([typeElem intValue] == 294)
	    {
	      gsl_vector_set (func, nEqu, forcesVigaX);
	      gsl_vector_set (func, nEqu + 1, forcesVigaY);

	      nEqu += 2;
	    }
	}
      
      
      // Build the equations
      typeFS = [[solidForceTypes objectAtIndex: codeSolidForce] intValue];
      
      /* Do nothing for value 5, since Trusses not need equations, 
	 only its joints. */
      switch (typeFS)
	{
	case 1:
	  {
	    gsl_vector_set (func, nEqu, forcesX);
	    gsl_vector_set (func, nEqu + 1, forcesY);
	    gsl_vector_set (func, nEqu + 2, momentum);
	    
	    nEqu += 3;
	  }
	  break;
	case 2:
	case 3:
	case 4:
	  {
	    if (typeFS == 2)
	      {
		gsl_vector_set (func, nEqu, forcesX);
		nEqu += 1;
	      }
	    else if (typeFS == 3)
	      {
		gsl_vector_set (func, nEqu, forcesY);
		nEqu += 1;
	      }
	    
	    gsl_vector_set (func, nEqu, momentum);
	    
	    nEqu += 1;
	  }
	  break;
	}
      
      codeSolidForce += 1;
    }

  enumerator = [codOthers objectEnumerator];
  while ((object = [enumerator nextObject]))
    {
      NSNumber *type = [[dictionary objectForKey: object]
			 objectForKey: @"Type"];
      NSMutableArray *dat = [[dictionary objectForKey: object]
			      objectForKey: @"Values"];
      
      switch ([type intValue])
	{
	case 295:
	  {
	    int k;
	    double a1, a2;
	    
	    k = [vars indexOfObject: [dat objectAtIndex: 0]];
	    a1 = gsl_vector_get (v, k);
	    
	    k = [vars indexOfObject: [dat objectAtIndex: 1]];
	    a2 = gsl_vector_get (v, k);
	    
	    // Build the equation
	    gsl_vector_set (func, nEqu, sin(M_PI*a1/180) - cos(M_PI*a2/180));
	    
	    nEqu += 1;
	  }
	  break;
	}
    }
  
  return GSL_SUCCESS;
}

@interface FLStaticsRigidBodies (Private)
- (void) makeSystem;
@end

@implementation FLStaticsRigidBodies (Private)
- (void) makeSystem
{
  int increase = 1;
  double newValue;
  BOOL follow;
  
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;
  
  int state = 0, stateInt, k, length;
  int nvar = [vars count];
  const size_t n = nvar;
  double par;
  NSString *message;
  size_t iter;
  
  gsl_vector *x = gsl_vector_alloc (n);
  int countRes = 0;
  id anObj;
  NSMutableArray *results = [NSMutableArray array];
  NSEnumerator *varCount;
  id dataSign;
  NSNumber *typeOther;
  NSMutableArray *verifSign, *beamStatus;
  NSEnumerator *signObj;
  
  // Generator of random numbers
  const gsl_rng_type * Y;
  gsl_rng * r;
  gsl_rng_env_setup();
  Y = gsl_rng_default;
  r = gsl_rng_alloc (Y);
  
  do
    {
      gsl_multiroot_function f = {&buildSystem, n, (__bridge void *)(self)};
      iter = 0;
      for (k = 0; k < nvar; k++)
	{
	  if ( increase <= 30 )
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
      
      T = gsl_multiroot_fsolver_hybrids;
      s = gsl_multiroot_fsolver_alloc (T, nvar);
      gsl_multiroot_fsolver_set (s, &f, x);
      
      do
	{
	  iter++;
	  stateInt = gsl_multiroot_fsolver_iterate (s);
	  
	  if (stateInt)
	    break;
	  
	  state = gsl_multiroot_test_residual (s->f, 1e-7);
	}
      while (state == GSL_CONTINUE && iter < 1000);  
      
      // Check the status
      if ( (stateInt) && (increase < 60) )
	{
	  increase += 1;
	  follow = YES;
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
  varCount = [vars objectEnumerator];
  beamStatus = [NSMutableArray array];
  
  while ((anObj = [varCount nextObject]))
    {
      [results addObject: [NSNumber numberWithDouble:
				      gsl_vector_get (s->x, countRes)]];
      [beamStatus addObject: @""];
      countRes += 1;
    }
  
  // Check and correct the signs
  signObj = [dictionary objectEnumerator];
  
  int par1, w, g, sign;
  double nv, nf;
  while ((dataSign = [signObj nextObject]))
    {
      par1 = 0;
      w = 0;
      g = 0;
      sign = 1;
      nv = 0;
      nf = 0;

      typeOther = [dataSign objectForKey: @"Type"];
      
      switch ([typeOther intValue])
	{
	case 252:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify the angle of the beam
	    if ([vars containsObject: [verifSign objectAtIndex: 3]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 3]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv > 180 )
		  {
		    nv -= floor(nv/180)*180;
		  }
		
                if ( nv < 0 )
		  {
		    nv += (floor(-1*nv/180) + 1)*180;                
		  }
		
                [results replaceObjectAtIndex: w
				   withObject: [NSNumber numberWithDouble: nv]];
	      }
	  }
	  break;
	case 253:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify the angle of the solid
	    if ([vars containsObject: [verifSign objectAtIndex: 4]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 4]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv > 180 )
		  {
		    nv -= floor(nv/180)*180;
		  }
		
                if ( nv < 0 )
		  {
		    nv += (floor(-1*nv/180) + 1)*180;
		  }
		
                [results replaceObjectAtIndex: w
				   withObject: [NSNumber numberWithDouble: nv]];
	      }
	  }
	  break;
	case 257 ... 260:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify the angle of the force
	    if ( [vars containsObject: [verifSign objectAtIndex: 0]] &&
		 [vars containsObject: [verifSign objectAtIndex: 1]] )
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 0]];
		nf = [[results objectAtIndex: g] doubleValue];
		
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv > 360 )
		  {
		    nv -= floor(nv/360)*360;
		  }
		
                if ( nv < 0 )
		  {
		    nv += (floor(-1*nv/360) + 1)*360;                
		  }
		
		if ( (nv > 180) && (nv < 270) )
		  {
		    sign = -1;
		    nv -= 180;
		  }
		
		[results replaceObjectAtIndex: g
			   withObject: [NSNumber numberWithDouble: sign*nf]];
                [results replaceObjectAtIndex: w
			   withObject: [NSNumber numberWithDouble: nv]];
	      }
	    else if ([vars containsObject: [verifSign objectAtIndex: 1]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
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
	case 265 ... 268:
	  {
	    /* We check the angle of the friction force. Although this don't
	       have sense. If the user apply a friction force we expect that
	       the user provide the angle. */
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    if ([vars containsObject: [verifSign objectAtIndex: 2]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 2]];
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
	case 273:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify the sign of the resultant
	    if ( [vars containsObject: [verifSign objectAtIndex: 1]] &&
		 [vars containsObject: [verifSign objectAtIndex: 2]] )
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 1]];
		
		if ( [[results objectAtIndex: g] doubleValue] < 0 )
		  {
		    nf = -1*[[results objectAtIndex: g] doubleValue];
		    [results replaceObjectAtIndex: g
			       withObject: [NSNumber numberWithDouble: nf]];
		    par1 = 1;
		  }

		w = [vars indexOfObject: [verifSign objectAtIndex: 2]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( par1 == 1 )
		  {
		    nv += 180;
		  }
		
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
	    else if ([vars containsObject: [verifSign objectAtIndex: 2]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 2]];
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
	case 277 ... 280:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify oblique 2 forces beam
	    if ( [vars containsObject: [verifSign objectAtIndex: 0]] &&
		 [vars containsObject: [verifSign objectAtIndex: 1]] )
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 0]];
		nf = [[results objectAtIndex: g] doubleValue];
		
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv > 360 )
		  {
		    nv -= floor(nv/360)*360;
		  }
		
                if ( nv < 0 )
		  {
		    nv += (floor(-1*nv/360) + 1)*360;                
		  }
		
		if ( (nv > 180) && (nv < 270) )
		  {
		    sign = -1;
		    nv -= 180;
		  }
		
		[results replaceObjectAtIndex: g
			   withObject: [NSNumber numberWithDouble: sign*nf]];
                [results replaceObjectAtIndex: w
			   withObject: [NSNumber numberWithDouble: nv]];

		if (sign*nf > 0)
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
						  _(@"[compression] ")]];
		  }
		else
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
							  _(@"[tension] ")]];
		  }
	      }
	    else if ([vars containsObject: [verifSign objectAtIndex: 1]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
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
	    else if ([vars containsObject: [verifSign objectAtIndex: 0]])
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 0]];
		nf = [[results objectAtIndex: g] doubleValue];

		if (nf > 0)
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
						  _(@"[compression] ")]];
		  }
		else
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
							  _(@"[tension] ")]];
		  }
	      }
	  }
	  break;
	case 281 ... 284:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify horizontal and vertical 2 forces beam
	    if ([vars containsObject: [verifSign objectAtIndex: 0]])
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 0]];
		nf = [[results objectAtIndex: g] doubleValue];
		
		if (nf > 0)
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
						  _(@"[compression] ")]];
		  }
		else
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
							  _(@"[tension] ")]];
		  }
	      }
	  }
	  break;
	case 285 ... 288:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify oblique truss's beam
	    if ( [vars containsObject: [verifSign objectAtIndex: 0]] &&
		 [vars containsObject: [verifSign objectAtIndex: 1]] )
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 0]];
		nf = [[results objectAtIndex: g] doubleValue];
		
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv > 360 )
		  {
		    nv -= floor(nv/360)*360;
		  }
		
                if ( nv < 0 )
		  {
		    nv += (floor(-1*nv/360) + 1)*360;                
		  }
		
		if ( (nv > 180) && (nv < 270) )
		  {
		    sign = -1;
		    nv -= 180;
		  }
		
		[results replaceObjectAtIndex: g
			   withObject: [NSNumber numberWithDouble: sign*nf]];
                [results replaceObjectAtIndex: w
				   withObject: [NSNumber numberWithDouble: nv]];

		if (sign*nf > 0)
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
							  _(@"[tension] ")]];
		  }
		else
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
						  _(@"[compression] ")]];
		  }
	      }
	    else if ([vars containsObject: [verifSign objectAtIndex: 1]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
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
	    else if ([vars containsObject: [verifSign objectAtIndex: 0]])
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 0]];
		nf = [[results objectAtIndex: g] doubleValue];

		if (nf > 0)
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
							  _(@"[tension] ")]];
		  }
		else
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
						  _(@"[compression] ")]];
		  }
	      }
	  }
	  break;
	case 289 ... 292:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify horizontal and vertical truss's beam
	    if ([vars containsObject: [verifSign objectAtIndex: 0]])
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 0]];
		nf = [[results objectAtIndex: g] doubleValue];
		
		if (nf > 0)
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
							  _(@"[tension] ")]];
		  }
		else
		  {
		    [beamStatus replaceObjectAtIndex: g
				  withObject: [NSString stringWithString:
						  _(@"[compression] ")]];
		  }
	      }
	  }
	  break;
	}
    }
  
  // Print the results
  [self printUnknowns: vars withResults: results withStatus: beamStatus];
  
  // Print the calculus state
  message = [NSString stringWithFormat: [errors objectAtIndex: 5],
		      gsl_strerror (state)];
  length = [[[self viewer] textStorage] length];
  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: message];
  
  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  gsl_rng_free (r);
}
@end

@implementation FLStaticsRigidBodies

- (id) init
{
  NSBundle *messages;
  self = [super init];
  
  vars = [NSMutableArray new];
  codObjects = [NSMutableArray new];
  namesObjects = [NSMutableArray new];
  codSolids = [NSMutableArray new];
  namesSolids = [NSMutableArray new];
  typeElements = [NSMutableArray new];
  forceObjs = [NSMutableArray new];
  forceTypes = [NSMutableArray new];
  resultants = [NSMutableArray new];
  solidForceTypes = [NSMutableArray new];
  codResul = [NSMutableArray new];
  codOthers = [NSMutableArray new];
  dictionary = [NSMutableDictionary new];
  
  // Build array of messages
  messages = [NSBundle mainBundle];
  errors = [[NSArray alloc] initWithContentsOfFile:
			  [messages pathForResource: @"staticsRigidMessages"
					     ofType: @"plist"]];
  
  return self;
}

- (void) makeEquationsForData: (NSMutableDictionary *)list
{
  int width, height, totalCells; //For handle the chalkboard size
  int length, numEquations = 0, numForces = 0, errorNumber = 0;
  NSUInteger sys = [self system];
  BOOL error = NO;
  BOOL errorSolid = NO;
  BOOL errorName = NO;
  BOOL errorNameElem = NO;
  BOOL errorElement = NO;
  BOOL errorNum = NO;
  BOOL errorInSolid = NO;
  BOOL errorType = NO;
  BOOL errorForce = NO;
  BOOL errorResultant = NO;
  BOOL errorJoint = NO;
  BOOL errorAngles = NO;
  NSNumber *identifier, *code;

  NSMutableArray *codForces = [NSMutableArray array];
  NSMutableArray *codForcesObjs = [NSMutableArray array];
  NSArray *keys;
  NSEnumerator *enumerator;
  NSArray *objectsOrder = [self cells];
  // To verify the applied forces to objects
  int k; 
  NSButton *cell;
  
  varG = 0;
  gravityDat = 0;

  // For handle the chalkboard size
  width = [self chalkboardWidth];
  height = [self chalkboardHeight];
  totalCells = width*height - 1;
  
  [dictionary setDictionary: list];
  keys = [[NSArray alloc] initWithArray: [list allKeys]];
  enumerator = [keys objectEnumerator];
  
  
  while ((code = [enumerator nextObject]) && !error)
    {
      int x;
      NSString *key;
      NSArray *terms;
      NSNumber *number;

      NSArray *titles = [[dictionary objectForKey: code]
			  objectForKey: @"Titles"];
      NSMutableArray *info = [[dictionary objectForKey: code]
			       objectForKey: @"Data"];
      NSMutableArray *values = [[dictionary objectForKey: code]
				 objectForKey: @"Values"];

      // Search the applied forces in each object
      int pos;
      NSNumber *ident;
      NSEnumerator *search;
      
      identifier = [[dictionary objectForKey: code] objectForKey: @"Type"];
      [values removeAllObjects];
      
      // Count the number of variables
      for (x = 0; x < [info count]; x++)
	{
	  NSString *data = [info objectAtIndex: x];
	  NSString *title = [[titles objectAtIndex: x] description];
	  
	  if (![self isNumericDataTheString: data] &&
	      ![title isEqualToString: _(@"Name")] &&
	      ![title isEqualToString: _(@"Points")] &&
	      ![title isEqualToString: _(@"Beam")] &&
	      ![title isEqualToString: _(@"Solid")] &&
	      ![title isEqualToString: _(@"Truss")])
	    {
	      if ([self hasConversionTheString: data])
		{
		  terms = [[info objectAtIndex: x]
			    componentsSeparatedByString: @"@"];
		  key = [[terms objectAtIndex: 1] stringByTrimmingSpaces];
		  
		  if ([[[self conversions] allKeys] containsObject: key])
		    {
		      NSString *var = [[terms objectAtIndex: 0]
					stringByTrimmingSpaces];
		      
		      if ([self isNumericDataTheString: var])
                        {
			  number = [NSNumber numberWithDouble:
				       [var doubleValue]*
				       [[[self conversions] objectForKey: key]
					 doubleValue]];
			  [values addObject: [number stringValue]];
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
                      NSString *advert = [NSString stringWithFormat:
				     [errors objectAtIndex: 0], [key cString]];
                      error = YES;
		      errorNumber = 0;
		      
		      length = [[[self viewer] textStorage] length];
                      [[self viewer] replaceCharactersInRange:
				                   NSMakeRange(length, 0)
						   withString: advert];
		    }
		  
		}
	      else
		{
		  // Add a simple variable (without a conversion factor)
		  NSString *varFactor = nil;
		  
		  // Check the type of variable
		  if ([title hasPrefix: @"m"])
		    {
		      if (sys == 0)
			{ varFactor = @"kg"; }
		      else
			{ varFactor = @"slug"; }
		    }
		  else if ([title hasPrefix: @"f"] || [title hasPrefix: @"N"] ||
			   [title hasPrefix: @"t"])
		    {
		      if (sys == 0)
			{ varFactor = @"N"; }
		      else
			{ varFactor = @"lb"; }
		    }
		  else if ([title hasPrefix: @"ang"])
		    {
		      varFactor = [NSString stringWithString: _(@"degrees")];
		    }
		  else if ([title hasPrefix: @"g"])
		    {
		      if (sys == 0)
			{ varFactor = @"m/s2"; }
		      else
			{ varFactor = @"ft/s2"; }
		    }
		  else if ([title hasPrefix: @"x"] || [title hasPrefix: @"y"] ||
			   [title hasPrefix: @"lc"] || [title hasPrefix: @"l"])
		    {
		      if (sys == 0)
			{ varFactor = @"m"; }
		      else
			{ varFactor = @"ft"; }
		    }
		  else if ([title hasPrefix: @"M"])
		    {
		      if (sys == 0)
			{ varFactor = @"N*m"; }
		      else
			{ varFactor = @"lb*ft"; }
		    }
		  else
		    {
		      varFactor = @"ad";
		    }
		  
		  data = [data stringByAppendingString: @"@"];
		  data = [data stringByAppendingString: varFactor];
		  
		  [values addObject: data];
		  if (![vars containsObject: data])
		    {
		      [vars addObject: data];
		    }
		}
	    }
          else
	    {
	      [values addObject: data];
	    }
	}    
      // Here ends the count of variables
      
      if (error)
	break;
      
      // Verify the number of equations
      switch ([identifier intValue])
	{
	case 250:
	  {
	    if (![self isNumericDataTheString: [values objectAtIndex: 0]])
	      {
		gravityVar = [values objectAtIndex: 0];
		varG = 1;
	      }
	    else
	      {
		gravityDat = [[values objectAtIndex: 0] doubleValue];
		varG = 2;
	      }
	  }
	  break;
	case 251:
	  {
	    NSString *name = [values objectAtIndex: 0];
	    if (![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		[namesObjects addObject: name];
		[codObjects addObject: code];
	      }
	    else
	      {
		errorNameElem = YES;
		errorNumber = 7;
	      }
	  }    
	  break;
	case 252:
	case 253:
	case 276:
	case 293:
	  {
	    NSString *name = [values objectAtIndex: 0];

	    if ([identifier intValue] != 293)
	      {
		numEquations += 3;
	      }

	    if (![namesSolids containsObject: name] &&
		![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		[namesSolids addObject: name];
		[codSolids addObject: code];
	      }
	    else
	      {
		errorName = YES;
		errorNumber = 6;
	      } 
	  }
	  break;
	case 254:
	  {
	    numForces += 1;
	    [codForces addObject: code];
	  }
	  break;
	case 255:
	case 256:
	case 294:
	  {
	    NSString *name = [values objectAtIndex: 0];

	    if ([identifier intValue] == 294)
	      {
		numEquations += 2;
	      }

	    if (![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		[namesObjects addObject: name];
		[codObjects addObject: code];
	      }
	    else
	      {
		errorNameElem = YES;
		errorNumber = 7;
	      }
	  }
	  break;
	case 257 ... 272:
	  {
	    numForces += 1;
	    [codForces addObject: code];
	  }
	  break;
	case 273 ... 275:
	  {
	    numForces += 1;
	    [codForces addObject: code];
	  }
	  break;
	case 277 ... 292:
	  {
	    // Elements beam of 2 forces and beam of truss
	    numForces += 1;
	    [codForces addObject: code];
	  }
	  break;
	case 295:
	  {
	    // Relation of angles
	    if (![self isNumericDataTheString: [values objectAtIndex: 0]] &&
		![self isNumericDataTheString: [values objectAtIndex: 1]])
	      {
		numEquations += 1;
		[codOthers addObject: code];
	      }
	    else
	      {
		errorAngles = YES;
		errorNumber = 16;
	      }
	  }
	  break;
	}
      // Here ends the verify for equations
      
      if (errorName || errorNameElem || errorJoint || errorAngles)
	break;
      
      search = [objectsOrder objectEnumerator];
      
      if ( [identifier intValue] == 251 || [identifier intValue] == 255 ||
	   [identifier intValue] == 256 || [identifier intValue] == 294 )
	{
	  NSMutableArray *f = [NSMutableArray array];
	  // Search the applied forces to each object
	  int type, horizontalForces = 0, verticalForces = 0, couples = 0,
	    resul = 0;
	  NSNumber *fType;
	  NSEnumerator *verif;
	  k = 0;
	  pos = 0;
	  
	  while ((cell = [search nextObject]))
	    {
	      if ([cell tag] == [code intValue])
		{
		  pos = k;
		  break;
		}
	      k += 1;
	    }
	  
	  
	  if ( (pos%width != 0) && (pos%width != (width - 1)) )
	    {
	      if (pos - 1 >= 0)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos - 1] tag]];

		  if ([ident intValue] != 0)
		    { [f addObject: ident]; }
		}
	      
	      if (pos - (width - 1) >= 0)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - (width - 1)] tag]];

		  if ([ident intValue] != 0)
		    { [f addObject: ident]; }
		}
	      
	      if (pos - width >= 0)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos - width] tag]];

		  if ([ident intValue] != 0)
		    { [f addObject: ident]; }
		}
	      
	      if (pos - (width + 1) >= 0)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - (width + 1)] tag]];

		  if ([ident intValue] != 0)
		    { [f addObject: ident]; }
		}
	      
	      if (pos + 1 <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos + 1] tag]];

		  if ([ident intValue] != 0)
		    { [f addObject: ident]; }
		}
	      
	      if (pos + (width - 1) <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + (width - 1)] tag]];

		  if ([ident intValue] != 0)
		    { [f addObject: ident]; }
		}
	      
	      if (pos + width <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos + width] tag]];

		  if ([ident intValue] != 0)
		    { [f addObject: ident]; }
		}
	      
	      if (pos + (width + 1) <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + (width + 1)] tag]];

		  if ([ident intValue] != 0)
		    { [f addObject: ident]; }
		}
	    }
	  else
	    {
	      if (pos%width == 0)
		{
		  if (pos - (width - 1) >= 0)
		    {
		      ident = [NSNumber numberWithInt:
     		        [[objectsOrder objectAtIndex: pos - (width - 1)] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		  
		  if (pos - width >= 0)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos - width] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		  
		  if (pos + 1 <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
				  [[objectsOrder objectAtIndex: pos + 1] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		  
		  if (pos + width <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos + width] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		  
		  if (pos + (width + 1) <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			 [[objectsOrder objectAtIndex: pos + (width + 1)] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		}
	      else
		{
		  if (pos - 1 >= 0)
		    {
		      ident = [NSNumber numberWithInt:
				  [[objectsOrder objectAtIndex: pos - 1] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		  
		  if (pos - width >= 0)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos - width] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		  
		  if (pos - (width + 1) >= 0)
		    {
		      ident = [NSNumber numberWithInt:
  		        [[objectsOrder objectAtIndex: pos - (width + 1)] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		  
		  if (pos + (width - 1) <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			 [[objectsOrder objectAtIndex: pos + (width - 1)] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		  
		  if (pos + width <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos + width] tag]];

		      if ([ident intValue] != 0)
			{ [f addObject: ident]; }
		    }
		}
	    }
	  
	  
	  // Verify the applied forces
	  if ( ([identifier intValue] == 251 || [identifier intValue] == 255 ||
		[identifier intValue] == 256) && [f count] > 0 )
	    {
	      verif = [f objectEnumerator];
              while ((fType = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: fType]
			     objectForKey: @"Type"] intValue];

		  if ( (t == 261) || (t == 262) || (t == 269) || (t == 270) ||
		       (t == 274) || (t == 281) || (t == 282) )
		    {
		      horizontalForces += 1;
		    }
		  
		  if ( (t == 263) || (t == 264) || (t == 271) || (t == 272) ||
		       (t == 275) || (t == 283) || (t == 284) )
		    {
		      verticalForces += 1;
		    }
		  
		  if (t == 254)
		    {
		      couples += 1;
		    }
		  
		  if ( (t >= 273) && (t <= 275) )
		    {
		      resul += 1;
		    }
		}
	      
	      /* We classify the elements according with the kind 
		 of forces applied: 
		 1) Couples and general forces.
		 2) Couples and horizontal forces.
		 3) Couples and vertical forces.
		 4) Only couples. 
		 Even if there are only horizontal or vertical forces. 
		 We asume that there are couples since we assume that 
		 not all are applied along the same line.*/
              if ([f count] == couples)
		{
		  type = 4;
		}
              else
		{
                  if ([f count] == (verticalForces + couples))
		    {
		      type = 3;
		    }
                  else
		    {
		      if ([f count] == (horizontalForces + couples))
			{
			  type = 2;
			}
		      else
			{
			  type = 1;
			}
		    }
		}
	      
	      [forceTypes addObject: [NSNumber numberWithInt: type]];
	      [resultants addObject: [NSNumber numberWithInt: resul]];
	    }

	  // Verify the type of forces and beams applied to each joint
	  if ( [identifier intValue] == 294 && [f count] > 0 )
	    {
	      verif = [f objectEnumerator];
              while ((fType = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: fType]
			     objectForKey: @"Type"] intValue];

		  if ( !(t >= 285 && t <= 292) )
		    {
		      if ( !(t >= 257 && t <= 264) )
			{
			  errorJoint = YES;
			  errorNumber = 14;
			  break;
			}
		    }

		  if ( (t == 261) || (t == 262) || (t == 289) || (t == 290) )
		    {
		      horizontalForces += 1;
		    }

		  if ( (t == 263) || (t == 264) || (t == 291) || (t == 292) )
		    {
		      verticalForces += 1;
		    }
		}

	      /* We don't allow all forces are applied along one axis, since we 
		 require 2 equations. This is the expected in a Truss's
		 joint. */
	      if (horizontalForces == [f count] || verticalForces == [f count])
		{
		  errorJoint = YES;
		  errorNumber = 15;
		}

	      /* Even when each joint have only two equations. We add here (1) 
		 just to be consistent with the size of arrays, but this value 
		 is not used. The equations for each joint are write 
		 automatically, since this is we expect.*/
	      [forceTypes addObject: [NSNumber numberWithInt: 1]];
	      [resultants addObject: [NSNumber numberWithInt: 0]];
	    }
	  
	  
	  if ([f count] > 0)
	    {
	      [forceObjs addObject: f];
	    }
	}
    }
  
  
  if (!error && !errorName && !errorNameElem && !errorJoint && !errorAngles)
    {
      // Check if there is defined solids
      if ([codSolids count] == 0)
	{
	  errorSolid = YES;
	  errorNumber = 8;
	}
      else
	{
	  // Check the elements in each solid
	  int j, q, amount;
	  id checkName;
	  NSEnumerator *checkNum;
	  NSEnumerator *checkElemt = [namesObjects objectEnumerator];
	  NSMutableArray *countElemt = [NSMutableArray array];
	  
	  for (j = 0; j < [namesSolids count]; j++)
	    {
	      [countElemt addObject: [NSNumber numberWithInt: 0]];
	    }
	  
	  while ((checkName = [checkElemt nextObject]))
	    {
	      if ([namesSolids containsObject: [checkName description]])
		{
		  q = [namesSolids indexOfObject: [checkName description]];
		  amount = [[countElemt objectAtIndex: q] intValue] + 1;

		  [countElemt replaceObjectAtIndex: q
				withObject: [NSNumber numberWithInt: amount]];
		}
	      else
		{
		  errorElement = YES;
		  errorNumber = 9;
		}
	    }
	  
	  if (!errorElement)
	    {
	      // Check if solids have at least two elements
	      int typeSolidNum;
	      NSNumber *elementSolid;
	      checkNum = [countElemt objectEnumerator];
	      j = 0;
	      while ((elementSolid = [checkNum nextObject]))
		{
		  typeSolidNum = 
		    [[[dictionary objectForKey: [codSolids objectAtIndex: j]]
		       objectForKey: @"Type"] intValue];
		  
		  if (typeSolidNum == 276 && [elementSolid intValue] <= 1)
		    {
		      errorNum = YES;
		      errorNumber = 10;
		    }
		  
		  if ((typeSolidNum == 252 || typeSolidNum == 253) &&
		      [elementSolid intValue] == 0)
		    {
		      errorNum = YES;
		      errorNumber = 10;
		    }

		  if (typeSolidNum == 293 && [elementSolid intValue] <= 2)
		    {
		      errorNum = YES;
		      errorNumber = 10;
		    }
		  
		  j++;
		}
	    }
	      
	  if (!errorElement && !errorNum)
	    {
	      id typeElemt;
	      NSNumber *typeSolidElem;
	      NSEnumerator *verifType;
		  
	      // Check if all elements in solid are of the same type
	      for (j = 0; j < [namesSolids count]; j++)
		{
		  [typeElements addObject: [NSMutableArray array]];
		}
		  
	      j = 0;
	      checkNum = [namesObjects objectEnumerator];
	      while ((checkName = [checkNum nextObject]))
		{
		  q = [namesSolids indexOfObject: [checkName description]];
		  [[typeElements objectAtIndex: q] addObject:
				   [codObjects objectAtIndex: j]];
		  j++;
		}
		  
	      checkNum = [typeElements objectEnumerator];
	      while ((checkName = [checkNum nextObject]))
		{
		  j = 0;
		  verifType = [checkName objectEnumerator];
		  q = [[[dictionary objectForKey: [checkName objectAtIndex: 0]]
			 objectForKey: @"Type"] intValue];
		      
		  while ((typeElemt = [verifType nextObject]))
		    {
		      typeSolidElem = [[dictionary objectForKey:
					     typeElemt] objectForKey: @"Type"];
			  
		      if (q == [typeSolidElem intValue])
			{
			  j++;
			}
		    }
		      
		  if (j != [checkName count])
		    {
		      errorInSolid = YES;
		      errorNumber = 11;
		      break;
		    }
		}
	    }
		  
	  if (!errorElement && !errorNum && !errorInSolid)
	    {
	      NSNumber *cType, *typeSold, *typeElem;
	      // Check if the type of elements is valid
	      checkNum = [codSolids objectEnumerator];
	      j = 0;
	      
	      while ((cType = [checkNum nextObject]))
		{
		  typeSold = [[dictionary objectForKey: cType]
			       objectForKey: @"Type"];
		  typeElem = [[dictionary objectForKey:
				 [[typeElements objectAtIndex: j]
				 objectAtIndex: 0]] objectForKey: @"Type"];
			  
		  if ([typeSold intValue] == 252 && [typeElem intValue] != 255)
		    {
		      errorType = YES;
		      errorNumber = 12;
		      break;
		    }
			  
		  if ([typeSold intValue] == 253 && [typeElem intValue] != 256)
		    {
		      errorType = YES;
		      errorNumber = 12;
		      break;
		    }
			  
		  if ([typeSold intValue] == 276 && [typeElem intValue] != 251)
		    {
		      errorType = YES;
		      errorNumber = 12;
		      break;
		    }

		  if ([typeSold intValue] == 293 && [typeElem intValue] != 294)
		    {
		      errorType = YES;
		      errorNumber = 12;
		      break;
		    }
		  
		  j++;
		}
	    }
		      
	  if (!errorElement && !errorNum && !errorInSolid && !errorType)
	    {
	      // Verify if all elements have applied forces
	      if ([codObjects count] != [forceObjs count])
		{
		  errorForce = YES;
		  errorNumber = 4;
		}
	      else
		{
		  /* Verified that there is no more than one result applied to
		     the solid and if all forces are horizontal or vertical */
		  int numResults, elementsH, elementsV, elementsPair,
		    typeF, typeSolid;
		  NSNumber *element, *typeFS = nil, *codObjRes;
		  NSMutableArray *elementsSolid;
		  NSEnumerator *checkResul, *checkSolidElem;
		  j = 0;
			      
		  checkResul = [typeElements objectEnumerator];
		  while ((elementsSolid = [checkResul nextObject]))
		    {
		      numResults = 0;
		      elementsH = 0;
		      elementsV = 0;
		      elementsPair = 0;
		      codObjRes = nil;

		      typeSolid =
		       [[[dictionary objectForKey: [codSolids objectAtIndex: j]]
			  objectForKey: @"Type"] intValue];
				  
		      checkSolidElem = [elementsSolid objectEnumerator];
		      while ((element = [checkSolidElem nextObject]))
			{
			  q = [codObjects indexOfObject: element];
			  numResults += [[resultants objectAtIndex: q]
					  intValue];
			  typeF = [[forceTypes objectAtIndex: q] intValue];
			  
			  if (typeF == 2)
			    {
			      elementsH++; 
			    }
			  
			  if (typeF == 3)
			    {
			      elementsV++;
			    }
			  
			  if (typeF == 4)
			    {
			      elementsPair++;
			    }
				      
			  /* Save the code of the element with applied
			     resultant, if any. */
			  if ([[resultants objectAtIndex: q] intValue] == 1)
			    {
			      codObjRes = element; 
			    }
			}
	  
		      if (numResults >= 2)
			{
			  errorResultant = YES;
			  errorNumber = 13;
			  break;
			}
		      else
			{
			  /* Add the code of the element with applied
			     resultant to codResul, if any. */
			  if (codObjRes != nil)
			    {
			      [codResul addObject: codObjRes];
			    }
			  else
			    {
			      [codResul addObject: [NSNull null]];
			    }
			}
		      

		      if (elementsPair == [elementsSolid count] &&
			  typeSolid == 276 )
			{
			  /* If only couples are applied to the solid. Then 
			     we have one equation, since a solid made with 
			     points don't have weight. */
			  numEquations -= 2;
			  typeFS = [NSNumber numberWithInt: 4];
			}
		      else if ((elementsH + elementsPair) ==
			       [elementsSolid count])
			{
			  /* Even if "elementsPair" is zero, we expect a
			     moment equation, since we expect the forces
			     aren't applied along a line. */
			  if (typeSolid == 276)
			    {
			      numEquations--;
			      typeFS = [NSNumber numberWithInt: 2];
			    }
			  else
			    {
			      /* We need 3 equation if is a Beam or a Solid.
				 Since there is a weight and we need vertical
				 forces. 2 equations are only allowed in Points
				 since there is no weight. See above. */
			      typeFS = [NSNumber numberWithInt: 1];
			    }
			}
		      else if ((elementsV + elementsPair) ==
			       [elementsSolid count])
			{
			  /* Even if "elementsPairs" is zero, we expect a
			     moment equation, since we expect the forces aren't
			     applied along a line. */
			  numEquations--;
			  typeFS = [NSNumber numberWithInt: 3];
			}
		      else
			{
			  if (typeSolid == 293)
			    {
			      /* The value 5 is for not write equations. 
				 A Truss not need equations, only its 
				 joints. */
			      typeFS = [NSNumber numberWithInt: 5];
			    }
			  else
			    {
			      typeFS = [NSNumber numberWithInt: 1];
			    }
			}
		      
		      [solidForceTypes addObject: typeFS];
		      j++;
		    }
		}
	    }
	}
    }
  
  // Check if there are errors
  if (!error && !errorForce && !errorName && !errorNameElem && !errorSolid &&
      !errorElement && !errorNum && !errorInSolid && !errorType &&
      !errorResultant && !errorJoint && !errorAngles)
    {
      // Count the number of applied forces
      int v;
      int w = 0;
      NSEnumerator *en;
      NSNumber *fn;
      for (v = 0; v < [codObjects count]; v++)
	{
	  [codForcesObjs addObjectsFromArray: [forceObjs objectAtIndex: v]];  
	}
      
      // Check if all forces are applied
      en = [codForces objectEnumerator];
      while ((fn = [en nextObject]))
	{
	  if ([codForcesObjs containsObject: fn])
	    {
	      w += 1;
	    }
	}
      
      // Final verifications
      if ([vars count] == numEquations)
	{
	  if (w == [codForces count] && w == [codForcesObjs count])
	    {
	      // All OK, build and solve the system
	      [self makeSystem];
	    }
	  else
	    {
	      NSString *advert = nil;
	      
	      if (w < [codForces count])
		{
		  advert = [NSString stringWithString:
				       [errors objectAtIndex: 1]];
		}
	      
	      if (w < [codForcesObjs count])
		{
		  advert = [NSString stringWithString:
				       [errors objectAtIndex: 2]];
		}
	      
	      length = [ [[self viewer] textStorage] length];
	      [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
					   withString: advert];
	    }
	}
      else
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange:NSMakeRange (length, 0)
				       withString: [errors objectAtIndex: 3]];
	}
    }
  else
    {
      if (!error)
	{      
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: [errors objectAtIndex: errorNumber]]; 
	}
    }
  
  [keys release];
}

- (void) dealloc
{
  [dictionary release];
  [vars release];
  [namesObjects release];
  [codObjects release];
  [namesSolids release];
  [codSolids release];
  [typeElements release];
  [forceObjs release];
  [forceTypes release];
  [resultants release];
  [solidForceTypes release];
  [codResul release];
  [codOthers release];
  [errors release];
  [super dealloc];
}

@end
