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
#import "FLStatics.h"


static int buildSystem (const gsl_vector *v, void *p, gsl_vector *func)
{
  int gIndex, forceCode = 0;
  int nEqu = 0;
  double gf;
  NSNumber *object;
  NSEnumerator *enumerator;

  // Get properties of FL object.
  FLStatics *FLObj = (__bridge FLStatics *)(p);

  NSInteger varG = FLObj->varG;
  NSString *gravityVar = FLObj->gravityVar;
  double gravityDat = FLObj->gravityDat;
  NSMutableArray *vars = FLObj->vars;
  NSMutableArray *forceObjs = FLObj->forceObjs;
  NSMutableArray *forceTypes = FLObj->forceTypes;
  NSMutableArray *codObjects = FLObj->codObjects;
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
  
  enumerator = [codObjects objectEnumerator];
  
  while ((object = [enumerator nextObject]))
    {
      NSNumber *type = [[dictionary objectForKey: object]
			 objectForKey: @"Type"];
      NSMutableArray *dat = [[dictionary objectForKey: object]
			      objectForKey: @"Values"];
      
      switch ([type intValue])
	{
	case 51:
	  {
	    double weigth, mass;
            
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		mass = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		mass = gsl_vector_get (v, k);
	      }
	    
	    weigth = mass*gf;
            
	    if ([[forceTypes objectAtIndex: forceCode] intValue] == 3)
	      {
		double sign; 
		double force = 0;
		NSString *obj = nil;
		NSNumber *codForceObj;
		NSEnumerator *forceObj;
		force = -1*weigth;
		forceObj = [[forceObjs objectAtIndex: forceCode]
			     objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    if ( t == 62 )
		      { sign = 1; }
		    else
		      { sign = - 1; }   
		    
		    if (![vars containsObject: obj])
		      {
			force += sign*[obj doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: obj];
			force += sign*(gsl_vector_get (v, k));
		      }
		  }
		
		// Build the equation
		gsl_vector_set (func, nEqu, force);
		
		nEqu += 1;
	      }
	    else
	      {
		double signx, signy; 
		double forcex = 0;
		double forcey = 0;
		NSString *obj = nil;
		NSNumber *codForceObj;
		NSEnumerator *forceObj;
		forcey = -1*weigth;
		forceObj = [[forceObjs objectAtIndex: forceCode]
			     objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    switch (t)
		      {
		      case 60:
		      case 61:
			{
			  if (t == 60)
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      forcex += signx*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex += signx*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 62:
		      case 63:
			{
			  if (t == 62)
			    { signy = 1; }
			  else
			    { signy = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      forcey += signy*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcey += signy*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 56 ... 59:
			{
			  double ang;
			  NSString *a;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
                          
			  if ( (t == 56) || (t == 59) )
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if ( (t == 56) || (t == 58) )
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
			  
			  if (![vars containsObject: obj])
			    {
			      forcex += signx*[obj doubleValue]*
				cos(M_PI*ang/180);
			      forcey += signy*[obj doubleValue]*
				sin(M_PI*ang/180);
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex += signx*(gsl_vector_get (v, k))*
				cos(M_PI*ang/180);
			      forcey += signy*(gsl_vector_get (v, k))*
				sin(M_PI*ang/180);
			    }
			}
			break;
		      case 68:
		      case 69:
			{
			  double u;
			  NSString *a;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
			  
			  if (t == 68)
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
			  
			  if (![vars containsObject: obj])
			    {
			      forcex += signx*u*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex += signx*u*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 70:
		      case 71:
			{
			  double u;
			  NSString *a;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];

			  if (t == 70)
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
			  
			  if (![vars containsObject: obj])
			    {
			      forcey += signy*u*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcey += signy*u*(gsl_vector_get (v, k));
			    }
			}
			break;
		      }
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, forcex);
		gsl_vector_set (func, nEqu + 1, forcey);
		
		nEqu += 2;
	      }
	  }
	  break;
	case 52:
	  {
	    double signx = 0, signy = 0; 
	    double forcex = 0;
	    double forcey = 0;
	    double mass, ang;
	    NSString *obj = nil;
	    NSNumber *codForceObj;
	    NSEnumerator *forceObj;
            
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		ang = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		ang = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		mass = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		mass = gsl_vector_get (v, k);
	      }
	    
	    forcex = -1*mass*sin(M_PI*ang/180)*gf;
	    forcey = -1*mass*cos(M_PI*ang/180)*gf;
	    forceObj = [[forceObjs objectAtIndex: forceCode] objectEnumerator];
            
	    while ((codForceObj = [forceObj nextObject]))
	      {
		int t = [[[dictionary objectForKey: codForceObj]
			   objectForKey: @"Type"] intValue];
		obj = [[[dictionary objectForKey: codForceObj]
			 objectForKey: @"Values"] objectAtIndex: 0];
                
		switch (t)
		  {
		  case 60:
		  case 61:
		    {
		      if (t == 60)
			{ signx = 1;
			  signy = - 1; }
		      else
			{ signx = - 1; 
			  signy = 1; }
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*[obj doubleValue]*cos(M_PI*ang/180);
			  forcey += signy*[obj doubleValue]*sin(M_PI*ang/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*(gsl_vector_get (v, k))*
			    cos(M_PI*ang/180);
			  forcey += signy*(gsl_vector_get (v, k))*
			    sin(M_PI*ang/180);
			}
		    }
		    break;
		  case 62:
		  case 63:
		    {
		      if (t == 62)
			{ signx = 1;
			  signy = 1; }
		      else
			{ signx = - 1;
			  signy = - 1; }
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*[obj doubleValue]*sin(M_PI*ang/180);
			  forcey += signy*[obj doubleValue]*cos(M_PI*ang/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*(gsl_vector_get (v, k))*
			    sin(M_PI*ang/180);
			  forcey += signy*(gsl_vector_get (v, k))*
			    cos(M_PI*ang/180);
			}
		    }
		    break;
		  case 56 ... 59:
		    {
		      double angf = 0, angr = 0;
		      NSString *a;
		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];
                      
		      if (![vars containsObject: a])
			{
			  angf = [a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  angf = gsl_vector_get (v, k);
			}
		      
		      if (t == 56)
			{
			  signx = 1;
			  signy = 1; 
			  angr = angf - ang;
			}
		      
		      if (t == 57)
			{
			  signx = - 1;
			  signy = 1;
			  angr = ang - angf;
			}
		      
		      if (t == 58)
			{
			  signy = 1;
			  signx = -1;
			  angr = angf + ang;
			}
		      
		      if (t == 59)
			{
			  signy = - 1;
			  signx = 1; 
			  angr = angf + ang;
			}
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*[obj doubleValue]*cos(M_PI*angr/180);
			  forcey += signy*[obj doubleValue]*sin(M_PI*angr/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*(gsl_vector_get (v, k))*
			    cos(M_PI*angr/180);
			  forcey += signy*(gsl_vector_get (v, k))*
			    sin(M_PI*angr/180);
			}
		    }
		    break;
		  case 64:
		  case 65:
		    {
		      double u;
		      NSString *a;
		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];

		      if (t == 64)
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
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*u*[obj doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*u*(gsl_vector_get (v, k));
			}
		    }
		    break;
		  }
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, forcex);
	    gsl_vector_set (func, nEqu + 1, forcey);
	    
	    nEqu += 2;
	  }
	  break;
	case 53:
	  {
	    double signx = 0, signy = 0; 
	    double forcex = 0;
	    double forcey = 0;
	    double mass, ang;
	    NSString *obj = nil;
	    NSNumber *codForceObj;
	    NSEnumerator *forceObj;
            
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		ang = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		ang = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		mass = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		mass = gsl_vector_get (v, k);
	      }
	    
	    forcex = -1*mass*sin(M_PI*ang/180)*gf;
	    forcey = -1*mass*cos(M_PI*ang/180)*gf;
	    forceObj = [[forceObjs objectAtIndex: forceCode] objectEnumerator];
            
	    while ((codForceObj = [forceObj nextObject]))
	      {
		int t = [[[dictionary objectForKey: codForceObj]
			   objectForKey: @"Type"] intValue];
		obj = [[[dictionary objectForKey: codForceObj]
			 objectForKey: @"Values"] objectAtIndex: 0];
                
		switch (t)
		  {
		  case 60:
		  case 61:
		    {
		      if (t == 61)
			{ signx = 1;
			  signy = - 1; }
		      else
			{ signx = - 1; 
			  signy = 1; }
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*[obj doubleValue]*cos(M_PI*ang/180);
			  forcey += signy*[obj doubleValue]*sin(M_PI*ang/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*(gsl_vector_get (v, k))*
			    cos(M_PI*ang/180);
			  forcey += signy*(gsl_vector_get (v, k))*
			    sin(M_PI*ang/180);
			}
		    }
		    break;
		  case 62:
		  case 63:
		    {
		      if (t == 62)
			{ signx = 1;
			  signy = 1; }
		      else
			{ signx = - 1;
			  signy = - 1; }
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*[obj doubleValue]*sin(M_PI*ang/180);
			  forcey += signy*[obj doubleValue]*cos(M_PI*ang/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*(gsl_vector_get (v, k))*
			    sin(M_PI*ang/180);
			  forcey += signy*(gsl_vector_get (v, k))*
			    cos(M_PI*ang/180);
			}
		    }
		    break;
		  case 56 ... 59:
		    {
		      double angf = 0, angr = 0;
		      NSString *a;
		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];
                      
		      if (![vars containsObject: a])
			{
			  angf = [a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  angf = gsl_vector_get (v, k);
			}
		      
		      if (t == 56)
			{
			  signy = 1;
			  signx = -1;
			  angr = ang + angf;
			}
		      
		      if (t == 57)
			{
			  signy = - 1;
			  signx = 1;
			  angr = angf + ang;
			}
		      
		      if (t == 58)
			{
			  signx = 1;
			  signy = 1;
			  angr = angf - ang;
			}
		      
		      if (t == 59)
			{
			  signx = - 1;
			  signy = 1;
			  angr = ang - angf;
			}
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*[obj doubleValue]*cos(M_PI*angr/180);
			  forcey += signy*[obj doubleValue]*sin(M_PI*angr/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*(gsl_vector_get (v, k))*
			    cos(M_PI*angr/180);
			  forcey += signy*(gsl_vector_get (v, k))*
			    sin(M_PI*angr/180);
			}
		    }
		    break;
		  case 66:
		  case 67:
		    {
		      double u;
		      NSString *a;
		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];

		      if (t == 66)
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
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*u*[obj doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*u*(gsl_vector_get (v, k));
			}
		    }
		    break;
		  }
	      }
            
	    // Build the equations
	    gsl_vector_set (func, nEqu, forcex);
	    gsl_vector_set (func, nEqu + 1, forcey);
	    
	    nEqu += 2;
	  }
	  break;
	case 54:
	  {
	    
	    if ( [[forceTypes objectAtIndex: forceCode] intValue] == 2 ||
		 [[forceTypes objectAtIndex: forceCode] intValue] == 3 )
	      {
		double sign;
		double force = 0;
		NSString *obj = nil;
		NSNumber *codForceObj;
		NSEnumerator *forceObj = [[forceObjs objectAtIndex: forceCode]
					   objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    if ( (t == 60) || (t == 62) )
		      { sign = 1; }
		    else
		      { sign = - 1; }   
		    
		    if (![vars containsObject: obj])
		      {
			force += sign*[obj doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: obj];
			force += sign*(gsl_vector_get (v, k));
		      }
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, force);
		
		nEqu += 1;
	      }
	    else
	      {
		double signx, signy;  
		double forcex = 0;
		double forcey = 0;
		NSString *obj = nil;
		NSNumber *codForceObj;
		NSEnumerator *forceObj = [[forceObjs objectAtIndex: forceCode]
					   objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    switch (t)
		      {
		      case 60:
		      case 61:
			{
			  if (t == 60)
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      forcex += signx*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex += signx*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 62:
		      case 63:
			{
			  if (t == 62)
			    { signy = 1; }
			  else
			    { signy = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      forcey += signy*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcey += signy*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 56 ... 59:
			{
			  double ang;
			  NSString *a;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
                          
			  if ( (t == 56) || (t == 59) )
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if ( (t == 56) || (t == 58) )
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
			  
			  if (![vars containsObject: obj])
			    {
			      forcex += signx*[obj doubleValue]*
				cos(M_PI*ang/180);
			      forcey += signy*[obj doubleValue]*
				sin(M_PI*ang/180);
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex += signx*(gsl_vector_get (v, k))*
				cos(M_PI*ang/180);
			      forcey += signy*(gsl_vector_get (v, k))*
				sin(M_PI*ang/180);
			    }
			}
			break;
		      }
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, forcex);
		gsl_vector_set (func, nEqu + 1, forcey);
		
		nEqu += 2;
	      }
	  }
	  break;
	case 55:
	  {
	    if ( [[forceTypes objectAtIndex: forceCode] intValue] == 2 ||
		 [[forceTypes objectAtIndex: forceCode] intValue] == 3 )
	      {
		double sign = 0;
		double force = 0;
		NSString *obj = nil;
		NSNumber *codForceObj;
		NSEnumerator *forceObj = [[forceObjs objectAtIndex: forceCode]
					   objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    switch (t)
		      {
		      case 60:
		      case 61:
		      case 62:
		      case 63:
			{   
			  if ( (t == 60) || (t == 62) )
			    { sign = 1; }
			  else
			    { sign = - 1; }   
			  
			  if (![vars containsObject: obj])
			    {
			      force += sign*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      force += sign*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 73:
		      case 74:
			{
			  if (![vars containsObject: obj])
			    {
			      force -= sign*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      force -= sign*(gsl_vector_get (v, k));
			    }
			}
			break;
		      }
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, force);
		
		nEqu += 1;
	      }
	    else
	      {
		double signx, signy; 
		double forcex = 0;
		double forcey = 0;
		NSString *obj = nil;
		NSNumber *codForceObj;
		NSEnumerator *forceObj = [[forceObjs objectAtIndex: forceCode]
					   objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    switch (t)
		      {
		      case 60:
		      case 61:
			{
			  if (t == 60)
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      forcex += signx*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex += signx*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 62:
		      case 63:
			{
			  if (t == 62)
			    { signy = 1; }
			  else
			    { signy = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      forcey += signy*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcey += signy*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 56 ... 59:
			{
			  double ang;
			  NSString *a;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
                          
			  if ( (t == 56) || (t == 59) )
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if ( (t == 56) || (t == 58) )
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
			  
			  if (![vars containsObject: obj])
			    {
			      forcex += signx*[obj doubleValue]*
				cos(M_PI*ang/180);
			      forcey += signy*[obj doubleValue]*
				sin(M_PI*ang/180);
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex += signx*(gsl_vector_get (v, k))*
				cos(M_PI*ang/180);
			      forcey += signy*(gsl_vector_get (v, k))*
				sin(M_PI*ang/180);
			    }
			}
			break;
		      case 72:
			{
			  double ang;
			  NSString *a;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
                          
			  if (![vars containsObject: a])
			    {
			      ang = [a doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: a];
			      ang = gsl_vector_get (v, k);
			    }
			  
			  if (![vars containsObject: obj])
			    {
			      forcex -= [obj doubleValue]*cos(M_PI*ang/180);
			      forcey -= [obj doubleValue]*sin(M_PI*ang/180);
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex -= (gsl_vector_get (v, k))*
				cos(M_PI*ang/180);
			      forcey -= (gsl_vector_get (v, k))*
				sin(M_PI*ang/180);
			    }
			}
			break;
		      }
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, forcex);
		gsl_vector_set (func, nEqu + 1, forcey);
		
		nEqu += 2;
	      }
	  }
	  break;
	case 75 ... 78:
	  {
	    int pForce, t;
	    double sign = 0; 
	    double kr, d, force = 0;
	    NSString *obj = nil;
	    NSNumber *codForceObj = [[forceObjs objectAtIndex: forceCode]
				      objectAtIndex: 0];
	    
	    if ([[forceTypes objectAtIndex: forceCode] intValue] == 4)
	      {
		pForce = 1;
	      }
	    else
	      {
		pForce = -1;
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		kr = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		kr = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		d = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		d = gsl_vector_get (v, k);
	      }
	    
	    t = [[[dictionary objectForKey: codForceObj]
		   objectForKey: @"Type"] intValue];
	    obj = [[[dictionary objectForKey: codForceObj]
		     objectForKey: @"Values"] objectAtIndex: 0];
            
	    switch (t)
	      {
	      case 56:
	      case 58:
	      case 61:
	      case 62:
		{
		  sign = 1;
		}
		break;
	      case 57:
	      case 59:
	      case 60:
	      case 63:
		{
		  sign = -1;
		}
		break;
	      }
	    
	    if (![vars containsObject: obj])
	      {
		force = sign*pForce*[obj doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: obj];
		force = sign*pForce*(gsl_vector_get (v, k));
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, kr*d - force);
	    
	    nEqu += 1;
	  }
	  break;                   
	}

      forceCode += 1;
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
	case 79:
	  {
	    int k;
	    double a1, a2;

	    k = [vars indexOfObject: [dat objectAtIndex: 0]];
	    a1 = gsl_vector_get (v, k);
	    
	    k = [vars indexOfObject: [dat objectAtIndex: 1]];
	    a2 = gsl_vector_get (v, k);
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, sin(M_PI*a1/180) - cos(M_PI*a2/180));
	    
	    nEqu += 1;
	  }
	  break;
	}
    }

  return GSL_SUCCESS;
}

@interface FLStatics (Private)
- (void) makeSystem;
@end

@implementation FLStatics (Private)
- (void) makeSystem
{
  int increase = 1;
  double newValue;
  BOOL follow;
  
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;
  
  int state = 0, stateInt, k, length;
  int nvar  = [vars count];
  double par;
  NSString *message;
  size_t iter;
  
  const size_t n = nvar;
  
  int countRes = 0;
  id anObj;
  NSMutableArray *results = [NSMutableArray array];
  NSEnumerator *varCount;
  
  id dataSign;
  NSNumber *typeOther;
  NSMutableArray *verifSign;
  NSEnumerator *signObj;
  
  gsl_vector *x = gsl_vector_alloc (n);
  
  // Generator of random numbers
  const gsl_rng_type * Y;
  gsl_rng * r;
  gsl_multiroot_function f;
  gsl_rng_env_setup();
  Y = gsl_rng_default;
  r = gsl_rng_alloc (Y);
  
  do
    {
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
	  
	  if (stateInt)
	    break;
	  
	  state = gsl_multiroot_test_residual (s->f, 1e-7);
	}
      while (state == GSL_CONTINUE && iter < 1000);  
      
      // Check the state
      if ( (stateInt) && (increase < 60) )
	{
	  increase = increase + 1;
	  follow = YES;
	}
      else
	{
	  follow = NO;
	}
    }
  while (follow);
  // The search of the solution has terminated
  
  // Move the results to array results
  varCount = [vars objectEnumerator];
  
  while ((anObj = [varCount nextObject]))
    {
      [results addObject: [NSNumber numberWithDouble:
				      gsl_vector_get (s->x, countRes)]];
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
	case 52 ... 53:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify the angle of the plane
	    if ([vars containsObject: [verifSign objectAtIndex: 1]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
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
	case 56 ... 59:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify the force
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
	case 72:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Verify the resultant
	    if ( [vars containsObject: [verifSign objectAtIndex: 0]] &&
		 [vars containsObject: [verifSign objectAtIndex: 1]] )
	      {
		g = [vars indexOfObject: [verifSign objectAtIndex: 0]];
		
		if ( [[results objectAtIndex: g] doubleValue] < 0 )
		  {
		    nf = -1*[[results objectAtIndex: g] doubleValue];
		    [results replaceObjectAtIndex: g
			     withObject: [NSNumber numberWithDouble: nf]];
		    par1 = 1;
		  }

		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
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
	}
    } 
  
  // Print the results
  [self printUnknowns: vars withResults: results];
  
  // Print the calculus state
  message = [NSString stringWithFormat: [errors objectAtIndex: 6],
		      gsl_strerror (state)];
  length = [[[self viewer] textStorage] length];
  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: message];
  
  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  gsl_rng_free (r);
}
@end

@implementation FLStatics

- (id) init
{
  NSBundle *messages;
  self = [super init];
  
  vars = [NSMutableArray new];
  codObjects = [NSMutableArray new];
  codOthers = [NSMutableArray new];
  forceObjs = [NSMutableArray new];
  forceTypes = [NSMutableArray new];
  dictionary = [NSMutableDictionary new];
  
  // Build the array of messages
  messages = [NSBundle mainBundle];
  errors = [[NSArray alloc] initWithContentsOfFile:
			 [messages pathForResource: @"staticsMessages"
					    ofType: @"plist"]];
  
  return self;
}

- (void) makeEquationsForData: (NSMutableDictionary *)list
{
  int width, height, totalCells; //For handle the chalkboard size
  int length, numEquations = 0, numForces = 0;
  NSUInteger sys = [self system];
  BOOL error = NO;
  BOOL errorForce = NO;
  BOOL errorAngles = NO;
  NSNumber *identifier, *code;
  NSMutableArray *codForces = [NSMutableArray array];
  NSMutableArray *codForcesObjs = [NSMutableArray array];
  NSArray *keys;
  NSEnumerator *enumerator;
  NSArray *objectsOrder = [self cells];
  // To check the applied forces to the objects
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
  
  while ((code = [enumerator nextObject])) 
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

      // To the forces
      int pos;
      NSNumber *ident;
      NSEnumerator *search;
      // -------------------
      identifier = [[dictionary objectForKey: code] objectForKey: @"Type"];
      [values removeAllObjects];
      
      // Count the number of variables
      for (x = 0; x < [info count]; x++)
	{
	  NSString *data = [info objectAtIndex: x];
	  
	  if (![self isNumericDataTheString: data] &&
	      ![[[titles objectAtIndex: x] description] isEqualToString:
							  _(@"Name")])
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
					     [[[self conversions] objectForKey:
							    key] doubleValue]];
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
						     [errors objectAtIndex: 0],
						     [key cString]];
                      length = [[[self viewer] textStorage] length];
                      [[self viewer] replaceCharactersInRange:
				                   NSMakeRange(length, 0)
						   withString: advert];
                      error = YES;
		    }
		}
	      else
		{
		  // Add a simple variable (without a conversion factor)
		  NSString *varTit, *varFactor;   
		  
		  // Check the type of variable
		  varTit = [[titles objectAtIndex: x] stringByTrimmingSpaces];
		  
		  if ([varTit hasPrefix: @"m"])
		    {
		      if (sys == 0)
			{ varFactor = @"kg"; }
		      else
			{ varFactor = @"slug"; }
		    }
		  else if ([varTit hasPrefix: @"f"] || [varTit hasPrefix: @"N"])
		    {
		      if (sys == 0)
			{ varFactor = @"N"; }
		      else
			{ varFactor = @"lb"; }
		    }
		  else if ([varTit hasPrefix: @"ang"])
		    {
		      varFactor = [NSString stringWithString: _(@"degrees")];
		    }
		  else if ([varTit hasPrefix: @"g"])
		    {
		      if (sys == 0)
			{ varFactor = @"m/s2"; }
		      else
			{ varFactor = @"ft/s2"; }
		    }
		  else if ([varTit hasPrefix: @"d"])
		    {
		      if (sys == 0)
			{ varFactor = @"m"; }
		      else
			{ varFactor = @"ft"; }
		    }
		  else if ([varTit hasPrefix: @"k"])
		    {
		      if (sys == 0)
			{ varFactor = @"N/m"; }
		      else
			{ varFactor = @"lb/ft"; }
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
	case 50:
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
	case 51:
	  {
	    numEquations += 2;
	    [codObjects addObject: code];
	  }    
	  break;
	case 52:
	  {
	    numEquations += 2;
	    [codObjects addObject: code];
	  }
	  break;
	case 53:
	  {
	    numEquations += 2;
	    [codObjects addObject: code];
	  }
	  break;
	case 54:
	  {
	    numEquations += 2;
	    [codObjects addObject: code];
	  }
	  break;
	case 55:
	  {
	    numEquations += 2;
	    [codObjects addObject: code];
	  }
	  break;
	case 56 ... 74:
	  {
	    numForces += 1;
	    [codForces addObject: code];
	  }
	  break;
	case 75 ... 78:
	  {
	    numEquations += 1;
	    [codObjects addObject: code];
	  }
	  break;
	case 79:
	  {
	    if (![self isNumericDataTheString: [values objectAtIndex: 0]] &&
		![self isNumericDataTheString: [values objectAtIndex: 1]])
	      {
		numEquations += 1;
		[codOthers addObject: code];
	      }
	    else
	      {
		errorAngles = YES;
	      }
	  }
	  break;
	}
      // Here ends the verify for equations

      if (errorAngles)
	break;
      
      // Find the forces around the objects
      search = [objectsOrder objectEnumerator];
      
      if ( [identifier intValue] >= 51 && [identifier intValue] <= 55 )
	{
	  NSMutableArray *f = [NSMutableArray array];
	  int horizontalF = 0, verticalF = 0;   
	  NSNumber *typeF;  
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
	      k = k + 1;   
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
	  
	  /* Check the applied forces and correct the number of equations, if
	     needed */
	  if ( [identifier intValue] == 52 && [f count] > 0 )
	    {
	      [forceTypes addObject: [NSNumber numberWithInt: 1]];
	      
	      verif = [f objectEnumerator];
              while ((typeF = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: typeF]
			     objectForKey: @"Type"] intValue];

		  if ( (t >= 66) || ((t >= 50) && (t <= 55)))
		    {
		      errorForce = YES;
		    } 
		  
		}
	    }
	  
	  if ( [identifier intValue] == 53 && [f count] > 0 )
	    {
	      [forceTypes addObject: [NSNumber numberWithInt: 1]];
	      
	      verif = [f objectEnumerator];
              while ((typeF = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: typeF]
			     objectForKey: @"Type"] intValue];

		  if ( (t == 64) || (t == 65) || (t >= 68) ||
		       ((t >= 50) && (t <= 55)) )
		    {
		      errorForce = YES;
		    } 
		  
		}
	    }
	  
	  if ( [identifier intValue] == 54 && [f count] > 0 )
	    {
	      verif = [f objectEnumerator];
              while ((typeF = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: typeF]
			     objectForKey: @"Type"] intValue];

		  if ( (t == 60) || (t == 61) )
		    {
		      horizontalF = horizontalF + 1;
		    }
		  
		  if ( (t == 62) || (t == 63) )
		    {
		      verticalF = verticalF + 1;
		    }
		  
		  if ( (t >= 64) || (t <= 55) || ((t >= 50) && (t <= 55)) )
		    {
		      errorForce = YES;
		    } 
		  
		}
              
              if ([f count] == horizontalF)
		{
		  numEquations -= 1;
		  [forceTypes addObject: [NSNumber numberWithInt: 2]];
		}
              else
		{
                  if ([f count] == verticalF)
		    {
		      numEquations -= 1;
		      [forceTypes addObject: [NSNumber numberWithInt: 3]];
		    }
                  else
		    {
		      [forceTypes addObject: [NSNumber numberWithInt: 1]];
		    }
		}
              
	    }
	  
	  if ( [identifier intValue] == 55 && [f count] > 0 )
	    {
	      verif = [f objectEnumerator];
              while ((typeF = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: typeF]
			     objectForKey: @"Type"] intValue];

		  if ( (t == 60) || (t == 61) || (t == 73) )
		    {
		      horizontalF += 1;
		    }
		  
		  if ( (t == 62) || (t == 63) || (t == 74) )
		    {
		      verticalF += 1;
		    }
		  
		  if ( ((t >= 64) && (t <= 71)) || ((t >= 50) && (t <= 55)) )
		    {
		      errorForce = YES;
		    }
		  
		}
              
              if ([f count] == horizontalF)
		{
		  numEquations -= 1;
		  [forceTypes addObject: [NSNumber numberWithInt: 2]];
		}
              else
		{
                  if ([f count] == verticalF)
		    {
		      numEquations -= 1;
		      [forceTypes addObject: [NSNumber numberWithInt: 3]];
		    }
                  else
		    {
		      [forceTypes addObject: [NSNumber numberWithInt: 1]];
		    }
		}
              
	    }
          
	  if ( [identifier intValue] == 51 && [f count] > 0 )
	    {
	      verif = [f objectEnumerator];
              while ((typeF = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: typeF]
			     objectForKey: @"Type"] intValue];

		  if ( (t == 62) || (t == 63) )
		    {
		      verticalF += 1;
		    }
		  
		  if ( ((t >= 64) && (t <= 67)) || (t >= 72) ||
		       ((t >= 50) && (t <= 55)) )
		    {
		      errorForce = YES;
		    }
		  
		}
	      
              if ([f count] == verticalF)
		{
		  numEquations -= 1;
		  [forceTypes addObject: [NSNumber numberWithInt: 3]];
		}
              else
		{
		  [forceTypes addObject: [NSNumber numberWithInt: 1]];
		}
              
	    }
	  
	  if ([f count] > 0)
	    {
	      [forceObjs addObject: f]; 
	    }       
	}
      else
	{
	  int sign = 0;
	  NSMutableArray *f = [NSMutableArray array];
	  
	  if (([identifier intValue] >= 75) && ([identifier intValue] <= 78))
	    {
	      int rForce = 0;
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
	      
	      if ([identifier intValue] == 75)
		{
		  rForce = width - 1;
		}
	      else
		{ 
		  if ([identifier intValue] == 76)
		    {
		      rForce = width + 1;
		    }
		  else
		    {
		      if ([identifier intValue] == 77)
			{
			  rForce = width;
			}
		      else
			{
			  rForce = 1;
			}
		    }
		}             
	      
              if ( (pos%width != 0) && (pos%width != (width - 1)) )
                {
		  if (pos - rForce >= 0)
                    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos - rForce] tag]];

		      if ([ident intValue] != 0)
			{
			  [f addObject: ident]; 
			  sign = 1;
			}
                    }
		  
		  if (pos + rForce <= totalCells)
                    {
		      ident = [NSNumber numberWithInt:
                          [[objectsOrder objectAtIndex: pos + rForce] tag]];

		      if ([ident intValue] != 0)
			{
			  [f addObject: ident];
			  sign = -1;
			}
                    }
                }
              else
                {
		  if (pos%width == 0)
		    { 
		      if ( (pos - rForce >= 0) &&
			   ((rForce != 1) || (rForce != (width + 1))) )
			{
			  ident = [NSNumber numberWithInt:
                            [[objectsOrder objectAtIndex: pos - rForce] tag]];

			  if ([ident intValue] != 0)
			    {
			      [f addObject: ident];
			      sign = 1;
			    }
			}
		      
		      if ( (pos + rForce <= totalCells) &&
			   (rForce != (width - 1)) )
			{
			  ident = [NSNumber numberWithInt:
                            [[objectsOrder objectAtIndex: pos + rForce] tag]];

			  if ([ident intValue] != 0)
			    {
			      [f addObject: ident];
			      sign = -1;
			    }
			}
		    }
		  else
		    {
		      if ((pos - rForce >= 0) && (rForce != (width - 1))) 
			{
			  ident = [NSNumber numberWithInt:
                             [[objectsOrder objectAtIndex: pos - rForce] tag]];

			  if ([ident intValue] != 0)
			    {
			      [f addObject: ident];
			      sign = 1;
			    }
			}
		      
		      if ( (pos + rForce <= totalCells) &&
			   ((rForce != 1) || (rForce != (width + 1))) )
			{
			  ident = [NSNumber numberWithInt:
                             [[objectsOrder objectAtIndex: pos + rForce] tag]];
			  if ([ident intValue] != 0)
			    {
			      [f addObject: ident];
			      sign = -1;
			    }
			}
		    }
                } 
	    }
	  
	  // Check the forces
	  if ([f count] > 0)
	    {
	      int f1, f2;
	      
              if (sign > 0) 
		{
		  [forceTypes addObject: [NSNumber numberWithInt: 4]];
		}
              else
		{
		  if ([f count] == 1)
		    {
		      [forceTypes addObject: [NSNumber numberWithInt: 5]];
		    }
		  else
		    {
		      [forceTypes addObject: [NSNumber numberWithInt: 4]];
		    }
		}
	      
	      // Check applied force in strings
	      f1 = [[[dictionary objectForKey: [f objectAtIndex: 0]]
		      objectForKey: @"Type"] intValue];

	      if (([identifier intValue] == 75) && (f1 != 56) && (f1 != 57))
		{
		  errorForce = YES;
		}
	      
	      if (([identifier intValue] == 76) && (f1 != 58) && (f1 != 59))
		{
		  errorForce = YES;
		}
	      
	      if (([identifier intValue] == 77) && (f1 != 62) && (f1 != 63))
		{
		  errorForce = YES;
		}
	      
	      if (([identifier intValue] == 78) && (f1 != 60) && (f1 != 61))
		{
		  errorForce = YES;
		}
              
	      
	      // Check the case when there are two forces applied in strings
              if ([f count] > 1)
		{
		  f2 = [[[dictionary objectForKey: [f objectAtIndex: 1]]
			  objectForKey: @"Type"] intValue];
		  
		  if (([identifier intValue] == 75) && (f2 != 56) && (f2 != 57))
		    {
		      errorForce = YES;
		    }
		  
		  if (([identifier intValue] == 76) && (f2 != 58) && (f2 != 59))
		    {
		      errorForce = YES;
		    }
		  
		  if (([identifier intValue] == 77) && (f2 != 62) && (f2 != 63))
		    {
		      errorForce = YES;
		    }
		  
		  if (([identifier intValue] == 78) && (f2 != 60) && (f2 != 61))
		    {
		      errorForce = YES;
		    }
		  
		  if (f1 == f2)
		    {
		      errorForce = YES;
		    }
		}
	    }
	  
	  if ([f count] > 0)
	    {
	      [forceObjs addObject: f]; 
	    }     
	}
    }
  

  // Check if all objects have applied forces
  if ( ([codObjects count] == [forceObjs count]) && !error && !errorForce &&
       !errorAngles)
    {
      // Count the number of applied forces
      int v;
      int w = 0;
      NSEnumerator *check;
      NSNumber *f;

      for (v = 0; v < [codObjects count]; v++)
	{
	  [codForcesObjs addObjectsFromArray: [forceObjs objectAtIndex: v]];
	}
      
      // Check if all forces are applied
      check = [codForces objectEnumerator];
      
      while ((f = [check nextObject]))
	{
	  if ([codForcesObjs containsObject: f])
	    {
	      w += 1;
	    }
	}
      
     
      // Final verifications
      if (([vars count] == numEquations) && (numEquations > 0))
	{
	  if ( w == [codForces count] && w == [codForcesObjs count] )
	    {
	      // All OK, build and solve the system of equations
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
	      
	      length = [[[self viewer] textStorage] length];
	      [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
					   withString: advert];
	    }
	}
      else
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 3]];
	}
    }
  else
    {
      if (errorForce)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 5]];
	}
      else if (errorAngles)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 7]];
	}
      else if ([codObjects count] != [forceObjs count])
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 4]];
	}
    }
  
  [keys release];
}

- (void) dealloc
{
  [dictionary release];
  [vars release];
  [codObjects release];
  [codOthers release];
  [forceObjs release];
  [forceTypes release];
  [errors release];
  [super dealloc];
}

@end
