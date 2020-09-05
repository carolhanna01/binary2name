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
#import "FLDynamics.h"


static int buildSystem (const gsl_vector *v, void *p, gsl_vector *func)
{
  int tIndex, gIndex, forceCod = 0;
  int nEqu = 0;
  int numCol;
  double tf, gf;
  NSNumber *object;
  NSEnumerator *enumObjects;
  NSEnumerator *enumRel;
  NSEnumerator *enumCol;
  NSEnumerator *enumOthers;
  // To the objects momentum
  int countImp = 0;
  NSNumber *typeObj = nil;
  NSEnumerator *enumImp;
  // To the objects power
  int countPow = 0;
  NSNumber *typePot = nil;
  NSEnumerator *enumPow;

  // Get properties of FL object.
  FLDynamics *FLObj = (__bridge FLDynamics *)(p);

  NSInteger varT = FLObj->varT;
  NSInteger varG = FLObj->varG;
  NSString *timeVar = FLObj->timeVar;
  NSString *gravityVar = FLObj->gravityVar;
  double timeDat = FLObj->timeDat;
  double gravityDat = FLObj->gravityDat;
  NSMutableArray *vars = FLObj->vars;
  NSMutableArray *forceObjs = FLObj->forceObjs;
  NSMutableArray *forceTypes = FLObj->forceTypes;
  NSMutableArray *codObjects = FLObj->codObjects;
  NSMutableArray *codCol = FLObj->codCol;
  NSMutableArray *typeCol = FLObj->typeCol;
  NSMutableArray *codRel = FLObj->codRel;
  NSMutableArray *others = FLObj->others;
  NSMutableArray *impulse = FLObj->impulse;
  NSMutableArray *impulseObjs = FLObj->impulseObjs;
  NSMutableArray *power = FLObj->power;
  NSMutableArray *powerObjs = FLObj->powerObjs;
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
  
  if (varT == 1)
    {
      tIndex = [vars indexOfObject: timeVar];
      tf = gsl_vector_get (v, tIndex);   
    }
  else
    {
      if (varT == 2)
	{
          tf = timeDat;
	}
      else
	{
          tf = 0;
	}
    }
  
  enumObjects = [codObjects objectEnumerator];
  
  while ((object = [enumObjects nextObject]))
    {
      NSNumber *type = [[dictionary objectForKey: object]
			 objectForKey: @"Type"];
      NSMutableArray *dat = [[dictionary objectForKey: object]
			      objectForKey: @"Values"];
      
      switch ([type intValue])
	{
	case 104:
	  {
	    int a_status = 0;
	    double weight, mass, a, vi, vf, d;
            
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
		a = [[dat objectAtIndex: 2] doubleValue];
		a_status = 1;
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		a = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		vi = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		vf = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		d = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		d = gsl_vector_get (v, k);
	      }
	    
	    weight = mass*gf;
            
	    if ([[forceTypes objectAtIndex: forceCod] intValue] == 3)
	      {
		double sign;
		double force = 0;
		NSString *obj = nil;
		NSNumber *codForceObj;
		NSEnumerator *forceObj;
		force = -1*weight;
		forceObj = [[forceObjs objectAtIndex: forceCod]
			     objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    if ( t == 115 )
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
		gsl_vector_set (func, nEqu, force - mass*a);
		gsl_vector_set (func, nEqu + 1, 0.5*(vf*vf - vi*vi) - a*d); 
                
		if ( (a_status = 1) && (a == 0) )
		  {
		    gsl_vector_set (func, nEqu + 2, vi*tf - d);
		  }
		else
		  {
		    gsl_vector_set (func, nEqu + 2, (vf - vi) - a*tf); 
		  }
		
		nEqu += 3;
	      }
	    else
	      {
		double signx, signy;
		double a_rel = 0;
		double forcex = 0;
		double forcey = 0;
		NSString *obj = nil;
		NSNumber *codForceObj;
		NSEnumerator *forceObj;
		forcey = -1*weight;
		forceObj = [[forceObjs objectAtIndex: forceCod]
			     objectEnumerator];
                
		if (![[dat objectAtIndex: 6] isEqualToString: @"sf"])
                  {     
		    NSEnumerator *search = [dictionary objectEnumerator];
		    NSNumber *typeObj = nil;   
		    NSMutableArray *objectData = nil;
		    NSMutableDictionary *searchName;
		    NSString *nameObj = [dat objectAtIndex: 6];  
		    
		    while ((searchName = [search nextObject]))
		      {
			NSString *n = [[searchName objectForKey: @"Values"]
					objectAtIndex: 0];
			NSString *t = [[searchName objectForKey: @"Titles"]
					objectAtIndex: 0];
                        
			if ([t isEqualToString: _(@"Name")])
			  { 
			    if ([nameObj isEqualToString: n])
			      {
                                typeObj = [searchName objectForKey: @"Type"];
                                objectData = [searchName objectForKey:
							   @"Values"];
			      }
			  }
		      }
		    
		    if ([typeObj intValue] == 105)
		      {
			if (![vars containsObject:
				     [objectData objectAtIndex: 2]])
			  {
			    a_rel = [[objectData objectAtIndex: 2] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [objectData objectAtIndex: 2]];
			    a_rel = gsl_vector_get (v, k);
			  }
		      }
		  }       
		
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    switch (t)
		      {
		      case 113:
		      case 114:
			{
			  if (t == 113)
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      forcex = forcex + signx*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex = forcex + signx*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 115:
		      case 116:
			{
			  if (t == 115)
			    { signy = 1; }
			  else
			    { signy = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      forcey = forcey + signy*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcey = forcey + signy*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 109 ... 112:
			{
			  double ang;
			  NSString *a = nil;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
                          
			  if ( (t == 109) || (t == 112) )
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if ( (t == 109) || (t == 111) )
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
		      case 123:
		      case 124:
			{
			  double n;
			  NSString *a = nil;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];

			  if (t == 123)
			    { signy = 1; }
			  else
			    { signy = - 1; }
			  
			  if (![vars containsObject: obj])
			    {
			      n = [obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      n = gsl_vector_get (v, k);
			    }
			  
			  if (![vars containsObject: a])
			    {
			      forcey += signy*n*[a doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: a];
			      forcey += signy*n*gsl_vector_get (v, k);
			    }
			}
			break;
		      case 131:
		      case 132:   
			{
			  double u;
			  NSString *a = nil;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
                          
			  if (t == 131)
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
			      int k = [vars indexOfObject: a];
			      forcey += signy*u*(gsl_vector_get (v, k));
			    }
			}
			break;
		      case 125:
		      case 126:
		      case 127:
		      case 128:
			{
			  double ang, u;
			  NSString *a = nil, *b = nil;
			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
			  b = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 2];
                          
			  if ( (t == 125) || (t == 128) )
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if ( (t == 125) || (t == 127) )
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
			  
			  if (![vars containsObject: b])
			    {
			      ang = [b doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: b];
			      ang = gsl_vector_get (v, k);
			    }
			  
			  if (![vars containsObject: obj])
			    {
			      forcex += signx*u*[obj doubleValue]*
				cos(M_PI*ang/180);
			      forcey += signy*u*[obj doubleValue]*
				sin(M_PI*ang/180);
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      forcex += signx*u*(gsl_vector_get (v, k))*
				cos(M_PI*ang/180);
			      forcey += signy*u*(gsl_vector_get (v, k))*
				sin(M_PI*ang/180);
			    }
			}
			break;
		      }
		  }
		
		// Buidl the equations
		gsl_vector_set (func, nEqu, forcex - mass*a_rel);
		gsl_vector_set (func, nEqu + 1, forcey - mass*a);
		gsl_vector_set (func, nEqu + 2, 0.5*(vf*vf - vi*vi) - a*d); 
		//gsl_vector_set (func, nEqu + 3, (vf - vi) - a*tf);
                
		if ( (a_status = 1) && (a == 0) )
		  {
		    gsl_vector_set (func, nEqu + 3, vi*tf - d);
		  }
		else
		  {
		    //gsl_vector_set (func, nEqu + 3, (vf - vi) - a*tf); 
		    gsl_vector_set (func, nEqu + 3, vi*tf + 0.5*a*tf*tf - d);
		  }
		
		nEqu += 4;
	      }
	  }
	  break;
	case 105:
	  {
	    int a_status = 0; 
	    double signx, signy; 
	    double mass, a, vi, vf, d;
	    double a_sign = 1, a_rel = 0, ang_rel = 0;
	    double forcex = 0;
	    double forcey = 0;
	    NSString *obj = nil;
	    NSNumber *codForceObj;
	    NSEnumerator *forceObj;
            
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
		a = [[dat objectAtIndex: 2] doubleValue];
		a_status = 1;
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		a = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		vi = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		vf = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		d = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		d = gsl_vector_get (v, k);
	      }
	    
	    if (![[dat objectAtIndex: 6] isEqualToString: @"sf"])
	      {     
		NSEnumerator *search = [dictionary objectEnumerator];
		NSNumber *typeObj = nil;   
		NSMutableArray *objectData = nil;
		NSMutableDictionary *searchName;
		NSString *nameObj = [dat objectAtIndex: 6];  
                
		while ((searchName = [search nextObject]))
		  {
		    NSString *n = [[searchName objectForKey: @"Values"]
				    objectAtIndex: 0];
		    NSString *t = [[searchName objectForKey: @"Titles"]
				    objectAtIndex: 0];
                    
		    if ([t isEqualToString: _(@"Name")])
		      { 
			if ([nameObj isEqualToString: n])
			  {
			    typeObj = [searchName objectForKey: @"Type"];
			    objectData = [searchName objectForKey: @"Values"];
			    break;
			  }
		      }   
		  }
		
		switch ([typeObj intValue])
		  {
		  case 105:
		    {
		      NSString *a = [objectData objectAtIndex: 2];

		      if (![vars containsObject: a])
			{
			  a_rel = [a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  a_rel = gsl_vector_get (v, k);
			}
		    }
		    break;
		  case 106:
		    {
		      NSString *a = [objectData objectAtIndex: 2];
		      NSString *b = [objectData objectAtIndex: 3];

		      if (![vars containsObject: a])
			{
			  ang_rel = [a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  ang_rel = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: b])
			{
			  a_rel = [b doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: b];
			  a_rel = gsl_vector_get (v, k);
			}
		    }
		    break;
		  case 107:
		    {
		      a_sign = -1;
		      NSString *a = [objectData objectAtIndex: 2];
		      NSString *b = [objectData objectAtIndex: 3];
                      
		      if (![vars containsObject: a])
			{
			  ang_rel = [a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  ang_rel = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: b])
			{
			  a_rel = [b doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: b];
			  a_rel = gsl_vector_get (v, k);
			}
		    }
		    break;
		  }
	      }   
	    
	    forcey = -1*mass*gf;
	    forceObj = [[forceObjs objectAtIndex: forceCod] objectEnumerator];
            
	    while ((codForceObj = [forceObj nextObject]))
	      {
		int t = [[[dictionary objectForKey: codForceObj]
			   objectForKey: @"Type"] intValue];
		obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                
		switch (t)
		  {
		  case 113:
		  case 114:
		    {
		      if (t == 113)
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
		  case 115:
		  case 116:
		    {
		      if (t == 115)
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
		  case 109 ... 112:
		    {
		      double ang;
		      NSString *a;

		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];
                      
		      if ( (t == 109) || (t == 112) )
			{ signx = 1; }
		      else
			{ signx = - 1; }
		      
		      if ( (t == 109) || (t == 111) )
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
		  case 121:
		  case 122:
		    {
		      double n;
		      NSString *a;

		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];
		      
		      if (t == 121)
			{ signx = 1; }
		      else
			{ signx = - 1; }
		      
		      if (![vars containsObject: obj])
			{
			  n = [obj doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  n = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: a])
			{
			  forcex += signx*n*[a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  forcex += signx*n*gsl_vector_get (v, k);
			}
		    }
		    break;
		  case 129:
		  case 130:   
		    {
		      double u;
		      NSString *a;

		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];
                      
		      if (t == 129)
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
		  case 125:
		  case 126:
		  case 127:
		  case 128:
		    {
		      double ang, u;
		      NSString *a, *b;

		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];
		      b = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 2];
                      
		      if ( (t == 125) || (t == 128) )
			{ signx = 1; }
		      else
			{ signx = - 1; }
		      
		      if ( (t == 125) || (t == 127) )
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
		      
		      if (![vars containsObject: b])
			{
			  ang = [b doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: b];
			  ang = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*u*[obj doubleValue]*cos(M_PI*ang/180);
			  forcey += signy*u*[obj doubleValue]*sin(M_PI*ang/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*u*(gsl_vector_get (v, k))*
			    cos(M_PI*ang/180);
			  forcey += signy*u*(gsl_vector_get (v, k))*
			    sin(M_PI*ang/180);
			}
		    }
		    break;
		  }
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, forcex - 
			    mass*(a + a_sign*a_rel*cos(M_PI*ang_rel/180)) );
	    gsl_vector_set (func, nEqu + 1, forcey -
			    mass*a_rel*sin(M_PI*ang_rel/180));
	    gsl_vector_set (func, nEqu + 2, 0.5*(vf*vf - vi*vi) - a*d);
	    //gsl_vector_set (func, nEqu + 3, (vf - vi) - a*tf);
            
	    if ( (a_status = 1) && (a == 0) )
	      {
		gsl_vector_set (func, nEqu + 3, vi*tf - d);
	      }
	    else
	      {
		//gsl_vector_set (func, nEqu + 3, (vf - vi) - a*tf); 
		gsl_vector_set (func, nEqu + 3, vi*tf + 0.5*a*tf*tf - d);
	      }
	    
	    nEqu += 4;
	  }
	  break;
	case 106:
	  {
	    int a_status = 0;
	    double signx = 0, signy = 0; 
	    double a_signx = 1, a_signy = 1, a_rel = 0, ang_rel = 0;
	    double forcex = 0;
	    double forcey = 0;
	    double mass, ang, a, vi, vf, d;
	    NSString *obj = nil;
	    NSNumber *codForceObj;
	    NSEnumerator *forceObj;
            
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		ang = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		ang = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		mass = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		mass = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		a = [[dat objectAtIndex: 3] doubleValue];
		a_status = 1;
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		a = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		vi = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		vf = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		d = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		d = gsl_vector_get (v, k);
	      }
	    
	    if (![[dat objectAtIndex: 7] isEqualToString: @"sf"])
	      {     
		NSEnumerator *search = [dictionary objectEnumerator];
		NSNumber *typeObj = nil;   
		NSMutableArray *objectData = nil;
		NSMutableDictionary *searchName;
		NSString *nameObj = [dat objectAtIndex: 7];  
                
		while ((searchName = [search nextObject]))
		  {
		    NSString *n = [[searchName objectForKey: @"Values"]
				    objectAtIndex: 0];
		    NSString *t = [[searchName objectForKey: @"Titles"]
				    objectAtIndex: 0];
                    
		    if ([t isEqualToString: _(@"Name")])
		      { 
			if ([nameObj isEqualToString: n])
			  {
			    typeObj = [searchName objectForKey: @"Type"];
			    objectData = [searchName objectForKey: @"Values"];
			    break;
			  }
		      }
		  }
		
		switch ([typeObj intValue])
		  {
		  case 105:
		    {
		      a_signy = -1;
                      
		      if (![vars containsObject: [objectData objectAtIndex: 2]])
			{
			  a_rel = [[objectData objectAtIndex: 2] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objectData objectAtIndex: 2]];
			  a_rel = gsl_vector_get (v, k);
			}
		      
		      ang_rel = ang;
		    }
		    break;
		  case 106:
		    {
		      if (![vars containsObject: [objectData objectAtIndex: 3]])
			{
			  a_rel = [[objectData objectAtIndex: 3] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objectData objectAtIndex: 3]];
			  a_rel = gsl_vector_get (v, k);
			}
		    }
		    break;
		  case 107:
		    {
		      a_signx = -1;
                      
		      if (![vars containsObject: [objectData objectAtIndex: 2]])
			{
			  ang_rel = [[objectData objectAtIndex: 2] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objectData objectAtIndex: 2]];
			  ang_rel = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: [objectData objectAtIndex: 3]])
			{
			  a_rel = [[objectData objectAtIndex: 3] doubleValue]
			    + ang;
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objectData objectAtIndex: 3]];
			  a_rel = gsl_vector_get (v, k) + ang;
			}
		    }
		    break;
		  }
	      }   
	    
	    forcex = -1*mass*sin(M_PI*ang/180)*gf;
	    forcey = -1*mass*cos(M_PI*ang/180)*gf;
	    forceObj = [[forceObjs objectAtIndex: forceCod] objectEnumerator];
            
	    while ((codForceObj = [forceObj nextObject]))
	      {
		int t = [[[dictionary objectForKey: codForceObj]
			   objectForKey: @"Type"] intValue];
		obj = [[[dictionary objectForKey: codForceObj]
			 objectForKey: @"Values"] objectAtIndex: 0];
                
		switch (t)
		  {
		  case 113:
		  case 114:
		    {
		      if (t == 113)
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
		  case 115:
		  case 116:
		    {
		      if (t == 115)
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
		  case 109 ... 112:
		    {
		      double angf, angr = 0;
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
		      
		      if (t == 109)
			{
			  signx = 1;
			  signy = 1; 
			  angr = angf - ang;
			}
		      
		      if (t == 110)
			{
			  signx = - 1;
			  signy = 1;
			  angr = ang - angf;
			}
		      
		      if (t == 111)
			{
			  signy = 1;
			  signx = - 1;
			  angr = angf + ang;
			}
		      
		      if (t == 112)
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
		  case 117:
		  case 118:
		    {
		      double n;
		      NSString *a;

		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];

		      if (t == 117)
			{ signx = 1; }
		      else
			{ signx = - 1; }
		      
		      if (![vars containsObject: obj])
			{
			  n = [obj doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  n = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: a])
			{
			  forcex += signx*n*[a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  forcex += signx*n*gsl_vector_get (v, k);
			}
		    }
		    break;
		  case 125 ... 128:
		    {
		      double angf, u, angr = 0;
		      NSString *a, *b;

		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];
		      b = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 2];
                      
		      if (![vars containsObject: a])
			{
			  u = [a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  u = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: b])
			{
			  angf = [b doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: b];
			  angf = gsl_vector_get (v, k);
			}
		      
		      if (t == 125)
			{
			  signx = 1;
			  signy = 1; 
			  angr = angf - ang;
			}
		      
		      if (t == 126)
			{
			  signx = - 1;
			  signy = 1;
			  angr = ang - angf;
			}
		      
		      if (t == 127)
			{
			  signy = 1;
			  signx = - 1;
			  angr = angf + ang;
			}
		      
		      if (t == 128)
			{
			  signy = - 1;
			  signx = 1; 
			  angr = angf + ang;
			}
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*u*[obj doubleValue]*
			    cos(M_PI*angr/180);
			  forcey += signy*u*[obj doubleValue]*
			    sin(M_PI*angr/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*u*(gsl_vector_get (v, k))*
			    cos(M_PI*angr/180);
			  forcey += signy*u*(gsl_vector_get (v, k))*
			    sin(M_PI*angr/180);
			}
		    }
		    break;
		  }
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, forcex - mass*
			    (a + a_signx*a_rel*cos(M_PI*ang_rel/180)) );
	    gsl_vector_set (func, nEqu + 1, forcey - mass*
			    a_signy*a_rel*sin(M_PI*ang_rel/180) );
	    gsl_vector_set (func, nEqu + 2, 0.5*(vf*vf - vi*vi) - a*d);
	    //gsl_vector_set (func, nEqu + 3, (vf - vi) - a*tf); 
            
	    if ( (a_status = 1) && (a == 0) )
	      {
		gsl_vector_set (func, nEqu + 3, vi*tf - d);
	      }
	    else
	      {
		//gsl_vector_set (func, nEqu + 3, (vf - vi) - a*tf);
		gsl_vector_set (func, nEqu + 3, vi*tf + 0.5*a*tf*tf - d); 
	      }
	    
	    nEqu += 4;
	  }
	  break;
	case 107:
	  {
	    int a_status = 0; 
	    double signx = 0, signy = 0;
	    double a_sign = 1, a_rel = 0, ang_rel = 0;
	    double forcex = 0;
	    double forcey = 0;
	    double mass, ang, a, vi, vf, d;
	    NSString *obj = nil;
	    NSNumber *codForceObj;
	    NSEnumerator *forceObj;
            
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		ang = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		ang = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		mass = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		mass = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		a = [[dat objectAtIndex: 3] doubleValue];
		a_status = 1;
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		a = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		vi = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		vf = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		d = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		d = gsl_vector_get (v, k);
	      }
	    
	    if (![[dat objectAtIndex: 7] isEqualToString: @"sf"])
	      {     
		NSEnumerator *search = [dictionary objectEnumerator];
		NSNumber *typeObj = nil;   
		NSMutableArray *objectData = nil;
		NSMutableDictionary *searchName;
		NSString *nameObj = [dat objectAtIndex: 7];  
                
		while ((searchName = [search nextObject]))
		  {
		    NSString *n = [[searchName objectForKey: @"Values"]
				    objectAtIndex: 0];
		    NSString *t = [[searchName objectForKey: @"Titles"]
				    objectAtIndex: 0];
                    
		    if ([t isEqualToString: _(@"Name")])
		      { 
			if ([nameObj isEqualToString: n])
			  {
			    typeObj = [searchName objectForKey: @"Type"];
			    objectData = [searchName objectForKey: @"Values"];
			    break;
			  }
		      }
		  }
		
		switch ([typeObj intValue])
		  {
		  case 105:
		    {
		      a_sign = -1;
                      
		      if (![vars containsObject: [objectData objectAtIndex: 2]])
			{
			  a_rel = [[objectData objectAtIndex: 2] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objectData objectAtIndex: 2]];
			  a_rel = gsl_vector_get (v, k);
			}
		      
		      ang_rel = ang;
		    }
		    break;
		  case 106:
		    {
		      a_sign = -1;                            
                      
		      if (![vars containsObject: [objectData objectAtIndex: 2]])
			{
			  ang_rel = [[objectData objectAtIndex: 2] doubleValue]
			    + ang;
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objectData objectAtIndex: 2]];
			  ang_rel = gsl_vector_get (v, k) + ang;
			}
		      
		      if (![vars containsObject: [objectData objectAtIndex: 3]])
			{
			  a_rel = [[objectData objectAtIndex: 3] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objectData objectAtIndex: 3]];
			  a_rel = gsl_vector_get (v, k);
			}
		    }
		    break;
		  case 107:
		    {
		      if (![vars containsObject: [objectData objectAtIndex: 3]])
			{
			  a_rel = [[objectData objectAtIndex: 3] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objectData objectAtIndex: 3]];
			  a_rel = gsl_vector_get (v, k);
			}
		    }
		    break;
		  }
	      }   
	    
	    forcex = -1*mass*sin(M_PI*ang/180)*gf;
	    forcey = -1*mass*cos(M_PI*ang/180)*gf;
	    forceObj = [[forceObjs objectAtIndex: forceCod] objectEnumerator];
            
	    while ((codForceObj = [forceObj nextObject]))
	      {
		int t = [[[dictionary objectForKey: codForceObj]
			   objectForKey: @"Type"] intValue];
		obj = [[[dictionary objectForKey: codForceObj]
			 objectForKey: @"Values"] objectAtIndex: 0];
                
		switch (t)
		  {
		  case 113:
		  case 114:
		    {
		      if (t == 114)
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
		  case 115:
		  case 116:
		    {
		      if (t == 115)
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
		  case 109 ... 112:
		    {
		      double angf, angr = 0;
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
		      
		      if (t == 109)
			{
			  signy = 1;
			  signx = -1;
			  angr = ang + angf;
			}
		      
		      if (t == 110)
			{
			  signy = - 1;
			  signx = 1;
			  angr = angf + ang;
			}
		      
		      if (t == 111)
			{
			  signx = 1;
			  signy = 1;
			  angr = angf - ang;
			}
		      
		      if (t == 112)
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
		  case 119:
		  case 120:
		    {
		      double n;
		      NSString *a;

		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];

		      if (t == 119)
			{ signx = 1; }
		      else
			{ signx = - 1; }
		      
		      if (![vars containsObject: obj])
			{
			  n = [obj doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  n = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: a])
			{
			  forcex += signx*n*[a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  forcex += signx*n*gsl_vector_get (v, k);
			}
		      
		    }
		    break;
		  case 125 ... 128:
		    {
		      double angf, u, angr = 0;
		      NSString *a, *b;

		      a = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 1];
		      b = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 2];
                      
		      if (![vars containsObject: a])
			{
			  u = [a doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: a];
			  u = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: b])
			{
			  angf = [b doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject: b];
			  angf = gsl_vector_get (v, k);
			}
		      
		      if (t == 125)
			{
			  signy = 1;
			  signx = - 1; 
			  angr = angf + ang;
			}
		      
		      if (t == 126)
			{
			  signy = - 1;
			  signx = 1;
			  angr = angf + ang;
			}
		      
		      if (t == 127)
			{
			  signx = 1;
			  signy = 1;
			  angr = angf - ang;
			}
		      
		      if (t == 128)
			{
			  signx = - 1;
			  signy = 1;
			  angr = ang - angf;
			}
		      
		      if (![vars containsObject: obj])
			{
			  forcex += signx*u*[obj doubleValue]*
			    cos(M_PI*angr/180);
			  forcey += signy*u*[obj doubleValue]*
			    sin(M_PI*angr/180);
			}
		      else
			{
			  int k = [vars indexOfObject: obj];
			  forcex += signx*u*(gsl_vector_get (v, k))*
			    cos(M_PI*angr/180);
			  forcey += signy*u*(gsl_vector_get (v, k))*
			    sin(M_PI*angr/180);
			}
		    }
		    break;                
		  }
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, forcex - mass*
			    (a + a_sign*a_rel*cos(M_PI*ang_rel/180)) );
	    gsl_vector_set (func, nEqu + 1, forcey - mass*
			    a_rel*sin(M_PI*ang_rel/180) );
	    gsl_vector_set (func, nEqu + 2, 0.5*(vf*vf - vi*vi) - a*d); 
	    //gsl_vector_set (func, nEqu + 3, (vf - vi) - a*tf);
            
	    if ( (a_status = 1) && (a == 0) )
	      {
		gsl_vector_set (func, nEqu + 3, vi*tf - d);
	      }
	    else
	      {
		//gsl_vector_set (func, nEqu + 3, (vf - vi) - a*tf); 
		gsl_vector_set (func, nEqu + 3, vi*tf + 0.5*a*tf*tf - d);
	      } 
	    
	    nEqu += 4;
	  }
	  break;
	case 108:
	  {
	    
	    if ([[forceTypes objectAtIndex: forceCod] intValue] == 2 ||
		[[forceTypes objectAtIndex: forceCod] intValue] == 3 )
	      {
		double sign;
		double force = 0;
		NSString *obj;
		NSNumber *codForceObj;
		NSEnumerator *forceObj = [[forceObjs objectAtIndex: forceCod]
					   objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    if ( (t == 113) || (t == 115) )
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
		NSString *obj;
		NSNumber *codForceObj;
		NSEnumerator *forceObj = [[forceObjs objectAtIndex: forceCod]
					   objectEnumerator];
                
		while ((codForceObj = [forceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: codForceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: codForceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
                    
		    switch (t)
		      {
		      case 113:
		      case 114:
			{
			  if (t == 113)
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
		      case 115:
		      case 116:
			{
			  if (t == 115)
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
		      case 109 ... 112:
			{
			  double ang;
			  NSString *a;

			  a = [[[dictionary objectForKey: codForceObj]
				 objectForKey: @"Values"] objectAtIndex: 1];
                          
			  if ( (t == 109) || (t == 112) )
			    { signx = 1; }
			  else
			    { signx = - 1; }
			  
			  if ( (t == 109) || (t == 111) )
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
	case 133 ... 136:
	  {
	    int pForce, t; 
	    double sign = 0, kr, xi, xf, force = 0;
	    NSString *obj;
	    NSNumber *codForceObj = [[forceObjs objectAtIndex: forceCod]
				      objectAtIndex: 0];
	    
	    if ([[forceTypes objectAtIndex: forceCod] intValue] == 4)
	      {
		pForce = 1;
	      }
	    else
	      {
		pForce = -1;
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		kr = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		kr = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		xi = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		xi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		xf = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		xf = gsl_vector_get (v, k);
	      }
	    
	    t = [[[dictionary objectForKey: codForceObj]
		   objectForKey: @"Type"] intValue];
	    obj = [[[dictionary objectForKey: codForceObj]
		     objectForKey: @"Values"] objectAtIndex: 0];
            
	    switch (t)
	      {
	      case 109:
	      case 111:
	      case 114:
	      case 115:
		{
		  sign = 1;
		}
		break;
	      case 110:
	      case 112:
	      case 113:
	      case 116:
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
	    gsl_vector_set (func, nEqu, (kr*(xf - xi)/2) - force);
	    
	    nEqu += 1;
	  }
	  break;
	}
      forceCod += 1;
    }
  
  // Objects for relations
  enumRel = [codRel objectEnumerator];
  
  while ((object = [enumRel nextObject]))
    {
      NSMutableArray *dat = [[dictionary objectForKey: object]
			      objectForKey: @"Values"];
      
      double a_one, a_two, z;
      
      if (![vars containsObject: [dat objectAtIndex: 0]])
	{
	  a_one = [[dat objectAtIndex: 0] doubleValue];
	}
      else
	{
	  int k = [vars indexOfObject: [dat objectAtIndex: 0]];
	  a_one = gsl_vector_get (v, k);
	}
      
      if (![vars containsObject: [dat objectAtIndex: 1]])
	{
	  a_two = [[dat objectAtIndex: 1] doubleValue];
	}
      else
	{
	  int k = [vars indexOfObject: [dat objectAtIndex: 1]];
	  a_two = gsl_vector_get (v, k);
	}
      
      if (![vars containsObject: [dat objectAtIndex: 2]])
	{
	  z = [[dat objectAtIndex: 2] doubleValue];
	}
      else
	{
	  int k = [vars indexOfObject: [dat objectAtIndex: 2]];
	  z = gsl_vector_get (v, k);
	}
      
      // Build the equations
      gsl_vector_set (func, nEqu, a_one - z*a_two);
      
      nEqu += 1;
    }
  
  // Objects collision
  enumCol = [codCol objectEnumerator];
  numCol = 0;
  
  while ((object = [enumCol nextObject]))
    {
      double e, angn, m1, m2;
      NSMutableArray *others = [[dictionary objectForKey: object]
				 objectForKey: @"Values"];
      int typeCollision = [[typeCol objectAtIndex: numCol] intValue];
      
      NSString *nameObj1 = [others objectAtIndex: 0];
      NSString *nameObj2 = [others objectAtIndex: 1];
      NSEnumerator *search = [dictionary objectEnumerator];
      NSMutableArray *mobileObj1 = nil, *mobileObj2 = nil;
      NSMutableDictionary *searchName;

      numCol += 1;
      
      while ((searchName = [search nextObject]))
	{
	  NSString *n = [[searchName objectForKey: @"Values"] objectAtIndex: 0];
	  NSString *t = [[searchName objectForKey: @"Titles"] objectAtIndex: 0];
          
	  if ([t isEqualToString: _(@"Name")])
	    { 
	      if ([nameObj1 isEqualToString: n])
		{
		  mobileObj1 = [searchName objectForKey: @"Values"];
		}
	      
	      if ([nameObj2 isEqualToString: n])
		{
		  mobileObj2 = [searchName objectForKey: @"Values"];
		}

	      if (mobileObj1 != nil && mobileObj2 != nil)
		{
		  break;
		}
	    }
	}
      
      if (![vars containsObject: [others objectAtIndex: 2]])
	{
          e = [[others objectAtIndex: 2] doubleValue];
	}
      else
	{
          int k = [vars indexOfObject: [others objectAtIndex: 2]];
          e = gsl_vector_get (v, k);
	}
      
      if (![vars containsObject: [others objectAtIndex: 3]])
	{
          angn = [[others objectAtIndex: 3] doubleValue];
	}
      else
	{
	  int k = [vars indexOfObject: [others objectAtIndex: 3]];
	  angn = gsl_vector_get (v, k);
	}
      
      if (![vars containsObject: [mobileObj1 objectAtIndex: 1]])
	{
	  m1 = [[mobileObj1 objectAtIndex: 1] doubleValue];
	}
      else
	{
          int k = [vars indexOfObject: [mobileObj1 objectAtIndex: 1]];
          m1 = gsl_vector_get (v, k);
	}   
      
      if (![vars containsObject: [mobileObj2 objectAtIndex: 1]])
	{
          m2 = [[mobileObj2 objectAtIndex: 1] doubleValue];
	}
      else
	{
          int k = [vars indexOfObject: [mobileObj2 objectAtIndex: 1]];
          m2 = gsl_vector_get (v, k);
	}          
      
      if (typeCollision == 2)
	{
	  double vi_one, vf_one, vi_two, vf_two;   
          
	  if (![vars containsObject: [mobileObj1 objectAtIndex: 2]])
	    {
	      vi_one = [[mobileObj1 objectAtIndex: 2] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj1 objectAtIndex: 2]];
	      vi_one = gsl_vector_get (v, k);
	    }      
	  
	  if (![vars containsObject: [mobileObj1 objectAtIndex: 3]])
	    {
	      vf_one = [[mobileObj1 objectAtIndex: 3] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj1 objectAtIndex: 3]];
	      vf_one = gsl_vector_get (v, k);
	    }      
	  
	  if (![vars containsObject: [mobileObj2 objectAtIndex: 2]])
	    {
	      vi_two = [[mobileObj2 objectAtIndex: 2] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj2 objectAtIndex: 2]];
	      vi_two = gsl_vector_get (v, k);
	    }      
                        
	  if (![vars containsObject: [mobileObj2 objectAtIndex: 3]])
	    {
	      vf_two = [[mobileObj2 objectAtIndex: 3] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj2 objectAtIndex: 3]];
	      vf_two = gsl_vector_get (v, k);
	    }      
	  
	  // Build the equations
	  gsl_vector_set (func, nEqu, m1*vi_one + m2*vi_two -m1*vf_one -
			  m2*vf_two);
	  gsl_vector_set (func, nEqu + 1, e*(vi_one - vi_two) -
			  (vf_two - vf_one) );
          
	  nEqu += 2;
	}
      else
	{
	  double vi_one, angi_one, vf_one, angf_one, vi_two, angi_two,
	    vf_two, angf_two;
          
	  if (![vars containsObject: [mobileObj1 objectAtIndex: 2]])
	    {
	      vi_one = [[mobileObj1 objectAtIndex: 2] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj1 objectAtIndex: 2]];
	      vi_one = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [mobileObj1 objectAtIndex: 3]])
	    {
	      angi_one = [[mobileObj1 objectAtIndex: 3] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj1 objectAtIndex: 3]];
	      angi_one = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [mobileObj1 objectAtIndex: 4]])
	    {
	      vf_one = [[mobileObj1 objectAtIndex: 4] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj1 objectAtIndex: 4]];
	      vf_one = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [mobileObj1 objectAtIndex: 5]])
	    {
	      angf_one = [[mobileObj1 objectAtIndex: 5] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj1 objectAtIndex: 5]];
	      angf_one = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [mobileObj2 objectAtIndex: 2]])
	    {
	      vi_two = [[mobileObj2 objectAtIndex: 2] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj2 objectAtIndex: 2]];
	      vi_two = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [mobileObj2 objectAtIndex: 3]])
	    {
	      angi_two = [[mobileObj2 objectAtIndex: 3] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj2 objectAtIndex: 3]];
	      angi_two = gsl_vector_get (v, k);
	    }
	  
	  if (![vars containsObject: [mobileObj2 objectAtIndex: 4]])
	    {
	      vf_two = [[mobileObj2 objectAtIndex: 4] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj2 objectAtIndex: 4]];
	      vf_two = gsl_vector_get (v, k);
	    }      
	  
	  if (![vars containsObject: [mobileObj2 objectAtIndex: 5]])
	    {
	      angf_two = [[mobileObj2 objectAtIndex: 5] doubleValue];
	    }
	  else
	    {
	      int k = [vars indexOfObject: [mobileObj2 objectAtIndex: 5]];
	      angf_two = gsl_vector_get (v, k);
	    } 
	  
	  // Build the equations
	  gsl_vector_set (func, nEqu, m1*vi_one*cos(M_PI*(angi_one - angn)/180)
			  + m2*vi_two*cos(M_PI*(angi_two - angn)/180) -
			  m1*vf_one*cos(M_PI*(angf_one - angn)/180) -
			  m2*vf_two*cos(M_PI*(angf_two - angn)/180));
	  gsl_vector_set (func, nEqu + 1, e*
			  (vi_one*cos(M_PI*(angi_one - angn)/180) -
			   vi_two*cos(M_PI*(angi_two - angn)/180)) -
			  (vf_two*cos(M_PI*(angf_two - angn)/180) -
			   vf_one*cos(M_PI*(angf_one - angn)/180)) );
	  gsl_vector_set (func, nEqu + 2, vi_one*sin(M_PI*(angi_one - angn)/180)
			  - vf_one*sin(M_PI*(angf_one - angn)/180) );
	  gsl_vector_set (func, nEqu + 3, vi_two*sin(M_PI*(angi_two - angn)/180)
			  - vf_two*sin(M_PI*(angf_two - angn)/180) );
	  
	  nEqu += 4;
	}
      
    }
  
  // Objects energy and relative motion
  enumOthers = [others objectEnumerator];
  
  while ((object = [enumOthers nextObject]))
    {
      NSNumber *type = [[dictionary objectForKey: object]
			 objectForKey: @"Type"];
      
      switch ([type intValue])
	{
	case 139:
	  {
	    double pw;
	    double mass[4] = {0};
	    double vInitial[4] = {0};
	    double vFinal[4] = {0};
	    double dist[4] = {0};
	    double angs[4] = {0};
	    double kspr[4] = {0};
	    double xsi[4] = {0};
	    double xsf[4] = {0};

	    int countMov = 0, countSpr = 0, count = 0, class;   
	    int vi = 0, vf = 0, d = 0, ang = 0;
            
	    NSMutableArray *otherObjs = [[dictionary objectForKey: object]
					 objectForKey: @"Values"];
	    NSMutableArray *codEnergy = [NSMutableArray array];
	    NSMutableArray *sEnergy = [NSMutableArray array];
	    
	    NSNumber *obj1, *obj2, *obj3, *obj4, *objects;                 
	    NSString *nameObj1 = [otherObjs objectAtIndex: 0];
	    NSString *nameObj2 = [otherObjs objectAtIndex: 1];
	    NSString *nameObj3 = [otherObjs objectAtIndex: 2];
	    NSString *nameObj4 = [otherObjs objectAtIndex: 3];
	    NSMutableArray *sObj, *object1, *object2, *object3, *object4;
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableDictionary *searchName;

	    NSEnumerator *data;
            
	    while ((searchName = [search nextObject]))
	      {
		NSString *n = [[searchName objectForKey: @"Values"]
				objectAtIndex: 0];
		NSString *t = [[searchName objectForKey: @"Titles"]
				objectAtIndex: 0];
                
		if ([t isEqualToString: _(@"Name")]) 
		  { 
		    if ([nameObj1 isEqualToString: n] &&
			[nameObj1 length] > 0 &&
			![nameObj1 isEqualToString: @"0"])
		      {
			obj1 = [searchName objectForKey: @"Type"];
			object1 = [searchName objectForKey: @"Values"];
			[codEnergy addObject: obj1];
			[sEnergy addObject: object1];
		      }
		    
		    if ([nameObj2 isEqualToString: n] &&
			[nameObj2 length] > 0 &&
			![nameObj2 isEqualToString: @"0"])
		      {
			obj2 = [searchName objectForKey: @"Type"];
			object2 = [searchName objectForKey: @"Values"];
			[codEnergy addObject: obj2];
			[sEnergy addObject: object2];
		      }
		    
		    if ([nameObj3 isEqualToString: n] &&
			[nameObj3 length] > 0 &&
			![nameObj3 isEqualToString: @"0"])
		      {
			obj3 = [searchName objectForKey: @"Type"];
			object3 = [searchName objectForKey: @"Values"];
			[codEnergy addObject: obj3];
			[sEnergy addObject: object3];
		      }   
		    
		    if ([nameObj4 isEqualToString: n] &&
			[nameObj4 length] > 0 &&
			![nameObj4 isEqualToString: @"0"])
		      {
			obj4 = [searchName objectForKey: @"Type"];
			object4 = [searchName objectForKey: @"Values"];
			[codEnergy addObject: obj4];
			[sEnergy addObject: object4];
		      }
		  } 
	      }
	    
      
	    data = [codEnergy objectEnumerator]; 
	    while ((objects = [data nextObject]))
	      {
		class = 0;
		sObj = [sEnergy objectAtIndex: count];

		if ([objects intValue] >= 101 &&
		    [objects intValue] <= 107)
		  {
		    switch ([objects intValue])
		      {
		      case 101:
			{
			  vi = 2;
			  vf = 4;
			}
			break;
		      case 102:
		      case 103:
			{
			  vi = 2;
			  vf = 3;
			}
			break;
		      case 104:
			{
			  vi = 3;
			  vf = 4;
			  d = 5;
			  class = 1;
			}
			break;
		      case 105:
			{
			  vi = 3;
			  vf = 4;                                        
			}
			break;     
		      case 106:
		      case 107:
			{
			  vi = 4;
			  vf = 5;
			  d = 6;
			  ang = 2;
			  class =2;
			}
			break;
		      }
		    
		    // Add the data
		    if (![vars containsObject: [sObj objectAtIndex: 1]])
		      {
			mass[countMov] = [[sObj objectAtIndex: 1] doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: [sObj objectAtIndex: 1]];
			mass[countMov] = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [sObj objectAtIndex: vi]])
		      {
			vInitial[countMov] = [[sObj objectAtIndex: vi]
					       doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: [sObj objectAtIndex: vi]];
			vInitial[countMov] = gsl_vector_get (v, k);
		      }   
		    
		    if (![vars containsObject: [sObj objectAtIndex: vf]])
		      {
			vFinal[countMov] = [[sObj objectAtIndex: vf]
					     doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: [sObj objectAtIndex: vf]];
			vFinal[countMov] = gsl_vector_get (v, k);
		      }
		    
		    if (class == 1)
		      {
			if (![vars containsObject: [sObj objectAtIndex: d]])
			  {
			    dist[countMov] = [[sObj objectAtIndex: d]
					       doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [sObj objectAtIndex: d]];
			    dist[countMov] = gsl_vector_get (v, k);
			  }  
			
			angs[countMov] = 90;                                   
		      }
		    
		    if (class == 2)
		      {
			if (![vars containsObject: [sObj objectAtIndex: d]])
			  {
			    dist[countMov] = [[sObj objectAtIndex: d]
					       doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [sObj objectAtIndex: d]];
			    dist[countMov] = gsl_vector_get (v, k);
			  }  
			
			if (![vars containsObject: [sObj objectAtIndex: ang]])
			  {
			    angs[countMov] = [[sObj objectAtIndex: ang]
					       doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [sObj objectAtIndex: ang]];
			    angs[countMov] = gsl_vector_get (v, k);
			  }
		      }
		    
		    count += 1;  
		    countMov += 1;
		  }
		else
		  {
		    if (![vars containsObject: [sObj objectAtIndex: 1]])
		      {
			kspr[countSpr] = [[sObj objectAtIndex: 1] doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: [sObj objectAtIndex: 1]];
			kspr[countSpr] = gsl_vector_get (v, k);
		      }
                                     
		    if (![vars containsObject: [sObj objectAtIndex: 2]])
		      {
			xsi[countSpr] = [[sObj objectAtIndex: 2] doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: [sObj objectAtIndex: 2]];
			xsi[countSpr] = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [sObj objectAtIndex: 3]])
		      {
			xsf[countSpr] = [[sObj objectAtIndex: 3] doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject: [sObj objectAtIndex: 3]];
			xsf[countSpr] = gsl_vector_get (v, k);
		      }
		    
		    count += 1;
		    countSpr += 1;
		  }
	      }
	    
	    // Power data
	    if (![vars containsObject: [otherObjs objectAtIndex: 4]])
	      {
		pw = [[otherObjs objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [otherObjs objectAtIndex: 4]];
		pw = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, 0.5*(mass[0]*vFinal[0]*vFinal[0] +
					     mass[1]*vFinal[1]*vFinal[1] +
					     mass[2]*vFinal[2]*vFinal[2] +
					     mass[3]*vFinal[3]*vFinal[3]) -
			    0.5*(mass[0]*vInitial[0]*vInitial[0] +
				 mass[1]*vInitial[1]*vInitial[1] +
				 mass[2]*vInitial[2]*vInitial[2] +
				 mass[3]*vInitial[3]*vInitial[3]) +
			    gf*(mass[0]*dist[0]*sin(M_PI*angs[0]/180) +
				mass[1]*dist[1]*sin(M_PI*angs[1]/180) +
				mass[2]*dist[2]*sin(M_PI*angs[2]/180) +
				mass[3]*dist[3]*sin(M_PI*angs[3]/180)) +
			    (0.5*kspr[0]*(xsf[0]*xsf[0] - xsi[0]*xsi[0])) +
			    (0.5*kspr[1]*(xsi[1]*xsi[1] - xsf[1]*xsf[1])) +
			    (0.5*kspr[2]*(xsi[2]*xsi[2] - xsf[2]*xsf[2])) +
			    (0.5*kspr[3]*(xsi[3]*xsi[3] - xsf[3]*xsf[3])) -
			    pw );
            
	    nEqu += 1;
	  }
	  break;
	case 143:
	  {
	    int indName;
	    double a_sf, ang_asf, vf_sf, ang_vfsf, d_sf, ang_dsf;

	    int index1, index2, sign1 = 1, sign2 = 1;    
	    double a1, vf1, d1, ang1, a2, vf2, d2, ang2;
            
	    NSMutableArray *otherObjs = [[dictionary objectForKey: object]
					 objectForKey: @"Values"];
	    
	    NSNumber *obj1 = nil, *obj2 = nil;                 
	    NSString *nameObj1 = [otherObjs objectAtIndex: 0];
	    NSString *nameObj2;
	    NSMutableArray *object1 = nil, *object2 = nil;
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableDictionary *searchName;
            
	    while ((searchName = [search nextObject]))
	      {
		NSString *n = [[searchName objectForKey: @"Values"]
				objectAtIndex: 0];
		NSString *t = [[searchName objectForKey: @"Titles"]
				objectAtIndex: 0];
                
		if ([t isEqualToString: _(@"Name")]) 
		  { 
		    if ([nameObj1 isEqualToString: n] &&
			[nameObj1 length] > 0 &&
			![nameObj1 isEqualToString: @"0"])
		      {
			obj1 = [searchName objectForKey: @"Type"];
			object1 = [searchName objectForKey: @"Values"];
			break;
		      }
		  } 
	      }
	    
	    if ( ([obj1 intValue] == 104) || ([obj1 intValue] == 105) )
	      {
		indName = 6;
	      }
	    else
	      {
		indName = 7;
	      }
	    
	    nameObj2 = [object1 objectAtIndex: indName];
	    search = [dictionary objectEnumerator];
            
	    while ((searchName = [search nextObject]))
	      {
		NSString *n = [[searchName objectForKey: @"Values"]
				objectAtIndex: 0];
		NSString *t = [[searchName objectForKey: @"Titles"]
				objectAtIndex: 0];
                
		if ([t isEqualToString: _(@"Name")]) 
		  { 
		    if ([nameObj2 isEqualToString: n] &&
			[nameObj2 length] > 0 &&
			![nameObj2 isEqualToString: @"0"])
		      {
			obj2 = [searchName objectForKey: @"Type"];
			object2 = [searchName objectForKey: @"Values"];
			break;
		      }
		  } 
	      }
	    
	    if (![vars containsObject: [otherObjs objectAtIndex: 1]])
	      {
		a_sf = [[otherObjs objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [otherObjs objectAtIndex: 1]];
		a_sf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [otherObjs objectAtIndex: 2]])
	      {
		ang_asf = [[otherObjs objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [otherObjs objectAtIndex: 2]];
		ang_asf = gsl_vector_get (v, k);
	      }                   
	    
	    if (![vars containsObject: [otherObjs objectAtIndex: 3]])
	      {
		vf_sf = [[otherObjs objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [otherObjs objectAtIndex: 3]];
		vf_sf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [otherObjs objectAtIndex: 4]])
	      {
		ang_vfsf = [[otherObjs objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [otherObjs objectAtIndex: 4]];
		ang_vfsf = gsl_vector_get (v, k);
	      }                   
	    
	    if (![vars containsObject: [otherObjs objectAtIndex: 5]])
	      {
		d_sf = [[otherObjs objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [otherObjs objectAtIndex: 5]];
		d_sf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [otherObjs objectAtIndex: 6]])
	      {
		ang_dsf = [[otherObjs objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [otherObjs objectAtIndex: 6]];
		ang_dsf = gsl_vector_get (v, k);
	      }         
	    
            
	    if ( ([obj1 intValue] == 104) || ([obj1 intValue] == 105) )
	      {
		ang1 = 0;
		index1 = 2;
	      }
	    else
	      {
		if (![vars containsObject: [object1 objectAtIndex: 2]])
		  {
		    ang1 = [[object1 objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [object1 objectAtIndex: 2]];
		    ang1 = gsl_vector_get (v, k);
		  } 
		
		index1 = 3;
                
		if ([obj1 intValue] == 107)
		  {
                    sign1 = -1;
		  }
	      }
	    
	    if (![vars containsObject: [object1 objectAtIndex: index1]])
	      {
		a1 = [[object1 objectAtIndex: index1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [object1 objectAtIndex: index1]];
		a1 = gsl_vector_get (v, k);
	      }
	    
	    index1 += 2;  
	    if (![vars containsObject: [object1 objectAtIndex: index1]])
	      {
		vf1 = [[object1 objectAtIndex: index1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [object1 objectAtIndex: index1]];
		vf1 = gsl_vector_get (v, k);
	      } 
	    
	    index1 += 1;
	    if (![vars containsObject: [object1 objectAtIndex: index1]])
	      {
		d1 = [[object1 objectAtIndex: index1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [object1 objectAtIndex: index1]];
		d1 = gsl_vector_get (v, k);
	      }       
	    
	    if ( ([obj2 intValue] == 104) || ([obj2 intValue] == 105) )
	      {
		ang2 = 0;
		index2 = 2;
	      }
	    else
	      {
		if (![vars containsObject: [object2 objectAtIndex: 2]])
		  {
		    ang2 = [[object2 objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [object2 objectAtIndex: 2]];
		    ang2 = gsl_vector_get (v, k);
		  } 
		
		index2 = 3;
                
		if ([obj2 intValue] == 107)
		  {
                    sign2 = -1;
		  }
	      }
	    
	    if (![vars containsObject: [object2 objectAtIndex: index2]])
	      {
		a2 = [[object2 objectAtIndex: index2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [object2 objectAtIndex: index2]];
		a2 = gsl_vector_get (v, k);
	      }     
	    
	    index2 += 2;
	    if (![vars containsObject: [object2 objectAtIndex: index2]])
	      {
		vf2 = [[object2 objectAtIndex: index2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [object2 objectAtIndex: index2]];
		vf2 = gsl_vector_get (v, k);
	      } 
	    
	    index2 += 1;
	    if (![vars containsObject: [object2 objectAtIndex: index2]])
	      {
		d2 = [[object2 objectAtIndex: index2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [object2 objectAtIndex: index2]];
		d2 = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, sign1*a1*cos(M_PI*ang1/180) +
			    sign2*a2*cos(M_PI*ang2/180) -
			    a_sf*cos(M_PI*ang_asf/180));
	    gsl_vector_set (func, nEqu + 1, a1*sin(M_PI*ang1/180) +
			    a2*sin(M_PI*ang2/180) -
			    a_sf*sin(M_PI*ang_asf/180));
	    gsl_vector_set (func, nEqu + 2, sign1*vf1*cos(M_PI*ang1/180) +
			    sign2*vf2*cos(M_PI*ang2/180) -
			    vf_sf*cos(M_PI*ang_vfsf/180));
	    gsl_vector_set (func, nEqu + 3, vf1*sin(M_PI*ang1/180) +
			    vf2*sin(M_PI*ang2/180) -
			    vf_sf*sin(M_PI*ang_vfsf/180));
	    gsl_vector_set (func, nEqu + 4, sign1*d1*cos(M_PI*ang1/180) +
			    sign2*d2*cos(M_PI*ang2/180) -
			    d_sf*cos(M_PI*ang_dsf/180));
	    gsl_vector_set (func, nEqu + 5, d1*sin(M_PI*ang1/180) +
			    d2*sin(M_PI*ang2/180) -
			    d_sf*sin(M_PI*ang_dsf/180));
            
	    nEqu += 6;
	  }
	  break;
	}
    }   
  
  // Objects momentum
  enumImp = [impulse objectEnumerator];
  
  while ((object = [enumImp nextObject]))
    {
      double m = 0, vi = 0, vf = 0, imp, ang, vi_mob = 0, angi = 0,
	vf_mob = 0, angf = 0, f_imp;
      
      NSEnumerator *search = [dictionary objectEnumerator];
      NSMutableArray *objectData = nil;
      NSMutableArray *data = [[dictionary objectForKey: object]
			       objectForKey: @"Values"];
      NSMutableDictionary *searchName;
      NSString *nameObj = [impulseObjs objectAtIndex: countImp];
      
      if (![vars containsObject: [data objectAtIndex: 1]])
	{
	  imp = [[data objectAtIndex: 1] doubleValue];
	}
      else
	{
	  int k = [vars indexOfObject: [data objectAtIndex: 1]];
	  imp = gsl_vector_get (v, k);
	}
      
      if (![vars containsObject: [data objectAtIndex: 2]])
	{
	  ang = [[data objectAtIndex: 2] doubleValue];
	}
      else
	{
	  int k = [vars indexOfObject: [data objectAtIndex: 2]];
	  ang = gsl_vector_get (v, k);
	}
      
      if (![vars containsObject: [data objectAtIndex: 3]])
	{
	  f_imp = [[data objectAtIndex: 3] doubleValue];
	}
      else
	{
	  int k = [vars indexOfObject: [data objectAtIndex: 3]];
	  f_imp = gsl_vector_get (v, k);
	}
      
      while ((searchName = [search nextObject]))
	{
	  NSString *n = [[searchName objectForKey: @"Values"] objectAtIndex: 0];
	  NSString *t = [[searchName objectForKey: @"Titles"] objectAtIndex: 0];
          
	  if ([t isEqualToString: _(@"Name")] &&
	      [nameObj isEqualToString: n])
	    {
	      typeObj = [searchName objectForKey: @"Type"];
	      objectData = [searchName objectForKey: @"Values"];
	      break;
	    }
	}
      
      switch ([typeObj intValue])
	{
	case 101:
	  {
	    if (![vars containsObject: [objectData objectAtIndex: 1]])
	      {
		m = [[objectData objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 1]];
		m = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 2]])
	      {
		vi_mob = [[objectData objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 2]];
		vi_mob = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 3]])
	      {
		angi = [[objectData objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 3]];
		angi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 4]])
	      {
		vf_mob = [[objectData objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 4]];
		vf_mob = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 5]])
	      {
		angf = [[objectData objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 5]];
		angf = gsl_vector_get (v, k);
	      }
	  }
	  break;
	case 102:
	case 103:
	  {
	    if (![vars containsObject: [objectData objectAtIndex: 1]])
	      {
		m = [[objectData objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 1]];
		m = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 2]])
	      {
		vi = [[objectData objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 2]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 3]])
	      {
		vf = [[objectData objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 3]];
		vf = gsl_vector_get (v, k);
	      }
	  }
	  break;
	case 104:
	case 105:
	  {
	    if (![vars containsObject: [objectData objectAtIndex: 1]])
	      {
		m = [[objectData objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 1]];
		m = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 3]])
	      {
		vi = [[objectData objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 3]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 4]])
	      {
		vf = [[objectData objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 4]];
		vf = gsl_vector_get (v, k);
	      }
	  }
	  break;
	case 106:
	case 107:
	  {
	    if (![vars containsObject: [objectData objectAtIndex: 1]])
	      {
		m = [[objectData objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 1]];
		m = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 4]])
	      {
		vi = [[objectData objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 4]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 5]])
	      {
		vf = [[objectData objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 5]];
		vf = gsl_vector_get (v, k);
	      }
	  }
	  break;
	}
      
      // Build the equations
      if ([typeObj intValue] == 101)
	{
	  gsl_vector_set (func, nEqu, m*vf_mob*cos(M_PI*angf/180) -
			  m*vi_mob*cos(M_PI*angi/180) -
			  imp*cos(M_PI*ang/180) );
	  gsl_vector_set (func, nEqu + 1, m*vf_mob*sin(M_PI*angf/180) -
			  m*vi_mob*sin(M_PI*angi/180) -
			  imp*sin(M_PI*ang/180) );
	  gsl_vector_set (func, nEqu + 2, (sqrt(imp*imp)/tf) - f_imp);
	  
	  nEqu += 3;
	}
      else
	{
	  gsl_vector_set (func, nEqu, m*vf - m*vi - imp);
	  gsl_vector_set (func, nEqu + 1, (sqrt(imp*imp)/tf) - f_imp);
	  
	  nEqu += 2;
	}
      
      countImp += 1;
    }
  
  // Objects power
  enumPow = [power objectEnumerator];
  
  while ((object = [enumPow nextObject]))
    {
      double m = 0, vi = 0, vf = 0, d = 0, ang = 90, pw;
      
      NSEnumerator *search = [dictionary objectEnumerator];
      NSMutableArray *objectData = nil;
      NSMutableArray *data = [[dictionary objectForKey: object]
			       objectForKey: @"Values"];
      NSMutableDictionary *searchName;
      NSString *nameObj = [powerObjs objectAtIndex: countPow];
      
      if (![vars containsObject: [data objectAtIndex: 1]])
	{
	  pw = [[data objectAtIndex: 1] doubleValue];
	}
      else
	{
	  int k = [vars indexOfObject: [data objectAtIndex: 1]];
	  pw = gsl_vector_get (v, k);
	}
      
      
      while ((searchName = [search nextObject]))
	{
	  NSString *n = [[searchName objectForKey: @"Values"] objectAtIndex: 0];
	  NSString *t = [[searchName objectForKey: @"Titles"] objectAtIndex: 0];
          
	  if ([t isEqualToString: _(@"Name")] &&
	      [nameObj isEqualToString: n])
	    {
	      typePot = [searchName objectForKey: @"Type"];
	      objectData = [searchName objectForKey: @"Values"];
	      break;
	    }
	}
      
      
      switch ([typePot intValue])
	{
	case 104:
	case 105:
	  {
	    if (![vars containsObject: [objectData objectAtIndex: 1]])
	      {
		m = [[objectData objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 1]];
		m = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 3]])
	      {
		vi = [[objectData objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 3]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 4]])
	      {
		vf = [[objectData objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 4]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 5]])
	      {
		d = [[objectData objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 5]];
		d = gsl_vector_get (v, k);
	      }
	  }
	  break;
	case 106:
	case 107:
	  {
	    if (![vars containsObject: [objectData objectAtIndex: 1]])
	      {
		m = [[objectData objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 1]];
		m = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 4]])
	      {
		vi = [[objectData objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 4]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 5]])
	      {
		vf = [[objectData objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 5]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 2]])
	      {
		ang = [[objectData objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 2]];
		ang = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [objectData objectAtIndex: 6]])
	      {
		d = [[objectData objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [objectData objectAtIndex: 6]];
		d = gsl_vector_get (v, k);
	      }
	  }
	  break; 
	}
      
      
      // Build the equations
      
      if ([typePot intValue] == 105)
	{
	  gsl_vector_set (func, nEqu, (0.5*m*(vf*vf - vi*vi))/tf - pw);
	}
      else
	{
	  gsl_vector_set (func, nEqu, (0.5*m*(vf*vf - vi*vi) +
				       m*gf*d*sin(M_PI*ang/180))/tf - pw);
	}
      
      nEqu += 1;
      countPow += 1;
    }                        
  
  return GSL_SUCCESS;
}

@interface FLDynamics (Private)
- (void) makeSystem;
@end

@implementation FLDynamics (Private)
- (void) makeSystem
{
  int increase = 1;
  double newValue;
  BOOL follow;
  
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;
  
  int state = 0, stateInt, k, length;
  int nvar  = [vars count];
  const size_t n = nvar;
  double par;
  NSString *message;
  size_t iter;
  
  gsl_vector *x = gsl_vector_alloc (n);
  int Tindex;
  int countRes = 0;
  id anObj;
  NSMutableArray *results = [NSMutableArray array];
  NSEnumerator *varCount;
  id dataSign;
  NSNumber *typeOther;
  NSMutableArray *verifSign;
  NSEnumerator *signObjs;
  
  // Generator of random numbers
  const gsl_rng_type * Y;
  gsl_rng * r;
  gsl_rng_env_setup();
  Y = gsl_rng_default;
  r = gsl_rng_alloc (Y);
  
  // Search the solution
  do
    {
      gsl_multiroot_function f = {&buildSystem, n, (__bridge void *)(self)};

      iter = 0;
      for (k = 0; k < nvar; k++)
	{
	  if ( increase <= 50 )
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
      if ( (stateInt) && (increase < 90) )
	{
	  increase += 1;
	  follow = YES;
	}
      else
	{
	  if (varT == 1)
            {
	      Tindex = [vars indexOfObject: timeVar];
	      if ( ((gsl_vector_get (s->x, Tindex)) < 0) && (increase < 90) )
		{
		  increase += 1;
		  follow = YES;
		}
	      else
		{
		  follow = NO;
		}
            }
	  else
            {
	      follow = NO;
            }
	}
      // Verification has terminated
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
  signObjs = [dictionary objectEnumerator];
  
  int par1, par2, par3, w, g, sign;
  double nv, nf, d1, d2, d3, d4;
  while ((dataSign = [signObjs nextObject]))
    {
      par1 = 0;
      par2 = 0;
      par3 = 0;
      w = 0;
      g = 0;
      sign = 1;
      nv = 0;
      nf = 0;
      d1 = 0;
      d2 = 0;
      d3 = 0;
      d4 = 0;

      typeOther = [dataSign objectForKey: @"Type"];
      
      switch ([typeOther intValue])
	{
	case 101:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the sign for vi
	    if ([vars containsObject: [verifSign objectAtIndex: 2]] &&
		[vars containsObject: [verifSign objectAtIndex: 3]] )
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 2]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
			       withObject: [NSNumber numberWithDouble: nv]];
		    par1 = 1;
		  }
	      }
	    
	    // Correct the initial angle
	    if ([vars containsObject: [verifSign objectAtIndex: 3]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 3]];
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
	    
	    // Correct the vf
	    if ([vars containsObject: [verifSign objectAtIndex: 4]] &&
		[vars containsObject: [verifSign objectAtIndex: 5]] )
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 4]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
			       withObject: [NSNumber numberWithDouble: nv]];
		    par2 = 1;
		  }
	      }
	    
	    // Correct the final angle
	    if ([vars containsObject: [verifSign objectAtIndex: 5]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 5]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( par2 == 1 )
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
	  }
	  break;
	case 104 ... 105:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];

	    // Correct the sign of final velocity
	    if ([vars containsObject: [verifSign objectAtIndex: 4]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 4]];
		nf = [[results objectAtIndex: w] doubleValue];

		// Get a
		if ([vars containsObject: [verifSign objectAtIndex: 2]])
		  {
		    g = [vars indexOfObject: [verifSign objectAtIndex: 2]];
		    d1 = [[results objectAtIndex: g] doubleValue];
		  }
		else
		  {
		    d1 = [[verifSign objectAtIndex: 2] doubleValue];
		  }

		// Get vi
		if ([vars containsObject: [verifSign objectAtIndex: 3]])
		  {
		    g = [vars indexOfObject: [verifSign objectAtIndex: 3]];
		    d2 = [[results objectAtIndex: g] doubleValue];
		  }
		else
		  {
		    d2 = [[verifSign objectAtIndex: 3] doubleValue];
		  }

		// Get time
		if (varT == 1)
		  {
		    g = [vars indexOfObject: timeVar];
		    d3 = [[results objectAtIndex: g] doubleValue];
		  }
		else
		  {
		    d3 = timeDat;
		  }

		// Calculate the final velocity, again
		// vf = vi + a*t
		d4 = d2 + d1*d3;

		// Check
		if ( (nf > 0 && d4 < 0) ||
		     (nf < 0 && d4 > 0) )
		  {
		    // Change the sign
		    nf *= -1;

		    [results replaceObjectAtIndex: w
			     withObject: [NSNumber numberWithDouble: nf]];
		  }
	      }
	  }
	  break;
	case 106 ... 107:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];

	    // Correct the sign of final velocity
	    if ([vars containsObject: [verifSign objectAtIndex: 5]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 5]];
		nf = [[results objectAtIndex: w] doubleValue];

		// Get a
		if ([vars containsObject: [verifSign objectAtIndex: 3]])
		  {
		    g = [vars indexOfObject: [verifSign objectAtIndex: 3]];
		    d1 = [[results objectAtIndex: g] doubleValue];
		  }
		else
		  {
		    d1 = [[verifSign objectAtIndex: 3] doubleValue];
		  }

		// Get vi
		if ([vars containsObject: [verifSign objectAtIndex: 4]])
		  {
		    g = [vars indexOfObject: [verifSign objectAtIndex: 4]];
		    d2 = [[results objectAtIndex: g] doubleValue];
		  }
		else
		  {
		    d2 = [[verifSign objectAtIndex: 4] doubleValue];
		  }

		// Get time
		if (varT == 1)
		  {
		    g = [vars indexOfObject: timeVar];
		    d3 = [[results objectAtIndex: g] doubleValue];
		  }
		else
		  {
		    d3 = timeDat;
		  }

		// Calculate the final velocity, again
		// vf = vi + a*t
		d4 = d2 + d1*d3;

		// Check
		if ( (nf > 0 && d4 < 0) ||
		     (nf < 0 && d4 > 0) )
		  {
		    // Change the sign
		    nf *= -1;

		    [results replaceObjectAtIndex: w
			     withObject: [NSNumber numberWithDouble: nf]];
		  }
	      }
	    
	    // Correct the angle
	    if ([vars containsObject: [verifSign objectAtIndex: 2]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 2]];
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
	case 109 ... 112:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the force
	    if ([vars containsObject: [verifSign objectAtIndex: 0]] &&
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
	case 125 ... 128:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the angle of the contact
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
	case 138:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the angle of the collision
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
	case 140:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the impulse
	    if ([vars containsObject: [verifSign objectAtIndex: 1]] &&
		[vars containsObject: [verifSign objectAtIndex: 2]] )
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
			       withObject: [NSNumber numberWithDouble: nv]];
		    par1 = 1;
		  }
	      }
	    
	    // Correct the angle of the impulse
	    if ([vars containsObject: [verifSign objectAtIndex: 2]])
	      {
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
	  }
	  break;   
	case 143:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the acceleration
	    if ([vars containsObject: [verifSign objectAtIndex: 1]] &&
		[vars containsObject: [verifSign objectAtIndex: 2]] )
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 1]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
			       withObject: [NSNumber numberWithDouble: nv]];
		    par1 = 1;
		  }
	      }
	    
	    // Correct the angle of the acceleration
	    if ([vars containsObject: [verifSign objectAtIndex: 2]])
	      {
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
	    
	    // Correct data vf
	    if ([vars containsObject: [verifSign objectAtIndex: 3]] &&
		[vars containsObject: [verifSign objectAtIndex: 4]] )
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 3]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
			       withObject: [NSNumber numberWithDouble: nv]];
		    par2 = 1;
		  }
	      }
	    
	    // Correct the vf angle
	    if ([vars containsObject: [verifSign objectAtIndex: 4]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 4]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( par2 == 1 )
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
	    
	    // Correct data d
	    if ([vars containsObject: [verifSign objectAtIndex: 5]] &&
		[vars containsObject: [verifSign objectAtIndex: 6]] )
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 5]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
			       withObject: [NSNumber numberWithDouble: nv]];
		    par3 = 1;
		  }
	      }
	    
	    // Correct the angle of data d
	    if ([vars containsObject: [verifSign objectAtIndex: 6]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 6]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( par3 == 1 )
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
	  }
	  break;
	}
    }
  
  // Print the results
  [self printUnknowns: vars withResults: results];
  
  // Print the calculus state
  message = [NSString stringWithFormat: [errors objectAtIndex: 10],
		      gsl_strerror (state)];
  length = [[[self viewer] textStorage] length];
  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: message];
  
  // If there are frictions applied, display an advertise
  if (messageFr)
    {
      length = [[[self viewer] textStorage] length];
      [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				   withString: [errors objectAtIndex: 11]];
    }
  
  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  gsl_rng_free (r);
}
@end

@implementation FLDynamics

- (id) init
{
  NSBundle *messages;
  self = [super init];
  
  vars = [NSMutableArray new];
  codObjects = [NSMutableArray new];
  forceObjs = [NSMutableArray new];
  forceTypes = [NSMutableArray new];
  codCol = [NSMutableArray new];
  codObjCol = [NSMutableArray new];
  typeCol = [NSMutableArray new];
  codContainedObjects = [NSMutableArray new];
  codRel = [NSMutableArray new];
  others = [NSMutableArray new];
  energyObjs = [NSMutableArray new];
  impulse = [NSMutableArray new];
  impulseObjs = [NSMutableArray new];
  power = [NSMutableArray new];
  powerObjs = [NSMutableArray new];
  dictionary = [NSMutableDictionary new];
  
  // Build the array of messages
  messages = [NSBundle mainBundle];
  errors = [[NSArray alloc] initWithContentsOfFile:
			  [messages pathForResource: @"dynamicsMessages"
					     ofType: @"plist"]];
  
  return self;
}

- (void) makeEquationsForData: (NSMutableDictionary *)list
{
  int width, height, totalCells; //For handle the chalkboard size
  int length, numEquations = 0, numForces = 0;
  NSUInteger sys = [self system];

  // To search the applied forces
  int k; 
  NSButton *cell;
  // To handle errors
  int objContained = 0, w = 0;

  BOOL error = NO;
  BOOL errorName = NO;
  BOOL errorForce = NO;
  BOOL errorCol = NO;
  BOOL errorImp = NO;
  BOOL errorPow = NO;
  BOOL errorMovRel = NO;
  BOOL errorObjMovRel = NO;
  BOOL errorMRO = NO;
  BOOL errorBlockRel = NO;
  BOOL errorContent = NO;
  NSNumber *identifier, *code;

  NSMutableArray *codForces = [NSMutableArray array];
  NSMutableArray *codForcesObj = [NSMutableArray array];
  NSMutableArray *namesObjects = [NSMutableArray array];
  NSMutableArray *namesContainedObjects = [NSMutableArray array];
  NSMutableArray *movRelObj = [NSMutableArray array];
  NSMutableArray *objectRel = [NSMutableArray array];
  NSMutableArray *objectRel_104 = [NSMutableArray array];

  NSArray *objectsOrder = [self cells];

  NSArray *keys;
  NSEnumerator *enumerator;

  messageFr = NO;
  
  varT = 0;
  varG = 0;
  timeDat = 0;
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
      // To check the forces applied to each object
      int pos;
      NSNumber *ident;
      NSEnumerator *search;
      //-----

      NSArray *titles = [[dictionary objectForKey: code]
			  objectForKey: @"Titles"];
      NSMutableArray *info = [[dictionary objectForKey: code]
			       objectForKey: @"Data"];
      NSMutableArray *values = [[dictionary objectForKey: code]
				 objectForKey: @"Values"];

      identifier = [[dictionary objectForKey: code] objectForKey: @"Type"];
      [values removeAllObjects];
      
      // Count the amount of variables on the system
      for (x = 0; x < [info count]; x++)
	{
	  NSString *data = [info objectAtIndex: x];
	  NSString *title = [[titles objectAtIndex: x] description];
	  
	  if (![self isNumericDataTheString: data] &&
	      ![title isEqualToString: _(@"Name")] &&
	      ![title isEqualToString: _(@"Object 1")] &&
	      ![title isEqualToString: _(@"Object 2")] &&
	      ![title isEqualToString: _(@"Object 3")] &&
	      ![title isEqualToString: _(@"Object 4")] &&
	      ![title isEqualToString: _(@"Object")] &&
	      ![title isEqualToString: _(@"Relative to")])
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
		  NSString *varFactor = nil;
		  
		  // Check the type of variable
		  if ([title hasPrefix: @"m"])
		    {
		      if (sys == 0)
			{ varFactor = @"kg"; }
		      else
			{ varFactor = @"slug"; }
		    }
		  else if ([title hasPrefix: @"f"] || [title hasPrefix: @"N"])
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
		  else if ([title hasPrefix: @"g"] || [title hasPrefix: @"a"])
		    {
		      if (sys == 0)
			{ varFactor = @"m/s2"; }
		      else
			{ varFactor = @"ft/s2"; }
		    }
		  else if ([title hasPrefix: @"x"] || [title hasPrefix: @"y"] ||
			   [title hasPrefix: @"d"])
		    {
		      if (sys == 0)
			{ varFactor = @"m"; }
		      else
			{ varFactor = @"ft"; }
		    }
		  else if ([title hasPrefix: @"v"]) 
		    {
		      if (sys == 0)
			{ varFactor = @"m/s"; }
		      else
			{ varFactor = @"ft/s"; }
		    }
		  else if ([title hasPrefix: @"t"])
		    {
		      varFactor = @"s";
		    }
		  else if ([title hasPrefix: @"k"])
		    {
		      if (sys == 0)
			{ varFactor = @"N/m"; }
		      else
			{ varFactor = @"lb/ft"; }
		    }
		  else if ([title hasPrefix: @"W"] || [title hasPrefix: @"E"])
		    {
		      if (sys == 0)
			{ varFactor = @"J"; }
		      else
			{ varFactor = @"lb*ft"; }
		    }
		  else if ([title hasPrefix: @"I"])
		    {
		      if (sys == 0)
			{ varFactor = @"N*s"; }
		      else
			{ varFactor = @"lb*s"; }
		    }
		  else if ([title hasPrefix: @"P"])
		    {
		      if (sys == 0)
			{ varFactor = @"W"; }
		      else
			{ varFactor = @"lb*ft/s"; }
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
	      [values addObject: [info objectAtIndex: x]];
	    }
	}    
      // Here ends the count of variables
      
      if (error || errorName || errorContent)
	break;
      
      // Add a message of warning if there are friction forces
      if ([identifier intValue] >= 117 && [identifier intValue] <= 132)
	{
	  messageFr = YES;
	} 
      
      // Verify the number of equations
      switch ([identifier intValue])
	{
	case 100:
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
	    
	    if (![self isNumericDataTheString: [values objectAtIndex: 1]])
	      {
		timeVar = [values objectAtIndex: 1];
		varT = 1;
	      }
	    else
	      {
		timeDat = [[values objectAtIndex: 1] doubleValue];
		varT = 2;
	      }
	    
	  }
	  break;
	case 101:
	  {
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
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
	    else
	      {
		[namesObjects addObject: [NSNull null]];
	      }
	    
	    [codContainedObjects addObject: code];
	  }
	  break;
	case 102:
	  {
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
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
	    else
	      {
		[namesObjects addObject: [NSNull null]];
	      }
	    
	    [codContainedObjects addObject: code];
	  }
	  break;
	case 103:
	  {
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
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
	    else
	      {
		[namesObjects addObject: [NSNull null]];
	      }
	    
	    [codContainedObjects addObject: code];   
	  }
	  break;
	case 104:
	  {
	    numEquations += 4;
	    [codObjects addObject: code];
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
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
	    else
	      {
		[namesObjects addObject: [NSNull null]];
	      }
	    
	    [codContainedObjects addObject: code];

	    if (![[values objectAtIndex: 6] isEqualToString: @"sf"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 6]];
		[objectRel_104 addObject: [values objectAtIndex: 6]];
	      }
	  }
	  break;
	case 105:
	  {
	    numEquations += 4;
	    [codObjects addObject: code];
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
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
	    else
	      {
		[namesObjects addObject: [NSNull null]];
	      }
	    
	    [codContainedObjects addObject: code];

	    if (![[values objectAtIndex: 6] isEqualToString: @"sf"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 6]];
		[objectRel addObject: [values objectAtIndex: 6]];
	      }
	  }
	  break;
	case 106:
	  {
	    numEquations += 4;
	    [codObjects addObject: code];
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
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
	    else
	      {
		[namesObjects addObject: [NSNull null]];
	      }
	    
	    [codContainedObjects addObject: code];

	    if (![[values objectAtIndex: 7] isEqualToString: @"sf"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 7]];
		[objectRel addObject: [values objectAtIndex: 7]];
	      }
	  }
	  break;
	case 107:
	  {
	    numEquations += 4;
	    [codObjects addObject: code];
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
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
	    else
	      {
		[namesObjects addObject: [NSNull null]];
	      }
	    
	    [codContainedObjects addObject: code];
  
	    if (![[values objectAtIndex: 7] isEqualToString: @"sf"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 7]];
		[objectRel addObject: [values objectAtIndex: 7]];
	      }
	  }
	  break;                
	case 108:
	  {
	    numEquations += 2;
	    [codObjects addObject: code];
	  }
	  break;
	case 109 ... 124:
	  {
	    numForces += 1;
	    [codForces addObject: code];
	  }
	  break;
	case 125 ... 132:
	  {
	    numForces += 1;
	    [codForces addObject: code];                
	  }
	  break;
	case 133 ... 136:
	  {
	    numEquations += 1;
	    [codObjects addObject: code];
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
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
	    else
	      {
		[namesObjects addObject: [NSNull null]];
	      }
	    
	    [codContainedObjects addObject: code];
	  }
	  break;
	case 137:
	  {
	    numEquations += 1;
	    [codRel addObject: code];
	  }
	  break;
	case 138:
	  {
	    numEquations += 4;
	    [codCol addObject: code];
	    [codObjCol addObject: [NSArray arrayWithObjects:
					     [values objectAtIndex: 0],
				      [values objectAtIndex: 1], nil]];
	    [namesContainedObjects addObject: [values objectAtIndex: 0]];
	    [namesContainedObjects addObject: [values objectAtIndex: 1]];
	    
	    if (![[values objectAtIndex: 0] length] > 0 ||
		[[values objectAtIndex: 0] isEqualToString: @"0"] ||
		![[values objectAtIndex: 1] length] > 0 ||
		[[values objectAtIndex: 1] isEqualToString: @"0"])
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 139:
	  {
	    numEquations += 1;
	    [others addObject: code];

	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
		[energyObjs addObject: [values objectAtIndex: 0]];
	      }

	    if ([[values objectAtIndex: 1] length] > 0 &&
		![[values objectAtIndex: 1] isEqualToString: @"0"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
		[energyObjs addObject: [values objectAtIndex: 1]];
	      }

	    if ([[values objectAtIndex: 2] length] > 0 &&
		![[values objectAtIndex: 2] isEqualToString: @"0"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 2]];
		[energyObjs addObject: [values objectAtIndex: 2]];
	      }

	    if ([[values objectAtIndex: 3] length] > 0 &&
		![[values objectAtIndex: 3] isEqualToString: @"0"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 3]];
		[energyObjs addObject: [values objectAtIndex: 3]];
	      }
	  }
	  break;
	case 140:
	  {
	    numEquations += 3;
	    [impulse addObject: code];

	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      { 
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
		[impulseObjs addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 141:
	  {
	    numEquations += 1;
	    [power addObject: code];

	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      { 
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
		[powerObjs addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 143:
	  {
	    numEquations += 6;
	    [others addObject: code];

	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      { 
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
		[movRelObj addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	}
      // Here ends the verify for equations
      
      // Search the applied forces in each object
      search = [[self cells] objectEnumerator];
      
      if ([identifier intValue] >= 104 && [identifier intValue] <= 108)
	{
	  int horizontalForces = 0, verticalForces = 0;   
	  NSNumber *forceType;  
	  NSEnumerator *verif;
	  NSMutableArray *forces = [NSMutableArray array];

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
		    { [forces addObject: ident]; }
		}
	      
	      if (pos - (width - 1) >= 0)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - (width - 1)] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident]; }
		}
	      
	      if (pos - width >= 0)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos - width] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident]; }
		}
	      
	      if (pos - (width + 1) >= 0)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - (width + 1)] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident]; }
		}
	      
	      if (pos + 1 <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos + 1] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident]; }
		}
	      
	      if (pos + (width - 1) <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + (width - 1)] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident]; }
		}
	      
	      if (pos + width <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos + width] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident]; }
		}
	      
	      if (pos + (width + 1) <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + (width + 1)] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident]; }
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
			{ [forces addObject: ident]; }
		    }
		  
		  if (pos - width >= 0)
		    {
		      ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos - width] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		  
		  if (pos + 1 <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
				  [[objectsOrder objectAtIndex: pos + 1] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		  
		  if (pos + width <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
   			     [[objectsOrder objectAtIndex: pos + width] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		  
		  if (pos + (width + 1) <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
		       [[objectsOrder objectAtIndex: pos + (width + 1)] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		}
	      else
		{
		  if (pos - 1 >= 0)
		    {
		      ident = [NSNumber numberWithInt:
				  [[objectsOrder objectAtIndex: pos - 1] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		  
		  if (pos - width >= 0)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos - width] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		  
		  if (pos - (width + 1) >= 0)
		    {
		      ident = [NSNumber numberWithInt:
			 [[objectsOrder objectAtIndex: pos - (width + 1)] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		  
		  if (pos + (width - 1) <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			 [[objectsOrder objectAtIndex: pos + (width - 1)] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		  
		  if (pos + width <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos + width] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; }
		    }
		}
	    }
	  
	  /* Check the type of applied forces and correct the number
	     of equations */
	  if ([identifier intValue] == 106 && [forces count] > 0)
	    {
	      [forceTypes addObject: [NSNumber numberWithInt: 1]];
	      
	      verif = [forces objectEnumerator];
              while ((forceType = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: forceType]
			     objectForKey: @"Type"] intValue];

		  if ( (t >= 131) || ((t >= 100) && (t <= 108)) ||
		       ((t >= 119) && (t <= 124)) )
		    {
		      errorForce = YES;
		    }
		  
		}
	    }
	  
	  if ([identifier intValue] == 107 && [forces count] > 0)
	    {
	      [forceTypes addObject: [NSNumber numberWithInt: 1]];
	      
	      verif = [forces objectEnumerator];
              while ((forceType = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: forceType]
			     objectForKey: @"Type"] intValue];

		  if ( (t >= 131) || (t == 117) || (t == 118) ||
		       ((t >= 100) && (t <= 108)) ||
		       ((t >= 121) && (t <= 124)) )
		    {
		      errorForce = YES;
		    }
		  
		}
	    }
	  
	  if ([identifier intValue] == 104 && [forces count] > 0)
	    {
	      verif = [forces objectEnumerator];
              while ((forceType = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: forceType]
			     objectForKey: @"Type"] intValue];

		  if ( (t == 115) || (t == 116) || (t == 123) || (t == 124) ||
		       (t == 127) )
		    {
		      verticalForces += 1;
		    }
		  
		  if ( (t >= 133) || (t == 129) || (t == 130) ||
		       ((t >= 100) && (t <= 108)) ||
		       ((t >= 117) && (t <= 120)) )
		    {
		      errorForce = YES;
		    } 
		  
		}
              
              if ([forces count] == verticalForces)
		{
		  numEquations -= 1;
		  [forceTypes addObject: [NSNumber numberWithInt: 3]];
		}
              else
		{
		  [forceTypes addObject: [NSNumber numberWithInt: 1]];
		}
              
	    }
	  
	  if ([identifier intValue] == 105 && [forces count] > 0)
	    {
	      verif = [forces objectEnumerator];
              while ((forceType = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: forceType]
			     objectForKey: @"Type"] intValue];

		  if ( (t >= 131) || ((t >= 100) && (t <= 108)) ||
		       ((t >= 117) && (t <= 120)) )
		    {
		      errorForce = YES;
		    } 
		}
              
              [forceTypes addObject: [NSNumber numberWithInt: 1]];
	    }         
	  
	  if ([identifier intValue] == 108 && [forces count] > 0)
	    {
	      verif = [forces objectEnumerator];
              while ((forceType = [verif nextObject]))
		{
		  int t = [[[dictionary objectForKey: forceType]
			     objectForKey: @"Type"] intValue];

		  if ( (t == 113) || (t == 114) )
		    {
		      horizontalForces += 1;
		    }
		  
		  if ( (t == 115) || (t == 116) )
		    {
		      verticalForces += 1;
		    }
		  
		  if ( (t >= 117) || ((t >= 100) && (t <= 108)) )
		    {
		      errorForce = YES;
		    }
		  
		}
              
              if ([forces count] == horizontalForces)
		{
		  numEquations -= 1;
		  [forceTypes addObject: [NSNumber numberWithInt: 2]];
		}
              else
		{
                  if ([forces count] == verticalForces)
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
	  
	  if ([forces count] > 0)
	    {
	      [forceObjs addObject: forces];
	    }       
	  else
	    {
	      if ([identifier intValue] == 104)
		{
		  [forceObjs addObject: forces]; 
		  [forceTypes addObject: [NSNumber numberWithInt: 3]];
		  numEquations -= 1;
		}
	    }
	}
      else
	{
	  int sign = 0;
	  NSMutableArray *forces = [NSMutableArray array];
	  
	  if ([identifier intValue] >= 133 && [identifier intValue] <= 136)
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
	      
	      if ([identifier intValue] == 133)
		{
		  rForce = width - 1;
		}
	      else
		{ 
		  if ([identifier intValue] == 134)
		    {
		      rForce = width + 1;
		    }
		  else
		    {
		      if ([identifier intValue] == 135)
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
			{ [forces addObject: ident]; 
			  sign = 1; }
                    }
		  
		  if (pos + rForce <= totalCells)
                    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos + rForce] tag]];

		      if ([ident intValue] != 0)
			{ [forces addObject: ident]; 
			  sign = -1; }
                    }
                }
              else
                {
		  if (pos%width == 0)
		    { 
		      if ((pos - rForce >= 0) &&
			  ((rForce != 1) || (rForce != (width + 1))))
			{
			  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos - rForce] tag]];

			  if ([ident intValue] != 0)
			    { [forces addObject: ident]; 
			      sign = 1; }
			}
		      
		      if ((pos + rForce <= totalCells) &&
			  (rForce != (width - 1)))
			{
			  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos + rForce] tag]];

			  if ([ident intValue] != 0)
			    { [forces addObject: ident]; 
			      sign = -1;}
			}
		    }
		  else
		    {
		      if ((pos - rForce >= 0) && (rForce != (width - 1))) 
			{
			  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos - rForce] tag]];

			  if ([ident intValue] != 0)
			    { [forces addObject: ident]; 
			      sign = 1; }
			}
		      
		      if ((pos + rForce <= totalCells) &&
			  ((rForce != 1) || (rForce != (width + 1))))
			{
			  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos + rForce] tag]];

			  if ([ident intValue] != 0)
			    { [forces addObject: ident]; 
			      sign = -1; }
			}
		    }
                }
	    }
	  
	  // Check the applied forces
	  if ([forces count] > 0)
	    {
	      int f1, f2;
	      
              if (sign > 0) 
		{
		  [forceTypes addObject: [NSNumber numberWithInt: 4]];
		}
              else
		{
		  if ([forces count] == 1)
		    {
		      [forceTypes addObject: [NSNumber numberWithInt: 5]];
		    }
		  else
		    {
		      [forceTypes addObject: [NSNumber numberWithInt: 4]];
		    }
		}
	      
	      // Check the forces applied in springs
	      f1 = [[[dictionary objectForKey: [forces objectAtIndex: 0]]
		      objectForKey: @"Type"] intValue];

	      if ([identifier intValue] == 133 && (f1 != 109) && (f1 != 110))
		{
		  errorForce = YES;
		}
	      
	      if ([identifier intValue] == 134 && (f1 != 111) && (f1 != 112))
		{
		  errorForce = YES;
		}
	      
	      if ([identifier intValue] == 135 && (f1 != 115) && (f1 != 116))
		{
		  errorForce = YES;
		}
	      
	      if ([identifier intValue] == 136 && (f1 != 113) && (f1 != 114))
		{
		  errorForce = YES;
		}
              
	      // Check the case when are two applied forces
              if ([forces count] >1)
		{
		  f2 = [[[dictionary objectForKey: [forces objectAtIndex: 1]]
			  objectForKey: @"Type"] intValue];
		  
		  if ([identifier intValue] == 133 && (f2 != 109) &&
		      (f2 != 110))
		    {
		      errorForce = YES;
		    }
		  
		  if ([identifier intValue] == 134 && (f2 != 111) &&
		      (f2 != 112))
		    {
		      errorForce = YES;
		    }
		  
		  if ([identifier intValue] == 135 && (f2 != 115) &&
		      (f2 != 116))
		    {
		      errorForce = YES;
		    }
		  
		  if ([identifier intValue] == 136 && (f2 != 113) &&
		      (f2 != 114))
		    {
		      errorForce = YES;
		    }
		  
		  if (f1 == f2)
		    {
		      errorForce = YES;
		    }
		}
              
	      if ([forces count] > 0)
                {
		  [forceObjs addObject: forces];
                }
              
	    }
	}
    }
  
  // Final verifications
  if (!error && !errorName && !errorContent && !errorForce)
    {
      // Check if exist the referenced objects
      int c;
      for (c = 0; c < [namesContainedObjects count]; c++)
	{
	  if ([namesObjects containsObject: [namesContainedObjects
					      objectAtIndex: c]])
	    {  
	      objContained += 1;
	    }
	}
      
      if (objContained == [namesContainedObjects count])
	{
	  // Check the number of equations in collisions
	  int index1, index2;  
	  NSNumber *typeMov1, *typeMov2;
	  // Check the number of equations in energy objects
	  int indexEnerg;
	  NSNumber *typeEnerg;
	  id codEnerg;
	  NSEnumerator *energObj;
	  // Check the number of equations in impulse objects
	  int indexImp;
	  NSNumber *typeImp;
	  id codImp;
	  NSEnumerator *impObj;
	  // Check the number of equations in power objects
	  int indexPow;
	  NSNumber *typePow;
	  id codPow;
	  NSEnumerator *powObj;
	  // Check the number of equations in object relative motion
	  int indexMR, indexMRO;
	  NSNumber *typeMR;
	  NSMutableArray *dataMRO;
	  id codMR;
	  NSEnumerator *mrObj;
	  // Check referenced object in 104
	  int indexBRV;
	  NSNumber *typeBRV;
	  id codBRV;
	  NSEnumerator *blockVObj;
	  // Check referenced objects in blocks
	  int indexBR;
	  NSNumber *typeBR;
	  id codBR;
	  NSEnumerator *blockObj;

	  for (c = 0; c < [codCol count]; c++)
	    {
	      index1 = [namesObjects indexOfObject:
			       [[codObjCol objectAtIndex: c] objectAtIndex: 0]];
	      index2 = [namesObjects indexOfObject:
			       [[codObjCol objectAtIndex: c] objectAtIndex: 1]];
	      
	      typeMov1 = [[dictionary objectForKey:
					[codContainedObjects objectAtIndex:
					     index1]] objectForKey: @"Type"];
	      typeMov2 = [[dictionary objectForKey:
					[codContainedObjects objectAtIndex:
					     index2]] objectForKey: @"Type"];
	      
	      if ( ([typeMov1 intValue] == 103 && [typeMov2 intValue] == 103) ||
		   ([typeMov1 intValue] == 102 && [typeMov2 intValue] == 102) )
		{  
		  [typeCol addObject: [NSNumber numberWithInt: 2]];
		  numEquations -= 2;
		}
	      else
		{
		  if ([typeMov1 intValue] == 101 &&
		      [typeMov2 intValue] == 101)
		    {
		      [typeCol addObject: [NSNumber numberWithInt: 1]];
		    }
		  else
		    {
		      errorCol = YES;
		    }
		}
	    }
	  
	  // Check the objects in energy
	  energObj = [energyObjs objectEnumerator];
	  
	  while ((codEnerg = [energObj nextObject]))
	    {
	      indexEnerg = [namesObjects indexOfObject: codEnerg];
	      typeEnerg = [[dictionary objectForKey: [codContainedObjects
				       objectAtIndex: indexEnerg]]
     			               objectForKey: @"Type"];
	      
	      // Check if the movement of the block is relative
	      if ([typeEnerg intValue] >= 104 && [typeEnerg intValue] <= 107)
		{
		  int index;
		  NSMutableArray *dataEnerg = [[dictionary objectForKey:
				    [codContainedObjects objectAtIndex:
				     indexEnerg]] objectForKey: @"Values"];
		  
		  if ([typeEnerg intValue] == 104 ||
		      [typeEnerg intValue] == 105)
		    {
		      index = 6;
		    }
		  else
		    {
		      index = 7;
		    }
		  
		  if (![[dataEnerg objectAtIndex: index]
			 isEqualToString: @"sf"])
		    {
		      errorObjMovRel = YES;
		    }
		}
	    }
	  
	  // Check objects on impulse
	  impObj = [impulseObjs objectEnumerator];
	  
	  while ((codImp = [impObj nextObject]))
	    {
	      indexImp = [namesObjects indexOfObject: codImp];
	      typeImp = [[dictionary objectForKey:
				       [codContainedObjects objectAtIndex:
					  indexImp]] objectForKey: @"Type"];
	      
	      if ([typeImp intValue] != 101)
		{
		  numEquations -= 1;
		}
	      
	      if ( !([typeImp intValue] >= 101 && [typeImp intValue] <= 107) )
		{
		  errorImp = YES;
		}
	      
	      // Chaeck that blocks don't have a relative motion
	      if ([typeImp intValue] >= 104 && [typeImp intValue] <= 107)
		{
		  int index;
		  NSMutableArray *dataBlock =
		    [[dictionary objectForKey: [codContainedObjects
			 objectAtIndex: indexImp]] objectForKey: @"Values"];
		  
		  if ([typeImp intValue] == 104 || [typeImp intValue] == 105)
		    {
		      index = 6;
		    }
		  else
		    {
		      index = 7;
		    }
		  
		  if (![[dataBlock objectAtIndex: index]
			 isEqualToString: @"sf"])
		    {
		      errorObjMovRel = YES;
		    }
		}
	    } 
	  
	  // Check objects in power
	  powObj = [powerObjs objectEnumerator];
	  
	  while ((codPow = [powObj nextObject]))
	    {
	      indexPow = [namesObjects indexOfObject: codPow];
	      typePow = [[dictionary objectForKey:
		       [codContainedObjects objectAtIndex: indexPow]]
			  objectForKey: @"Type"];
	      
	      if ( !([typePow intValue] >= 104 && [typePow intValue] <= 107) )
		{
		  errorPow = YES;
		}
	      
	      // Chaeck that blocks don't have a relative motion
	      if ([typePow intValue] >= 104 && [typePow intValue] <= 107)
		{
		  int index;
		  NSMutableArray *dataBlock =
		    [[dictionary objectForKey: [codContainedObjects
			 objectAtIndex: indexPow]] objectForKey: @"Values"];
		  
		  if ([typePow intValue] == 104 || [typePow intValue] == 105)
		    {
		      index = 6;
		    }
		  else
		    {
		      index = 7;
		    }
		  
		  if (![[dataBlock objectAtIndex: index]
			 isEqualToString: @"sf"])
		    {
		      errorObjMovRel = YES;
		    }
		}
	    }
	  
	  // Check objects in relative motion
	  mrObj = [movRelObj objectEnumerator];
	  
	  while ((codMR = [mrObj nextObject]))
	    {
	      indexMR = [namesObjects indexOfObject: codMR];

	      typeMR = [[dictionary objectForKey:
			      [codContainedObjects objectAtIndex: indexMR]]
			       objectForKey: @"Type"];
	      dataMRO = [[dictionary objectForKey:
			      [codContainedObjects objectAtIndex: indexMR]]
			       objectForKey: @"Values"];
	      
	      if ( !([typeMR intValue] >= 104 && [typeMR intValue] <= 107) )
		{
		  errorMovRel = YES;
		}
	      else
		{
		  if ([typeMR intValue] == 104 || [typeMR intValue] == 105)
		    {
		      indexMRO = 6;
		    }
		  else
		    {
		      indexMRO = 7;
		    }
		  
		  if ([[dataMRO objectAtIndex: indexMRO]
			isEqualToString: @"sf"])
		    {
		      errorMRO = YES;
		    }
		}
	    }
	  
	  // Check referenced object in 104
	  blockVObj = [objectRel_104 objectEnumerator];
	  
	  while ((codBRV = [blockVObj nextObject]))
	    {
	      indexBRV = [namesObjects indexOfObject: codBRV];
	      typeBRV = [[dictionary objectForKey: [codContainedObjects
			     objectAtIndex: indexBRV]] objectForKey: @"Type"];
	      
	      if ([typeBRV intValue] != 105)
		{
		  errorBlockRel = YES;
		}
	    }
	  
	  // Check referenced objects in block
	  blockObj = [objectRel objectEnumerator];
	  
	  while ((codBR = [blockObj nextObject]))
	    {
	      indexBR = [namesObjects indexOfObject: codBR];
	      typeBR = [[dictionary objectForKey: [codContainedObjects
			    objectAtIndex: indexBR]] objectForKey: @"Type"];
	      
	      if ( !([typeBR intValue] >= 105 && [typeBR intValue] <= 107) )
		{
		  errorBlockRel = YES;
		}
	    }
	}
    }
  
  // Check if there are errors and if all objects have applied forces
  if ( (([codObjects count] == [forceObjs count]) && !errorForce) &&
       !errorCol && !errorImp && !errorPow && !errorMovRel && !errorMRO &&
       !errorBlockRel && !errorObjMovRel && !errorContent && !errorName)
    {
      // Count the applied forces
      int v;
      NSEnumerator *e;
      NSNumber *f;

      for (v = 0; v < [codObjects count]; v++)
	{
	  [codForcesObj addObjectsFromArray: [forceObjs objectAtIndex: v]];
	}
      
      // check if all forces are applied
      e = [codForces objectEnumerator];
      while ((f = [e nextObject]))
	{
	  if ([codForcesObj containsObject: f])
	    {
	      w += 1;
	    }
	}
      
      // Final verificationes
      if ([vars count] == numEquations && numEquations > 0 && !error)
	{
	  if (objContained == [namesContainedObjects count])
	    {
	      if (w == [codForces count] && w == [codForcesObj count])
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
		  
		  if (w < [codForcesObj count])
		    {
		      advert = [NSString stringWithString:
					   [errors objectAtIndex: 2]];
		    }
		  
		  length = [ [[self viewer] textStorage] length];
		  [[self viewer] replaceCharactersInRange:
 			                       NSMakeRange(length, 0)
					       withString: advert];
		}
	    }
	  else
	    {
	      length = [ [[self viewer] textStorage] length];
	      [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				   withString: [errors objectAtIndex: 3]];
	    }
	}
      else
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 4]];
	}
    }
  else
    {
      if (errorCol)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 5]];
	}
      else if (errorImp)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 6]];
	}
      else if (errorPow)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 7]];
	}
      else if (errorMovRel)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 12]];
	}
      else if (errorMRO)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 16]];
	}
      else if (errorBlockRel)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 13]];
	}
      else if (errorContent)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 14]];
	}
      else if (errorName)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 15]];
	}
      else if (errorObjMovRel)
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 17]];
	}
      else if ([codObjects count] != [forceObjs count])
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 8]];
	}
      else
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 9]];
	}
    }
  
  [keys release];
}


- (void) dealloc
{
  [dictionary release];
  [vars release];
  [codObjects release];
  [forceObjs release];
  [forceTypes release];
  [codCol release];
  [codObjCol release];
  [typeCol release];
  [codContainedObjects release];
  [codRel release];
  [others release];
  [energyObjs release];
  [impulse release];
  [impulseObjs release];
  [power release];
  [powerObjs release];
  [errors release];
  [super dealloc];
}

@end
