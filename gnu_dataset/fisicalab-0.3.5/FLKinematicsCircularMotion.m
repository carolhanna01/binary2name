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
#import "FLKinematicsCircularMotion.h"


static int buildSystem (const gsl_vector *v, void *p, gsl_vector *func)
{
  int tIndex;
  int nEqu = 0;
  double tf;
  NSMutableDictionary *object;
  NSEnumerator *enumerator;

  // Get properties of FL object.
  FLKinematicsCircularMotion *FLObj =
    (__bridge FLKinematicsCircularMotion *)(p);

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
      NSNumber *type = [object objectForKey: @"Type"];
      NSMutableArray *dat = [object objectForKey: @"Values"];
      
      switch ([type intValue])
	{
	case 201:
	  {
	    double r, aci, at, angi, vi, ti, angf, vf, acf;
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		r = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		r = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		aci = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		aci = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		at = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		at = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		angi = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		angi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		vi = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 7]])
	      {
		ti = [[dat objectAtIndex: 7] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 7]];
		ti = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 8]])
	      {
		angf = [[dat objectAtIndex: 8] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 8]];
		angf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 9]])
	      {
		vf = [[dat objectAtIndex: 9] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 9]];
		vf = gsl_vector_get (v, k);
	      }

	    if (![vars containsObject: [dat objectAtIndex: 10]])
	      {
		acf = [[dat objectAtIndex: 10] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 10]];
		acf = gsl_vector_get (v, k);
	      }
	    
            
	    // Build the equations
	    gsl_vector_set (func, nEqu, vf - vi - at*(tf - ti));
	    gsl_vector_set (func, nEqu + 1, (M_PI*angf/180) - (M_PI*angi/180) -
			    (vi/r)*(tf - ti) - 0.5*(at/r)*(tf - ti)*(tf - ti));
	    gsl_vector_set (func, nEqu + 2, aci - (vi*vi)/r);
	    gsl_vector_set (func, nEqu + 3, acf - (vf*vf)/r);
	    
	    nEqu += 4;
	  }
	  break;
	case 202:
	  {
	    double aa, ar, angi, ri, vai, vri, ti, angf, rf, vaf, vrf; 
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		aa = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		aa = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		ar = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		ar = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		angi = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		angi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		ri = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		ri = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		vai = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		vai = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 7]])
	      {
		vri = [[dat objectAtIndex: 7] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 7]];
		vri = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 8]])
	      {
		ti = [[dat objectAtIndex: 8] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 8]];
		ti = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 9]])
	      {
		angf = [[dat objectAtIndex: 9] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 9]];
		angf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 10]])
	      {
		rf = [[dat objectAtIndex: 10] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 10]];
		rf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 11]])
	      {
		vaf = [[dat objectAtIndex: 11] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 11]];
		vaf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 12]])
	      {
		vrf = [[dat objectAtIndex: 12] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 12]];
		vrf = gsl_vector_get (v, k);
	      }
	    
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, vaf - vai - aa*(tf - ti));
	    gsl_vector_set (func, nEqu + 1, (M_PI*angf/180) - (M_PI*angi/180) -
			    vai*(tf - ti) - 0.5*aa*(tf - ti)*(tf - ti));
	    gsl_vector_set (func, nEqu + 2, vrf - vri - ar*(tf - ti));
	    gsl_vector_set (func, nEqu + 3, rf - ri - vri*(tf - ti) -
			    0.5*ar*(tf - ti)*(tf - ti));
	    
	    nEqu += 4;
	  }
	  break;
	case 203:
	  {
	    double wi, wf, vi, vf, r;
	    
	    NSString *nameObj = [dat objectAtIndex: 0];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableArray *mObj = nil;
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
		    break;
		  }
	      }
	    
	    
	    if (![vars containsObject: [mObj objectAtIndex: 6]])
	      {
		vi = [[mObj objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: 6]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObj objectAtIndex: 9]])
	      {
		vf = [[mObj objectAtIndex: 9] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: 9]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObj objectAtIndex: 2]])
	      {
		r = [[mObj objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: 2]];
		r = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		wi = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		wi = gsl_vector_get (v, k);
	      }

	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		wf = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		wf = gsl_vector_get (v, k);
	      }
	    
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, wi - (vi/r));
	    gsl_vector_set (func, nEqu + 1, wf - (vf/r));
	    
	    nEqu += 2;
	  }
	  break;
	case 204:
	  {
	    double aa, vi, vf, ti, r;
	    
	    NSString *nameObj = [dat objectAtIndex: 0];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableArray *mObj = nil;
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
		    break;
		  }
	      }
	    
	    
	    if (![vars containsObject: [mObj objectAtIndex: 6]])
	      {
		vi = [[mObj objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: 6]];
		vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObj objectAtIndex: 9]])
	      {
		vf = [[mObj objectAtIndex: 9] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: 9]];
		vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObj objectAtIndex: 2]])
	      {
		r = [[mObj objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: 2]];
		r = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObj objectAtIndex: 7]])
	      {
		ti = [[mObj objectAtIndex: 7] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: 7]];
		ti = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObj objectAtIndex: 2]])
	      {
		r = [[mObj objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: 2]];
		r = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		aa = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		aa = gsl_vector_get (v, k);
	      }
	    
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, aa - (vf/r - vi/r)/(tf - ti));
	    
	    nEqu += 1;
	  }
	  break;
	case 205:
	  {
	    double atoti, aangi, atotf, aangf;
	    
	    NSString *nameObj = [dat objectAtIndex: 0];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSNumber *typeObj = nil;
	    NSMutableArray *mObj = nil;
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
		    typeObj = [sName objectForKey: @"Type"];
		    break;
		  }
	      }
	    
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		atoti = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		atoti = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		aangi = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		aangi = gsl_vector_get (v, k);
	      }

	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		atotf = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		atotf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		aangf = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		aangf = gsl_vector_get (v, k);
	      }
	    
	    
	    if ([typeObj intValue] == 201)
	      {
		double aci, acf, at;
		
		if (![vars containsObject: [mObj objectAtIndex: 3]])
		  {
		    aci = [[mObj objectAtIndex: 3] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 3]];
		    aci = gsl_vector_get (v, k);
		  }

		if (![vars containsObject: [mObj objectAtIndex: 10]])
		  {
		    acf = [[mObj objectAtIndex: 10] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 10]];
		    acf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 4]])
		  {
		    at = [[mObj objectAtIndex: 4] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 4]];
		    at = gsl_vector_get (v, k);
		  }
		
		
		// Build the equations
		gsl_vector_set (func, nEqu, atoti - gsl_hypot(aci, at));
		gsl_vector_set (func, nEqu + 1, atan2(at, -1*aci) -
				(M_PI*aangi/180));
		gsl_vector_set (func, nEqu + 2, atotf - gsl_hypot(acf, at));
		gsl_vector_set (func, nEqu + 3, atan2(at, -1*acf) -
				(M_PI*aangf/180));
		
		nEqu += 4;
	      }
	    else
	      {
		double ri, rf, vri, vrf, vai, vaf, ar, aa;
		
		if (![vars containsObject: [mObj objectAtIndex: 2]])
		  {
		    aa = [[mObj objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 2]];
		    aa = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 3]])
		  {
		    ar = [[mObj objectAtIndex: 3] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 3]];
		    ar = gsl_vector_get (v, k);
		  }

		if (![vars containsObject: [mObj objectAtIndex: 5]])
		  {
		    ri = [[mObj objectAtIndex: 5] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 5]];
		    ri = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 6]])
		  {
		    vai = [[mObj objectAtIndex: 6] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 6]];
		    vai = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 7]])
		  {
		    vri = [[mObj objectAtIndex: 7] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 7]];
		    vri = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 10]])
		  {
		    rf = [[mObj objectAtIndex: 10] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 10]];
		    rf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 11]])
		  {
		    vaf = [[mObj objectAtIndex: 11] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 11]];
		    vaf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 12]])
		  {
		    vrf = [[mObj objectAtIndex: 12] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 12]];
		    vrf = gsl_vector_get (v, k);
		  }
		
		
		// Build the equations
		gsl_vector_set (func, nEqu, atoti -
				gsl_hypot(ar - ri*vai*vai, ri*aa + 2*vri*vai));
		gsl_vector_set (func, nEqu + 1, atan2(ri*aa + 2*vri*vai,
						      ar - ri*vai*vai) -
				                (M_PI*aangi/180));
		gsl_vector_set (func, nEqu + 2, atotf -
				gsl_hypot(ar - rf*vaf*vaf, rf*aa + 2*vrf*vaf));
		gsl_vector_set (func, nEqu + 3, atan2(rf*aa + 2*vrf*vaf,
						      ar - rf*vaf*vaf) -
				                (M_PI*aangf/180));
		
		nEqu += 4;
	      }
	  }
	  break;
	case 206 ... 208:
	  {
	    double ftn;
	    
	    NSString *nameObj = [dat objectAtIndex: 0];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSNumber *typeObj = nil;
	    NSMutableArray *mObj = nil;
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
		    typeObj = [sName objectForKey: @"Type"];
		    break;
		  }
	      }
	    
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		ftn = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		ftn = gsl_vector_get (v, k);
	      }
	    
	    
	    if ([typeObj intValue] == 201)
	      {
		double ti, r, vi, vf;
		
		if (![vars containsObject: [mObj objectAtIndex: 2]])
		  {
		    r = [[mObj objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 2]];
		    r = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 6]])
		  {
		    vi = [[mObj objectAtIndex: 6] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 6]];
		    vi = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 9]])
		  {
		    vf = [[mObj objectAtIndex: 9] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 9]];
		    vf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 7]])
		  {
		    ti = [[mObj objectAtIndex: 7] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 7]];
		    ti = gsl_vector_get (v, k);
		  }
		
		
		// Build the equations
		switch ([type intValue])
		  {
		  case 206:
		    {
		      gsl_vector_set (func, nEqu, ftn - ((vf/r) +
							 (vi/r))/(4*M_PI));
		    }
		    break;
		  case 207:
		    {
		      gsl_vector_set (func, nEqu, ftn - (4*M_PI)/((vf/r) +
								  (vi/r)));
		    }
		    break;
		  case 208:
		    {
		      gsl_vector_set (func, nEqu, ftn - ((vf/r) +
						  (vi/r))*(tf - ti)/(4*M_PI));
		    }
		    break;
		  }
		
		nEqu += 1;
	      }
	    else
	      {
		double vai, vaf, ti;
		
		if (![vars containsObject: [mObj objectAtIndex: 6]])
		  {
		    vai = [[mObj objectAtIndex: 6] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 6]];
		    vai = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 11]])
		  {
		    vaf = [[mObj objectAtIndex: 11] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 11]];
		    vaf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 8]])
		  {
		    ti = [[mObj objectAtIndex: 8] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 8]];
		    ti = gsl_vector_get (v, k);
		  }
		
		
		// Build the equation
		
		switch ([type intValue])
		  {
		  case 206:
		    {
		      gsl_vector_set (func, nEqu, ftn - (vaf + vai)/(4*M_PI));
		    }
		    break;
		  case 207:
		    {
		      gsl_vector_set (func, nEqu, ftn - (4*M_PI)/(vaf + vai));
		    }
		    break;
		  case 208:
		    {
		      gsl_vector_set (func, nEqu, ftn -
				      (vaf + vai)*(tf - ti)/(4*M_PI));
		    }
		    break;
		  }
		
		nEqu += 1;
	      }
	  }
	  break;
	case 210:
	  {
	    double d;
	    
	    NSString *nameObjOne = [dat objectAtIndex: 0];
	    NSString *nameObjTwo = [dat objectAtIndex: 1];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSNumber *typeObjOne = nil, *typeObjTwo = nil;
	    NSMutableArray *mObjOne = nil, *mObjTwo = nil;
	    NSMutableDictionary *sName;
	    int sControl = 0, ctrlNameOne = 0, ctrlNameTwo = 0;
	    NSMutableArray *centOne = nil, *centTwo = nil;
	    NSString *centNameOne = nil, *centNameTwo = nil;
	    int angfOneInd, rfOneInd;
	    int angfTwoInd, rfTwoInd;
	    double angfOne, rfOne, angfTwo, rfTwo, xcOne = 0, ycOne = 0,
	      xcTwo = 0, ycTwo = 0;
            
	    while ((sName = [search nextObject]))
	      {
		NSString *n = [[sName objectForKey: @"Values"] objectAtIndex:
								 0];
		NSString *t = [[sName objectForKey: @"Titles"] objectAtIndex:
								 0];
                
		if ([t isEqualToString: _(@"Name")])
		  { 
		    if ([nameObjOne isEqualToString: n])
		      {
			mObjOne = [sName objectForKey: @"Values"];
			typeObjOne = [sName objectForKey: @"Type"];
			sControl += 1;
		      }
		    
		    if ([nameObjTwo isEqualToString: n])
		      {
			mObjTwo = [sName objectForKey: @"Values"];
			typeObjTwo = [sName objectForKey: @"Type"];
			sControl += 1;
		      }
		    
		    if (sControl == 2)
		      {
			break;
		      }
		  }
	      }
	    
	    search = [dictionary objectEnumerator];
	    sControl = 0;
	    
	    if ( [[mObjOne objectAtIndex: 1] length] > 0 &&
		 ![[mObjOne objectAtIndex: 1] isEqualToString: @"0"] )
	      {
		centNameOne = [mObjOne objectAtIndex: 1];
	      }
	    else
	      {
		ctrlNameOne = 1;
		sControl = sControl + 1;
	      }
	    
	    if ( [[mObjTwo objectAtIndex: 1] length] > 0 &&
		 ![[mObjTwo objectAtIndex: 1] isEqualToString: @"0"] )
	      {
		centNameTwo = [mObjTwo objectAtIndex: 1];
	      }
	    else
	      {
		ctrlNameTwo = 1;
		sControl = sControl + 1;
	      } 
	    
	    if (sControl != 2)
	      {
		while ((sName = [search nextObject]))
		  {
		    NSString *n = [[sName objectForKey: @"Values"]
				    objectAtIndex: 0];
		    NSString *t = [[sName objectForKey: @"Titles"]
				    objectAtIndex: 0];
		    
		    if ([t isEqualToString: _(@"Name")])
		      {
			if (ctrlNameOne != 1)
			  {
			    if ([centNameOne isEqualToString: n])
			      {
				centOne = [sName objectForKey: @"Values"];
				sControl = sControl + 1;
			      }
			  }
			
			if (ctrlNameTwo != 1)
			  {
			    if ([centNameTwo isEqualToString: n])
			      {
				centTwo = [sName objectForKey: @"Values"];
				sControl = sControl + 1;
			      }
			  }
			
			if (sControl == 2)
			  {
			    break;
			  }
		      }
		  }
	      }
	    
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		d = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		d = gsl_vector_get (v, k);
	      }
	    
	    if ([typeObjOne intValue] == 201)
	      {
		rfOneInd = 2;
		angfOneInd = 8;
	      }
	    else
	      {
		rfOneInd = 10;
		angfOneInd = 9;
	      }
	    
	    if ([typeObjTwo intValue] == 201)
	      {
		rfTwoInd = 2;
		angfTwoInd = 8;
	      }
	    else
	      {
		rfTwoInd = 10;
		angfTwoInd = 9;
	      }
	    
	    
	    // Object One
	    if (![vars containsObject: [mObjOne objectAtIndex: rfOneInd]])
	      {
		rfOne = [[mObjOne objectAtIndex: rfOneInd] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObjOne objectAtIndex: rfOneInd]];
		rfOne = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObjOne objectAtIndex: angfOneInd]])
	      {
		angfOne = [[mObjOne objectAtIndex: angfOneInd] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject:
				[mObjOne objectAtIndex: angfOneInd]];
		angfOne = gsl_vector_get (v, k);
	      }
	    
	    // Object Two
	    if (![vars containsObject: [mObjTwo objectAtIndex: rfTwoInd]])
	      {
		rfTwo = [[mObjTwo objectAtIndex: rfTwoInd] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObjTwo objectAtIndex: rfTwoInd]];
		rfTwo = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObjTwo objectAtIndex: angfTwoInd]])
	      {
		angfTwo = [[mObjTwo objectAtIndex: angfTwoInd] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject:
				[mObjTwo objectAtIndex: angfTwoInd]];
		angfTwo = gsl_vector_get (v, k);
	      }
	    
	    // Coordinates X,Y of center one
	    if (ctrlNameOne == 0)
	      {
		if (![vars containsObject: [centOne objectAtIndex: 1]])
		  {
		    xcOne = [[centOne objectAtIndex: 1] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [centOne objectAtIndex: 1]];
		    xcOne = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [centOne objectAtIndex: 2]])
		  {
		    ycOne = [[centOne objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [centOne objectAtIndex: 2]];
		    ycOne = gsl_vector_get (v, k);
		  }
	      }
	    
	    // Coordinates X,Y of center two
	    if (ctrlNameTwo == 0)
	      {
		if (![vars containsObject: [centTwo objectAtIndex: 1]])
		  {
		    xcTwo = [[centTwo objectAtIndex: 1] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [centTwo objectAtIndex: 1]];
		    xcTwo = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [centTwo objectAtIndex: 2]])
		  {
		    ycTwo = [[centTwo objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [centTwo objectAtIndex: 2]];
		    ycTwo = gsl_vector_get (v, k);
		  }
	      }
	    
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, d*d -
			    ((xcTwo + rfTwo*cos(M_PI*angfTwo/180)) -
			     (xcOne + rfOne*cos(M_PI*angfOne/180)))*
			    ((xcTwo + rfTwo*cos(M_PI*angfTwo/180)) -
			     (xcOne + rfOne*cos(M_PI*angfOne/180))) -
			    ((ycTwo + rfTwo*sin(M_PI*angfTwo/180)) -
			     (ycOne + rfOne*sin(M_PI*angfOne/180)))*
			    ((ycTwo + rfTwo*sin(M_PI*angfTwo/180)) -
			     (ycOne + rfOne*sin(M_PI*angfOne/180))) );
	    
	    nEqu += 1;
	  }
	  break;
	case 211:
	  {
	    double sl;
	    
	    NSString *nameObj = [dat objectAtIndex: 0];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSNumber *typeObj = nil;
	    NSMutableArray *mObj = nil;
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
		    typeObj = [sName objectForKey: @"Type"];
		    break;
		  }
	      }
	    
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		sl = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		sl = gsl_vector_get (v, k);
	      }
	    
	    
	    if ([typeObj intValue] == 201)
	      {
		double ti, vi, at; //r
		
		/*if (![vars containsObject: [mObj objectAtIndex: 2]])
		  {
		  r = [[mObj objectAtIndex: 2] doubleValue];
		  }
		  else
		  {
		  int k = [vars indexOfObject: [mObj objectAtIndex: 2]];
		  r = gsl_vector_get (v, k);
		  }*/
		
		if (![vars containsObject: [mObj objectAtIndex: 6]])
		  {
		    vi = [[mObj objectAtIndex: 6] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 6]];
		    vi = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 4]])
		  {
		    at = [[mObj objectAtIndex: 4] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 4]];
		    at = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 7]])
		  {
		    ti = [[mObj objectAtIndex: 7] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 7]];
		    ti = gsl_vector_get (v, k);
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, sl - vi*(tf - ti) -
				0.5*at*(tf - ti)*(tf - ti));
		
		nEqu += 1;
	      }
	    else
	      {
		double ri, rf, aa, vai, ti; //vaf
		
		if (![vars containsObject: [mObj objectAtIndex: 5]])
		  {
		    ri = [[mObj objectAtIndex: 5] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 5]];
		    ri = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 10]])
		  {
		    rf = [[mObj objectAtIndex: 10] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 10]];
		    rf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 2]])
		  {
		    aa = [[mObj objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 2]];
		    aa = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mObj objectAtIndex: 6]])
		  {
		    vai = [[mObj objectAtIndex: 6] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 6]];
		    vai = gsl_vector_get (v, k);
		  }
		
		/*if (![vars containsObject: [mObj objectAtIndex: 11]])
		  {
		  vaf = [[mObj objectAtIndex: 11] doubleValue];
		  }
		  else
		  {
		  int k = [vars indexOfObject: [mObj objectAtIndex: 11]];
		  vaf = gsl_vector_get (v, k);
		  }*/
		
		if (![vars containsObject: [mObj objectAtIndex: 8]])
		  {
		    ti = [[mObj objectAtIndex: 8] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mObj objectAtIndex: 8]];
		    ti = gsl_vector_get (v, k);
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, sl -
				(vai*(tf - ti) + 0.5*aa*(tf - ti)*
				 (tf - ti))*(rf + ri)/(4*M_PI));
		
		nEqu += 1;
	      }
	  }
	  break;
	case 212:
	  {
	    int angfInd, rfInd;
	    double xp, yp, angf = 0, rf = 0, xc = 0, yc = 0;
	    
	    NSString *nameObj = [dat objectAtIndex: 0];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSNumber *typeObj = nil;
	    NSMutableArray *mObj = nil;
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
		    typeObj = [sName objectForKey: @"Type"];
		    break;
		  }
	      }
	    
	    
	    if ( [[mObj objectAtIndex: 1] length] > 0 &&
		 ![[mObj objectAtIndex: 1] isEqualToString: @"0"] )
	      {
		NSMutableArray *cent = nil;
		NSString *centName;
		search = [dictionary objectEnumerator];
		
		centName = [mObj objectAtIndex: 1];
		
		while ((sName = [search nextObject]))
		  {
		    NSString *n = [[sName objectForKey: @"Values"]
				    objectAtIndex: 0];
		    NSString *t = [[sName objectForKey: @"Titles"]
				    objectAtIndex: 0];
		    
		    if ([t isEqualToString: _(@"Name")] &&
			[centName isEqualToString: n])
		      {
			cent = [sName objectForKey: @"Values"];
			break;
		      }
		  }
		
		if (![vars containsObject: [cent objectAtIndex: 1]])
		  {
		    xc = [[cent objectAtIndex: 1] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [cent objectAtIndex: 1]];
		    xc = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [cent objectAtIndex: 2]])
		  {
		    yc = [[cent objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [cent objectAtIndex: 2]];
		    yc = gsl_vector_get (v, k);
		  }
	      }
	    
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		xp = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		xp = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		yp = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		yp = gsl_vector_get (v, k);
	      }
	    
	    
	    if ([typeObj intValue] == 201)
	      {
		rfInd = 2;
		angfInd = 8;
	      }
	    else
	      {
		rfInd = 10;
		angfInd = 9;
	      }
	    
	    
	    if (![vars containsObject: [mObj objectAtIndex: rfInd]])
	      {
		rf = [[mObj objectAtIndex: rfInd] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: rfInd]];
		rf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [mObj objectAtIndex: angfInd]])
	      {
		angf = [[mObj objectAtIndex: angfInd] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [mObj objectAtIndex: angfInd]];
		angf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, xp - xc - rf*cos(M_PI*angf/180) );
	    gsl_vector_set (func, nEqu + 1, yp - yc - rf*sin(M_PI*angf/180));
	    
	    nEqu += 2;
	  }
	  break;
	case 213:
	  {
	    double vr, ang;
	    
	    NSString *nameObjOne = [dat objectAtIndex: 0];
	    NSString *nameObjTwo = [dat objectAtIndex: 1];
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSNumber *typeObjOne = nil, *typeObjTwo = nil;
	    NSMutableArray *mObjOne = nil, *mObjTwo = nil;
	    NSMutableDictionary *sName;
	    int sControl = 0;
            
	    while ((sName = [search nextObject]))
	      {
		NSString *n = [[sName objectForKey: @"Values"] objectAtIndex:
								 0];
		NSString *t = [[sName objectForKey: @"Titles"] objectAtIndex:
								 0];
                
		if ([t isEqualToString: _(@"Name")])
		  { 
		    if ([nameObjOne isEqualToString: n])
		      {
			mObjOne = [sName objectForKey: @"Values"];
			typeObjOne = [sName objectForKey: @"Type"];
			sControl = sControl + 1;
		      }
		    
		    if ([nameObjTwo isEqualToString: n])
		      {
			mObjTwo = [sName objectForKey: @"Values"];
			typeObjTwo = [sName objectForKey: @"Type"];
			sControl = sControl + 1;
		      }
		    
		    if (sControl == 2)
		      {
			break;
		      }
		  }
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		vr = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		vr = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		ang = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		ang = gsl_vector_get (v, k);
	      }
	    
	    
	    if ([typeObjOne intValue] == 209 ||
		[typeObjTwo intValue] == 209)
	      {
		NSMutableArray *mobil;
		double angf, rf, vaf, vrf;
		
		// Search the mobile object
		if ([typeObjOne intValue] == 209)
		  {
		    mobil = mObjTwo;
		  }
		else
		  {
		    mobil = mObjOne;
		  }
		
		// Get the data
		if (![vars containsObject: [mobil objectAtIndex: 9]])
		  {
		    angf = [[mobil objectAtIndex: 9] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mobil objectAtIndex: 9]];
		    angf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mobil objectAtIndex: 10]])
		  {
		    rf = [[mobil objectAtIndex: 10] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mobil objectAtIndex: 10]];
		    rf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mobil objectAtIndex: 11]])
		  {
		    vaf = [[mobil objectAtIndex: 11] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mobil objectAtIndex: 11]];
		    vaf = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [mobil objectAtIndex: 12]])
		  {
		    vrf = [[mobil objectAtIndex: 12] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [mobil objectAtIndex: 12]];
		    vrf = gsl_vector_get (v, k);
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, vr - gsl_hypot(vaf*rf, vrf));
		gsl_vector_set (func, nEqu + 1, (M_PI*ang/180) -
				atan2(vaf*rf, vrf) - (M_PI*angf/180));
		
		nEqu += 2;
	      }
	    else
	      {
		if ([typeObjOne intValue] == 201 &&
		    [typeObjTwo intValue] == 201)
		  {
		    double angfOne, vfOne, angfTwo, vfTwo;
		    
		    // Data of mobile one
		    if (![vars containsObject: [mObjOne objectAtIndex: 8]])
		      {
			angfOne = [[mObjOne objectAtIndex: 8] doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject:
					[mObjOne objectAtIndex: 8]];
			angfOne = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObjOne objectAtIndex: 9]])
		      {
			vfOne = [[mObjOne objectAtIndex: 9] doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject:
					[mObjOne objectAtIndex: 9]];
			vfOne = gsl_vector_get (v, k);
		      }
		    
		    // Data of mobile two
		    if (![vars containsObject: [mObjTwo objectAtIndex: 8]])
		      {
			angfTwo = [[mObjTwo objectAtIndex: 8] doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject:
					[mObjTwo objectAtIndex: 8]];
			angfTwo = gsl_vector_get (v, k);
		      }
		    
		    if (![vars containsObject: [mObjTwo objectAtIndex: 9]])
		      {
			vfTwo = [[mObjTwo objectAtIndex: 9] doubleValue];
		      }
		    else
		      {
			int k = [vars indexOfObject:
					[mObjTwo objectAtIndex: 9]];
			vfTwo = gsl_vector_get (v, k);
		      }
		    
		    // Build the equations
		    gsl_vector_set (func, nEqu, vr -
		     gsl_hypot(vfOne*cos(M_PI*(angfOne - angfTwo)/180) - vfTwo,
			       vfOne*sin(M_PI*(angfOne - angfTwo)/180)));
		    gsl_vector_set (func, nEqu + 1,
			   (M_PI*ang/180) -
			   atan2(vfOne*sin(M_PI*(angfOne - angfTwo)/180),
			     vfOne*cos(M_PI*(angfOne - angfTwo)/180) - vfTwo) -
		           (M_PI*angfTwo/180)- (M_PI/2));
		    
		    nEqu += 2;
		  }
		else
		  {
		    if ([typeObjOne intValue] == 202 &&
			[typeObjTwo intValue] == 202)
		      {
			double rfOne, angfOne, vafOne, vrfOne, rfTwo, angfTwo,
			  vafTwo, vrfTwo; 
			
			// Data of mobile one
			if (![vars containsObject: [mObjOne objectAtIndex: 10]])
			  {
			    rfOne = [[mObjOne objectAtIndex: 10] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mObjOne objectAtIndex: 10]];
			    rfOne = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mObjOne objectAtIndex: 9]])
			  {
			    angfOne = [[mObjOne objectAtIndex: 9] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mObjOne objectAtIndex: 9]];
			    angfOne = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mObjOne objectAtIndex: 11]])
			  {
			    vafOne = [[mObjOne objectAtIndex: 11] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mObjOne objectAtIndex: 11]];
			    vafOne = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mObjOne objectAtIndex: 12]])
			  {
			    vrfOne = [[mObjOne objectAtIndex: 12] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mObjOne objectAtIndex: 12]];
			    vrfOne = gsl_vector_get (v, k);
			  }
			
			// Data of mobile two
			if (![vars containsObject: [mObjTwo objectAtIndex: 10]])
			  {
			    rfTwo = [[mObjTwo objectAtIndex: 10] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mObjTwo objectAtIndex: 10]];
			    rfTwo = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mObjTwo objectAtIndex: 9]])
			  {
			    angfTwo = [[mObjTwo objectAtIndex: 9] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mObjTwo objectAtIndex: 9]];
			    angfTwo = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mObjTwo objectAtIndex: 11]])
			  {
			    vafTwo = [[mObjTwo objectAtIndex: 11] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mObjTwo objectAtIndex: 11]];
			    vafTwo = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mObjTwo objectAtIndex: 12]])
			  {
			    vrfTwo = [[mObjTwo objectAtIndex: 12] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mObjTwo objectAtIndex: 12]];
			    vrfTwo = gsl_vector_get (v, k);
			  }
			
			// Build the equations
			gsl_vector_set (func, nEqu, vr -
			  gsl_hypot(vafOne*rfOne*sin(M_PI*(90 + angfOne -
							   angfTwo)/180) +
				    vrfOne*sin(M_PI*(angfOne - angfTwo)/180) -
				    vafTwo*rfTwo,
				    vafOne*rfOne*cos(M_PI*(90 + angfOne -
							   angfTwo)/180) +
				    vrfOne*cos(M_PI*(angfOne - angfTwo)/180) -
				    vrfTwo));
			gsl_vector_set (func, nEqu + 1, (M_PI*ang/180) -
			     atan2(vafOne*rfOne*sin(M_PI*(90 + angfOne -
							  angfTwo)/180) +
				   vrfOne*sin(M_PI*(angfOne - angfTwo)/180) -
				   vafTwo*rfTwo,
				   vafOne*rfOne*cos(M_PI*(90 + angfOne -
							  angfTwo)/180) +
				   vrfOne*cos(M_PI*(angfOne - angfTwo)/180) -
				   vrfTwo) - (M_PI*angfTwo/180));
			
			nEqu += 2;
		      }
		    else
		      {
			int typeRV;
			double vfA, angfA, rfB, angfB, vafB, vrfB;
			NSMutableArray *mobileA, *mobileB;
			
			if ([typeObjOne intValue] == 201 &&
			    [typeObjTwo intValue] == 202)
			  {
			    typeRV = 1;
			    mobileA = mObjOne;
			    mobileB = mObjTwo;
			  }
			else
			  {
			    typeRV = 2;
			    mobileA = mObjTwo;
			    mobileB = mObjOne;
			  }
			
			// Get data of mobile A
			if (![vars containsObject: [mobileA objectAtIndex: 8]])
			  {
			    angfA = [[mobileA objectAtIndex: 8] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mobileA objectAtIndex: 8]];
			    angfA = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mobileA objectAtIndex: 9]])
			  {
			    vfA = [[mobileA objectAtIndex: 9] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mobileA objectAtIndex: 9]];
			    vfA = gsl_vector_get (v, k);
			  }
			
			// Get data of mobile B
			if (![vars containsObject: [mobileB objectAtIndex: 10]])
			  {
			    rfB = [[mobileB objectAtIndex: 10] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mobileB objectAtIndex: 10]];
			    rfB = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mobileB objectAtIndex: 9]])
			  {
			    angfB = [[mobileB objectAtIndex: 9] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mobileB objectAtIndex: 9]];
			    angfB = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mobileB objectAtIndex: 11]])
			  {
			    vafB = [[mobileB objectAtIndex: 11] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mobileB objectAtIndex: 11]];
			    vafB = gsl_vector_get (v, k);
			  }
			
			if (![vars containsObject: [mobileB objectAtIndex: 12]])
			  {
			    vrfB = [[mobileB objectAtIndex: 12] doubleValue];
			  }
			else
			  {
			    int k = [vars indexOfObject:
					    [mobileB objectAtIndex: 12]];
			    vrfB = gsl_vector_get (v, k);
			  }
			
			// Build the equations
			if (typeRV == 1)
			  {
			    gsl_vector_set (func, nEqu, vr -
			     gsl_hypot(vfA*sin(M_PI*(90 + angfA - angfB)/180) -
				       vafB*rfB,
				       vfA*cos(M_PI*(90 + angfA - angfB)/180) -
				       vrfB));
			    gsl_vector_set (func, nEqu + 1, (M_PI*ang/180) -
				atan2(vfA*sin(M_PI*(90 + angfA - angfB)/180) -
				      vafB*rfB,
				      vfA*cos(M_PI*(90 + angfA - angfB)/180) -
				      vrfB) - (M_PI*angfB/180));
			  }
			else
			  {
			    gsl_vector_set (func, nEqu, vr -
			       gsl_hypot(vafB*rfB*sin(M_PI*(90 + angfB -
							    angfA)/180) +
					 vrfB*sin(M_PI*(angfB - angfA)/180) -
					 vfA, vafB*rfB*cos(M_PI*(90 + angfB -
								 angfA)/180) +
					 vrfB*cos(M_PI*(angfB - angfA)/180)));
			    gsl_vector_set (func, nEqu + 1, (M_PI*ang/180) -
				  atan2(vafB*rfB*sin(M_PI*(90 + angfB -
							   angfA)/180) +
					vrfB*sin(M_PI*(angfB - angfA)/180) -
					vfA, vafB*rfB*cos(M_PI*(90 + angfB -
								angfA)/180) +
					vrfB*cos(M_PI*(angfB - angfA)/180)) -
					    (M_PI*angfA/180));
			  }
			
			nEqu += 2;
		      }
		  }
	      }
	  }
	  break;
	}
    }

  return GSL_SUCCESS;
}

@interface FLKinematicsCircularMotion (Private)
- (void) makeSystem;
@end

@implementation FLKinematicsCircularMotion (Private)
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
  
  gsl_vector *x = gsl_vector_alloc (n);
  
  int countRes = 0;
  id anObj;
  NSMutableArray *results = [NSMutableArray array];
  NSEnumerator *varCount;
  
  id dataSign;
  NSNumber *typeOther;
  NSMutableArray *verifSign;
  NSEnumerator *signObj;
  
  // Generator of random numbers
  const gsl_rng_type * Y;
  gsl_rng * r;
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
	  if ( increase <= 15 )
	    {
	      newValue = 1;
	    }
	  else if ( increase <= 30 )
	    {
	      newValue = 10;
	    }
	  else if ( increase <= 60 )
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
      
      // Check the time variables
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
	case 201:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the initial angle
	    if ([vars containsObject: [verifSign objectAtIndex: 5]])
	      {
		double nv;
		w = [vars indexOfObject: [verifSign objectAtIndex: 5]];
		nv = [[results objectAtIndex: w] doubleValue];
		
		if ( nv >= 360 )
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
	    
	    // Correct the final angle
	    if ([vars containsObject: [verifSign objectAtIndex: 8]])
	      {
		double nv;
		w = [vars indexOfObject: [verifSign objectAtIndex: 8]];
		nv = [[results objectAtIndex: w] doubleValue];
		
		if ( nv >= 360 )
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
	case 202:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the initial angle
	    if ([vars containsObject: [verifSign objectAtIndex: 4]])
	      {
		double nv;
		w = [vars indexOfObject: [verifSign objectAtIndex: 4]];
		nv = [[results objectAtIndex: w] doubleValue];
		
		if ( nv >= 360 )
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
	    
	    // Correct the final angle
	    if ([vars containsObject: [verifSign objectAtIndex: 9]])
	      {
		double nv;
		w = [vars indexOfObject: [verifSign objectAtIndex: 9]];
		nv = [[results objectAtIndex: w] doubleValue];
		
		if ( nv >= 360 )
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
	case 205:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the angle of acceleration
	    if ([vars containsObject: [verifSign objectAtIndex: 2]])
	      {
		double nv;
		w = [vars indexOfObject: [verifSign objectAtIndex: 2]];
		nv = [[results objectAtIndex: w] doubleValue];
		
		if ( nv >= 360 )
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
	case 210:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the sign of the distance
	    if ([vars containsObject: [verifSign objectAtIndex: 2]])
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 2]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    double nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
				       withObject: [NSNumber numberWithDouble:
							       nv]];
		  }
	      }
	  }
	  break;
	case 213:
	  {
	    verifSign = [dataSign objectForKey: @"Values"];
	    
	    // Correct the velocity
	    if ( [vars containsObject: [verifSign objectAtIndex: 2]] &&
		 [vars containsObject: [verifSign objectAtIndex: 3]] )
	      {
		w = [vars indexOfObject: [verifSign objectAtIndex: 2]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    double nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
				       withObject: [NSNumber numberWithDouble:
							       nv]];
		    par1 = 1;
		  }
	      }
	    
	    // Correct the angle of velocity
	    if ([vars containsObject: [verifSign objectAtIndex: 3]])
	      {
		double nv;
		w = [vars indexOfObject: [verifSign objectAtIndex: 3]];
		nv = [[results objectAtIndex: w] doubleValue];
		
		if ( par1 == 1 )
		  {
		    nv += 180;
		  }
                
		if ( nv >= 360 )
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
  message = [NSString stringWithFormat: [errors objectAtIndex: 3],
		      gsl_strerror (state)];
  length = [[[self viewer] textStorage] length];
  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: message];
  
  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  gsl_rng_free (r);
}
@end

@implementation FLKinematicsCircularMotion

- (id) init
{
  NSBundle *messages;
  self = [super init];

  vars = [NSMutableArray new];
  varsTime = [NSMutableArray new];
  dictionary = [NSMutableArray new];
  
  // Build the messages array
  messages = [NSBundle mainBundle];
  errors = [[NSArray alloc] initWithContentsOfFile:
		 [messages pathForResource: @"kinematicsCircularMessages"
				    ofType: @"plist"]];
  
  return self;
}

- (void) makeEquationsForData: (NSMutableDictionary *)list
{
  int length, c, objContained = 0, numEquations = 0;
  NSUInteger sys = [self system];
  BOOL error = NO;
  BOOL errorContent = NO;
  BOOL errorCent = NO;
  BOOL errorWA = NO;
  BOOL errorAFTN = NO;
  BOOL errorDist = NO;
  BOOL errorArc = NO;
  BOOL errorCo = NO;
  BOOL errorVr = NO;
  NSNumber *identifier;
  NSMutableDictionary *object;
  NSMutableArray *namesObjects = [NSMutableArray array];
  NSMutableArray *codObjects = [NSMutableArray array];
  NSMutableArray *namesContainedObjects = [NSMutableArray array];
  NSMutableArray *namesContObjsMov = [NSMutableArray array];
  NSMutableArray *namesContObjsDatOne = [NSMutableArray array];
  NSMutableArray *namesContObjsDatTwo = [NSMutableArray array];
  NSMutableArray *namesContObjsDist = [NSMutableArray array];
  NSMutableArray *namesContObjsArc = [NSMutableArray array];
  NSMutableArray *namesContObjsCo = [NSMutableArray array];
  NSMutableArray *namesContObjsVr = [NSMutableArray array];
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
      NSArray *titles = [object objectForKey: @"Titles"];
      NSMutableArray *info = [object objectForKey: @"Data"];
      NSMutableArray *values = [object objectForKey: @"Values"];
      identifier = [object objectForKey: @"Type"];
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
	      ![title isEqualToString: _(@"Name")] &&
	      ![title isEqualToString: @"C"])
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
			  
			  // If is a variable of time add this to varsTime
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
		  NSString *varFactor;
		  
		  // Check the type of variable
		  if ([title hasPrefix: @"r"] || [title hasPrefix: @"d"] || 
		     [title hasPrefix: @"s"] || [title hasPrefix: @"x"] || 
		     [title hasPrefix: @"y"])
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
		  else if ([title hasPrefix: @"va"])
		    {
		      varFactor = @"rad/s";
		    }
		  else if ([title hasPrefix: @"ang"])
		    { 
		      varFactor = [NSString stringWithString: _(@"degrees")];
		    }
		  else if ([title hasPrefix: @"at"] ||
			   [title hasPrefix: @"ar"] ||
			   [title hasPrefix: @"atot"] ||
			   [title hasPrefix: @"ac"])
		    {
		      if (sys == 0)
			{ varFactor = @"m/s2"; }
		      else
			{ varFactor = @"ft/s2"; }
		    }
		  else if ([title hasPrefix: @"aa"])
		    {
		      varFactor = @"rad/s2";
		    }
		  else if ([title hasPrefix: @"v"])
		    {
		      if (sys == 0)
			{ varFactor = @"m/s"; }
		      else
			{ varFactor = @"ft/s"; }
		    }
		  else if ([title hasPrefix: @"f"])
		    {
		      varFactor = @"hz";
		    }
		  else if ([title hasPrefix: @"T"])
		    {
		      varFactor = @"1/hz";
		    }
		  else
		    {
		      varFactor = @"rev";
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
      
      // Here ends the count of variables
      switch ([identifier intValue])
	{
	case 200:
	  {
	    if (![self isNumericDataTheString: [values objectAtIndex: 0]])
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
	case 201:
	  {
	    numEquations += 4;
	    [namesObjects addObject: [values objectAtIndex: 0]];
	    [codObjects addObject: identifier];
	    
	    if ([[values objectAtIndex: 1] length] > 0 &&
		![[values objectAtIndex: 1] isEqualToString: @"0"])
	      {
		[namesContObjsMov addObject: [values objectAtIndex: 1]];
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }
	  }    
	  break;
	case 202:
	  {
	    numEquations += 4;
	    [namesObjects addObject: [values objectAtIndex: 0]];
	    [codObjects addObject: identifier];
	    
	    if ([[values objectAtIndex: 1] length] > 0 &&
		![[values objectAtIndex: 1] isEqualToString: @"0"])
	      {
		[namesContObjsMov addObject: [values objectAtIndex: 1]];
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }
	  }
	  break;
	case 203:
	case 204:
	  {
	    if ([identifier intValue] == 203)
	      {
		numEquations += 2;
	      }
	    else
	      {
		numEquations += 1;
	      }
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContObjsDatOne addObject: [values objectAtIndex: 0]];
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 205:
	  {
	    numEquations += 4;
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContObjsDatTwo addObject: [values objectAtIndex: 0]];
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 206 ... 208:
	  {
	    numEquations += 1;
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContObjsDatTwo addObject: [values objectAtIndex: 0]];
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 209:
	  {
	    [namesObjects addObject: [values objectAtIndex: 0]];
	    [codObjects addObject: identifier];
	  }
	  break;
	case 210:
	  {
	    numEquations += 1;
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContObjsDist addObject: [values objectAtIndex: 0]];
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	    
	    if ([[values objectAtIndex: 1] length] > 0 &&
		![[values objectAtIndex: 1] isEqualToString: @"0"])
	      {
		[namesContObjsDist addObject: [values objectAtIndex: 1]];
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 211:
	  {
	    numEquations += 1;
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContObjsArc addObject: [values objectAtIndex: 0]];
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 212:
	  {
	    numEquations += 2;
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContObjsCo addObject: [values objectAtIndex: 0]];
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	case 213:
	  {
	    numEquations += 2;
	    
	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContObjsVr addObject: [values objectAtIndex: 0]];
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	    
	    if ([[values objectAtIndex: 1] length] > 0 &&
		![[values objectAtIndex: 1] isEqualToString: @"0"])
	      {
		[namesContObjsVr addObject: [values objectAtIndex: 1]];
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }
	    else
	      {
		errorContent = YES;
	      }
	  }
	  break;
	}
      // Here ends the verify for equations
    }
  
  // Here begins the verify of the problem
  
  // Check if the referenced objects exist
  for (c = 0; c < [namesContainedObjects count]; c++)
    {
      if ([namesObjects containsObject:
			  [namesContainedObjects objectAtIndex: c]])
        {  
	  objContained += 1;
        }
    }
  
  if (objContained == [namesContainedObjects count] && 
      [vars count] == numEquations && numEquations > 0 && !error)
    {
      // Check that mobiles contains valid centers, if any
      int indexObj;
      id nameObj;
      NSNumber *verifType;
      NSEnumerator *countObj = [namesContObjsMov objectEnumerator];
      int vrcount, indexObjOne, indexObjTwo;
      NSNumber *verifTypeOne, *verifTypeTwo;
      
      while ((nameObj = [countObj nextObject]))
	{
	  indexObj = [namesObjects indexOfObject: nameObj];
	  verifType = [codObjects objectAtIndex: indexObj];    
	  
	  if ([verifType intValue] != 209)
	    {
	      errorCent = YES;
	    }
	}
      
      /* Check that objects angular velocity only contains objects circular
	 mobile*/
      countObj = [namesContObjsDatOne objectEnumerator];
      
      while ((nameObj = [countObj nextObject]))
	{
	  indexObj = [namesObjects indexOfObject: nameObj];
	  verifType = [codObjects objectAtIndex: indexObj];    
	  
	  if ([verifType intValue] != 201)
	    {
	      errorWA = YES;
	    }
	}
      
      /* Check that objects total acceleration, frequency, period and number
	 of laps, only contains objects mobile */
      countObj = [namesContObjsDatTwo objectEnumerator];
      
      while ((nameObj = [countObj nextObject]))
	{
	  indexObj = [namesObjects indexOfObject: nameObj];
	  verifType = [codObjects objectAtIndex: indexObj];    
	  
	  if ([verifType intValue] == 209)
	    {
	      errorAFTN = YES;
	    }
	}
      
      // Check that objects distance only contains objects mobile
      countObj = [namesContObjsDist objectEnumerator];
      
      while ((nameObj = [countObj nextObject]))
	{
	  indexObj = [namesObjects indexOfObject: nameObj];
	  verifType = [codObjects objectAtIndex: indexObj];    
	  
	  if ([verifType intValue] == 209)
	    {
	      errorDist = YES;
	    }
	}
      
      // Check that objects arc only contains objects mobile
      countObj = [namesContObjsArc objectEnumerator];
      
      while ((nameObj = [countObj nextObject]))
	{
	  indexObj = [namesObjects indexOfObject: nameObj];
	  verifType = [codObjects objectAtIndex: indexObj];    
	  
	  if ([verifType intValue] == 209)
	    {
	      errorArc = YES;
	    }
	}
      
      // Check that objects coordinates only contains objects mobile
      countObj = [namesContObjsCo objectEnumerator];
      
      while ((nameObj = [countObj nextObject]))
	{
	  indexObj = [namesObjects indexOfObject: nameObj];
	  verifType = [codObjects objectAtIndex: indexObj];    
	  
	  if ([verifType intValue] == 209)
	    {
	      errorCo = YES;
	    }
	}
      
      /* Check that objects relative velocity don't contains a combination
	 of circular mobile and center */
      for (vrcount = 0; vrcount < [namesContObjsVr count]; vrcount += 2)
	{
	  indexObjOne = [namesObjects indexOfObject:
				[namesContObjsVr objectAtIndex: vrcount]];
	  indexObjTwo = [namesObjects indexOfObject:
				[namesContObjsVr objectAtIndex: vrcount + 1]];
	  verifTypeOne = [codObjects objectAtIndex: indexObjOne];  
	  verifTypeTwo = [codObjects objectAtIndex: indexObjTwo];    
	  
	  if ( ([verifTypeOne intValue] == 201 &&
		[verifTypeTwo intValue] == 209) ||
	       ([verifTypeOne intValue] == 209 &&
		[verifTypeTwo intValue] == 201) ||
	       ([verifTypeOne intValue] == 209 &&
		[verifTypeTwo intValue] == 209) )
	    {
	      errorVr = YES;
	    }
	}
    }
  
  // Final verifications
  if ([vars count] == numEquations && numEquations > 0 && !error &&
      !errorContent && !errorCent && !errorWA && !errorAFTN && !errorDist &&
      !errorArc && !errorCo && !errorVr)
    {
      if (objContained == [namesContainedObjects count])
	{
	  [self makeSystem];
	}
      else
	{
	  length = [ [[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 1]];
	}
    }
  else
    {
      if (errorContent)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 4]];
	}
      else if (errorCent)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 5]];
	}
      else if (errorWA)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 6]];
	}
      else if (errorAFTN)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 7]];
	}
      else if (errorDist)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 8]];
	}
      else if (errorArc)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 9]];
	}
      else if (errorCo)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 10]];
	}
      else if (errorVr)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				       withString: [errors objectAtIndex: 11]];
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
