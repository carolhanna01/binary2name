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
#import "FLCalorimetry.h"


static int buildSystem (const gsl_vector *v, void *p, gsl_vector *func)
{
  int tIndex, countSource = 0;
  int nEqu = 0;
  double tf;
  NSNumber *object;
  NSEnumerator *enumObjects;
  // Objects contained in codeOthers
  NSEnumerator *enumOthers;

  // Get properties of FL object.
  FLCalorimetry *FLObj = (__bridge FLCalorimetry *)(p);

  BOOL messageTemp = FLObj->messageTemp;
  BOOL changeOrCal = FLObj->changeOrCal;
  NSInteger varT = FLObj->varT;
  NSString *timeVar = FLObj->timeVar;
  double timeData = FLObj->timeData;
  NSMutableArray *vars = FLObj->vars;
  NSMutableArray *codeContainedObjects = FLObj->codeContainedObjects;
  NSMutableArray *codeOthers = FLObj->codeOthers;
  NSMutableArray *systemsApplied = FLObj->systemsApplied;
  NSMutableArray *systemsType = FLObj->systemsType;
  NSMutableDictionary *dictionary = FLObj->dictionary;
  // ----------------------------------------
  
  if (varT == 1)
    {
      tIndex = [vars indexOfObject: timeVar];
      tf = gsl_vector_get (v, tIndex);
    }
  else
    {
      if (varT == 2)
	{
	  tf = timeData;
	}
      else
	{
	  tf = 0;
	}
    }
  
  // Objects contained in codeContainedObjects
  enumObjects = [codeContainedObjects objectEnumerator];
  
  while ((object = [enumObjects nextObject]))
    {
      NSNumber *type =[[dictionary objectForKey: object]
			objectForKey: @"Type"];
      NSMutableArray *dat = [[dictionary objectForKey: object]
			      objectForKey: @"Values"];
      
      switch ([type intValue])
	{
	case 155:
	case 156:
	case 169:
	  {
	    if ([[systemsApplied objectAtIndex: countSource] count] > 0)
	      {
		double mass, ce, Ti, Tf, Tm;
		int count = 0;
		double sign;
		double sources[8] = {0};
		NSNumber *idSourceObj;
		NSString *obj;
		NSEnumerator *sourceObj;
		
		// Check the type of the applied source
		if ([[systemsType objectAtIndex: countSource] doubleValue] == 1)
		  {
		    Tm = 1;
		  }
		else
		  {
		    Tm = tf;
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
		
		if (![vars containsObject: [dat objectAtIndex: 2]])
		  {
		    ce = [[dat objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		    ce = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [dat objectAtIndex: 3]])
		  {
		    Ti = [[dat objectAtIndex: 3] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		    Ti = gsl_vector_get (v, k);
		  }
		
		if (![vars containsObject: [dat objectAtIndex: 4]])
		  {
		    Tf = [[dat objectAtIndex: 4] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		    Tf = gsl_vector_get (v, k);
		  }

		
		sourceObj = [[systemsApplied objectAtIndex: countSource]
			      objectEnumerator];
		
		while ((idSourceObj = [sourceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: idSourceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: idSourceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
		    
		    switch (t)
		      {
		      case 151 ... 154:
			{
			  if ((t == 151) || (t == 152))
			    {
			      sign = 1; 
			    }
			  else
			    {
			      sign = -1;
			    }
			  
			  if (![vars containsObject: obj])
			    {
			      sources[count] = sign*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      sources[count] = sign*(gsl_vector_get (v, k));
			    }
			  
			  count += 1;
			}
			break;
		      }
		    
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, Tm*(sources[0] + sources[1] +
						sources[2] + sources[3] +
						sources[4] + sources[5] +
						sources[6] + sources[7]) -
				mass*ce*(Tf - Ti));
		
		
		nEqu += 1;
	      }
	  }
	  break;
	case 160:
	case 161:
	  {
	    if ([[systemsApplied objectAtIndex: countSource] count] > 0)
	      {
		double mass, cf, Tm, sense = 1;
		NSString *sent;
		int count = 0;
		double sign;
		double sources[8] = {0};
		NSNumber *idSourceObj;
		NSString *obj;
		NSEnumerator *sourceObj;
		
		// Check the type of applied source
		if ([[systemsType objectAtIndex: countSource] doubleValue] == 1)
		  {
		    Tm = 1;
		  }
		else
		  {
		    Tm = tf;
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
		
		if (![vars containsObject: [dat objectAtIndex: 2]])
		  {
		    cf = [[dat objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		    cf = gsl_vector_get (v, k);
		  }
		
		sent = [NSString stringWithString: [dat objectAtIndex: 3]];
		
		if ([sent isEqualToString: @"<"])
		  {
		    sense = -1;
		  }

		
		sourceObj = [[systemsApplied objectAtIndex: countSource]
			      objectEnumerator];
		
		while ((idSourceObj = [sourceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: idSourceObj]
			       objectForKey: @"Type"] intValue];
		    obj = [[[dictionary objectForKey: idSourceObj]
			     objectForKey: @"Values"] objectAtIndex: 0];
		    
		    switch (t)
		      {
		      case 151 ... 154:
			{
			  if ((t == 151) || (t == 152))
			    {
			      sign = 1; 
			    }
			  else
			    {
			      sign = -1;
			    }
			  
			  if (![vars containsObject: obj])
			    {
			      sources[count] = sign*[obj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: obj];
			      sources[count] = sign*(gsl_vector_get (v, k));
			    }
			  
			  count += 1;
			}
			break;
		      }
		    
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, Tm*(sources[0] + sources[1] +
						sources[2] + sources[3] +
						sources[4] + sources[5] +
						sources[6] + sources[7]) -
				sense*mass*cf);
		
		nEqu += 1;
	      }
	  }
	  break;
	case 162:
	  {
	    if ([[systemsApplied objectAtIndex: countSource] count] > 0)
	      {
		double Tm;
		// To search the data
		int countGen = 0, count1 = 0, count2 = 0;
		double mState[3] = {0};
		double cElem[3] = {0};
		double mPhase[2] = {0};
		double cfElem[2] = {0};
		double senElem[2] = {1};
		double tIniElem[3] = {0};
		double tFinElem[3] = {0};   
		NSEnumerator *dEnum;
		// To search the sources
		int count = 0;
		double sign;
		double sources[8] = {0};
		NSNumber *idSourceObj;
		NSString *sObj;
		NSEnumerator *sourceObj;
		// To search the elements
		NSMutableArray *otherObjs = [[dictionary objectForKey: object]
					     objectForKey: @"Values"];
		NSMutableArray *codElemts = [NSMutableArray array];
		NSMutableArray *elemts = [NSMutableArray array];
		
		NSNumber *obj1, *obj2, *obj3, *obj4, *obj5;                 
		NSString *objName1 = [otherObjs objectAtIndex: 1];
		NSString *objName2 = [otherObjs objectAtIndex: 2];
		NSString *objName3 = [otherObjs objectAtIndex: 3];
		NSString *objName4 = [otherObjs objectAtIndex: 4];
		NSString *objName5 = [otherObjs objectAtIndex: 5];

		NSMutableArray *object1, *object2, *object3, *object4,
		  *object5, *objChang;
		NSEnumerator *search = [dictionary objectEnumerator];
		NSMutableDictionary *sName;

		// Check the applied source
		if ([[systemsType objectAtIndex: countSource] doubleValue] == 1)
		  {
		    Tm = 1;
		  }
		else
		  {
		    Tm = tf;
		  }
                

		while ((sName = [search nextObject]))
		  {
		    NSString *n = [[sName objectForKey: @"Values"]
				    objectAtIndex: 0];
		    NSString *t = [[sName objectForKey: @"Titles"]
				    objectAtIndex: 0];
                    
		    if ([t isEqualToString: _(@"Name")])
		      { 
			if ([objName1 isEqualToString: n] &&
			    [objName1 length] > 0 &&
			    ![objName1 isEqualToString: @"0"])
			  {
			    obj1 = [sName objectForKey: @"Type"];
			    object1 = [sName objectForKey: @"Values"];
			    [codElemts addObject: obj1];
			    [elemts addObject: object1];
			  }
			
			if ([objName2 isEqualToString: n] &&
			    [objName2 length] > 0 &&
			    ![objName2 isEqualToString: @"0"])
			  {
			    obj2 = [sName objectForKey: @"Type"];
			    object2 = [sName objectForKey: @"Values"];
			    [codElemts addObject: obj2];
			    [elemts addObject: object2];
			  }
			
			if ([objName3 isEqualToString: n] &&
			    [objName3 length] > 0 &&
			    ![objName3 isEqualToString: @"0"])
			  {
			    obj3 = [sName objectForKey: @"Type"];
			    object3 = [sName objectForKey: @"Values"];
			    [codElemts addObject: obj3];
			    [elemts addObject: object3];
			  }
			
			if ([objName4 isEqualToString: n] &&
			    [objName4 length] > 0 &&
			    ![objName4 isEqualToString: @"0"])
			  {
			    obj4 = [sName objectForKey: @"Type"];
			    object4 = [sName objectForKey: @"Values"];
			    [codElemts addObject: obj4];
			    [elemts addObject: object4];
			  }
			
			if ([objName5 isEqualToString: n] &&
			    [objName5 length] > 0 &&
			    ![objName5 isEqualToString: @"0"])
			  {
			    obj5 = [sName objectForKey: @"Type"];
			    object5 = [sName objectForKey: @"Values"];
			    [codElemts addObject: obj5];
			    [elemts addObject: object5];
			  }
		      }
		  }
		
		dEnum = [elemts objectEnumerator]; 
		while ((objChang = [dEnum nextObject]))
		  {
		    switch ([[codElemts objectAtIndex: countGen] intValue])
		      {
		      case 155:
		      case 156:
		      case 169:
			{
			  if (![vars containsObject:
				       [objChang objectAtIndex: 1]])
			    {
			      mState[count1] = [[objChang objectAtIndex: 1]
						 doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject:
					      [objChang objectAtIndex: 1]];
			      mState[count1] = gsl_vector_get (v, k);
			    }
			  
			  if (![vars containsObject:
				       [objChang objectAtIndex: 2]])
			    {
			      cElem[count1] = [[objChang objectAtIndex: 2]
						doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject:
					      [objChang objectAtIndex: 2]];
			      cElem[count1] = gsl_vector_get (v, k);
			    }
			  
			  if (![vars containsObject:
				       [objChang objectAtIndex: 3]])
			    {
			      tIniElem[count1] = [[objChang objectAtIndex: 3]
						   doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject:
					      [objChang objectAtIndex: 3]];
			      tIniElem[count1] = gsl_vector_get (v, k);
			      messageTemp = YES;
			    }
			  
			  if (![vars containsObject:
				       [objChang objectAtIndex: 4]])
			    {
			      tFinElem[count1] = [[objChang objectAtIndex: 4]
						   doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject:
					      [objChang objectAtIndex: 4]];
			      tFinElem[count1] = gsl_vector_get (v, k);
			      messageTemp = YES;
			    }
			  
			  count1 += 1;
			}
			break;
		      case 160:
		      case 161:
			{
			  NSString *sense;
			  changeOrCal = YES;
			  
			  if (![vars containsObject:
				       [objChang objectAtIndex: 1]])
			    {
			      mPhase[count2] = [[objChang objectAtIndex: 1]
						 doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject:
					      [objChang objectAtIndex: 1]];
			      mPhase[count2] = gsl_vector_get (v, k);
			    }
			  
			  if (![vars containsObject:
				       [objChang objectAtIndex: 2]])
			    {
			      cfElem[count2] = [[objChang objectAtIndex: 2]
						 doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject:
					      [objChang objectAtIndex: 2]];
			      cfElem[count2] = gsl_vector_get (v, k);
			    }
			  
			  sense = [NSString stringWithString:
					      [objChang objectAtIndex: 3]];
			  
			  if ([sense isEqualToString: @"<"])
			    {
			      senElem[count2] = -1;
			    }
			  
			  count2 += 1;
			}
			break;
		      }
		    countGen += 1;
		  }

		
		sourceObj = [[systemsApplied objectAtIndex: countSource]
			      objectEnumerator];
		
		while ((idSourceObj = [sourceObj nextObject]))
		  {
		    int t = [[[dictionary objectForKey: idSourceObj]
			       objectForKey: @"Type"] intValue];
		    sObj = [[[dictionary objectForKey: idSourceObj]
			      objectForKey: @"Values"] objectAtIndex: 0];
		    
		    switch (t)
		      {
		      case 151 ... 154:
			{
			  if ((t == 151) || (t == 152))
			    {
			      sign = 1; 
			    }
			  else
			    {
			      sign = -1;
			    }
			  
			  if (![vars containsObject: sObj])
			    {
			      sources[count] = sign*[sObj doubleValue];
			    }
			  else
			    {
			      int k = [vars indexOfObject: sObj];
			      sources[count] = sign*(gsl_vector_get (v, k));
			    }
			  
			  count += 1;
			}
			break;
		      }
		    
		  }
		
		// Build the equations
		gsl_vector_set (func, nEqu, Tm*(sources[0] + sources[1] +
						sources[2] + sources[3] +
						sources[4] + sources[5] +
						sources[6] + sources[7]) -
			mState[0]*cElem[0]*(tFinElem[0] - tIniElem[0]) -
			mState[1]*cElem[1]*(tFinElem[1] - tIniElem[1]) -
			mState[2]*cElem[2]*(tFinElem[2] - tIniElem[2]) -
			senElem[0]*mPhase[0]*cfElem[0] -
				senElem[1]*mPhase[1]*cfElem[1]);
		
		nEqu += 1;
	      }
	  }
	  break;
	}
      countSource += 1;
    }
  
  
  // Objects contained in codeOthers
  enumOthers = [codeOthers objectEnumerator];
  
  while ((object = [enumOthers nextObject]))
    {
      NSNumber *type =[[dictionary objectForKey: object]
			objectForKey: @"Type"];
      NSMutableArray *dat = [[dictionary objectForKey: object]
			      objectForKey: @"Values"];
      
      switch ([type intValue])
	{
	case 157 ... 159:
	  {
	    double kl, Li, Lf, Ti, Tf;
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		kl = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		kl = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		Li = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		Li = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		Lf = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		Lf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		Ti = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		Ti = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		Tf = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		Tf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, Lf - Li*(1 + kl*(Tf - Ti)));
	    
	    nEqu += 1;
	  }
	  break;
	case 163:
	  {
	    // Calorimetry

	    // To contained objects
	    int countSub = 0, countSimple = 0, countCgSimp = 0,
	      countCgPhase = 0, countCgSense = 0, contElemSD = 0;
	    double elemSimple[16] = {0};
	    double cgSimple[48] = {0};
	    double cgPhase[16] = {0};
	    double cgSense[8] = {1};
	    NSEnumerator *calSub;
	    //----
            
	    NSMutableArray *otherObjs = [[dictionary objectForKey: object]
					  objectForKey: @"Values"];
	    NSMutableArray *codElemts = [NSMutableArray array];
	    NSMutableArray *elemts = [NSMutableArray array];
	    
	    NSNumber *obj1, *obj2, *obj3, *obj4;  
	    NSString *objName1 = [otherObjs objectAtIndex: 0];
	    NSString *objName2 = [otherObjs objectAtIndex: 1];
	    NSString *objName3 = [otherObjs objectAtIndex: 2];
	    NSString *objName4 = [otherObjs objectAtIndex: 3];
	    
	    NSMutableArray *object1, *object2, *object3, *object4, *objCal;
	    NSEnumerator *search = [dictionary objectEnumerator];
	    NSMutableDictionary *sName;
	    
	    while ((sName = [search nextObject]))
	      {
		NSString *n = [[sName objectForKey: @"Values"]
				objectAtIndex: 0];
		NSString *t = [[sName objectForKey: @"Titles"]
				objectAtIndex: 0];
                
		if ([t isEqualToString: _(@"Name")])
		  { 
		    if ([objName1 isEqualToString: n] &&
			[objName1 length] > 0 &&
			![objName1 isEqualToString: @"0"])
		      {
			obj1 = [sName objectForKey: @"Type"];
			object1 = [sName objectForKey: @"Values"];
			[codElemts addObject: obj1];
			[elemts addObject: object1];
		      }
		    
		    if ([objName2 isEqualToString: n] &&
			[objName2 length] > 0 &&
			![objName2 isEqualToString: @"0"])
		      {
			obj2 = [sName objectForKey: @"Type"];
			object2 = [sName objectForKey: @"Values"];
			[codElemts addObject: obj2];
			[elemts addObject: object2];
		      }
		    
		    if ([objName3 isEqualToString: n] &&
			[objName3 length] > 0 &&
			![objName3 isEqualToString: @"0"])
		      {
			obj3 = [sName objectForKey: @"Type"];
			object3 = [sName objectForKey: @"Values"];
			[codElemts addObject: obj3];
			[elemts addObject: object3];
		      }   
		    
		    if ([objName4 isEqualToString: n] &&
			[objName4 length] > 0 &&
			![objName4 isEqualToString: @"0"])
		      {
			obj4 = [sName objectForKey: @"Type"];
			object4 = [sName objectForKey: @"Values"];
			[codElemts addObject: obj4];
			[elemts addObject: object4];
		      }
		  }
	      }
	    

	    calSub = [elemts objectEnumerator];
	    
	    while ((objCal = [calSub nextObject]))
	      {
		switch ([[codElemts objectAtIndex: countSub] intValue])
		  {
		  case 155:
		  case 156:
		  case 169:
		    {
		      if (![vars containsObject: [objCal objectAtIndex: 1]])
			{
			  elemSimple[countSimple] = [[objCal objectAtIndex: 1]
						      doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objCal objectAtIndex: 1]];
			  elemSimple[countSimple] = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: [objCal objectAtIndex: 2]])
			{
			  elemSimple[countSimple + 1] =
			    [[objCal objectAtIndex: 2] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objCal objectAtIndex: 2]];
			  elemSimple[countSimple + 1] = gsl_vector_get (v, k);
			}
		      
		      if (![vars containsObject: [objCal objectAtIndex: 3]])
			{
			  elemSimple[countSimple + 2] =
			    [[objCal objectAtIndex: 3] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objCal objectAtIndex: 3]];
			  elemSimple[countSimple + 2] = gsl_vector_get (v, k);
			  messageTemp = YES;
			}
		      
		      if (![vars containsObject: [objCal objectAtIndex: 4]])
			{
			  elemSimple[countSimple + 3] =
			    [[objCal objectAtIndex: 4] doubleValue];
			}
		      else
			{
			  int k = [vars indexOfObject:
					  [objCal objectAtIndex: 4]];
			  elemSimple[countSimple + 3] = gsl_vector_get (v, k);
			  messageTemp = YES;
			}
		      
		      countSimple += 4;
		    }
		    break;
		  case 162:
		    {
		      // To contained objects
		      int subCgCount = 0;
		      NSEnumerator *dataSub;

		      NSMutableArray *codElemSub = [NSMutableArray array];
		      NSMutableArray *elemSub = [NSMutableArray array];
		      
		      NSNumber *sObj1, *sObj2, *sObj3, *sObj4, *sObj5;                 
		      NSString *sNameObj1 = [objCal objectAtIndex: 1];
		      NSString *sNameObj2 = [objCal objectAtIndex: 2];
		      NSString *sNameObj3 = [objCal objectAtIndex: 3];
		      NSString *sNameObj4 = [objCal objectAtIndex: 4];       
		      NSString *sNameObj5 = [objCal objectAtIndex: 5];                                  
		      NSMutableArray *sObject1, *sObject2, *sObject3, *sObject4,
			*sObject5, *sObjCg;
		      NSEnumerator *searchSub = [dictionary objectEnumerator];
		      NSMutableDictionary *sNameSub;
		      
		      while ((sNameSub = [searchSub nextObject]))
			{
			  NSString *n = [[sNameSub objectForKey: @"Values"]
					  objectAtIndex: 0];
			  NSString *t = [[sNameSub objectForKey: @"Titles"]
					  objectAtIndex: 0];
                          
			  if ([t isEqualToString: _(@"Name")])
			    { 
			      if ([sNameObj1 isEqualToString: n] &&
				  [sNameObj1 length] > 0 &&
				  ![sNameObj1 isEqualToString: @"0"])
				{
				  sObj1 = [sNameSub objectForKey: @"Type"];
				  sObject1 = [sNameSub objectForKey: @"Values"];
				  [codElemSub addObject: sObj1];
				  [elemSub addObject: sObject1];
				}
			      
			      if ([sNameObj2 isEqualToString: n] &&
				  [sNameObj2 length] > 0 &&
				  ![sNameObj2 isEqualToString: @"0"])
				{
				  sObj2 = [sNameSub objectForKey: @"Type"];
				  sObject2 = [sNameSub objectForKey: @"Values"];
				  [codElemSub addObject: sObj2];
				  [elemSub addObject: sObject2];
				}
			      
			      if ([sNameObj3 isEqualToString: n] &&
				  [sNameObj3 length] > 0 &&
				  ![sNameObj3 isEqualToString: @"0"])
				{
				  sObj3 = [sNameSub objectForKey: @"Type"];
				  sObject3 = [sNameSub objectForKey: @"Values"];
				  [codElemSub addObject: sObj3];
				  [elemSub addObject: sObject3];
				}   
			      
			      if ([sNameObj4 isEqualToString: n] &&
				  [sNameObj4 length] > 0 &&
				  ![sNameObj4 isEqualToString: @"0"])
				{
				  sObj4 = [sNameSub objectForKey: @"Type"];
				  sObject4 = [sNameSub objectForKey: @"Values"];
				  [codElemSub addObject: sObj4];
				  [elemSub addObject: sObject4];
				}
			      
			      if ([sNameObj5 isEqualToString: n] &&
				  [sNameObj5 length] > 0 &&
				  ![sNameObj5 isEqualToString: @"0"])
				{
				  sObj5 = [sNameSub objectForKey: @"Type"];
				  sObject5 = [sNameSub objectForKey: @"Values"];
				  [codElemSub addObject: sObj5];
				  [elemSub addObject: sObject5];
				}
			    }
			}
		      
		      dataSub = [elemSub objectEnumerator]; 
		      while ((sObjCg = [dataSub nextObject]))
			{
			  switch ([[codElemSub objectAtIndex: subCgCount]
				    intValue])
			    {
			    case 155:
			    case 156:
			    case 169:
			      {
				if (![vars containsObject:
					     [sObjCg objectAtIndex: 1]])
				  {
				    cgSimple[countCgSimp] =
				      [[sObjCg objectAtIndex: 1] doubleValue];
				  }
				else
				  {
				    int k = [vars indexOfObject:
						    [sObjCg objectAtIndex: 1]];
				    cgSimple[countCgSimp] =
				      gsl_vector_get (v, k);
				  }
				
				if (![vars containsObject:
					     [sObjCg objectAtIndex: 2]])
				  {
				    cgSimple[countCgSimp + 1] =
				      [[sObjCg objectAtIndex: 2] doubleValue];
				  }
				else
				  {
				    int k = [vars indexOfObject:
						    [sObjCg objectAtIndex: 2]];
				    cgSimple[countCgSimp + 1] =
				      gsl_vector_get (v, k);
				  }
				
				if (![vars containsObject:
					     [sObjCg objectAtIndex: 3]])
				  {
				    cgSimple[countCgSimp + 2] =
				      [[sObjCg objectAtIndex: 3] doubleValue];
				  }
				else
				  {
				    int k = [vars indexOfObject:
						    [sObjCg objectAtIndex: 3]];
				    cgSimple[countCgSimp + 2] =
				      gsl_vector_get (v, k);

				    messageTemp = YES;
				  }
				
				if (![vars containsObject:
					     [sObjCg objectAtIndex: 4]])
				  {
				    cgSimple[countCgSimp + 3] =
				      [[sObjCg objectAtIndex: 4] doubleValue];
				  }
				else
				  {
				    int k = [vars indexOfObject:
						    [sObjCg objectAtIndex: 4]];
				    cgSimple[countCgSimp + 3] =
				      gsl_vector_get (v, k);

				    messageTemp = YES;
				  }
				
				countCgSimp += 4;
			      }
			      break;
			    case 160:
			    case 161:
			      {
				NSString *sense;
				changeOrCal = YES;
				
				if (![vars containsObject:
					     [sObjCg objectAtIndex: 1]])
				  {
				    cgPhase[countCgPhase] =
				      [[sObjCg objectAtIndex: 1] doubleValue];
				  }
				else
				  {
				    int k = [vars indexOfObject:
						    [sObjCg objectAtIndex: 1]];
				    cgPhase[countCgPhase] =
				      gsl_vector_get (v, k);
				  }
				
				if (![vars containsObject:
					     [sObjCg objectAtIndex: 2]])
				  {
				    cgPhase[countCgPhase + 1] =
				      [[sObjCg objectAtIndex: 2] doubleValue];
				  }
				else
				  {
				    int k = [vars indexOfObject:
						    [sObjCg objectAtIndex: 2]];
				    cgPhase[countCgPhase + 1] =
				      gsl_vector_get (v, k);
				  }
				
				sense = [NSString stringWithString:
						    [sObjCg objectAtIndex: 3]];
				
				if ([sense isEqualToString: @"<"])
				  {
				    cgSense[countCgSense] = -1;
				  }
				
				countCgPhase += 2;
				countCgSense += 1;
			      }
			      break;
			    }
			  subCgCount += 1;
			}
		      contElemSD = contElemSD + 1;
		      countCgSimp = contElemSD*12;
		      countCgPhase = contElemSD*4;
		      countCgSense = contElemSD*2;
		    }
		    break;
		  }
		countSub += 1;
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, 
			    elemSimple[0]*elemSimple[1]*(elemSimple[3] -
							 elemSimple[2])
			    +  elemSimple[4]*elemSimple[5]*(elemSimple[7] -
							    elemSimple[6])
			    +  elemSimple[8]*elemSimple[9]*(elemSimple[11] -
							    elemSimple[10])
			    +  elemSimple[12]*elemSimple[13]*(elemSimple[15]
							      - elemSimple[14])
			    + cgSimple[0]*cgSimple[1]*(cgSimple[3] -
						       cgSimple[2])
			    + cgSense[0]*cgPhase[0]*cgPhase[1]
			    + cgSimple[4]*cgSimple[5]*(cgSimple[7] -
						       cgSimple[6])
			    + cgSense[1]*cgPhase[2]*cgPhase[3]
			    + cgSimple[8]*cgSimple[9]*(cgSimple[11] -
						       cgSimple[10])
			    + cgSimple[12]*cgSimple[13]*(cgSimple[15] -
							 cgSimple[14])
			    + cgSense[2]*cgPhase[4]*cgPhase[5]
			    + cgSimple[16]*cgSimple[17]*(cgSimple[19] -
							 cgSimple[18])
			    + cgSense[3]*cgPhase[6]*cgPhase[7]
			    + cgSimple[20]*cgSimple[21]*(cgSimple[23] -
							 cgSimple[22])
			    + cgSimple[24]*cgSimple[25]*(cgSimple[27] -
							 cgSimple[26])
			    + cgSense[4]*cgPhase[8]*cgPhase[9]
			    + cgSimple[28]*cgSimple[29]*(cgSimple[31] -
							 cgSimple[30])
			    + cgSense[5]*cgPhase[10]*cgPhase[11]
			    + cgSimple[32]*cgSimple[33]*(cgSimple[35] -
							 cgSimple[34])
			    + cgSimple[36]*cgSimple[37]*(cgSimple[39] -
							 cgSimple[38])
			    + cgSense[6]*cgPhase[12]*cgPhase[13]
			    + cgSimple[40]*cgSimple[41]*(cgSimple[43] -
							 cgSimple[42])
			    + cgSense[7]*cgPhase[14]*cgPhase[15]
			    + cgSimple[44]*cgSimple[45]*(cgSimple[47] -
							 cgSimple[46])
			    );
	    
	    nEqu += 1;
	  }
	  break;
	case 164:
	  {
	    // Gas at constant pressure
	    double Vi, Ti, Vf, Tf;
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		Vi = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		Vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		Ti = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		Ti = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		Vf = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		Vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		Tf = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		Tf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equation
	    gsl_vector_set (func, nEqu, Vi/Ti - Vf/Tf);
	    
	    nEqu += 1;
	  }
	  break;
	case 165:
	  {
	    // Gas at constant temperature
	    double Pi, Vi, Pf, Vf;
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		Pi = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		Pi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		Vi = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		Vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		Pf = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		Pf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		Vf = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		Vf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, Pi*Vi - Pf*Vf);
	    
	    nEqu += 1;
	  }
	  break;
	case 166:
	  {
	    // Gas at constant volume 
	    double Pi, Ti, Pf, Tf;
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		Pi = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		Pi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		Ti = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		Ti = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		Pf = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		Pf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		Tf = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		Tf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, Pi/Ti - Pf/Tf);
	    
	    nEqu += 1;
	  }
	  break;
	case 167:
	  {
	    // Ideal gas
	    double Pi, Vi, Ti, Pf, Vf, Tf; 
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		Pi = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		Pi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		Vi = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		Vi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		Ti = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		Ti = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		Pf = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		Pf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		Vf = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		Vf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		Tf = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		Tf = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, Pi*Vi/Ti - Pf*Vf/Tf);
	    
	    nEqu += 1;
	  }
	  break;
	case 168:
	  {
	    // Heat exchanger
	    double TRi, TRf, dR, cR, TFi, TFf, dF, cF;
	    
	    if (![vars containsObject: [dat objectAtIndex: 0]])
	      {
		TRi = [[dat objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 0]];
		TRi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 1]])
	      {
		TRf = [[dat objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 1]];
		TRf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 2]])
	      {
		dR = [[dat objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 2]];
		dR = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 3]])
	      {
		cR = [[dat objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 3]];
		cR = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 4]])
	      {
		TFi = [[dat objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 4]];
		TFi = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 5]])
	      {
		TFf = [[dat objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 5]];
		TFf = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 6]])
	      {
		dF = [[dat objectAtIndex: 6] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 6]];
		dF = gsl_vector_get (v, k);
	      }
	    
	    if (![vars containsObject: [dat objectAtIndex: 7]])
	      {
		cF = [[dat objectAtIndex: 7] doubleValue];
	      }
	    else
	      {
		int k = [vars indexOfObject: [dat objectAtIndex: 7]];
		cF = gsl_vector_get (v, k);
	      }
	    
	    // Build the equations
	    gsl_vector_set (func, nEqu, dR*cR*(TRf - TRi) + dF*cF*(TFf - TFi));
	    
	    nEqu += 1;
	  }
	  break;
	}
    }

  return GSL_SUCCESS;
}

@interface FLCalorimetry (Private)
- (void) makeSystem;
@end

@implementation FLCalorimetry (Private)
- (void) makeSystem
{
  int increase = 1;
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
  id anObject;
  NSMutableArray *results = [NSMutableArray array];
  NSEnumerator *varCount;
  
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
	  par = gsl_rng_uniform (r);
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
      
      // Verify the status
      if ( (stateInt) && (increase < 90) )
	{
	  increase = increase + 1;
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
  
  while ((anObject = [varCount nextObject]))
    {
      [results addObject: [NSNumber numberWithDouble:
				      gsl_vector_get (s->x, countRes)]];
      countRes += 1;
    }
  
  // Print the results
  [self printUnknowns: vars withResults: results];
  
  // Print the calculus state
  message = [NSString stringWithFormat: [errors objectAtIndex: 8],
		      gsl_strerror (state)];
  length = [ [[self viewer] textStorage] length];
  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: message];
  
  // Print a message about the temperature
  if ( (messageTemp) && (changeOrCal) )
    {
      length = [[[self viewer] textStorage] length];
      [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
				   withString: [errors objectAtIndex: 13]];
    }
  
  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  gsl_rng_free (r);
}
@end

@implementation FLCalorimetry

- (id) init
{
  NSBundle *messages;
  self = [super init];
  
  vars = [NSMutableArray new];
  codeContainedObjects = [NSMutableArray new];
  codeOthers = [NSMutableArray new];
  systemsApplied = [NSMutableArray new];
  systemsType = [NSMutableArray new];
  dictionary = [NSMutableDictionary new];
  
  // Build the array of messages
  messages = [NSBundle mainBundle];
  errors = [[NSArray alloc] initWithContentsOfFile:
    		         [messages pathForResource: @"calorimetryMessages"
					    ofType: @"plist"]];
  
  return self;
}

- (void) makeEquationsForData: (NSMutableDictionary *)list
{
  int numError = 0;

  int width, height, totalCells; //For handle the chalkboard size
  int length, numEquations = 0, numDeposits = 0, numFlows = 0;
  BOOL error = NO;
  BOOL errorName = NO;
  NSNumber *identifier, *code;
  NSMutableArray *namesObjects = [NSMutableArray array];
  NSMutableArray *namesContainedObjects = [NSMutableArray array];
  NSMutableArray *namesContainedChangeObjects = [NSMutableArray array];
  NSMutableArray *codDeposits = [NSMutableArray array];
  NSMutableArray *codFlows = [NSMutableArray array];
  NSMutableArray *controlProcess = [NSMutableArray array];
  NSArray *keys;
  NSEnumerator *count;

  // For previous verifications
  int objsContained = 0, w = 0;
  NSMutableArray *codSourcesObjs;

  messageTemp = NO;
  changeOrCal = NO;
  varT = 0;
  timeData = 0;

  // For handle the chalkboard size
  width = [self chalkboardWidth];
  height = [self chalkboardHeight];
  totalCells = width*height - 1;

  [dictionary setDictionary: list];
  keys = [[NSArray alloc] initWithArray: [list allKeys]];
  count = [keys objectEnumerator];
  
  while ((code = [count nextObject]) && !error)
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

      // To searchthe applied sources or flows
      int k, pos;
      NSNumber *ident;
      NSButton *cell;
      NSEnumerator *search;

      identifier = [[dictionary objectForKey: code] objectForKey: @"Type"];
      [values removeAllObjects];
      
      // Count the amount of unknowns
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
	      ![title isEqualToString: _(@"Object 5")] &&
	      ![title isEqualToString: _(@"Sense")])
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
			  if (![key isEqualToString: @"C"] &&
			      ![key isEqualToString: @"R"] &&
			      ![key isEqualToString: @"F"])
			    {
			      number = [NSNumber numberWithDouble:
					[var doubleValue]*
					[[[self conversions] objectForKey: key]
					  doubleValue]];
			    }
			  else
			    {
			      if ([key isEqualToString: @"C"])
				{
				  number = [NSNumber numberWithDouble:
					       [var doubleValue] + 273.15];
				}
			      else
				{
				  if ([key isEqualToString: @"R"])
				    {
				      number = [NSNumber numberWithDouble:
					   0.55555555*([var doubleValue] -
						       491.67) + 273.15];
				    }
				  else
				    {
				      number = [NSNumber numberWithDouble:
					   0.55555555*([var doubleValue] - 32)
							 + 273.15];
				    }
				}
			    }
			  
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
		      length = [[[self viewer] textStorage] length];
		      [[self viewer] replaceCharactersInRange: 
				                   NSMakeRange(length, 0)
						   withString: advert];
		      error = YES;
		    }
		}
	      else
		{
		  // Add a simple variable (without conversion factor)
		  NSString *varFactor = nil;
		  
		  //Se determina el tipo de variable
		  if ([title hasPrefix: @"m"])
		    {
		      varFactor = @"kg";
		    }
		  else if ([title hasPrefix: @"T"])
		    {
		      varFactor = @"K";
		    }
		  else if ([title hasPrefix: @"L"])
		    {
		      varFactor = @"m";
		    }
		  else if ([title hasPrefix: @"t"])
		    {
		      varFactor = @"s";
		    }
		  else if ([title hasPrefix: @"S"])
		    {
		      varFactor = @"m2";
		    }
		  else if ([title hasPrefix: @"V"])
		    {
		      varFactor = @"m3";
		    }
		  else if ([title hasPrefix: @"P"])
		    {
		      varFactor = @"Pa";
		    }
		  else if ([title hasPrefix: @"c"] ||
			   [title hasPrefix: @"cR"] ||
			   [title hasPrefix: @"cF"])
		    {
		      varFactor = @"J/kg*K";
		    }
		  else if ([title hasPrefix: @"cf"] ||
			   [title hasPrefix: @"cv"])
		    {
		      varFactor = @"J/kg";
		    }
		  else if ([title hasPrefix: @"k"])
		    {
		      varFactor = @"1/K";
		    }
		  else if ([title hasPrefix: @"Q"])
		    {
		      varFactor = @"J";
		    }
		  else if ([title hasPrefix: @"dQ"])
		    {
		      varFactor = @"W";
		    }
		  else
		    {
		      varFactor = @"m3/s";
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
      

      if (error)
	break;

      // Check if time is unknown and count the number of equations      
      switch ([identifier intValue])
	{
	case 150:
	  {
	    if (![self isNumericDataTheString: [values objectAtIndex: 0]])
	      {
		timeVar = [values objectAtIndex: 0];
		varT = 1;
	      }
	    else
	      {
		timeData = [[values objectAtIndex: 0] doubleValue];
		varT = 2;
	      }
	  }
	  break;
	case 151:
	case 153:
	  {
	    numDeposits += 1;
	    [codDeposits addObject: code];
	  }
	  break;
	case 152:
	case 154:
	  {
	    numFlows += 1;
	    [codFlows addObject: code];
	  }
	  break;
	case 155:
	case 156:
	case 169:
	  {
	    numEquations += 1;
	    [codeContainedObjects addObject: code];
	    
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
	  }
	  break;
	case 157 ... 159:
	  {
	    numEquations += 1;
	    [codeOthers addObject: code];
	  }
	  break;
	case 160:
	case 161:
	  {
	    numEquations += 1;
	    [codeContainedObjects addObject: code];
	    
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
	  }
	  break;
	case 162:
	  {
	    int c = 0;
	    numEquations += 1;
	    [codeContainedObjects addObject: code];
	    
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
	    
	    
	    if ([[values objectAtIndex: 1] length] > 0 &&
		![[values objectAtIndex: 1] isEqualToString: @"0"])
	      {
		[namesContainedChangeObjects addObject:
				 [values objectAtIndex: 1]];
		c++;
	      }

	    if ([[values objectAtIndex: 2] length] > 0 &&
		![[values objectAtIndex: 2] isEqualToString: @"0"])
	      {
		[namesContainedChangeObjects addObject:
				 [values objectAtIndex: 2]];
		c++;
	      }

	    if ([[values objectAtIndex: 3] length] > 0 &&
		![[values objectAtIndex: 3] isEqualToString: @"0"])
	      {
		[namesContainedChangeObjects addObject:
				 [values objectAtIndex: 3]];
		c++;
	      }

	    if ([[values objectAtIndex: 4] length] > 0 &&
		![[values objectAtIndex: 4] isEqualToString: @"0"])
	      {
		[namesContainedChangeObjects addObject:
				 [values objectAtIndex: 4]];
		c++;
	      }

	    if ([[values objectAtIndex: 5] length] > 0 &&
		![[values objectAtIndex: 5] isEqualToString: @"0"])
	      {
		[namesContainedChangeObjects addObject:
				 [values objectAtIndex: 5]];
		c++;
	      }

	    [controlProcess addObject: [NSNumber numberWithInt: c]];
	  }
	  break;
	case 163:
	  {
	    numEquations += 1;
	    [codeOthers addObject: code];

	    if ([[values objectAtIndex: 0] length] > 0 &&
		![[values objectAtIndex: 0] isEqualToString: @"0"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 0]];
	      }

	    if ([[values objectAtIndex: 1] length] > 0 &&
		![[values objectAtIndex: 1] isEqualToString: @"0"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 1]];
	      }

	    if ([[values objectAtIndex: 2] length] > 0 &&
		![[values objectAtIndex: 2] isEqualToString: @"0"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 2]];
	      }

	    if ([[values objectAtIndex: 3] length] > 0 &&
		![[values objectAtIndex: 3] isEqualToString: @"0"])
	      {
		[namesContainedObjects addObject: [values objectAtIndex: 3]];
	      }
	  }
	  break;
	case 164 ... 167:
	  {
	    numEquations += 1;
	    [codeOthers addObject: code];
	  }
	  break;
	case 168:
	  {
	    numEquations += 1;
	    [codeOthers addObject: code];
	  }
	  break;
	}
      // Here ends the verify for equations
      
      if (errorName)
	{
	  numError = 14;
	  break;
	}
      
      // Search the applied sources or flows
      search = [[self cells] objectEnumerator];
      
      if ([identifier intValue] == 155 || [identifier intValue] == 156 ||
	  [identifier intValue] == 169 || ([identifier intValue] >= 160 &&
					   [identifier intValue] <= 162))
	{
	  NSMutableArray *sources = [NSMutableArray array];
	  NSArray *objectsOrder = [self cells];
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
		    { [sources addObject: ident]; }
		}
	      
	      if (pos - (width - 1) >= 0)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - (width - 1)] tag]];

		  if ([ident intValue] != 0)
		    { [sources addObject: ident]; }
		}
	      
	      if (pos - width >= 0)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos - width] tag]];

		  if ([ident intValue] != 0)
		    { [sources addObject: ident]; }
		}
	      
	      if (pos - (width + 1) >= 0)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - (width + 1)] tag]];

		  if ([ident intValue] != 0)
		    { [sources addObject: ident]; }
		}
	      
	      if (pos + 1 <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos + 1] tag]];

		  if ([ident intValue] != 0)
		    { [sources addObject: ident]; }
		}
	      
	      if (pos + (width - 1) <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + (width - 1)] tag]];

		  if ([ident intValue] != 0)
		    { [sources addObject: ident]; }
		}
	      
	      if (pos + width <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
			      [[objectsOrder objectAtIndex: pos + width] tag]];

		  if ([ident intValue] != 0)
		    { [sources addObject: ident]; }
		}
	      
	      if (pos + (width + 1) <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + (width + 1)] tag]];

		  if ([ident intValue] != 0)
		    { [sources addObject: ident]; }
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
			{ [sources addObject: ident]; }
		    }
		  
		  if (pos - width >= 0)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos - width] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		  
		  if (pos + 1 <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
				  [[objectsOrder objectAtIndex: pos + 1] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		  
		  if (pos + width <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos + width] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		  
		  if (pos + (width + 1) <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
		        [[objectsOrder objectAtIndex: pos + (width + 1)] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		}
	      else
		{
		  if (pos - 1 >= 0)
		    {
		      ident = [NSNumber numberWithInt:
				  [[objectsOrder objectAtIndex: pos - 1] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		  
		  if (pos - width >= 0)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos - width] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		  
		  if (pos - (width + 1) >= 0)
		    {
		      ident = [NSNumber numberWithInt:
			[[objectsOrder objectAtIndex: pos - (width + 1)] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		  
		  if (pos + (width - 1) <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			[[objectsOrder objectAtIndex: pos + (width - 1)] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		  
		  if (pos + width <= totalCells)
		    {
		      ident = [NSNumber numberWithInt:
			  [[objectsOrder objectAtIndex: pos + width] tag]];

		      if ([ident intValue] != 0)
			{ [sources addObject: ident]; }
		    }
		}
	    }

	  // Add the sources/flows to the array
	  if ([sources count] == 0)
	    {
	      numEquations -= 1;
	    }
	  
	  [systemsApplied addObject: sources];
	}
    }
  
  
  // Previous verifications
  codSourcesObjs = [NSMutableArray array];
  if (!error && !errorName)
    {
      int c, v;
      int nameIndex = 0;
      id checkObj, subCheckObj;
      NSEnumerator *enumerator;
      NSNumber *source;

      // To check if only sources or flows are applied, not both
      int sApplied, controlSource, controlFlow;

      // Check if there is a time data if there are applied flows
      if ( (numFlows > 0) && (varT == 0) )
	{
	  numError = 5;
	}
  
      // Check if referenced objects exists
      for (c = 0; c < [namesContainedObjects count]; c++)
	{
	  if ([namesObjects containsObject:
			      [namesContainedObjects objectAtIndex: c]])
	    {  
	      objsContained += 1;
	    }
	}
      
      for (c = 0; c < [namesContainedChangeObjects count]; c++)
	{
	  if ([namesObjects containsObject:
			      [namesContainedChangeObjects objectAtIndex: c]])
	    {  
	      objsContained += 1;
	    }
	}
      
      // count the sources applied
      for (v = 0; v < [systemsApplied count]; v++)
	{
	  [codSourcesObjs addObjectsFromArray:
		[systemsApplied objectAtIndex: v]];
	}
      
      // Check if all sources are applied
      enumerator = [codDeposits objectEnumerator];
      while ((source = [enumerator nextObject]))
	{
	  if ([codSourcesObjs containsObject: source])
	    {
	      w += 1;
	    }
	}
      
      enumerator = [codFlows objectEnumerator];
      while ((source = [enumerator nextObject]))
	{
	  if ([codSourcesObjs containsObject: source])
	    {
	      w += 1;
	    }
	}
      
      // Check if objects have sources/flows applied when correspond
      enumerator = [namesObjects objectEnumerator];
      while ((checkObj = [enumerator nextObject]))
	{
	  if (checkObj != [NSNull null])
	    {
	      if (![namesContainedObjects containsObject: checkObj] &&
		  ![namesContainedChangeObjects containsObject: checkObj] &&
		  [[systemsApplied objectAtIndex: nameIndex] count] == 0)
		{
		  numError = 15;
		}
	    }
	  else
	    {
	      if ([[systemsApplied objectAtIndex: nameIndex] count] == 0)
		{
		  numError = 15;
		}
	    }
	  
	  nameIndex++;
	}
      
      // Check names
      if (objsContained == ([namesContainedObjects count] +
			    [namesContainedChangeObjects count]))
	{
	  /* Check if objects at calorimetry have applied sources/flows
	     or if are change phase objects */
	  int indexConObj, depApplied, typeObjCal;
	  int cc = 0, elemCount = 0, phaseCount = 0;
	  NSNumber *contChange;

	  enumerator = [namesContainedObjects objectEnumerator];
	  
	  while ((checkObj = [enumerator nextObject]))
	    {
	      indexConObj = [namesObjects indexOfObject: checkObj];
	      depApplied = [[systemsApplied objectAtIndex: indexConObj] count];
	      typeObjCal = [[[dictionary objectForKey:
			     [codeContainedObjects objectAtIndex: indexConObj]]
			      objectForKey: @"Type"] intValue];
	      
	      if (depApplied > 0)
		{
		  numError = 6;
		}
	      
	      if ((typeObjCal == 160) || (typeObjCal == 161))
		{
		  numError = 16;
		}
	    }
	  
	  /* Check if objects at process have applied sources/flows
	     or if are change phase objects */
	  enumerator = [namesContainedChangeObjects objectEnumerator];
	  
	  while ((checkObj = [enumerator nextObject]))
	    {
	      indexConObj = [namesObjects indexOfObject: checkObj];
	      depApplied = [[systemsApplied objectAtIndex: indexConObj] count];
	      contChange = [[dictionary objectForKey:
			     [codeContainedObjects objectAtIndex: indexConObj]]
			      objectForKey: @"Type"];
	      
	      if (depApplied > 0)
		{
		  numError = 9;
		}
	      
	      if ([contChange intValue] == 162)
		{
		  numError = 10;
		}
	    }
	  
	  // Check the other objects in process
	  enumerator = [namesContainedChangeObjects objectEnumerator];
	  while ((checkObj = [enumerator nextObject]))
	    {
	      indexConObj = [namesObjects indexOfObject: checkObj];
	      contChange = [[dictionary objectForKey:
			     [codeContainedObjects objectAtIndex: indexConObj]]
			      objectForKey: @"Type"];
	      
	      if ( ([contChange intValue] == 155) ||
		   ([contChange intValue] == 156) ||
		   ([contChange intValue] == 169) )
		{
		  elemCount += 1;
		}
	      
	      if ( ([contChange intValue] == 160) ||
		   ([contChange intValue] == 161) )
		{
		  phaseCount += 1;
		}
	      
	      if (elemCount > 3)
		{
		  numError = 11;
		}
	      
	      if (phaseCount > 2)
		{
		  numError = 12;
		}
	      
	      if ((elemCount + phaseCount) ==
		  [[controlProcess objectAtIndex: cc] intValue])
		{
		  elemCount = 0;
		  phaseCount = 0;
		  cc++;
		}
	    }
	}
      
      // Check sources/flows applied, not both at same time
      enumerator = [systemsApplied objectEnumerator];
      
      while ((checkObj = [enumerator nextObject]))
	{
	  controlSource = 0;
	  controlFlow = 0;
	  enumerator = [checkObj objectEnumerator];
	  
	  while ((subCheckObj = [enumerator nextObject]))
	    {
	      
	      sApplied = [[[dictionary objectForKey: subCheckObj]
			    objectForKey: @"Type"] intValue];

	      if ((sApplied == 151) || (sApplied == 153))
		{
		  controlSource += 1;
		}
	      else
		{
		  controlFlow += 1;
		}
	    }
	  
	  if ((controlSource == [checkObj count]) ||
	      (controlFlow == [checkObj count]))
	    {
	      if (controlSource == [checkObj count])
		{
		  [systemsType addObject: [NSNumber numberWithInt: 1]];
		}
	      else
		{
		  [systemsType addObject: [NSNumber numberWithInt: 2]];
		}
	    }
	  else
	    {
	      numError = 7;
	      break;
	    }
	}
    }
  
  
  // Final verifications 
  if (!error && !errorName)
    {
      if ([vars count] == numEquations)
	{
	  if (objsContained == ([namesContainedObjects count] +
				[namesContainedChangeObjects count]))
	    {
	      if ( (w == (numDeposits + numFlows)) &&
		   (w == [codSourcesObjs count]) )
		{
		  // All OK, build and solve the system
		  [self makeSystem];
		}
	      else
		{
		  NSString *advert = nil;
		  
		  if (w < (numDeposits + numFlows) )
		    {
		      advert = [NSString stringWithString:
					   [errors objectAtIndex: 3]];
		    }
		  
		  if (w < [codSourcesObjs count])
		    {
		      advert = [NSString stringWithString:
					   [errors objectAtIndex: 4]];
		    }
		  
		  length = [[[self viewer] textStorage] length];
		  [[self viewer] replaceCharactersInRange:
				               NSMakeRange(length, 0)
					       withString: advert];
		}
	    }
	  else
	    {
	      length = [[[self viewer] textStorage] length];
	      [[self viewer] replaceCharactersInRange:
			           NSMakeRange(length, 0) 
				   withString: [errors objectAtIndex: 2]];
	    }
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
      if (numError != 0)
	{
	  length = [[[self viewer] textStorage] length];
	  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: [errors objectAtIndex: numError]];
	}
    }
  
  [keys release];
}

- (void) dealloc
{
  [dictionary release];
  [vars release];
  [codeContainedObjects release];
  [errors release];
  [codeOthers release];
  [systemsApplied release];
  [systemsType release];
  [super dealloc];
}

@end
