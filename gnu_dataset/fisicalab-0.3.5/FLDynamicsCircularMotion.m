/* 
   Copyright (C) 2012, 2013, 2014 German A. Arias

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
#import "FLDynamicsCircularMotion.h"


static int buildSystem (const gsl_vector *vrs, void *params, gsl_vector *func)
{
  int tIndex, gIndex, elementType, problemType, idForce = 0, lawElement = 0;
  int nEcu = 0;
  double tf, gf;
  NSNumber *object, *type, *codForce;
  NSMutableArray *data;
  NSEnumerator *enumerator, *forceEnumerator;
  // For data element
  NSString *nameOne, *nameTwo;
  NSNumber *idOne, *idTwo;
  NSMutableArray *dataOne, *dataTwo, *forceData;

  // Get properties of FL object.
  FLDynamicsCircularMotion *FLObj =
    (__bridge FLDynamicsCircularMotion *)(params);

  NSInteger varT = FLObj->varT;
  NSInteger varG = FLObj->varG;
  NSString *timeVar = FLObj->timeVar;
  NSString *gravityVar = FLObj->gravityVar;
  double timeDat = FLObj->timeDat;
  double gravityDat = FLObj->gravityDat;
  NSMutableArray *unknowns = FLObj->unknowns;
  NSMutableArray *objectsLaw = FLObj->objectsLaw;
  NSMutableArray *objectLawType = FLObj->objectLawType;
  NSMutableArray *objectsNames = FLObj->objectsNames;
  NSMutableArray *objectsIds = FLObj->objectsIds;
  NSMutableArray *objectsOthers = FLObj->objectsOthers;
  NSMutableArray *objectsMobilAndSpring = FLObj->objectsMobilAndSpring;
  NSMutableArray *forcesType = FLObj->forcesType;
  NSMutableArray *forcesArrays = FLObj->forcesArrays;
  NSMutableArray *postCheck = FLObj->postCheck;
  NSMutableDictionary *objectsDictionary = FLObj->objectsDictionary;
  // ----------------------------------------

  if (varG == 1)
    {
      gIndex = [unknowns indexOfObject: gravityVar];
      gf = gsl_vector_get (vrs, gIndex);
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
      tIndex = [unknowns indexOfObject: timeVar];
      tf = gsl_vector_get (vrs, tIndex);
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
  
  enumerator = [objectsLaw objectEnumerator];
  
  while ((object = [enumerator nextObject]))
    {
      type = [[objectsDictionary objectForKey: object] objectForKey: @"Type"];
      data = [[objectsDictionary objectForKey: object] objectForKey: @"Values"];

      elementType = [type intValue];
      
      switch (elementType)
	{
	case 305:
	case 307:
	  {
	    double work = 0, power = 0;

	    // Get the names of the elements
	    nameOne = [[data objectAtIndex: 0] description];
	    nameTwo = [[data objectAtIndex: 1] description];
	    
	    // Get the ids of the elements
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    idTwo = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameTwo]];
	    
	    // Get the data of the elements
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];
	    dataTwo = [[objectsDictionary objectForKey: idTwo]
			objectForKey: @"Values"];
	    
	    if (elementType == 305)
	      {
		// Get the data of the work
		if (![unknowns containsObject: [data objectAtIndex: 2]])
		  {
		    work = [[data objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		    work = gsl_vector_get (vrs, k);
		  }
	      }
	    else
	      {
		// Get the data of the power
		if (![unknowns containsObject: [data objectAtIndex: 2]])
		  {
		    power = [[data objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		    power = gsl_vector_get (vrs, k);
		  }
	      }

	    problemType = [[objectLawType objectAtIndex: lawElement] intValue];

	    switch (problemType)
	      {
	      case 0:
		{
		  double massOne, massTwo, yOne, yTwo;

		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      yOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work - gf*
				      (massTwo*yTwo - massOne*yOne));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf - gf*
				      (massTwo*yTwo - massOne*yOne));
		    }
		}
		break;
	      case 1:
		{
		  double massOne, massTwo, yOne, yTwo, vTwo;

		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      yOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }
		  
		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*gf*yOne));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*gf*yOne));
		    }
		}
		break;
	      case 2:
		{
		  double massOne, massTwo, yOne, yTwo, vTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      yOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }
		  
		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*gf*yOne));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*gf*yOne));
		    }
		}
		break;
	      case 3:
		{
		  double massOne, massTwo, vOne, yOne, yTwo;

		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  //Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      (gf*massTwo*yTwo -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				      (gf*massTwo*yTwo -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		}
		break;
	      case 4:
		{
		  double massOne, massTwo, vOne, vTwo, yOne, yTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		}
		break;
	      case 5:
		{
		  double massOne, massTwo, vOne, vTwo, yOne, yTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		}
		break;
	      case 6:
		{
		  double massOne, massTwo, vOne, yOne, yTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }
		  
		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      (gf*massTwo*yTwo - massOne*
				       (0.5*vOne*vOne + gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				      (gf*massTwo*yTwo - massOne*
				       (0.5*vOne*vOne + gf*yOne)));
		    }
		}
		break;
	      case 7:
		{
		  double massOne, massTwo, vOne, vTwo, yOne, yTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		}
		break;
	      case 8:
		{
		  double massOne, massTwo, vOne, vTwo, yOne, yTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
				       massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		}
		break;
	      case 9:
		{
		  double massOne, massTwo, vOne, vtTwo, vrTwo, yOne, yTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vtTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vtTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 5]])
		    {
		      vrTwo = [[dataTwo objectAtIndex: 5] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 5]];
		      vrTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
			      (massTwo*(0.5*(vtTwo*vtTwo + vrTwo*vrTwo) +
			       gf*yTwo) - massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
			      (massTwo*(0.5*(vtTwo*vtTwo + vrTwo*vrTwo) +
			       gf*yTwo) - massOne*(0.5*vOne*vOne + gf*yOne)));
		    }
		}
		break;
	      case 10:
		{
		  double massOne, massTwo, vtOne, vrOne, vTwo, yOne, yTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vtOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vtOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataOne objectAtIndex: 5]])
		    {
		      vrOne = [[dataOne objectAtIndex: 5] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 5]];
		      vrOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
			      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
			       massOne*(0.5*(vtOne*vtOne + vrOne*vrOne) +
					gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
			      (massTwo*(0.5*vTwo*vTwo + gf*yTwo) -
			       massOne*(0.5*(vtOne*vtOne + vrOne*vrOne) +
					gf*yOne)));
		    }
		}
		break;
	      case 11:
		{
		  double massOne, massTwo, vtOne, vrOne, vtTwo, vrTwo,
		    yOne, yTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vtOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vtOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
		    {
		      yOne = [[dataOne objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 4]];
		      yOne = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataOne objectAtIndex: 5]])
		    {
		      vrOne = [[dataOne objectAtIndex: 5] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 5]];
		      vrOne = gsl_vector_get (vrs, k);
		    }

		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vtTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vtTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 4]])
		    {
		      yTwo = [[dataTwo objectAtIndex: 4] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 4]];
		      yTwo = gsl_vector_get (vrs, k);
		    }

		  if (![unknowns containsObject: [dataTwo objectAtIndex: 5]])
		    {
		      vrTwo = [[dataTwo objectAtIndex: 5] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 5]];
		      vrTwo = gsl_vector_get (vrs, k);
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
			      (massTwo*(0.5*(vtTwo*vtTwo + vrTwo*vrTwo) +
					gf*yTwo) -
			       massOne*(0.5*(vtOne*vtOne + vrOne*vrOne) +
					gf*yOne)));
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
			      (massTwo*(0.5*(vtTwo*vtTwo + vrTwo*vrTwo) +
					gf*yTwo) -
			       massOne*(0.5*(vtOne*vtOne + vrOne*vrOne) +
					gf*yOne)));
		    }
		}
		break;
		// System elements
	      case 12 ... 36:
		{
		  int z, objectType;
		  double initSystemEnergy = 0, finalSystemEnergy = 0;
		  double initTotalMass = 0, finalTotalMass = 0;
		  double initCenterVelocity = 0, finalCenterVelocity = 0;
		  NSString *elementName;
		  NSNumber *elementId;
		  NSMutableArray *elementData;

		  // Get the data of the elements on final system
		  for (z = 1; z <= 4; z++)
		    {
		      // Get the names of z element
		      elementName = [[dataTwo objectAtIndex: z] description];

		      if (![elementName isEqualToString: @""] &&
			  ![elementName isEqualToString: @"0"] )
			{
			  // Get the id of the z element
			  elementId = [objectsIds objectAtIndex:
				    [objectsNames indexOfObject: elementName]];
			  // Get the data of the z element
			  elementData = [[objectsDictionary objectForKey:
					 elementId] objectForKey: @"Values"];
			  // Get the type of the z element
			  objectType = [[[objectsDictionary objectForKey:
				 elementId] objectForKey: @"Type"] intValue];
			  
			  switch (objectType)
			    {
			    case 301:
			      {
				double mass, y;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    mass = [[elementData objectAtIndex: 1]
					     doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    mass = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    y = [[elementData objectAtIndex: 2]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    y = gsl_vector_get (vrs, k);
				  }
				
				finalSystemEnergy += mass*gf*y;
			      }
			      break;
			    case 302:
			    case 303:
			      {
				double mass, v, y;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    mass = [[elementData objectAtIndex: 1]
					     doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    mass = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    v = [[elementData objectAtIndex: 2]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    v = gsl_vector_get (vrs, k);

				    if (objectType == 302 &&
					((problemType >= 18 &&
					  problemType <= 20) ||
					 (problemType >= 32 &&
					  problemType <= 36)) )
				      {
					// Add the var to postCheck array.
					[postCheck addObject:
					     [elementData objectAtIndex: 2]];
				      }
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 4]])
				  {
				    y = [[elementData objectAtIndex: 4]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 4]];
				    y = gsl_vector_get (vrs, k);
				  }
				
				/* When a problem have a center, we assume
				   that events occurs in an horizontal plane. */
				if ((problemType >= 18 && problemType <= 20) ||
				    problemType == 35 || problemType == 36 )
				  {
				    finalSystemEnergy += 0.5*mass*v*v;
				  }
				else
				  {
				    finalSystemEnergy += mass*(0.5*v*v + gf*y);
				  }
				
				if (objectType == 303)
				  {
				    finalTotalMass += mass;
				  }
			      }
			      break;
			    case 317 ... 320:
			      {
				double kres, d;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    kres = [[elementData objectAtIndex: 1]
					     doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    kres = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    d = [[elementData objectAtIndex: 2]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    d = gsl_vector_get (vrs, k);
				  }
				
				finalSystemEnergy += 0.5*kres*d*d;
			      }
			      break;
			    case 334:
			      {
				double v;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    v = [[elementData objectAtIndex: 1]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    v = gsl_vector_get (vrs, k);

				    // Add the var to postCheck array.
				    [postCheck addObject:
					 [elementData objectAtIndex: 1]];
				  }
				
				finalCenterVelocity = v;
			      }
			      break;
			    case 339:
			      {
				double mass, vt, y, vr;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    mass = [[elementData objectAtIndex: 1]
					     doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    mass = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    vt = [[elementData objectAtIndex: 2]
					   doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    vt = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 4]])
				  {
				    y = [[elementData objectAtIndex: 4]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 4]];
				    y = gsl_vector_get (vrs, k);
				  }

				if (![unknowns containsObject:
					 [elementData objectAtIndex: 5]])
				  {
				    vr = [[elementData objectAtIndex: 5]
					   doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 5]];
				    vr = gsl_vector_get (vrs, k);
				  }
				
				finalSystemEnergy += mass*(0.5*(vt*vt + vr*vr) +
							   gf*y);
			      }
			      break;
			    }
			}
		    }
		      
		  
		  if (problemType >= 32 && problemType <= 36)
		    {
		      double mass, v;

		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 1]])
			{
			  mass = [[dataOne objectAtIndex: 1] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 1]];
			  mass = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 2]])
			{
			  v = [[dataOne objectAtIndex: 2] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 2]];
			  v = gsl_vector_get (vrs, k);

			  // Add the var to postCheck array.
			  [postCheck addObject: [dataOne objectAtIndex: 2]];
			}

		      initSystemEnergy += 0.5*mass*v*v;
		    }
		  else
		    {
		      // Get the data of the elements on initial system
		      for (z = 1; z <= 4; z++)
			{
			  // Get the names of z element
			  elementName = [[dataOne objectAtIndex: z]
					  description];
			  
			  if (![elementName isEqualToString: @""] &&
			      ![elementName isEqualToString: @"0"])
			    {
			      // Get the id of the z element
			      elementId = [objectsIds objectAtIndex:
				[objectsNames indexOfObject: elementName]];
			      // Get the data of the z element
			      elementData = [[objectsDictionary objectForKey:
				     elementId] objectForKey: @"Values"];
			      // Get the type of the z element
			      objectType = [[[objectsDictionary objectForKey:
			           elementId] objectForKey: @"Type"] intValue];
			      
			      switch (objectType)
				{
				case 301:
				  {
				    double mass, y;
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					mass = [[elementData objectAtIndex: 1]
						 doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					mass = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 2]])
				      {
					y = [[elementData objectAtIndex: 2]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 2]];
					y = gsl_vector_get (vrs, k);
				      }
				
				    initSystemEnergy += mass*gf*y;
				  }
				  break;
				case 302:
				case 303:
				  {
				    double mass, v, y;
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					mass = [[elementData objectAtIndex: 1]
						 doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					mass = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 2]])
				      {
					v = [[elementData objectAtIndex: 2]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 2]];
					v = gsl_vector_get (vrs, k);
					
					if (objectType == 302 &&
					    (problemType >= 32 &&
					     problemType <= 36))
					  {
					    // Add the var to postCheck array.
					    [postCheck addObject:
					       [elementData objectAtIndex: 2]];
					  }
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 4]])
				      {
					y = [[elementData objectAtIndex: 4]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 4]];
					y = gsl_vector_get (vrs, k);
				      }
				    
				    /* When a problem have a center, we assume
				       that events occurs in an horizontal
				       plane. */
				    if (problemType >= 18 && problemType <= 20)
				      {
					initSystemEnergy += 0.5*mass*v*v;
				      }
				    else
				      {
					initSystemEnergy += mass*(0.5*v*v +
								  gf*y);
				      }
				    
				    if (objectType == 303)
				      {
					initTotalMass += mass;
				      }
				  }
				  break;
				case 317 ... 320:
				  {
				    double kres, d;
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					kres = [[elementData objectAtIndex: 1]
						 doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					kres = gsl_vector_get (vrs, k);
				      }
				
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 2]])
				      {
					d = [[elementData objectAtIndex: 2]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					      [elementData objectAtIndex: 2]];
					d = gsl_vector_get (vrs, k);
				      }
				    
				    initSystemEnergy += 0.5*kres*d*d;
				  }
				  break;
				case 334:
				  {
				    double v;
				
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					v = [[elementData objectAtIndex: 1]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					v = gsl_vector_get (vrs, k);

					// Add the var to postCheck array.
					[postCheck addObject:
					     [elementData objectAtIndex: 1]];
				      }
				    
				    initCenterVelocity = v;
				  }
				  break;
				case 339:
				  {
				    double mass, vt, y, vr;
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					mass = [[elementData objectAtIndex: 1]
						 doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					mass = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 2]])
				      {
					vt = [[elementData objectAtIndex: 2]
					       doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 2]];
					vt = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 4]])
				      {
					y = [[elementData objectAtIndex: 4]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 4]];
					y = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 5]])
				      {
					vr = [[elementData objectAtIndex: 5]
					       doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 5]];
					vr = gsl_vector_get (vrs, k);
				      }
				    
				    initSystemEnergy += mass*(0.5*(vt*vt +
								   vr*vr) +
							      gf*y);
				  }
				  break;
				}
			    }
			}
		    }

		  // Write the equation
		  if (elementType == 305)
		    {
		      gsl_vector_set (func, nEcu, work -
				      ( (finalSystemEnergy +
					 0.5*finalTotalMass*finalCenterVelocity*
					 finalCenterVelocity) -
					(initSystemEnergy + 0.5*initTotalMass*
					 initCenterVelocity*initCenterVelocity)
					) );
		    }
		  else
		    {
		      gsl_vector_set (func, nEcu, power*tf -
				     ( (finalSystemEnergy + 0.5*finalTotalMass*
				      finalCenterVelocity*finalCenterVelocity) -
				       (initSystemEnergy + 0.5*initTotalMass*
					initCenterVelocity*initCenterVelocity)
				       ) );
		    }
		}
		break;
	      }

	    nEcu += 1;
	  }
	  break;
	case 306:
	  {
	    double momentum;

	    // Get the names of the elements
	    nameOne = [[data objectAtIndex: 0] description];
	    nameTwo = [[data objectAtIndex: 1] description];
	    
	    // Get the ids of the elements
	    idOne = [objectsIds objectAtIndex: [objectsNames indexOfObject:
							       nameOne]];
	    idTwo = [objectsIds objectAtIndex: [objectsNames indexOfObject:
							       nameTwo]];
	    
	    // Get the data of the elements
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];
	    dataTwo = [[objectsDictionary objectForKey: idTwo]
			objectForKey: @"Values"];

	    // Get the data of the momentum
	    if (![unknowns containsObject: [data objectAtIndex: 2]])
	      {
		momentum = [[data objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		momentum = gsl_vector_get (vrs, k);
	      }

	    problemType = [[objectLawType objectAtIndex: lawElement] intValue];

	    switch (problemType)
	      {
	      case 0 ... 3:
		{
		  double massOne, massTwo, vOne, vTwo, rOne, rTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 3]])
		    {
		      rOne = [[dataOne objectAtIndex: 3] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 3]];
		      rOne = gsl_vector_get (vrs, k);
		    }
		  
		  
		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 3]])
		    {
		      rTwo = [[dataTwo objectAtIndex: 3] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 3]];
		      rTwo = gsl_vector_get (vrs, k);
		    }

		  
		  // Write the equation
		  gsl_vector_set (func, nEcu, momentum*tf -
				  (massTwo*vTwo*rTwo - massOne*vOne*rOne));
		}
		break;
	      case 4 ... 13:
		{
		  int z;
		  double initialSystem = 0, finalSystem = 0;
		  double initCenterXV = 0, initCenterYV = 0,
		    initCenterX = 0, initCenterY = 0, initTotalMass = 0;
		  double finalCenterXV = 0, finalCenterYV = 0,
		    finalCenterX = 0, finalCenterY = 0, finalTotalMass = 0;
		  NSString *elementName;
		  NSNumber *elementId;
		  NSMutableArray *elementData;
		  
		  // Get the data of the elements on final system
		  for (z = 1; z <= 4; z++)
		    {
		      // Get the name of z element
		      elementName = [[dataTwo objectAtIndex: z] description];
		      
		      if (![elementName isEqualToString: @""] &&
			  ![elementName isEqualToString: @"0"] )
			{
			  // Get the id of the z element
			  elementId = [objectsIds objectAtIndex:
				    [objectsNames indexOfObject: elementName]];
			  // Get the data of the z element
			  elementData = [[objectsDictionary objectForKey:
					 elementId] objectForKey: @"Values"];
			  
			  switch ([[[objectsDictionary objectForKey: elementId]
				     objectForKey: @"Type"] intValue])
			    {
			    case 302:
			      {
				double mass, v, x, y, angle;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    mass = [[elementData objectAtIndex: 1]
					     doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    mass = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    v = [[elementData objectAtIndex: 2]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    v = gsl_vector_get (vrs, k);
				  }

				if (![unknowns containsObject:
					 [elementData objectAtIndex: 3]])
				  {
				    x = [[elementData objectAtIndex: 3]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 3]];
				    x = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 4]])
				  {
				    y = [[elementData objectAtIndex: 4]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 4]];
				    y = gsl_vector_get (vrs, k);
				  }

				if (![unknowns containsObject:
					 [elementData objectAtIndex: 5]])
				  {
				    angle = [[elementData objectAtIndex: 5]
					      doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 5]];
				    angle = gsl_vector_get (vrs, k);
				  }

				finalSystem = finalSystem + mass*v*
				  (x*sin(M_PI*angle/180) -
				   y*cos(M_PI*angle/180));
			      }
			      break;
			    case 303:
			      {
				double mass, v, r;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    mass = [[elementData objectAtIndex: 1]
					     doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    mass = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    v = [[elementData objectAtIndex: 2]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    v = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 3]])
				  {
				    r = [[elementData objectAtIndex: 3]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 3]];
				    r = gsl_vector_get (vrs, k);
				  }
				
				finalSystem += mass*v*r;
				finalTotalMass += mass;
			      }
			      break;
			    case 334:
			      {
				double v, angle, x, y;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    v = [[elementData objectAtIndex: 1]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    v = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    angle = [[elementData objectAtIndex: 2]
					      doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    angle = gsl_vector_get (vrs, k);
				  }

				if (![unknowns containsObject:
					 [elementData objectAtIndex: 3]])
				  {
				    x = [[elementData objectAtIndex: 3]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 3]];
				    x = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 4]])
				  {
				    y = [[elementData objectAtIndex: 4]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 4]];
				    y = gsl_vector_get (vrs, k);
				  }

				finalCenterXV = v*cos(M_PI*angle/180);
				finalCenterYV = v*sin(M_PI*angle/180);
				finalCenterX = x;
				finalCenterY = y;
			      }
			      break;
			    }
			}
		    }
	
	  
		  if (problemType >= 9 && problemType <= 13)
		    {
		      double mass, v, x, y, angle;
		      
		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 1]])
			{
			  mass = [[dataOne objectAtIndex: 1] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 1]];
			  mass = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 2]])
			{
			  v = [[dataOne objectAtIndex: 2] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 2]];
			  v = gsl_vector_get (vrs, k);
			}

		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 3]])
			{
			  x = [[dataOne objectAtIndex: 3] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 3]];
			  x = gsl_vector_get (vrs, k);
			}

		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 4]])
			{
			  y = [[dataOne objectAtIndex: 4] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 4]];
			  y = gsl_vector_get (vrs, k);
			}

		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 5]])
			{
			  angle = [[dataOne objectAtIndex: 5] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 5]];
			  angle = gsl_vector_get (vrs, k);
			}
		      
		      initialSystem += mass*v*(x*sin(M_PI*angle/180) -
					       y*cos(M_PI*angle/180));
		    }
		  else
		    {
		      // Get the data of the elements on initial system
		      for (z = 1; z <= 4; z++)
			{
			  // Get the name of z element
			  elementName = [[dataOne objectAtIndex: z]
					  description];
			  
			  if (![elementName isEqualToString: @""] &&
			      ![elementName isEqualToString: @"0"])
			    {
			      // Get the id of the z element
			      elementId = [objectsIds objectAtIndex:
				[objectsNames indexOfObject: elementName]];
			      // Get the data of the z element
			      elementData = [[objectsDictionary objectForKey:
				     elementId] objectForKey: @"Values"];
			      
			      switch ([[[objectsDictionary objectForKey:
						elementId] objectForKey:
							     @"Type"] intValue])
				{
				case 303:
				  {
				    double mass, v, r;
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					mass = [[elementData objectAtIndex: 1]
						 doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					mass = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 2]])
				      {
					v = [[elementData objectAtIndex: 2]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 2]];
					v = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 3]])
				      {
					r = [[elementData objectAtIndex: 3]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 3]];
					r = gsl_vector_get (vrs, k);
				      }
				    
				    initialSystem += mass*v*r;
				    initTotalMass += mass;
				  }
				  break;
				case 334:
				  {
				    double v, angle, x, y;
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					v = [[elementData objectAtIndex: 1]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					v = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 2]])
				      {
					angle = [[elementData objectAtIndex: 2]
						  doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 2]];
					angle = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 3]])
				      {
					x = [[elementData objectAtIndex: 3]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 3]];
					x = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 4]])
				      {
					y = [[elementData objectAtIndex: 4]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 4]];
					y = gsl_vector_get (vrs, k);
				      }
				    
				    initCenterXV = v*cos(M_PI*angle/180);
				    initCenterYV = v*sin(M_PI*angle/180);
				    initCenterX = x;
				    initCenterY = y;
				  }
				  break;
				}
			    }
			}
		    }
		  
		  // Write the equation
		  gsl_vector_set (func, nEcu, momentum*tf -
				  ( (finalSystem + finalTotalMass*
				     (finalCenterX*finalCenterYV -
				      finalCenterY*finalCenterXV)) - 
				    (initialSystem + initTotalMass*
				     (initCenterX*initCenterYV -
				      initCenterY*initCenterXV)) ) );
		}
		break;
	      }

	    nEcu++;
	  }
	  break;
	case 340:
	  {
	    double fx = 0, fy = 0;

	    // Get the names of the elements
	    nameOne = [[data objectAtIndex: 0] description];
	    nameTwo = [[data objectAtIndex: 1] description];
	    
	    // Get the ids of the elements
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    idTwo = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameTwo]];
	    
	    // Get the data of the elements
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];
	    dataTwo = [[objectsDictionary objectForKey: idTwo]
			objectForKey: @"Values"];

	    // Get the data of the applied forces
	    if (![unknowns containsObject: [data objectAtIndex: 2]])
	      {
		fx = [[data objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		fx = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 3]])
	      {
		fy = [[data objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 3]];
		fy = gsl_vector_get (vrs, k);
	      }

	    problemType = [[objectLawType objectAtIndex: lawElement] intValue];

	    switch (problemType)
	      {
	      case 0:
		{
		  double massOne, massTwo, vOne, vTwo, angOne, angTwo;
		  
		  // Get the data of the first element
		  if (![unknowns containsObject: [dataOne objectAtIndex: 1]])
		    {
		      massOne = [[dataOne objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 1]];
		      massOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
		    {
		      vOne = [[dataOne objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 2]];
		      vOne = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataOne objectAtIndex: 5]])
		    {
		      angOne = [[dataOne objectAtIndex: 5] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataOne objectAtIndex: 5]];
		      angOne = gsl_vector_get (vrs, k);
		    }
		  
		  
		  // Get the data of the second element
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
		    {
		      massTwo = [[dataTwo objectAtIndex: 1] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 1]];
		      massTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
		    {
		      vTwo = [[dataTwo objectAtIndex: 2] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 2]];
		      vTwo = gsl_vector_get (vrs, k);
		    }
		  
		  if (![unknowns containsObject: [dataTwo objectAtIndex: 5]])
		    {
		      angTwo = [[dataTwo objectAtIndex: 5] doubleValue];
		    }
		  else
		    {
		      int k = [unknowns indexOfObject:
					  [dataTwo objectAtIndex: 5]];
		      angTwo = gsl_vector_get (vrs, k);
		    }

		  
		  // Write the equations
		  gsl_vector_set (func, nEcu, fx*tf -
				  (massTwo*vTwo*cos(M_PI*angTwo/180) -
				   massOne*vOne*cos(M_PI*angOne/180)));
		  gsl_vector_set (func, nEcu + 1, fy*tf -
				  (massTwo*vTwo*sin(M_PI*angTwo/180) -
				   massOne*vOne*sin(M_PI*angOne/180)));
		}
		break;
	      case 1 ... 8:
		{
		  int z;
		  double initialSystemX = 0, initialSystemY = 0,
		    finalSystemX = 0, finalSystemY = 0;
		  double initCenterXV = 0, initCenterYV = 0, initTotalMass = 0;
		  double finalCenterXV = 0, finalCenterYV = 0,
		    finalTotalMass = 0;
		  NSString *elementName;
		  NSNumber *elementId;
		  NSMutableArray *elementData;
		  
		  // Get the data of the elements on final system
		  for (z = 1; z <= 4; z++)
		    {
		      // Get the names of z element
		      elementName = [[dataTwo objectAtIndex: z] description];

		      if (![elementName isEqualToString: @""] &&
			  ![elementName isEqualToString: @"0"] )
			{
			  // Get the id of the z element
			  elementId = [objectsIds objectAtIndex:
				    [objectsNames indexOfObject: elementName]];
			  //Get the data of the z element
			  elementData = [[objectsDictionary objectForKey:
					 elementId] objectForKey: @"Values"];

			  switch ([[[objectsDictionary objectForKey: elementId]
				     objectForKey: @"Type"] intValue])
			    {
			    case 302:
			      {
				double mass, v, angle;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    mass = [[elementData objectAtIndex: 1]
					     doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    mass = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    v = [[elementData objectAtIndex: 2]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    v = gsl_vector_get (vrs, k);
				  }

				if (![unknowns containsObject:
					 [elementData objectAtIndex: 5]])
				  {
				    angle = [[elementData objectAtIndex: 5]
					      doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 5]];
				    angle = gsl_vector_get (vrs, k);
				  }

				finalSystemX += mass*v*cos(M_PI*angle/180);
				finalSystemY += mass*v*sin(M_PI*angle/180);
			      }
			      break;
			    case 303:
			      {
				double mass;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    mass = [[elementData objectAtIndex: 1]
					     doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    mass = gsl_vector_get (vrs, k);
				  }
				
				finalTotalMass += mass;
			      }
			      break;
			    case 334:
			      {
				double v, angle;
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 1]])
				  {
				    v = [[elementData objectAtIndex: 1]
					  doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 1]];
				    v = gsl_vector_get (vrs, k);
				  }
				
				if (![unknowns containsObject:
					 [elementData objectAtIndex: 2]])
				  {
				    angle = [[elementData objectAtIndex: 2]
					      doubleValue];
				  }
				else
				  {
				    int k = [unknowns indexOfObject:
						[elementData objectAtIndex: 2]];
				    angle = gsl_vector_get (vrs, k);
				  }

				finalCenterXV = v*cos(M_PI*angle/180);
				finalCenterYV = v*sin(M_PI*angle/180);
			      }
			      break;
			    }
			}
		    }
		  

		  if (problemType >= 4 && problemType <= 8)
		    {
		      double mass, v, ang;

		      // Get the data of the first element
		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 1]])
			{
			  mass = [[dataOne objectAtIndex: 1] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 1]];
			  mass = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 2]])
			{
			  v = [[dataOne objectAtIndex: 2] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 2]];
			  v = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject:
				       [dataOne objectAtIndex: 5]])
			{
			  ang = [[dataOne objectAtIndex: 5] doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject:
					      [dataOne objectAtIndex: 5]];
			  ang = gsl_vector_get (vrs, k);
			}

		      initialSystemX += mass*v*cos(M_PI*ang/180);
		      initialSystemY += mass*v*sin(M_PI*ang/180);
		    }
		  else
		    {
		      // Get the data of the elements on initial system
		      for (z = 1; z <= 4; z++)
			{
			  // Get the names of z element
			  elementName = [[dataOne objectAtIndex: z]
					  description];
			  
			  if (![elementName isEqualToString: @""] &&
			      ![elementName isEqualToString: @"0"])
			    {
			      // Get the id of the z element
			      elementId = [objectsIds objectAtIndex:
				[objectsNames indexOfObject: elementName]];
			      // Get the data of the z element
			      elementData = [[objectsDictionary objectForKey:
				     elementId] objectForKey: @"Values"];
			      
			      switch ([[[objectsDictionary objectForKey:
						elementId] objectForKey:
							     @"Type"] intValue])
				{
				case 303:
				  {
				    double mass;
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					mass = [[elementData objectAtIndex: 1]
						 doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					mass = gsl_vector_get (vrs, k);
				      }
				    
				    initTotalMass += mass;
				  }
				  break;
				case 334:
				  {
				    double v, angle;
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 1]])
				      {
					v = [[elementData objectAtIndex: 1]
					      doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
					v = gsl_vector_get (vrs, k);
				      }
				    
				    if (![unknowns containsObject:
					     [elementData objectAtIndex: 2]])
				      {
					angle = [[elementData objectAtIndex: 2]
						  doubleValue];
				      }
				    else
				      {
					int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 2]];
					angle = gsl_vector_get (vrs, k);
				      }
				    
				    initCenterXV = v*cos(M_PI*angle/180);
				    initCenterYV = v*sin(M_PI*angle/180);
				  }
				  break;
				}
			    }
			}
		    }
		  
		  // Write the equation
		  gsl_vector_set (func, nEcu, fx*tf -
			  ( (finalSystemX + finalTotalMass*finalCenterXV) - 
			    (initialSystemX + initTotalMass*initCenterXV)) );
		  gsl_vector_set (func, nEcu + 1, fy*tf -
			  ( (finalSystemY + finalTotalMass*finalCenterYV) - 
			    (initialSystemY + initTotalMass*initCenterYV)) );
		}
		break;
	      }

	    nEcu += 2;
	  }
	  break;
	}

      lawElement++;
    }

  // Write equation for mobiles and springs (if have applied forces)
  enumerator = [objectsMobilAndSpring objectEnumerator];

  while ((object = [enumerator nextObject]))
    {
      type = [[objectsDictionary objectForKey: object] objectForKey: @"Type"];
      data = [[objectsDictionary objectForKey: object] objectForKey: @"Values"];
      
      elementType = [type intValue];

      switch (elementType)
	{
	case 302:
	  {
	    if ([forcesArrays objectAtIndex: idForce] != [NSNull null])
	      {
		int signX, signY;
		double mass, angle, accel;
		double forcesX = 0, forcesY = 0;
		NSString *obj;
		
		if (![unknowns containsObject: [data objectAtIndex: 1]])
		  {
		    mass = [[data objectAtIndex: 1] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		    mass = gsl_vector_get (vrs, k);
		  }
		
		if (![unknowns containsObject: [data objectAtIndex: 5]])
		  {
		    angle = [[data objectAtIndex: 5] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 5]];
		    angle = gsl_vector_get (vrs, k);
		  }

		if (![unknowns containsObject: [data objectAtIndex: 6]])
		  {
		    accel = [[data objectAtIndex: 6] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 6]];
		    accel = gsl_vector_get (vrs, k);
		  }
		
		
		forcesY -= mass*gf;
		
		forceEnumerator = [[forcesArrays objectAtIndex: idForce]
				    objectEnumerator];
		
		while ((codForce = [forceEnumerator nextObject]))
		  {
		    int t = [[[objectsDictionary objectForKey: codForce]
			       objectForKey: @"Type"] intValue];
		    forceData = [[objectsDictionary objectForKey: codForce]
				  objectForKey: @"Values"];
		    obj = [forceData objectAtIndex: 0];
		    
		    switch (t)
		      {
		      case 327:
		      case 328:
			{
			  if (t == 327)
			    { signX = 1; }
			  else
			    { signX = -1; }
			  
			  if (![unknowns containsObject: obj])
			    {
			      forcesX += signX*[obj doubleValue];
			    }
			  else
			    {
			      int k = [unknowns indexOfObject: obj];
			      forcesX += signX*(gsl_vector_get (vrs, k));
			    }
			}
			break;
		      case 325:
		      case 326:
			{
			  if (t == 325)
			    { signY = 1; }
			  else
			    { signY = -1; }
			  
			  if (![unknowns containsObject: obj])
			    {
			      forcesY += signY*[obj doubleValue];
			    }
			  else
			    {
			      int k = [unknowns indexOfObject: obj];
			      forcesY += signY*(gsl_vector_get (vrs, k));
			    }
			}
			break;
		      case 321 ... 324:
			{
			  double ang;
			  NSString *a;
			  a = [forceData objectAtIndex: 1];
			  
			  if ( (t == 322) || (t == 323) )
			    { signX = 1; }
			  else
			    { signX = -1; }
			  
			  if ( (t == 321) || (t == 323) )
			    { signY = 1; }
			  else
			    { signY = -1; }
			  
			  if (![unknowns containsObject: a])
			    {
			      ang = [a doubleValue];
			    }
			  else
			    {
			      int k = [unknowns indexOfObject: a];
			      ang = gsl_vector_get (vrs, k);
			    }
			  
			  if (![unknowns containsObject: obj])
			    {
			      forcesX += signX*[obj doubleValue]*
				cos(M_PI*ang/180);
			      forcesY += signY*[obj doubleValue]*
				sin(M_PI*ang/180);
			    }
			  else
			    {
			      int k = [unknowns indexOfObject: obj];
			      forcesX += signX*(gsl_vector_get (vrs, k))*
				cos(M_PI*ang/180);
			      forcesY += signY*(gsl_vector_get (vrs, k))*
				sin(M_PI*ang/180);
			    }
			}
			break;
		      }
		  }
		
		// Write the equations
		gsl_vector_set (func, nEcu, forcesX - mass*accel*
				cos(M_PI*angle/180));
		gsl_vector_set (func, nEcu + 1, forcesY -
				mass*accel*sin(M_PI*angle/180));
		
		nEcu += 2;
	      }
	  }
	  break;
	case 303:
	  {
	    if ([forcesArrays objectAtIndex: idForce] != [NSNull null])
	      {
		int sign;
		double mass, vt, r, force;
		NSString *obj;
		
		if (![unknowns containsObject: [data objectAtIndex: 1]])
		  {
		    mass = [[data objectAtIndex: 1] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		    mass = gsl_vector_get (vrs, k);
		  }
		
		if (![unknowns containsObject: [data objectAtIndex: 2]])
		  {
		    vt = [[data objectAtIndex: 2] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		    vt = gsl_vector_get (vrs, k);
		  }
		
		if (![unknowns containsObject: [data objectAtIndex: 3]])
		  {
		    r = [[data objectAtIndex: 3] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 3]];
		    r = gsl_vector_get (vrs, k);
		  }
		
		codForce = [[forcesArrays objectAtIndex: idForce]
			     objectAtIndex: 0];
		forceData = [[objectsDictionary objectForKey: codForce]
			      objectForKey: @"Values"];

		int t = [[[objectsDictionary objectForKey: codForce]
			   objectForKey: @"Type"] intValue];
		obj = [forceData objectAtIndex: 0];
		
		switch (t)
		  {
		  case 327:
		  case 328:
		    {
		      // Horizontal forces
		      if (![unknowns containsObject: obj])
			{
			  force = [obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  force = gsl_vector_get (vrs, k);
			}

		      // Write the equation
		      gsl_vector_set (func, nEcu, force - mass*(vt*vt/r));
		    }
		    break;
		  case 325:
		  case 326:
		    {
		      // Vertical forces
		      if (t == 325)
			{ sign = 1; }
		      else
			{ sign = -1; }
		      
		      if (![unknowns containsObject: obj])
			{
			  force = [obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  force = gsl_vector_get (vrs, k);
			}

		      // Write the equation
		      gsl_vector_set (func, nEcu, (force - sign*gf*mass) -
				      mass*(vt*vt/r));
		    }
		    break;
		  case 321 ... 324:
		    {
		      // Oblique forces
		      double ang;
		      NSString *a;
		      a = [forceData objectAtIndex: 1];
		      
		      if ( (t == 321) || (t == 323) )
			{ sign = 1; }
		      else
			{ sign = -1; }
	      
		      if (![unknowns containsObject: obj])
			{
			  force = [obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  force = gsl_vector_get (vrs, k);
			}

		      if (![unknowns containsObject: a])
			{
			  ang = [a doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: a];
			  ang = gsl_vector_get (vrs, k);
			}

		      // Write the equation
		      gsl_vector_set (func, nEcu, (force - sign*gf*mass*
						   sin(M_PI*ang/180)) -
				      mass*(vt*vt/r));
		    }
		    break;
		  }

		nEcu++;
	      }
	  }
	  break;
	case 339:
	  {
	    if ([forcesArrays objectAtIndex: idForce] != [NSNull null])
	      {
		int sign;
		double mass, ar, force;
		NSString *obj;
		
		if (![unknowns containsObject: [data objectAtIndex: 1]])
		  {
		    mass = [[data objectAtIndex: 1] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		    mass = gsl_vector_get (vrs, k);
		  }
		
		if (![unknowns containsObject: [data objectAtIndex: 6]])
		  {
		    ar = [[data objectAtIndex: 6] doubleValue];
		  }
		else
		  {
		    int k = [unknowns indexOfObject: [data objectAtIndex: 6]];
		    ar = gsl_vector_get (vrs, k);
		  }
		
	
		codForce = [[forcesArrays objectAtIndex: idForce]
			     objectAtIndex: 0];
		forceData = [[objectsDictionary objectForKey: codForce]
			      objectForKey: @"Values"];
		obj = [forceData objectAtIndex: 0];

		int t = [[[objectsDictionary objectForKey: codForce]
			   objectForKey: @"Type"] intValue];
		
		switch (t)
		  {
		  case 327:
		  case 328:
		    {
		      // Horizontal forces
		      if (![unknowns containsObject: obj])
			{
			  force = [obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  force = gsl_vector_get (vrs, k);
			}

		      // Write the equation
		      gsl_vector_set (func, nEcu, force - mass*ar);
		    }
		    break;
		  case 325:
		  case 326:
		    {
		      // Vertical forces
		      if (t == 325)
			{ sign = 1; }
		      else
			{ sign = -1; }
		      
		      if (![unknowns containsObject: obj])
			{
			  force = [obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  force = gsl_vector_get (vrs, k);
			}
		      
		      // Write the equation
		      gsl_vector_set (func, nEcu, (force - sign*gf*mass) -
				      mass*ar);
		    }
		    break;
		  case 321 ... 324:
		    {
		      // Oblique forces
		      double ang;
		      NSString *a;
		      a = [forceData objectAtIndex: 1];
		      
		      if ( (t == 321) || (t == 323) )
			{ sign = 1; }
		      else
			{ sign = -1; }
		      
		      if (![unknowns containsObject: obj])
			{
			  force = [obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  force = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject: a])
			{
			  ang = [a doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: a];
			  ang = gsl_vector_get (vrs, k);
			}
		      
		      // Write the equation
		      gsl_vector_set (func, nEcu, (force - sign*gf*mass*
						   sin(M_PI*ang/180)) -
				      mass*ar);
		    }
		    break;
		  }

		nEcu++;
	      }
	  }
	  break;
	case 304:
	  {
	    int signX, signY;
	    double mass, vt, r, at, ft;
	    double forcesX = 0, forcesY = 0, sense = 1;
	    NSString *obj;

	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		mass = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		mass = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 2]])
	      {
		vt = [[data objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		vt = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 3]])
	      {
		r = [[data objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 3]];
		r = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 4]])
	      {
		at = [[data objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 4]];
		at = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 5]])
	      {
		ft = [[data objectAtIndex: 5] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 5]];
		ft = gsl_vector_get (vrs, k);
	      }

	    if (![[data objectAtIndex: 6] isEqualToString: @">"])
	      {
		sense = -1;
	      }

	    
	    forcesY -= mass*gf;
	    
	    forceEnumerator = [[forcesArrays objectAtIndex: idForce]
				objectEnumerator];
	    
	    while ((codForce = [forceEnumerator nextObject]))
	      {
		int t = [[[objectsDictionary objectForKey: codForce]
			   objectForKey: @"Type"] intValue];
		forceData = [[objectsDictionary objectForKey: codForce]
			      objectForKey: @"Values"];
		obj = [forceData objectAtIndex: 0];
		
		switch (t)
		  {
		  case 327:
		  case 328:
		    {
		      if (t == 327)
			{ signX = 1; }
		      else
			{ signX = -1; }
		      
		      if (![unknowns containsObject: obj])
			{
			  forcesX += signX*[obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  forcesX += signX*(gsl_vector_get (vrs, k));
			}
		    }
		    break;
		  case 325:
		  case 326:
		    {
		      if (t == 325)
			{ signY = 1; }
		      else
			{ signY = -1; }
		      
		      if (![unknowns containsObject: obj])
			{
			  forcesY += signY*[obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  forcesY += signY*(gsl_vector_get (vrs, k));
			}
		    }
		    break;
		  case 321 ... 324:
		    {
		      double ang;
		      NSString *a;
		      a = [forceData objectAtIndex: 1];
		      
		      if ( (t == 322) || (t == 323) )
			{ signX = 1; }
		      else
			{ signX = -1; }
		      
		      if ( (t == 321) || (t == 323) )
			{ signY = 1; }
		      else
			{ signY = -1; }
		      
		      if (![unknowns containsObject: a])
			{
			  ang = [a doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: a];
			  ang = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject: obj])
			{
			  forcesX += signX*[obj doubleValue]*cos(M_PI*ang/180);
			  forcesY += signY*[obj doubleValue]*sin(M_PI*ang/180);
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  forcesX += signX*(gsl_vector_get (vrs, k))*
			    cos(M_PI*ang/180);
			  forcesY += signY*(gsl_vector_get (vrs, k))*
			    sin(M_PI*ang/180);
			}
		    }
		    break;
		  case 329:
		  case 333:
		    {
		      // Horizontal frictions
		      double u;
		      NSString *a;
		      a = [forceData objectAtIndex: 1];

		      if (t == 333)
			{ signX = 1; }
		      else
			{ signX = -1; }

		      if (![unknowns containsObject: a])
			{
			  u = [a doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: a];
			  u = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject: obj])
			{
			  forcesX += u*signX*[obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  forcesX += u*signX*(gsl_vector_get (vrs, k));
			}
		    }
		    break;
		  case 331:
		    {
		      // Vertical friction
		      double u;
		      NSString *a;
		      a = [forceData objectAtIndex: 1];

		      if (![unknowns containsObject: a])
			{
			  u = [a doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: a];
			  u = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject: obj])
			{
			  forcesY += u*[obj doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  forcesY += u*(gsl_vector_get (vrs, k));
			}
		    }
		    break;
		  case 330:
		  case 332:
		  case 341:
		  case 342:
		    {
		      // Oblique frictions
		      double ang, u;
		      NSString *a, *b;
		      a = [forceData objectAtIndex: 1];
		      b = [forceData objectAtIndex: 2];
		      
		      if ( (t == 332) || (t == 342) )
			{ signX = 1; }
		      else
			{ signX = -1; }

		      if ( (t == 330) || (t == 332) )
			{ signY = 1; }
		      else
			{ signY = -1; }
	      
		      if (![unknowns containsObject: a])
			{
			  u = [a doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: a];
			  u = gsl_vector_get (vrs, k);
			}

		      if (![unknowns containsObject: b])
			{
			  ang = [b doubleValue];
			}
		      else
			{
			  int k = [unknowns indexOfObject: b];
			  ang = gsl_vector_get (vrs, k);
			}
		      
		      if (![unknowns containsObject: obj])
			{
			  forcesX += u*signX*[obj doubleValue]*
			    cos(M_PI*ang/180);
			  forcesY += u*signY*[obj doubleValue]*
			    sin(M_PI*ang/180);
			}
		      else
			{
			  int k = [unknowns indexOfObject: obj];
			  forcesX += u*signX*(gsl_vector_get (vrs, k))*
			    cos(M_PI*ang/180);
			  forcesY += u*signY*(gsl_vector_get (vrs, k))*
			    sin(M_PI*ang/180);
			}
		    }
		    break;
		  }
	      }
	    
	    // Write the equations
	    gsl_vector_set (func, nEcu, sense*forcesX - mass*(vt*vt/r));
	    gsl_vector_set (func, nEcu + 1, forcesY);
	    gsl_vector_set (func, nEcu + 2, ft - mass*at);
	    
	    nEcu += 3;
	  }
	  break;
	case 317 ... 320:
	  {
	    // Springs
	    int forcePosition, t; 
	    double sign = 0, kr, x, force = 0;
	    codForce = [[forcesArrays objectAtIndex: idForce] objectAtIndex: 0];
	    
	    if ([[forcesType objectAtIndex: idForce] intValue] == 0)
	      {
		forcePosition = 1;
	      }
	    else
	      {
		forcePosition = -1;
	      }
	    
	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		kr = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		kr = gsl_vector_get (vrs, k);
	      }
	    
	    if (![unknowns containsObject: [data objectAtIndex: 2]])
	      {
		x = [[data objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		x = gsl_vector_get (vrs, k);
	      }
	    
	    
	    t = [[[objectsDictionary objectForKey: codForce]
		   objectForKey: @"Type"] intValue];
	    forceData = [[objectsDictionary objectForKey: codForce]
			  objectForKey: @"Values"];
            
	    switch (t)
	      {
	      case 321:
	      case 323:
	      case 325:
	      case 328:
		{
		  sign = 1;
		}
		break;
	      case 322:
	      case 324:
	      case 326:
	      case 327:
		{
		  sign = -1;
		}
		break;
	      }
	    
	    if (![unknowns containsObject: [forceData objectAtIndex: 0]])
	      {
		force = sign*forcePosition*[[forceData objectAtIndex: 0]
					     doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [forceData objectAtIndex: 0]];
		force = sign*forcePosition*(gsl_vector_get (vrs, k));
	      }
	    
	    // Write the equation
	    gsl_vector_set (func, nEcu, kr*x - force);
	    
	    nEcu++;
	  }
	  break;
	}

      idForce++;
    }

  // Write equation for other objects
  enumerator = [objectsOthers objectEnumerator];

  while ((object = [enumerator nextObject]))
    {
      type = [[objectsDictionary objectForKey: object] objectForKey: @"Type"];
      data = [[objectsDictionary objectForKey: object] objectForKey: @"Values"];
      
      elementType = [type intValue];

      switch (elementType)
	{
	case 310:
	  {
	    // Sine of angle
	    double x, y, ang;

	    if (![unknowns containsObject: [data objectAtIndex: 0]])
	      {
		x = [[data objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 0]];
		x = gsl_vector_get (vrs, k);
	      }
	    
	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		y = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		y = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 2]])
	      {
		ang = [[data objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		ang = gsl_vector_get (vrs, k);
	      }

	    // Write the equation
	    gsl_vector_set (func, nEcu, sin(M_PI*ang/180) - fabs(x)/y);
	    
	    nEcu++;
	  }
	  break;
	case 314:
	  {
	    // Angles
	    double ang1, ang2;

	    if (![unknowns containsObject: [data objectAtIndex: 0]])
	      {
		ang1 = [[data objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 0]];
		ang1 = gsl_vector_get (vrs, k);
	      }
	    
	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		ang2 = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		ang2 = gsl_vector_get (vrs, k);
	      }

	    // Write the equation
	    gsl_vector_set (func, nEcu, sin(M_PI*ang1/180) -
			    cos(M_PI*ang2/180));

	    nEcu++;
	  }
	  break;
	case 315:
	  {
	    // Couple
	    double moment, force, r;

	    if (![unknowns containsObject: [data objectAtIndex: 0]])
	      {
		moment = [[data objectAtIndex: 0] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 0]];
		moment = gsl_vector_get (vrs, k);
	      }
	    
	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		force = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		force = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 2]])
	      {
		r = [[data objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		r = gsl_vector_get (vrs, k);
	      }

	    // Write the equation
	    gsl_vector_set (func, nEcu, moment - force*r);

	    nEcu++;
	  }
	  break;
	case 316:
	  {
	    // Triangle
	    double atot, aang, vt, r, at;

	    // Get the name of the element
	    nameOne = [[data objectAtIndex: 0] description];
	    // Get the id of the element
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    // Get the data of the element
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];


	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		atot = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		atot = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 2]])
	      {
		aang = [[data objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		aang = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
	      {
		vt = [[dataOne objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 2]];
		vt = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 3]])
	      {
		r = [[dataOne objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 3]];
		r = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
	      {
		at = [[dataOne objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 4]];
		at = gsl_vector_get (vrs, k);
	      }

	    // Write the equation
	    gsl_vector_set (func, nEcu, atot - gsl_hypot(vt*vt/r, at));
	    gsl_vector_set (func, nEcu + 1, atan2(at, -1*(vt*vt/r)) -
			    (M_PI*aang/180));
	    
	    nEcu += 2;
	  }
	  break;
	case 335:
	  {
	    // Angular velocity
	    double vang, vt, r;

	    // Get the name of the element
	    nameOne = [[data objectAtIndex: 0] description];
	    // Get the id of the element
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    // Get the data of the element
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];

	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		vang = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		vang = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
	      {
		vt = [[dataOne objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 2]];
		vt = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 3]])
	      {
		r = [[dataOne objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 3]];
		r = gsl_vector_get (vrs, k);
	      }

	    // Write the equation
	    gsl_vector_set (func, nEcu, vang - vt/r);
	    
	    nEcu++;
	  }
	  break;
	case 336:
	  {
	    // Centripetal acceleration
	    double ac, vt, r;

	    // Get the name of the element
	    nameOne = [[data objectAtIndex: 0] description];
	    // Get the id of the element
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    // Get the data of the element
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];

	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		ac = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		ac = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
	      {
		vt = [[dataOne objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 2]];
		vt = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 3]])
	      {
		r = [[dataOne objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 3]];
		r = gsl_vector_get (vrs, k);
	      }

	    // Write the equation
	    gsl_vector_set (func, nEcu, ac - vt*vt/r);
	    
	    nEcu++;
	  }
	  break;
	case 337:
	  {
	    // Angular acceleration
	    double aang, at, r;

	    // Get the name of the element
	    nameOne = [[data objectAtIndex: 0] description];
	    // Get the id of the element
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    // Get the data of the element
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];

	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		aang = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		aang = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 3]])
	      {
		r = [[dataOne objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 3]];
		r = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
	      {
		at = [[dataOne objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 4]];
		at = gsl_vector_get (vrs, k);
	      }

	    // Write the equation
	    gsl_vector_set (func, nEcu, aang - at/r);
	    
	    nEcu++;
	  }
	  break;
	case 338:
	  {
	    // Max acceleration
	    double u, vt, r, at;

	    // Get the name of the element
	    nameOne = [[data objectAtIndex: 0] description];
	    // Get the id of the element
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    // Get the data of the element
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];

	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		u = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		u = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
	      {
		vt = [[dataOne objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 2]];
		vt = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 3]])
	      {
		r = [[dataOne objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 3]];
		r = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataOne objectAtIndex: 4]])
	      {
		at = [[dataOne objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 4]];
		at = gsl_vector_get (vrs, k);
	      }

	    // Write the equation
	    gsl_vector_set (func, nEcu, u*gf - gsl_hypot(vt*vt/r, at));
	    
	    nEcu++;
	  }
	  break;
	case 343:
	  {
	    // Inertia
	    int z;
	    double mTotal = 0, eRadius = 0;
	    double mass, r, inertia = 0, mSystem = 0;
	    NSString *elementName;
	    NSNumber *elementId;
	    NSMutableArray *elementData;

	    // Get the name of the element
	    nameOne = [[data objectAtIndex: 0] description];
	    // Get the id of the element
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    // Get the data of the element
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];


	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		mTotal = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		mTotal = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 2]])
	      {
		eRadius = [[data objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 2]];
		eRadius = gsl_vector_get (vrs, k);
	      }


	    // Get the data of the elements on system
	    for (z = 1; z <= 4; z++)
	      {
		// Get the names of z element
		elementName = [[dataOne objectAtIndex: z] description];
		
		if (![elementName isEqualToString: @""] &&
		    ![elementName isEqualToString: @"0"])
		  {
		    // Get the id of the z element
		    elementId = [objectsIds objectAtIndex:
			      [objectsNames indexOfObject: elementName]];
		    // Get the data of the z element
		    elementData = [[objectsDictionary objectForKey: elementId]
				    objectForKey: @"Values"];
		    
		    
		    if (![unknowns containsObject:
				     [elementData objectAtIndex: 1]])
		      {
			mass = [[elementData objectAtIndex: 1] doubleValue];
		      }
		    else
		      {
			int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 1]];
			mass = gsl_vector_get (vrs, k);
		      }
		    
		    if (![unknowns containsObject:
				     [elementData objectAtIndex: 3]])
		      {
			r = [[elementData objectAtIndex: 3] doubleValue];
		      }
		    else
		      {
			int k = [unknowns indexOfObject:
					    [elementData objectAtIndex: 3]];
			r = gsl_vector_get (vrs, k);
		      }

		    mSystem += mass;
		    inertia += mass*r*r;
		  }
	      }

	    // Write the equations
	    gsl_vector_set (func, nEcu, mTotal - mSystem);
	    gsl_vector_set (func, nEcu + 1, eRadius - sqrt(inertia/mSystem)); 
	    
	    nEcu += 2;
	  }
	  break;
	case 344:
	  {
	    // Absolute velocity
	    double vx, vy;
	    double vAbs, angle, vMobile, angMobile, vCenter, angCenter;
	    
	    // Get the names of the elements
	    nameOne = [[data objectAtIndex: 0] description];
	    nameTwo = [[data objectAtIndex: 2] description];
	    
	    // Get the ids of the elements
	    idOne = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameOne]];
	    idTwo = [objectsIds objectAtIndex:
				  [objectsNames indexOfObject: nameTwo]];
	    
	    // Get the data of the elements
	    dataOne = [[objectsDictionary objectForKey: idOne]
			objectForKey: @"Values"];
	    dataTwo = [[objectsDictionary objectForKey: idTwo]
			objectForKey: @"Values"];
	    
	    // Get the data of the element
	    if (![unknowns containsObject: [data objectAtIndex: 1]])
	      {
		angMobile = [[data objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 1]];
		angMobile = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [data objectAtIndex: 3]])
	      {
		vAbs = [[data objectAtIndex: 3] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 3]];
		vAbs = gsl_vector_get (vrs, k);
	      }
	    
	    if (![unknowns containsObject: [data objectAtIndex: 4]])
	      {
		angle = [[data objectAtIndex: 4] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [data objectAtIndex: 4]];
		angle = gsl_vector_get (vrs, k);
	      }

	    // Get the data of the element Mobile with circular movement
	    if (![unknowns containsObject: [dataOne objectAtIndex: 2]])
	      {
		vMobile = [[dataOne objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataOne objectAtIndex: 2]];
		vMobile = gsl_vector_get (vrs, k);
	      }
	    
	    // Get the data of the element Center
	    if (![unknowns containsObject: [dataTwo objectAtIndex: 1]])
	      {
		vCenter = [[dataTwo objectAtIndex: 1] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataTwo objectAtIndex: 1]];
		vCenter = gsl_vector_get (vrs, k);
	      }

	    if (![unknowns containsObject: [dataTwo objectAtIndex: 2]])
	      {
		angCenter = [[dataTwo objectAtIndex: 2] doubleValue];
	      }
	    else
	      {
		int k = [unknowns indexOfObject: [dataTwo objectAtIndex: 2]];
		angCenter = gsl_vector_get (vrs, k);
	      }
	    
	    // Write the equations
	    vx = vMobile*cos(M_PI*(angMobile + 90)/180) +
	      vCenter*cos(M_PI*angCenter/180);
	    vy = vMobile*sin(M_PI*(angMobile + 90)/180) +
	      vCenter*sin(M_PI*angCenter/180);

	    gsl_vector_set (func, nEcu, vAbs - gsl_hypot(vx, vy));
	    gsl_vector_set (func, nEcu + 1, M_PI*angle/180 - atan2(vy, vx));
	    
	    nEcu += 2;
	  }
	  break;
	}
    }
	    
  return GSL_SUCCESS;
}

@interface FLDynamicsCircularMotion (Private)
- (void) makeSystem;
@end

@implementation FLDynamicsCircularMotion (Private)
- (void) makeSystem
{
  int increase = 1;
  double newValue;
  BOOL follow = NO;
  
  const gsl_multiroot_fsolver_type *T;
  gsl_multiroot_fsolver *s;
  
  int status = 0, statusInt, k, length;
  int nvar  = [unknowns count];
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

  id dataToCheck;
  NSNumber *typeObject;
  NSMutableArray *checkData;
  NSEnumerator *enumerator;
  
  // Generator of random numbers
  const gsl_rng_type * Y;
  gsl_rng * r;
  gsl_rng_env_setup();
  Y = gsl_rng_default;
  r = gsl_rng_alloc (Y);
  
  // Search a solution
  do
    {
      gsl_multiroot_function f = {&buildSystem, n, (__bridge void *)(self)};

      iter = 0;
      if (increase <= 30)
	{
	  newValue = 100;
	}
      else if (increase <= 60)
	{
	  newValue = 1000;
	}
      else if (increase <= 75)
	{
	  newValue = 10000;
	}
      else
	{
	  newValue = 100000;
	}
      
      for (k = 0; k < nvar; k++)
	{
	  par = newValue*(gsl_rng_uniform (r));
	  gsl_vector_set (x, k, par);
	}
      
      T = gsl_multiroot_fsolver_hybrids;
      s = gsl_multiroot_fsolver_alloc (T, nvar);
      gsl_multiroot_fsolver_set (s, &f, x);
      
      do
	{
	  iter++;
	  statusInt = gsl_multiroot_fsolver_iterate (s);
	  
	  if (statusInt)
	    break;
	  
	  status = gsl_multiroot_test_residual (s->f, 1e-7);
	}
      while (status == GSL_CONTINUE && iter < 1000);  
      
      // Verify the status
      if ( (statusInt) && (increase < 90) )
	{
	  increase += 1;
	  follow = YES;
	}
      else
	{
	  if (varT == 1)
            {
	      Tindex = [unknowns indexOfObject: timeVar];
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
	  else if ([postCheck count] > 0)
	    {
	      // Try to found the expected result
	      int z, index;

	      for (z = 0; z < [postCheck count]; z++)
		{
		  index = [unknowns indexOfObject:
		      [[postCheck objectAtIndex: z] stringByTrimmingSpaces]];

		  if ( ((gsl_vector_get (s->x, index)) < 0) && (increase < 90) )
		    {
		      increase += 1;
		      follow = YES;
		      break;
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
	}
      // End of verification
    }
  while (follow);
  // End of solution search

  // Move the solution data to array results
  varCount = [unknowns objectEnumerator];
  
  while ((anObj = [varCount nextObject]))
    {
      [results addObject: [NSNumber numberWithDouble:
				      gsl_vector_get (s->x, countRes)]];
      countRes += 1;
    }

  // Check the sign of the data
  enumerator = [objectsDictionary objectEnumerator];
  
  int par1, w, g, sign;
  double nv, nf;
  while ((dataToCheck = [enumerator nextObject]))
    {
      par1 = 0;
      w = 0;
      g = 0;
      sign = 1;
      nv = 0;
      nf = 0;

      typeObject = [dataToCheck objectForKey: @"Type"];
      
      switch ([typeObject intValue])
	{
	case 302:
	case 316:
	case 334:
	case 344:
	  {
	    /* Check the velocity and angle of Linear mobiles and Centers.
	       Also the acceleration and angle at Total acceleration and
	       Absolute velocity. */
	    int one = 0, two = 0;

	    if ([typeObject intValue] == 302)
	      {
		one = 2;
		two = 5;
	      }
	    else if ([typeObject intValue] == 316)
	      {
		one = 1;
		two = 2;
	      }
	    else if ([typeObject intValue] == 334)
	      {
		one = 1;
		two = 2;
	      }
	    else if ([typeObject intValue] == 344)
	      {
		one = 3;
		two = 4;
	      }

	    checkData = [dataToCheck objectForKey: @"Values"];
	    
	    // Check the velocity
	    if ([unknowns containsObject: [checkData objectAtIndex: one]] &&
		[unknowns containsObject: [checkData objectAtIndex: two]])
	      {
		w = [unknowns indexOfObject: [checkData objectAtIndex: one]];
		
		if ( [[results objectAtIndex: w] doubleValue] < 0 )
		  {
		    nv = -1*[[results objectAtIndex: w] doubleValue];
		    [results replaceObjectAtIndex: w
			       withObject: [NSNumber numberWithDouble: nv]];
		    par1 = 1;
		  }
	      }
	    
	    // Check the angle
	    if ([unknowns containsObject: [checkData objectAtIndex: two]])
	      {
		w = [unknowns indexOfObject: [checkData objectAtIndex: two]];
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
	case 304:
	  {
	    /* We check the tangential velocity to display always a positive
	       value. */
	    checkData = [dataToCheck objectForKey: @"Values"];
	    
	    if ([unknowns containsObject: [checkData objectAtIndex: 2]])
	      {
		w = [unknowns indexOfObject: [checkData objectAtIndex: 2]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv < 0 )
		  {
		    nv = -1*nv;
		  }
		
                [results replaceObjectAtIndex: w
				   withObject: [NSNumber numberWithDouble: nv]];
	      }
	  }
	  break;
	case 310:
	  {
	    // Check the angle
	    checkData = [dataToCheck objectForKey: @"Values"];
	    
	    if ([unknowns containsObject: [checkData objectAtIndex: 2]])
	      {
		w = [unknowns indexOfObject: [checkData objectAtIndex: 2]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv > 360 )
		  {
		    nv -= floor(nv/360)*360;
		  }
                
                if ( nv < 0 )
		  {
		    nv += (floor(-1*nv/360) + 1)*360;                
		  }

		if ( nv > 270 )
		  {
		    nv = 360 - nv;
		  }

		if ( nv > 180 )
		  {
		    nv -= 180;
		  }

		if ( nv > 90 )
		  {
		    nv = 180 - nv;
		  }
                
                [results replaceObjectAtIndex: w
				   withObject: [NSNumber numberWithDouble: nv]];
	      }
	  }
	  break;
	case 321 ... 324:
	  {
	    checkData = [dataToCheck objectForKey: @"Values"];
	    
	    // Verify the sign of the force and its angle
	    if ([unknowns containsObject: [checkData objectAtIndex: 0]] &&
		[unknowns containsObject: [checkData objectAtIndex: 1]])
	      {
		g = [unknowns indexOfObject: [checkData objectAtIndex: 0]];
		nf = [[results objectAtIndex: g] doubleValue];
		
		w = [unknowns indexOfObject: [checkData objectAtIndex: 1]];
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
	    // Verify the angle of the force
	    else if ([unknowns containsObject: [checkData objectAtIndex: 1]])
	      {
		w = [unknowns indexOfObject: [checkData objectAtIndex: 1]];
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
	case 330:
	case 332:
	case 341:
	case 342:
	  {
	    /* We check the angle of the friction force. Although this don't
	       have sense. If the user apply a friction force we expect that
	       the user provide the angle. */
	    checkData = [dataToCheck objectForKey: @"Values"];
	    
	    if ([unknowns containsObject: [checkData objectAtIndex: 2]])
	      {
		w = [unknowns indexOfObject: [checkData objectAtIndex: 2]];
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
	case 339:
	  {
	    // We check the radial velocity to display always a positive value.
	    checkData = [dataToCheck objectForKey: @"Values"];
	    
	    if ([unknowns containsObject: [checkData objectAtIndex: 5]])
	      {
		w = [unknowns indexOfObject: [checkData objectAtIndex: 5]];
		nv = [[results objectAtIndex: w] doubleValue];
		
                if ( nv < 0 )
		  {
		    nv = -1*nv;
		  }
		
                [results replaceObjectAtIndex: w
				   withObject: [NSNumber numberWithDouble: nv]];
	      }
	  }
	  break;
	}
    }
  
 
  // Write the solution
  [self printUnknowns: unknowns withResults: results];
  
  // Write the status
  message = [NSString stringWithFormat: _(@"Status = %s \n"),
		      gsl_strerror (status)];
  length = [[[self viewer] textStorage] length];
  [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			       withString: message];
  
  /*if (mensajeFr)
    {
      length = [[[self viewer] textStorage] length];
      [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
      withString: [errors objectAtIndex: 11]];
      }*/
  
  gsl_multiroot_fsolver_free (s);
  gsl_vector_free (x);
  gsl_rng_free (r);
}
@end

@implementation FLDynamicsCircularMotion

- (id) init
{
  NSBundle *messages;
  self = [super init];
  
  unknowns = [NSMutableArray new];
  objectsLaw = [NSMutableArray new];
  objectLawType = [NSMutableArray new];
  objectsNames = [NSMutableArray new];
  objectsIds = [NSMutableArray new];
  objectsSystem = [NSMutableArray new];
  objectSystemGenericType = [NSMutableArray new];
  objectsMobilAndSpring = [NSMutableArray new];
  objectsOthers = [NSMutableArray new];
  forcesType = [NSMutableArray new];
  forcesArrays = [NSMutableArray new];
  postCheck = [NSMutableArray new];
  objectsDictionary = [NSMutableDictionary new];
  
  // Make the array of messages
  messages = [NSBundle mainBundle];
  errors = [[NSArray alloc] initWithContentsOfFile:
	     [messages pathForResource: @"dynamicsCircularMessages"
				ofType: @"plist"]];
  
  return self;
}

- (void) dealloc
{
  [objectsDictionary release];
  [objectsLaw release];
  [objectLawType release];
  [objectsNames release];
  [objectsIds release];
  [unknowns release];
  [errors release];
  [objectsSystem release];
  [objectSystemGenericType release];
  [objectsMobilAndSpring release];
  [objectsOthers release];
  [forcesType release];
  [forcesArrays release];
  [postCheck release];
  [super dealloc];
}

- (void) makeEquationsForData: (NSMutableDictionary *)list
{
  int c, objContained = 0;
  int width, height, totalCells; // For handle the chalkboard size
  int length, numberOfEquations = 0, numberOfForces = 0, errorNumber = 0;
  NSNumber *code, *identifier;

  BOOL error = NO;

  sys = [self system];
  //NSString *mensaje;

  NSMutableArray *nameContainedObjects = [NSMutableArray array];
  NSMutableArray *codForces = [NSMutableArray array];
  NSMutableArray *objCircular = [NSMutableArray array];

  NSArray *keys;
  NSEnumerator *enumerator;
  NSTextView *dynamicsCircularInfo = [self viewer];
  NSArray *objectsOrder = [self cells];

  // To check the applied forces
  int k, pos;
  NSButton *cell;
  NSNumber *ident;
  NSEnumerator *search;
  
  varG = 0;
  gravityDat = 0;

  // For handle the chalkboard size
  width = [self chalkboardWidth];
  height = [self chalkboardHeight];
  totalCells = width*height - 1;

  [objectsDictionary setDictionary: list];
  
  keys = [[NSArray alloc] initWithArray: [list allKeys]];
  enumerator = [keys objectEnumerator];
  
  
  while ((code = [enumerator nextObject]) && !error)
    {
      int x;
      NSString *key;
      NSArray *terms;
      NSNumber *number;
      NSArray *titles = [[objectsDictionary objectForKey: code]
			  objectForKey: @"Titles"];
      NSMutableArray *info = [[objectsDictionary objectForKey: code]
			       objectForKey: @"Data"];
      NSMutableArray *values = [[objectsDictionary objectForKey: code]
				 objectForKey: @"Values"];
      
      identifier = [[objectsDictionary objectForKey: code]
		     objectForKey: @"Type"];
      [values removeAllObjects];
      
      // Count the amount of unknowns
      for (x = 0; x < [info count]; x++)
	{
	  NSString *data = [info objectAtIndex: x];
	  NSString *title = [[titles objectAtIndex: x] description];
	  
	  if (![self isNumericDataTheString: data] && 
	      ![title isEqualToString: _(@"Name")] && 
	      ![title isEqualToString: _(@"System")] &&
	      ![title isEqualToString: _(@"System i")] &&
	      ![title isEqualToString: _(@"System f")] &&
	      ![title isEqualToString: _(@"Object 1")] &&
	      ![title isEqualToString: _(@"Object 2")] &&
	      ![title isEqualToString: _(@"Object 3")] &&
	      ![title isEqualToString: _(@"Object 4")] &&
	      ![title isEqualToString: _(@"Object")] &&
	      ![title isEqualToString: _(@"Center")] &&
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
				     [[[self conversions] objectForKey: key]
				       doubleValue]];
			  [values addObject: [number stringValue]];
                        }
		      else
                        {
			  var = [var stringByAppendingString: @"@"];
			  var = [var stringByAppendingString: key];
			  
			  [values addObject: var];
			  if (![unknowns containsObject: var])
			    {
			      [unknowns addObject: var];
			    }
                        }
		    }
		  else
		    {
                      NSString *message =
			[NSString stringWithFormat: [errors objectAtIndex: 0],
				  key];
                      error = YES;
		      errorNumber = 0;
		      
		      length = [[dynamicsCircularInfo textStorage] length];
                      [dynamicsCircularInfo replaceCharactersInRange:
			    NSMakeRange(length,0) withString: message];
		    }
		  
		}
	      else
		{
		  // Add a simple unknown data (without conversion factor)
		  NSString *varFactor = nil;
		  
		  // Search the type of unknown data
		  if ([title hasPrefix: @"m"])
		    {
		      if (sys == 0)
			{ varFactor = @"kg"; }
		      else
			{ varFactor = @"slug"; } 
		    }
		  else if ([title hasPrefix: @"f"] || [title hasPrefix: @"N"] ||
			   [title hasPrefix: @"F"])
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
		  else if ([title hasPrefix: @"aa"])
		    {
		      varFactor = @"rad/s2";
		    }
		  else if ([title hasPrefix: @"g"] || [title hasPrefix: @"a"])
		    {
		      if (sys == 0)
			{ varFactor = @"m/s2"; }
		      else
			{ varFactor = @"ft/s2"; }
		    }
		  else if ([title hasPrefix: @"r"] || [title hasPrefix: @"y"] ||
			   [title hasPrefix: @"x"] || [title hasPrefix: @"d"])
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
		  else if ([title hasPrefix: @"t"])
		    {
		      varFactor = @"s";
		    }
		  else if ([title hasPrefix: @"vang"])
		    {
		      varFactor = @"rad/s";
		    }
		  else if ([title hasPrefix: @"v"])
		    {
		      if (sys == 0)
			{ varFactor = @"m/s"; }
		      else
			{ varFactor = @"ft/s"; }
		    }
		  else if ([title hasPrefix: @"k"])
		    {
		      if (sys == 0)
			{ varFactor = @"N/m"; }
		      else
			{ varFactor = @"lb/ft"; }
		    }
		  else if ([title hasPrefix: @"P"])
		    {
		      if (sys == 0)
			{ varFactor = @"N*m/s"; }
		      else
			{ varFactor = @"lb*ft/s"; }
		    }
		  else if ([title hasPrefix: @"W"] || [title hasPrefix: @"E"])
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
		  // End the search of type
		  
		  data = [data stringByAppendingString: @"@"];
		  data = [data stringByAppendingString: varFactor];
		  
		  [values addObject: data];
		  if (![unknowns containsObject: data])
		    {
		      [unknowns addObject: data];
		    }
		  
		  // Unknown data added
		}
	    }
          else
	    {
	      [values addObject: [info objectAtIndex: x]];
	    }
	}    
      // End of the count
      
      if (error)
	break;
      
      // Check the gravity data and the numbers of equations
      switch ([identifier intValue])
	{
	case 300:
	  {
	    // Reference system
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
	case 301:
	case 302:
	case 303:
	case 304:
	case 339:
	  {
	    // Mobiles
	    // If had a name add this to the names array
	    NSString *name = [values objectAtIndex: 0];
	    if (![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		if (![objectsNames containsObject: name])
		  {
		    [objectsNames addObject: name];
		    [objectsIds addObject: code];
		  }
		else
		  {
		    error = YES;
		    errorNumber = 5;
		  }
	      }

	    // Add the element to the mobil array
	    if ([identifier intValue] != 301)
	      {
		[objectsMobilAndSpring addObject: code];
	      }
	    
	    if ([identifier intValue] == 301)
	      {
		// No equation needed
		//numberOfEquations = numberOfEquations + 2;
	      }
	    else if ([identifier intValue] == 302)
	      {
		numberOfEquations += 2;
	      }
	    else if ([identifier intValue] == 303 ||
		     [identifier intValue] == 339 )
	      {
		numberOfEquations += 1;
	      }
	    else if ([identifier intValue] == 304)
	      {
		numberOfEquations += 3;
	      }
	  }
	  break;
	case 305:
	case 306:
	case 307:
	  {
	    // Energy, Angular momentum and Power
	    NSString *name = [values objectAtIndex: 0];
	    if ([name isEqualToString: @"0"] || [name isEqualToString: @""])
	      {
		error = YES;
		errorNumber = 2;
	      }

	    name = [values objectAtIndex: 1];
	    if ([name isEqualToString: @"0"] || [name isEqualToString: @""])
	      {
		error = YES;
		errorNumber = 2;
	      }

	    [nameContainedObjects addObject: [values objectAtIndex: 0]];
	    [nameContainedObjects addObject: [values objectAtIndex: 1]];
	    [objectsLaw addObject: code];
	    numberOfEquations += 1;
	  }
	  break;
	case 308:
	case 309:
	  {
	    // Initial and final system
	    NSString *name = [values objectAtIndex: 0];
	    if (![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		if (![objectsNames containsObject: name])
		  {
		    if ([[values objectAtIndex: 1] length] > 0 &&
			![[values objectAtIndex: 1] isEqualToString: @"0"])
		      {
			[nameContainedObjects addObject:
				  [values objectAtIndex: 1]];
		      }
		    
		    if ([[values objectAtIndex: 2] length] > 0 &&
			![[values objectAtIndex: 2] isEqualToString: @"0"])
		      { 
			[nameContainedObjects addObject:
				  [values objectAtIndex: 2]];
		      }
		    
		    if ([[values objectAtIndex: 3] length] > 0 &&
			![[values objectAtIndex: 3] isEqualToString: @"0"])
		      { 
			[nameContainedObjects addObject:
				  [values objectAtIndex: 3]];
		      }
		    
		    if ([[values objectAtIndex: 4] length] > 0 &&
			![[values objectAtIndex: 4] isEqualToString: @"0"])
		      { 
			[nameContainedObjects addObject:
				  [values objectAtIndex: 4]];
		      }
		    
		    // Add the system name to the names array
		    [objectsNames addObject: name];
		    [objectsIds addObject: code];
		    // Add the system object to the system array
		    [objectsSystem addObject: code];
		  }
		else
		  {
		    error = YES;
		    errorNumber = 5;
		  }
	      }
	    else
	      {
		error = YES;
		errorNumber = 3;
	      }
	  }
	  break;
	case 310:
	  {
	    // Sine of angle
	    numberOfEquations += 1;
	    [objectsOthers addObject: code];
	  }
	  break;
	case 314:
	  {
	    // Relation of angles
	    if (![self isNumericDataTheString: [values objectAtIndex: 0]] &&
		![self isNumericDataTheString: [values objectAtIndex: 1]])
	      {
		numberOfEquations += 1;
		[objectsOthers addObject: code];
	      }
	    else
	      {
		error = YES;
		errorNumber = 7;
	      }
	  }
	  break;
	case 315:
	  {
	    // Couple
	    numberOfEquations += 1;
	    [objectsOthers addObject: code];
	  }
	  break;
	case 317 ... 320:
	  {
	    // Springs
	    NSString *name = [values objectAtIndex: 0];
	    
	    if (![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		if (![objectsNames containsObject: name])
		  {
		    [objectsNames addObject: name];
		    [objectsIds addObject: code];

		    [objectsMobilAndSpring addObject: code];
		    numberOfEquations += 1;
		  }
		else
		  {
		    error = YES;
		    errorNumber = 5;
		  }
	      }
	    else
	      {
		error = YES;
		errorNumber = 8;
	      }
	  }
	  break;
	case 321 ... 328:
	  {
	    // Forces
	    [codForces addObject: code];
	    numberOfForces += 1;
	  }
	  break;
	case 329 ... 333:
	case 341:
	case 342:
	  {
	    // Frictions
	    [codForces addObject: code];
	    numberOfForces += 1;
	  }
	  break;
	case 334:
	  {
	    // Center
	    NSString *name = [values objectAtIndex: 0];
	    
	    if (![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		if (![objectsNames containsObject: name])
		  {
		    [objectsNames addObject: name];
		    [objectsIds addObject: code];
		  }
		else
		  {
		    error = YES;
		    errorNumber = 5;
		  }
	      }
	    else
	      {
		error = YES;
		errorNumber = 9;
	      }
	  }
	  break;
	case 316:
	case 335:
	case 336:
	case 337:
	case 338:
	  {
	    /* Angular velocity, centripetal acceleration,
	     * total acceleration (triangle), angular acceleration
	     * and Max acceleration.
	     */
	    NSString *name = [values objectAtIndex: 0];
	    
	    if (![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		[nameContainedObjects addObject: name];
		
		[objCircular addObject: code];
		[objectsOthers addObject: code];

		if ([identifier intValue] == 316)
		  {
		    numberOfEquations += 2;
		  }
		else
		  {
		    numberOfEquations += 1;
		  }
	      }
	    else
	      {
		error = YES;
		errorNumber = 22;
	      }
	  }
	  break;
	case 340:
	  {
	    // Linear momentum
	    NSString *name = [values objectAtIndex: 0];
	    if ([name isEqualToString: @"0"] || [name isEqualToString: @""])
	      {
		error = YES;
		errorNumber = 2;
	      }

	    name = [values objectAtIndex: 1];
	    if ([name isEqualToString: @"0"] || [name isEqualToString: @""])
	      {
		error = YES;
		errorNumber = 2;
	      }

	    [nameContainedObjects addObject: [values objectAtIndex: 0]];
	    [nameContainedObjects addObject: [values objectAtIndex: 1]];
	    [objectsLaw addObject: code];
	    numberOfEquations += 2;
	  }
	  break;
	case 343:
	  {
	    // Inertia
	    NSString *name = [values objectAtIndex: 0];
	    
	    if (![name isEqualToString: @"0"] && ![name isEqualToString: @""])
	      {
		[nameContainedObjects addObject: name];
		
		[objCircular addObject: code];
		[objectsOthers addObject: code];
		
		numberOfEquations += 2;
	      }
	    else
	      {
		error = YES;
		errorNumber = 28;
	      }
	  }
	  break;
	case 344:
	  {
	    // Absolute velocity
	    NSString *name = [values objectAtIndex: 0];
	    if ([name isEqualToString: @"0"] || [name isEqualToString: @""])
	      {
		error = YES;
		errorNumber = 14;
	      }
	    
	    name = [values objectAtIndex: 2];
	    if ([name isEqualToString: @"0"] || [name isEqualToString: @""])
	      {
		error = YES;
		errorNumber = 14;
	      }
	    
	    if (!error)
	      {
		[objCircular addObject: code];
		[objectsOthers addObject: code];
		
		numberOfEquations += 2;
	      }
	  }
	  break;
	}
      // End the count of equations

      if (error)
	break;
      
      // Check the applied forces
      search = [[self cells] objectEnumerator];
      if (([identifier intValue] >= 302 && [identifier intValue] <= 304) ||
	  [identifier intValue] == 339)
	{
	  int horizontalForces = 0, verticalForces = 0;   
	  NSNumber *typeOfForce;  
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
	      k++;
	    }

	  // Check the forces if object isn't at borders
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
	  // Check the forces if the object is at left border
	  else if (pos%width == 0)
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
	  // Check the forces if the object is at rigth border
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


	  // Verify the applied forces
	  if ([identifier intValue] == 302)
	    {
	      if ([forces count] == 0)
		{
		  [forcesType addObject: [NSNull null]];
		  [forcesArrays addObject: [NSNull null]];
		  numberOfEquations -= 2;
		}
	      else if ([forces count] > 0)
		{
		  verif = [forces objectEnumerator];
		  while ((typeOfForce = [verif nextObject]))
		    {
		      int t = [[[objectsDictionary objectForKey: typeOfForce]
				 objectForKey: @"Type"] intValue];

		      if ( (t >= 321) && (t <= 328) )
			{
			  if ( (t == 327) || (t == 328) )
			    {
			      horizontalForces++;
			    }

			  if ( (t == 325) || (t == 326) )
			    {
			      verticalForces++;
			    }
			}
		      else
			{
			  error = YES;
			  errorNumber = 17;
			  break;
			}
		    }

		  if ([forces count] != horizontalForces &&
		      [forces count] != verticalForces)
		    {
		      [forcesType addObject: [NSNull null]];
		      [forcesArrays addObject: forces];
		    }
		  else
		    {
		      error = YES;
		      errorNumber = 18;
		    }
		}
	    }
	  else if ([identifier intValue] == 303 ||
		   [identifier intValue] == 339)
	    {
	      if ([forces count] == 0)
		{
		  [forcesType addObject: [NSNull null]];
		  [forcesArrays addObject: [NSNull null]];
		  numberOfEquations -= 1;
		}
	      else if ([forces count] == 1)
		{
		  int t = [[[objectsDictionary objectForKey:
			      [forces objectAtIndex: 0]] objectForKey: @"Type"]
			       intValue];

		  if ( (t >= 321) && (t <= 328) )
		    {
		      [forcesType addObject: [NSNull null]];
		      [forcesArrays addObject: forces];
		    }
		  else
		    {
		      error = YES;
		      errorNumber = 17;
		    }
		}
	      else if ([forces count] > 1)
		{
		  error = YES;
		  errorNumber = 19;
		}
	    }
	  else if ([identifier intValue] == 304)
	    {
	      if ([forces count] > 0)
		{
		  verif = [forces objectEnumerator];
		  while ((typeOfForce = [verif nextObject]))
		    {
		      int t = [[[objectsDictionary objectForKey: typeOfForce]
				 objectForKey: @"Type"] intValue];

		      if ( ((t >= 321) && (t <= 333)) ||
			   (t == 341) || (t == 342) )
			{
			  if ( (t == 327) || (t == 328) || (t == 329) ||
			       (t == 333) )
			    {
			      horizontalForces++;
			    }

			  if ( (t == 325) || (t == 326) || (t == 331) )
			    {
			      verticalForces++;
			    }
			}
		      else
			{
			  error = YES;
			  errorNumber = 17;
			  break;
			}
		    }

		  if ([forces count] != horizontalForces &&
		      [forces count] != verticalForces)
		    {
		      [forcesType addObject: [NSNull null]];
		      [forcesArrays addObject: forces];
		    }
		  else
		    {
		      error = YES;
		      errorNumber = 18;
		    }
		}
	      else
		{
		  error = YES;
		  errorNumber = 27;
		}
	    }
	}
      else if ([identifier intValue] >= 317 &&
	       [identifier intValue] <= 320)
	{
	  int sign = 0, positionForce = 0;
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
	      k++;
	    }
	  
	  if ([identifier intValue] == 317)
	    {
	      positionForce = width - 1;
	    }
	  else if ([identifier intValue] == 318)
	    {
	      positionForce = width + 1;
	    }
	  else if ([identifier intValue] == 319)
	    {
	      positionForce = width;
	    }
	  else
	    {
	      positionForce = 1;
	    }
	  
	  // Check the forces if the object isn't in the borders
	  if ( (pos%width != 0) && (pos%width != (width - 1)) )
	    {
	      if (pos - positionForce >= 0)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - positionForce] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident];
		      sign = 1; }
		}
	      
	      if (pos + positionForce <= totalCells)
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + positionForce] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident];
		      sign = -1; }
		}
	    }
	  // Check the forces if the object is at rigth border
	  else if (pos%width == 0)
	    { 
	      if ((pos - positionForce >= 0) && ((positionForce != 1) ||
					 (positionForce != (width + 1))))
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - positionForce] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident];
		      sign = 1; }
		}
	      
	      if ((pos + positionForce <= totalCells) &&
		  (positionForce != (width - 1)))
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + positionForce] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident];
		      sign = -1;}
		}
	    }
	  // Check the forces if the object is at left border
	  else
	    {
	      if ((pos - positionForce >= 0) && (positionForce != (width - 1))) 
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos - positionForce] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident];
		      sign = 1; }
		}
	      
	      if ((pos + positionForce <= totalCells) &&
		  ((positionForce != 1) || (positionForce != (width + 1))))
		{
		  ident = [NSNumber numberWithInt:
		      [[objectsOrder objectAtIndex: pos + positionForce] tag]];

		  if ([ident intValue] != 0)
		    { [forces addObject: ident];
		      sign = -1; }
		}
	    }

	  // Check the forces
	  if ([forces count] > 0)
	    {
	      int f1, f2;
	      
              if (sign > 0)
		{
		  // There is only one force before the object
		  [forcesType addObject: [NSNumber numberWithInt: 0]];
		}
              else if ([forces count] == 1)
		{
		  // There is only one force after the object
		  [forcesType addObject: [NSNumber numberWithInt: 1]];
		}
	      else
		{
		  // There are two forces, so we get the data of the
		  // first force
		  [forcesType addObject: [NSNumber numberWithInt: 0]];
		}
	      
	      // Check the type of the force
	      f1 = [[[objectsDictionary objectForKey:
	       [forces objectAtIndex: 0]] objectForKey: @"Type"] intValue];

	      if (([identifier intValue] == 317) && (f1 != 323) && (f1 != 324))
		{
		  error = YES;
		}
	      
	      if (([identifier intValue] == 318) && (f1 != 321) && (f1 != 322))
		{
		  error = YES;
		}
	      
	      if (([identifier intValue] == 319) && (f1 != 325) && (f1 != 326))
		{
		  error = YES;
		}
	      
	      if (([identifier intValue] == 320) && (f1 != 327) && (f1 != 328))
		{
		  error = YES;
		}
              
	      // Check the type of the forces (to the case when are two)
              if ([forces count] > 1)
		{
		  f2 = [[[objectsDictionary objectForKey:
		   [forces objectAtIndex: 1]] objectForKey: @"Type"] intValue];
		  
		  if (([identifier intValue] == 317) && (f2 != 323) &&
		      (f2 != 324))
		    {
		      error = YES;
		    }
		  
		  if (([identifier intValue] == 318) && (f2 != 321) &&
		      (f2 != 322))
		    {
		      error = YES;
		    }
		  
		  if (([identifier intValue] == 319) && (f2 != 325) &&
		      (f2 != 326))
		    {
		      error = YES;
		    }
		  
		  if (([identifier intValue] == 320) && (f2 != 327) &&
		      (f2 != 328))
		    {
		      error = YES;
		    }
		  
		  if (f1 == f2)
		    {
		      error = YES;
		    }
		}
	      
	      if (error)
		{
		  errorNumber = 17;
		}
	      
	      [forcesArrays addObject: forces];
	    }
	  else
	    {
	      error = YES;
	      errorNumber = 26;
	    }
	}
      
      if (error)
	break;
    }
  
  // Check if the names correspond to objects
  if (!error)
    {
      for (c = 0; c < [nameContainedObjects count]; c++)
	{
	  if ([objectsNames containsObject:
			      [nameContainedObjects objectAtIndex: c]])
	    {  
	      objContained++;
	    }
	}

      if (objContained != [nameContainedObjects count])
	{
	  error = YES;
	  errorNumber = 16;
	}
    }

  // Check objects in initial and final system
  id elementId;
  int elementType, typeOne, typeTwo, typeThree, typeFour;
  NSMutableArray *elementData;
  NSString *nameOne, *nameTwo, *nameThree, *nameFour;
  NSNumber *idOne, *idTwo, *idThree, *idFour;
  NSEnumerator *enumed = [objectsSystem objectEnumerator];

  if (!error)
    {
      while ((elementId = [enumed nextObject]))
	{
	  elementType = -1;
	  typeOne = 0;
	  typeTwo = 0;
	  typeThree = 0;
	  typeFour = 0;
	  nameOne = nil;
	  nameTwo = nil;
	  nameThree = nil;
	  nameFour = nil;
	  idOne = nil;
	  idTwo = nil;
	  idThree = nil;
	  idFour = nil;
	  elementData = nil;

	  // Get the object data
	  elementData = [[objectsDictionary objectForKey: elementId]
			  objectForKey: @"Values"];

	  // Get the names of the elements
	  nameOne = [[elementData objectAtIndex: 1] description];
	  nameTwo = [[elementData objectAtIndex: 2] description];
	  nameThree = [[elementData objectAtIndex: 3] description];
	  nameFour = [[elementData objectAtIndex: 4] description];

	  
	  // Get the ids of the elements
	  if ([objectsNames indexOfObject: nameOne] != NSNotFound)
	    {
	      idOne = [objectsIds objectAtIndex: [objectsNames indexOfObject:
								 nameOne]];
	    }
	  
	  if ([objectsNames indexOfObject: nameTwo] != NSNotFound)
	    {
	      idTwo = [objectsIds objectAtIndex: [objectsNames indexOfObject:
								 nameTwo]];
	    }
	  
	  if ([objectsNames indexOfObject: nameThree] != NSNotFound)
	    {
	      idThree = [objectsIds objectAtIndex: [objectsNames indexOfObject:
								   nameThree]];
	    }
	  
	  if ([objectsNames indexOfObject: nameFour] != NSNotFound)
	    {
	      idFour = [objectsIds objectAtIndex: [objectsNames indexOfObject:
								  nameFour]];
	    }
	  
	  // Get the types of the elements
	  if (idOne != nil)
	    {
	      typeOne = [[[objectsDictionary objectForKey: idOne]
			   objectForKey: @"Type"] intValue];
	      
	      /* If is a spring set type 1, so we don't need check
		 several values */
	      if ( (typeOne >= 317) && (typeOne <= 320) )
		{
		  typeOne = 1;
		}
	    }
      
	  if (idTwo != nil)
	    {
	      typeTwo = [[[objectsDictionary objectForKey: idTwo]
			   objectForKey: @"Type"] intValue];
	      
	      /* If is a spring set type 1, so we don't need check several
		 values */
	      if ( (typeTwo >= 317) && (typeTwo <= 320) )
		{
		  typeTwo = 1;
		}
	    }
	  
	  if (idThree != nil)
	    {
	      typeThree = [[[objectsDictionary objectForKey: idThree]
			     objectForKey: @"Type"] intValue];
	      
	      /* If is a spring set type 1, so we don't need check
		 several values */
	      if ( (typeThree >= 317) && (typeThree <= 320) )
		{
		  typeThree = 1;
		}
	    }
	  
	  if (idFour != nil)
	    {
	      typeFour = [[[objectsDictionary objectForKey: idFour]
			    objectForKey: @"Type"] intValue];
	      
	      /* If is a spring set type 1, so we don't need check
		 several values */
	      if ( (typeFour >= 317) && (typeFour <= 320) )
		{
		  typeFour = 1;
		}
	    }
	  
	  // System with two circular mobiles
	  if ( (typeOne == 303) && (typeTwo == 303) && (typeThree == 0) &&
	       (typeFour == 0) )
	    {
	      elementType = 0;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 0) &&
		    (typeThree == 303) && (typeFour == 0) )
	    {
	      elementType = 1;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 303) )
	    {
	      elementType = 2;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 0) )
	    {
	      elementType = 3;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 303) &&
		    (typeThree == 0) && (typeFour == 303) )
	    {
	      elementType = 4;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 5;
	    }
	  // System with three circular mobiles
	  else if ( (typeOne == 303) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 0) )
	    {
	      elementType = 6;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 303) &&
		    (typeThree == 0) && (typeFour == 303) )
	    {
	      elementType = 7;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 0) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 8;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 9;
	    }
	  // System with four circular mobiles
	  /*else if ( (typeOne == 303) && (typeTwo == 303) &&
	    (typeThree == 303) && (typeFour == 303) )
	    {
	      .......
	    }*/
	  // System with two linear mobiles
	  else if ( (typeOne == 302) && (typeTwo == 302) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 10;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 0) &&
		    (typeThree == 302) && (typeFour == 0) )
	    {
	      elementType = 11;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 302) )
	    {
	      elementType = 12;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 302) &&
		    (typeThree == 302) && (typeFour == 0) )
	    {
	      elementType = 13;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 302) &&
		    (typeThree == 0) && (typeFour == 302) )
	    {
	      elementType = 14;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 302) && (typeFour == 302) )
	    {
	      elementType = 15;
	    }
	  // System with three linear mobiles
	  else if ( (typeOne == 302) && (typeTwo == 302) &&
		    (typeThree == 302) && (typeFour == 0) )
	    {
	      elementType = 16;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 302) &&
		    (typeThree == 0) && (typeFour == 302) )
	    {
	      elementType = 17;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 0) &&
		    (typeThree == 302) && (typeFour == 302) )
	    {
	      elementType = 18;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 302) &&
		    (typeThree == 302) && (typeFour == 302) )
	    {
	      elementType = 19;
	    }
	  // System with one circular mobil and one spring
	  else if ( (typeOne == 303) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 20;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 21;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 22;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 303) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 23;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 303) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 24;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 303) && (typeFour == 1) )
	    {
	      elementType = 25;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 303) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 26;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 303) && (typeFour == 0) )
	    {
	      elementType = 27;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 303) )
	    {
	      elementType = 28;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 303) && (typeFour == 0) )
	    {
	      elementType = 29;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 303) )
	    {
	      elementType = 30;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 303) )
	    {
	      elementType = 31;
	    }
	  // System with one linear mobile and one spring
	  else if ( (typeOne == 302) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 32;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 33;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 34;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 302) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 35;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 302) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 36;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 302) && (typeFour == 1) )
	    {
	      elementType = 37;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 302) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 38;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 302) && (typeFour == 0) )
	    {
	      elementType = 39;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 302) )
	    {
	      elementType = 40;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 302) && (typeFour == 0) )
	    {
	      elementType = 41;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 302) )
	    {
	      elementType = 42;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 302) )
	    {
	      elementType = 43;
	    }
	  // System with one linear mobile and two springs
	  else if ( (typeOne == 302) && (typeTwo == 1) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 44;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 45;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 1) )
	    {
	      elementType = 46;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 302) &&
		    (typeThree == 1) && (typeFour == 1) )
	    {
	      elementType = 47;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 302) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 48;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 302) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 49;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 302) && (typeFour == 1) )
	    {
	      elementType = 50;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 1) &&
		    (typeThree == 302) && (typeFour == 0) )
	    {
	      elementType = 51;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 302) && (typeFour == 1) )
	    {
	      elementType = 52;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 302) )
	    {
	      elementType = 53;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 302) )
	    {
	      elementType = 54;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 1) && (typeFour == 302) )
	    {
	      elementType = 55;
	    }
	  // System with one liner mobile and three springs
	  else if ( (typeOne == 302) && (typeTwo == 1) &&
		    (typeThree == 1) && (typeFour == 1) )
	    {
	      elementType = 56;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 302) &&
		    (typeThree == 1) && (typeFour == 1) )
	    {
	      elementType = 57;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 1) &&
		    (typeThree == 302) && (typeFour == 1) )
	    {
	      elementType = 58;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 1) &&
		    (typeThree == 1) && (typeFour == 302) )
	    {
	      elementType = 59;
	    }
	  // System with two circular mobiles and one center
	  else if ( (typeOne == 334) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 0) )
	    {
	      elementType = 60;
	    }
	  else if ( (typeOne == 334) && (typeTwo == 303) &&
		    (typeThree == 0) && (typeFour == 303) )
	    {
	      elementType = 61;
	    }
	  else if ( (typeOne == 334) && (typeTwo == 0) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 62;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 334) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 63;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 334) &&
		    (typeThree == 303) && (typeFour == 0) )
	    {
	      elementType = 64;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 334) &&
		    (typeThree == 0) && (typeFour == 303) )
	    {
	      elementType = 65;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 0) &&
		    (typeThree == 334) && (typeFour == 303) )
	    {
	      elementType = 66;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 303) &&
		    (typeThree == 334) && (typeFour == 0) )
	    {
	      elementType = 67;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 303) &&
		    (typeThree == 334) && (typeFour == 303) )
	    {
	      elementType = 68;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 303) &&
		    (typeThree == 0) && (typeFour == 334) )
	    {
	      elementType = 69;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 0) &&
		    (typeThree == 303) && (typeFour == 334) )
	    {
	      elementType = 70;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 334) )
	    {
	      elementType = 71;
	    }
	  // System with three circular mobiles and one center
	  else if ( (typeOne == 334) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 72;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 334) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 73;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 303) &&
		    (typeThree == 334) && (typeFour == 303) )
	    {
	      elementType = 74;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 334) )
	    {
	      elementType = 75;
	    }
	  // System with two circular mobiles, one linear mobile and one center
	  else if ( (typeOne == 302) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 334) )
	    {
	      elementType = 76;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 303) &&
		    (typeThree == 334) && (typeFour == 303) )
	    {
	      elementType = 77;
	    }
	  else if ( (typeOne == 302) && (typeTwo == 334) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 78;
	    }
	  else if ( (typeOne == 334) && (typeTwo == 302) &&
		    (typeThree == 303) && (typeFour == 303) )
	    {
	      elementType = 79;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 302) &&
		    (typeThree == 303) && (typeFour == 334) )
	    {
	      elementType = 80;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 302) &&
		    (typeThree == 334) && (typeFour == 303) )
	    {
	      elementType = 81;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 334) &&
		    (typeThree == 302) && (typeFour == 303) )
	    {
	      elementType = 82;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 303) &&
		    (typeThree == 302) && (typeFour == 334) )
	    {
	      elementType = 83;
	    }
	  else if ( (typeOne == 334) && (typeTwo == 303) &&
		    (typeThree == 302) && (typeFour == 303) )
	    {
	      elementType = 84;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 303) &&
		    (typeThree == 334) && (typeFour == 302) )
	    {
	      elementType = 85;
	    }
	  else if ( (typeOne == 303) && (typeTwo == 334) &&
		    (typeThree == 303) && (typeFour == 302) )
	    {
	      elementType = 86;
	    }
	  else if ( (typeOne == 334) && (typeTwo == 303) &&
		    (typeThree == 303) && (typeFour == 302) )
	    {
	      elementType = 87;
	    }
	  // System with one spring and one mass in rest
	  else if ( (typeOne == 301) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 88;
	    }
	  else if ( (typeOne == 301) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 89;
	    }
	  else if ( (typeOne == 301) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 90;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 301) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 91;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 301) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 92;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 301) && (typeFour == 1) )
	    {
	      elementType = 93;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 301) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 94;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 301) && (typeFour == 0) )
	    {
	      elementType = 95;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 301) )
	    {
	      elementType = 96;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 301) && (typeFour == 0) )
	    {
	      elementType = 97;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 301) )
	    {
	      elementType = 98;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 301) )
	    {
	      elementType = 99;
	    }
	  // System with one spring and one polar mass
	  else if ( (typeOne == 339) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 100;
	    }
	  else if ( (typeOne == 339) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 101;
	    }
	  else if ( (typeOne == 339) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 102;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 339) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 103;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 339) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 104;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 339) && (typeFour == 1) )
	    {
	      elementType = 105;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 339) &&
		    (typeThree == 0) && (typeFour == 0) )
	    {
	      elementType = 106;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 339) && (typeFour == 0) )
	    {
	      elementType = 107;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 0) && (typeFour == 339) )
	    {
	      elementType = 108;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 339) && (typeFour == 0) )
	    {
	      elementType = 109;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 339) )
	    {
	      elementType = 110;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 339) )
	    {
	      elementType = 111;
	    }
	  // System with one mass in rest and two springs
	  else if ( (typeOne == 301) && (typeTwo == 1) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 112;
	    }
	  else if ( (typeOne == 301) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 113;
	    }
	  else if ( (typeOne == 301) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 1) )
	    {
	      elementType = 114;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 301) &&
		    (typeThree == 1) && (typeFour == 1) )
	    {
	      elementType = 115;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 301) &&
		    (typeThree == 1) && (typeFour == 0) )
	    {
	      elementType = 116;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 301) &&
		    (typeThree == 0) && (typeFour == 1) )
	    {
	      elementType = 117;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 301) && (typeFour == 1) )
	    {
	      elementType = 118;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 1) &&
		    (typeThree == 301) && (typeFour == 0) )
	    {
	      elementType = 119;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 301) && (typeFour == 1) )
	    {
	      elementType = 120;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 1) &&
		    (typeThree == 0) && (typeFour == 301) )
	    {
	      elementType = 121;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 0) &&
		    (typeThree == 1) && (typeFour == 301) )
	    {
	      elementType = 122;
	    }
	  else if ( (typeOne == 0) && (typeTwo == 1) &&
		    (typeThree == 1) && (typeFour == 301) )
	    {
	      elementType = 123;
	    }
	  // System with one mass in rest and three springs
	  else if ( (typeOne == 301) && (typeTwo == 1) &&
		    (typeThree == 1) && (typeFour == 1) )
	    {
	      elementType = 124;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 301) &&
		    (typeThree == 1) && (typeFour == 1) )
	    {
	      elementType = 125;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 1) &&
		    (typeThree == 301) && (typeFour == 1) )
	    {
	      elementType = 126;
	    }
	  else if ( (typeOne == 1) && (typeTwo == 1) &&
		    (typeThree == 1) && (typeFour == 301) )
	    {
	      elementType = 127;
	    }
	  // System with four linear mobiles
	  else if ( (typeOne == 302) && (typeTwo == 302) &&
		    (typeThree == 302) && (typeFour == 302) )
	    {
	      elementType = 128;
	    }
	  // Error
	  else
	    {
	      error = YES;
	      errorNumber = 10;
	    }
	  
	  // Set generic type
	  if ( (elementType >= 0) && (elementType <= 5) )
	    {
	      // System with two circular mobiles
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 0]];
	    }
	  else if ( (elementType >= 6) && (elementType <= 9) )
	    {
	      // System with three circular mobiles
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 1]];
	    }
	  else if ( (elementType >= 10) && (elementType <= 15) )
	    {
	      // System with two linear mobiles
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 2]];
	    }
	  else if ( (elementType >= 16) && (elementType <= 19) )
	    {
	      // System with three linear mobiles
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 3]];
	    }
	  else if ( (elementType >= 20) && (elementType <= 31) )
	    {
	      // System with one circular mobil and one spring
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 4]];
	    }
	  else if ( (elementType >= 32) && (elementType <= 43) )
	    {
	      // System with one linear mobile and one spring
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 5]];
	    }
	  else if ( (elementType >= 44) && (elementType <= 55) )
	    {
	      // System with one linear mobile and two springs
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 6]];
	    }
	  else if ( (elementType >= 56) && (elementType <= 59) )
	    {
	      // System with one liner mobile and three springs
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 7]];
	    }
	  else if ( (elementType >= 60) && (elementType <= 71) )
	    {
	      // System with two circular mobiles and one center
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 8]];
	    }
	  else if ( (elementType >= 72) && (elementType <= 75) )
	    {
	      // System with three circular mobiles and one center
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 9]];
	    }
	  else if ( (elementType >= 76) && (elementType <= 87) )
	    {
	      /* System with two circular mobiles, one linear mobile
		 and one center */
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 10]];
	    }
	  else if ( (elementType >= 88) && (elementType <= 99) )
	    {
	      // System with one mass in rest and one spring
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 11]];
	    }
	  else if ( (elementType >= 100) && (elementType <= 111) )
	    {
	      // System with one polar mass and one spring
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 12]];
	    }
	  else if ( (elementType >= 112) && (elementType <= 123) )
	    {
	      // System with one mass in rest and two spring
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 13]];
	    }
	  else if ( (elementType >= 124) && (elementType <= 127) )
	    {
	      // System with one mass in rest and three spring
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 14]];
	    }
	  else if (elementType == 128)
	    {
	      // System with four linear mobiles
	      [objectSystemGenericType addObject: [NSNumber numberWithInt: 15]];
	    }

	}
    }
  
  // Check objects in Energy, Momentum and Power elements
  enumed = [objectsLaw objectEnumerator];

  if (!error)
    {
      while ((elementId = [enumed nextObject]))
	{
	  typeOne = 0, typeTwo = 0, typeThree = 0, typeFour = 0;
	  idOne = nil, idTwo = nil, idThree = nil, idFour = nil;

	  elementType = [[[objectsDictionary objectForKey: elementId]
			   objectForKey: @"Type"] intValue];
	  elementData = [[objectsDictionary objectForKey: elementId]
			  objectForKey: @"Values"];
  
	  switch (elementType)
	    {
	    case 305:
	    case 307:
	      {
		// Get the names of the elements
		nameOne = [[elementData objectAtIndex: 0] description];
		nameTwo = [[elementData objectAtIndex: 1] description];
	    
		// Get the ids of the elements
		idOne = [objectsIds objectAtIndex:
				      [objectsNames indexOfObject: nameOne]];
		idTwo = [objectsIds objectAtIndex:
				      [objectsNames indexOfObject: nameTwo]];
	    
		// Get the types of the elements
		typeOne = [[[objectsDictionary objectForKey: idOne]
			     objectForKey: @"Type"] intValue];
		typeTwo = [[[objectsDictionary objectForKey: idTwo]
			     objectForKey: @"Type"] intValue];
	    
		if ( (typeOne == 301) && (typeTwo == 301) )
		  {
		    // This is a problem between two objects
		    [objectLawType addObject: [NSNumber numberWithInt: 0]];
		  }
		else if ( (typeOne == 301) && (typeTwo == 302) )
		  {
		    /* This is a problem between one object and
		       one lineal mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 1]];
		  }
		else if ( (typeOne == 301) && (typeTwo == 303) )
		  {
		    /* This is a problem between one object and
		       one circular mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 2]];
		  }
		else if ( (typeOne == 302) && (typeTwo == 301) )
		  {
		    /* This is a problem between one lineal mobil
		       and one object */
		    [objectLawType addObject: [NSNumber numberWithInt: 3]];
		  }
		else if ( (typeOne == 302) && (typeTwo == 302) )
		  {
		    /* This is a problem between one lineal mobil
		       and other lineal mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 4]];
		  }
		else if ( (typeOne == 302) && (typeTwo == 303) )
		  {
		    /* This is a problem between one lineal mobil
		       and one circular mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 5]];
		  }
		else if ( (typeOne == 303) && (typeTwo == 301) )
		  {
		    /* This is a problem between one circular mobil
		       and one object */
		    [objectLawType addObject: [NSNumber numberWithInt: 6]];
		  }
		else if ( (typeOne == 303) && (typeTwo == 302) )
		  {
		    /* This is a problem between one circular mobil
		       and one lineal mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 7]];
		  }
		else if ( (typeOne == 303) && (typeTwo == 303) )
		  {
		    /* This is a problem between one circular mobil
		       and other circular mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 8]];
		  }
		else if ( (typeOne == 303) && (typeTwo == 339) )
		  {
		    /* This is a problem between one circular mobil
		       and one polar mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 9]];
		  }
		else if ( (typeOne == 339) && (typeTwo == 303) )
		  {
		    /* This is a problem between one polar mobil
		       and one circular mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 10]];
		  }
		else if ( (typeOne == 339) && (typeTwo == 339) )
		  {
		    /* This is a problem between one polar mobil
		       and other polar mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 11]];
		  }
		else if ( (typeOne == 308) && (typeTwo == 309) )
		  {
		    // This is a dynamics problem with more than two objects

		    // First check if the combination is allowed
		    int systemOne, systemTwo;
		    systemOne = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idOne]] intValue];
		    systemTwo = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idTwo]] intValue];

		    // Set the type
		    if ( (systemOne == 0) && (systemTwo == 0) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 12]];
		      }
		    else if ( (systemOne == 1) && (systemTwo == 1) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 13]];
		      }
		    else if ( (systemOne == 4) && (systemTwo == 4) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 14]];
		      }
		    else if ( (systemOne == 5) && (systemTwo == 5) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 15]];
		      }
		    else if ( (systemOne == 6) && (systemTwo == 6) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 16]];
		      }
		    else if ( (systemOne == 7) && (systemTwo == 7) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 17]];
		      }
		    // This combination have a center
		    else if ( (systemOne == 8) && (systemTwo == 2) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 18]];
		      }
		    // This combination have a center
		    else if ( (systemOne == 9) && (systemTwo == 3) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 19]];
		      }
		    // This combination have a center
		    else if ( (systemOne == 9) && (systemTwo == 10) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 20]];
		      }
		    else if ( (systemOne == 4) && (systemTwo == 11) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 21]];
		      }
		    else if ( (systemOne == 11) && (systemTwo == 4) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 22]];
		      }
		    else if ( (systemOne == 5) && (systemTwo == 11) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 23]];
		      }
		    else if ( (systemOne == 11) && (systemTwo == 5) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 24]];
		      }
		    else if ( (systemOne == 4) && (systemTwo == 12) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 25]];
		      }
		    else if ( (systemOne == 12) && (systemTwo == 4) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 26]];
		      }
		    else if ( (systemOne == 12) && (systemTwo == 12) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 27]];
		      }
		    else if ( (systemOne == 6) && (systemTwo == 13) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 28]];
		      }
		    else if ( (systemOne == 13) && (systemTwo == 6) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 29]];
		      }
		    else if ( (systemOne == 7) && (systemTwo == 14) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 30]];
		      }
		    else if ( (systemOne == 14) && (systemTwo == 7) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 31]];
		      }
		    else
		      {
			if (elementType == 305)
			  {
			    error = YES;
			    errorNumber = 11;
			  }
			else
			  {
			    error = YES;
			    errorNumber = 12;
			  }
		      }
		  }
		else if ( (typeOne == 302) && (typeTwo == 309) )
		  {
		    /* This is a problem between one linear mobil
		       and one system */

		    // First check if the combination is allowed
		    int systemTwo;
		    systemTwo = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idTwo]] intValue];

		    // Set the type
		    if ( (typeOne == 302) && (systemTwo == 2) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 32]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 3) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 33]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 15) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 34]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 8) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 35]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 10) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 36]];
		      }
		    else
		      {
			if (elementType == 305)
			  {
			    error = YES;
			    errorNumber = 11;
			  }
			else
			  {
			    error = YES;
			    errorNumber = 12;
			  }
		      }
		  }
		else
		  {
		    // Error
		    if (elementType == 305)
		      {
			error = YES;
			errorNumber = 11;
		      }
		    else
		      {
			error = YES;
			errorNumber = 12;
		      }
		  }
	      }
	      break;
	    case 306:
	      {
		// Get the names of the elements
		nameOne = [[elementData objectAtIndex: 0] description];
		nameTwo = [[elementData objectAtIndex: 1] description];
	    
		// Get the ids of the elements
		idOne = [objectsIds objectAtIndex:
				      [objectsNames indexOfObject: nameOne]];
		idTwo = [objectsIds objectAtIndex:
				      [objectsNames indexOfObject: nameTwo]];
	    
		// Get the types of the elements
		typeOne = [[[objectsDictionary objectForKey: idOne]
			     objectForKey: @"Type"] intValue];
		typeTwo = [[[objectsDictionary objectForKey: idTwo]
			     objectForKey: @"Type"] intValue];

		if ( (typeOne == 303) && (typeTwo == 303) )
		  {
		    /* This is a problem between one circular mobil
		       and other circular mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 0]];
		  }
		else if ( (typeOne == 303) && (typeTwo == 339) )
		  {
		    /* This is a problem between one circular mobil
		       and one polar mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 1]];
		  }
		else if ( (typeOne == 339) && (typeTwo == 303) )
		  {
		    /* This is a problem between one polar mobil
		       and one circular mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 2]];
		  }
		else if ( (typeOne == 339) && (typeTwo == 339) )
		  {
		    /* This is a problem between one polar mobil
		       and other polar mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 3]];
		  }
		else if ( (typeOne == 308) && (typeTwo == 309) )
		  {
		    // This is a dynamics problem with more than two objects

		    // First check if the combination is allowed
		    int systemOne, systemTwo;
		    systemOne = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idOne]] intValue];
		    systemTwo = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idTwo]] intValue];

		    if ( (systemOne == 0) && (systemTwo == 0) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 4]];
		      }
		    else if ( (systemOne == 1) && (systemTwo == 1) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 5]];
		      }
		    else if ( (systemOne == 8) && (systemTwo == 2) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 6]];
		      }
		    else if ( (systemOne == 9) && (systemTwo == 3) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 7]];
		      }
		    else if ( (systemOne == 9) && (systemTwo == 10) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 8]];
		      }
		    else
		      {
			error = YES;
			errorNumber = 13;
		      }
		  }
		else if ( (typeOne == 302) && (typeTwo == 309) )
		  {
		    /* This is a problem between one linear mobil
		       and one system */

		    // First check if the combination is allowed
		    int systemTwo;
		    systemTwo = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idTwo]] intValue];

		    // Set the type
		    if ( (typeOne == 302) && (systemTwo == 2) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 9]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 3) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 10]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 15) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 11]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 8) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 12]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 10) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 13]];
		      }
		    else
		      {
			error = YES;
			errorNumber = 13;
		      }
		  }
		else
		  {
		    error = YES;
		    errorNumber = 13;
		  }
	      }
	      break;
	    case 340:
	      {
		// Get the names of the elements
		nameOne = [[elementData objectAtIndex: 0] description];
		nameTwo = [[elementData objectAtIndex: 1] description];
	    
		// Get the ids of the elements
		idOne = [objectsIds objectAtIndex:
				      [objectsNames indexOfObject: nameOne]];
		idTwo = [objectsIds objectAtIndex:
				      [objectsNames indexOfObject: nameTwo]];
	    
		// Get the types of the elements
		typeOne = [[[objectsDictionary objectForKey: idOne]
			     objectForKey: @"Type"] intValue];
		typeTwo = [[[objectsDictionary objectForKey: idTwo]
			     objectForKey: @"Type"] intValue];

		if ( (typeOne == 302) && (typeTwo == 302) )
		  {
		    /* This is a problem between one linear mobil
		       and other linear mobil */
		    [objectLawType addObject: [NSNumber numberWithInt: 0]];
		  }
		else if ( (typeOne == 308) && (typeTwo == 309) )
		  {
		    // This is a dynamics problem with more than two objects

		    // First check if the combination is allowed
		    int systemOne, systemTwo;
		    systemOne = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idOne]] intValue];
		    systemTwo = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idTwo]] intValue];


		    if ( (systemOne == 8) && (systemTwo == 2) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 1]];
		      }
		    else if ( (systemOne == 9) && (systemTwo == 3) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 2]];
		      }
		    else if ( (systemOne == 9) && (systemTwo == 10) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 3]];
		      }
		    else
		      {
			error = YES;
			errorNumber = 25;
		      }
		  }
		else if ( (typeOne == 302) && (typeTwo == 309) )
		  {
		    /* This is a problem between one linear mobil
		       and one system */

		    // First check if the combination is allowed
		    int systemTwo;
		    systemTwo = [[objectSystemGenericType objectAtIndex:
			   [objectsSystem indexOfObject: idTwo]] intValue];

		    // Set the type
		    if ( (typeOne == 302) && (systemTwo == 2) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 4]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 3) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 5]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 15) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 6]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 8) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 7]];
		      }
		    else if ( (typeOne == 302) && (systemTwo == 10) )
		      {
			[objectLawType addObject: [NSNumber numberWithInt: 8]];
		      }
		    else
		      {
			error = YES;
			errorNumber = 25;
		      }
		  }
		else
		  {
		    error = YES;
		    errorNumber = 25;
		  }
	      }
	      break;
	    }
	}
    }

  enumed = [objCircular objectEnumerator];
  if (!error)
    {
      while ((elementId = [enumed nextObject]))
	{
	  elementType = [[[objectsDictionary objectForKey: elementId]
			   objectForKey: @"Type"] intValue];
	  elementData = [[objectsDictionary objectForKey: elementId]
			  objectForKey: @"Values"];
	  
	  // Get the name of the element
	  nameOne = [[elementData objectAtIndex: 0] description];
	  // Get the id of the element
	  idOne = [objectsIds objectAtIndex:
				[objectsNames indexOfObject: nameOne]];
	  // Get the type of the element
	  typeOne = [[[objectsDictionary objectForKey: idOne]
		       objectForKey: @"Type"] intValue];

	  // We only allow circular mobiles in these elements
	  if ( ((elementType == 316) || (elementType == 337) ||
		(elementType == 338)) && (typeOne != 304) )
	    {
	      error = YES;
	      errorNumber = 24;
	    }
	  else if (elementType == 343)
	    {
	      // First cehck if the object is allowed
	      if ( (typeOne == 308) || (typeOne == 309) )
		{
		  // Second check if the combination is allowed
		  int sysType = -1;
		  sysType = [[objectSystemGenericType objectAtIndex:
			       [objectsSystem indexOfObject: idOne]] intValue];
		  
		  if ( (sysType != 0) && (sysType != 1) )
		    {
		      error = YES;
		      errorNumber = 6;
		    }
		}
	      else
		{
		  error = YES;
		  errorNumber = 4;
		}
	    }
	  else if (elementType == 344)
	    {
	      // Get the name of the second element
	      nameTwo = [[elementData objectAtIndex: 2] description];
	      // Get the id of the second element
	      idTwo = [objectsIds objectAtIndex:
				    [objectsNames indexOfObject: nameTwo]];
	      // Get the type of the second element
	      typeTwo = [[[objectsDictionary objectForKey: idTwo]
			   objectForKey: @"Type"] intValue];
	      
	      if ( (typeOne != 303) && (typeTwo != 334) )
		{
		  error = YES;
		  errorNumber = 15;
		}
	    }
	  else if ( (typeOne != 303) && (typeOne != 304) && (typeOne != 339) )
	    {
	      error = YES;
	      errorNumber = 23;
	    }
	}
    }

  // Check the number of applied forces
  if (!error)
    {
      // Count the number of applied forces in the objects
      int v, w = 0;
      NSEnumerator *check;
      NSNumber *codForce;
      NSMutableArray *forcesCount = [NSMutableArray array];

      // Make an array with all applied forces
      for (v = 0; v < [objectsMobilAndSpring count]; v++)
	{
	  if ([forcesArrays objectAtIndex: v] != [NSNull null])
	    {
	      [forcesCount addObjectsFromArray:
			     [forcesArrays objectAtIndex: v]];
	    }
	}
      
      // Check if all elements forces are present in applied forces
      check = [codForces objectEnumerator];
      while ((codForce = [check nextObject]))
	{
	  if ([forcesCount containsObject: codForce])
	    {
	      w++;
	    }
	}

      /* If the number of applied forces is less than the number of element
	 forces, this mean that there are forces not applied */
      if (w < [codForces count])
	{
	  error = YES;
	  errorNumber = 20;
	}
      
      /* If the number of applied forces is less than the summ of all applied
	 forces, this mean that at leat one force is applied at to objects */
      if (w < [forcesCount count])
	{
	  error = YES;
	  errorNumber = 21;
	}
    }

  // Last checks
  if (!error)
    {
      if (([unknowns count] == numberOfEquations) && (numberOfEquations > 0))
	{
	  // All OK, build and solve the system
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
      length = [[[self viewer] textStorage] length];
      [[self viewer] replaceCharactersInRange: NSMakeRange(length, 0)
			   withString: [errors objectAtIndex: errorNumber]];
    }

  [keys release];
}

@end
