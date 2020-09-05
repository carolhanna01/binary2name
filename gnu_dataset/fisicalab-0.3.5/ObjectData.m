/* 
   Copyright (C) 2009, 2010, 2011, 2012, 2013, 2014, 2015 German A. Arias

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

#import <math.h>
#import "ChalkboardView.h"
#import "FLKinematics.h"
#import "FLKinematicsCircularMotion.h"
#import "FLStatics.h"
#import "FLStaticsRigidBodies.h"
#import "FLDynamics.h"
#import "FLCalorimetry.h"
#import "FLDynamicsCircularMotion.h"
#import "ObjectData.h"

/* These group of methods allow some mathematical operations at entries
   of table view. */
@interface ObjectData ( Mathematics )
- (NSString *) processEntry: (NSString *)anEntry;
- (NSString *) processAngleEntry: (NSString *)anEntry;
- (NSString *) operatePolynomial: (NSString *)anEntry;
- (NSString *) operateTerm: (NSString *)anEntry;
- (NSString *) operateFunction: (NSString *)anEntry;
- (BOOL) isNumericArray: (NSArray *)anArray;
- (BOOL) isNumeric: (NSString *)anEntry;
@end

@implementation ObjectData ( Mathematics )

// Process an entry in the table view.
- (NSString *) processEntry: (NSString *)anEntry
{
  NSString *result;
  NSArray *data;

  /* Check if the entry is a polynomial, a term or a variable name. And process
     the entry according at its class. */
  data = [anEntry componentsSeparatedByCharactersInSet: plus_minusSet];
  
  if ([data count] > 1)
    {
      result = [self operatePolynomial: anEntry];
    }
  else
    {
      data = [anEntry componentsSeparatedByCharactersInSet:
			multiplication_divisionSet];
      
      if ([data count] > 1)
	{
	  result = [self operateTerm: anEntry];
	}
      else if ([anEntry hasSuffix: @")"])
	{
	  result = [self operateFunction: anEntry];
	}
      else
	{
	  return anEntry;
	}
    }

  // If result is numeric, set the corresponding format.
  if ([self isNumeric: result])
    {
      result = [NSString stringWithFormat: @"%.3f", [result doubleValue]];
    }

  return result;
}

// Process a inverse trigonometric function.
- (NSString *) processAngleEntry: (NSString *)anEntry
{
  int j = -1;
  NSArray *array;
  NSString *data = [NSString stringWithString: anEntry];

  if ([data hasPrefix: @"acos("] &&
      [data hasSuffix: @")"])
    {
      data = [data stringByDeletingPrefix: @"acos("];
      data = [data stringByDeletingSuffix: @")"];

      j = 0;
    }
  else if ([data hasPrefix: @"asin("] &&
	   [data hasSuffix: @")"])
    {
      data = [data stringByDeletingPrefix: @"asin("];
      data = [data stringByDeletingSuffix: @")"];

      j = 1;
    }
  else
    {
      j = 2;
    }

  array = [data componentsSeparatedByString: @"/"];
      
  if ( ([array count] == 2) &&
       [self isNumericArray: array] &&
       ([[array objectAtIndex: 1] doubleValue] != 0) )
    {
      double angle;

      switch (j)
	{
	case 0:
	  {
	    angle = 180*acos([[array objectAtIndex: 0] doubleValue]/
			     [[array objectAtIndex: 1] doubleValue])/M_PI;
	  }
	  break;
	case 1:
	  {
	    angle = 180*asin([[array objectAtIndex: 0] doubleValue]/
			     [[array objectAtIndex: 1] doubleValue])/M_PI;
	  }
	  break;
	case 2:
	  {
	    angle = 180*atan2([[array objectAtIndex: 0] doubleValue],
			      [[array objectAtIndex: 1] doubleValue])/M_PI;
	  }
	  break;
	}
  
      return [NSString stringWithFormat: @"%.3f", angle];
    }
  else
    {
      return anEntry;
    }
}

// Process a polynomial.
- (NSString *) operatePolynomial: (NSString *)anEntry
{
  int x;
  double sum = 0;
  BOOL numeric = YES;
  NSString *result;
  NSMutableString *data = [NSMutableString stringWithString: anEntry];
  NSMutableString *operations = [NSMutableString string];
  NSMutableArray *terms = [NSMutableArray arrayWithArray:
                             [anEntry componentsSeparatedByCharactersInSet:
                                         plus_minusSet]];
  NSArray *factors;

  /*
   * Check that the final factor is not an empty string.
   * If is, this mean that the final character is an
   * operator. And this is not allowed.
   */
  if ([[terms lastObject] length] == 0)
    {
      numeric = NO;
    }

  // Get a string with all the operators in polynomial.
  if (numeric)
    {
      for (x = 0; x < [terms count]; x++)
	{
	  if ([data hasPrefix: @"+"] || [data hasPrefix: @"-"])
	    {
	      [operations appendString: [data substringToIndex: 1]];
	      [data deleteCharactersInRange: NSMakeRange (0,1)];
	    }

	  if ([data hasPrefix: [terms objectAtIndex: x]])
	    {
	      [data deletePrefix: [terms objectAtIndex: x]];
	    }
	}
    }

  // Make the operations in all terms.
  if (numeric)
    {
      if ([[terms objectAtIndex: 0] length] == 0)
	{
	  [terms replaceObjectAtIndex: 0 withObject: @"0"];
	}

      for (x = 0; x < [terms count]; x++)
	{
	  factors = [[terms objectAtIndex: x]
		      componentsSeparatedByCharactersInSet:
			multiplication_divisionSet];

	  if ([factors count] > 1)
	    {
	      result = [self operateTerm: [terms objectAtIndex: x]];

	      if ([self isNumeric: result])
		{
		  [terms replaceObjectAtIndex: x
				   withObject: result];
		}
	      else
		{
		  numeric = NO;
		  break;
		}
	    }
	  else if ([[terms objectAtIndex: x] hasSuffix: @")"])
	    {
	      result = [self operateFunction: [terms objectAtIndex: x]];

	      if ([self isNumeric: result])
		{
		  [terms replaceObjectAtIndex: x
				   withObject: result];
		}
	      else
		{
		  numeric = NO;
		  break;
		}
	    }
	  else if (![self isNumeric: [terms objectAtIndex: x]])
	    {
	      numeric = NO;
	      break;
	    }
	}
    }

  // Sum the terms.
  if (numeric)
    {
      if ([terms count] > [operations length])
	{
	  sum += [[terms objectAtIndex: 0] doubleValue];
	  [terms removeObjectAtIndex: 0];
	}

      for (x = 0; x < [terms count]; x++)
	{
	  if ([operations hasPrefix: @"+"])
	    {
	      [operations deletePrefix: @"+"];

	      sum += [[terms objectAtIndex: x] doubleValue];
	    }
	  else if ([operations hasPrefix: @"-"])
	    {
	      [operations deletePrefix: @"-"];

	      sum -= [[terms objectAtIndex: x] doubleValue];
	    }
	}
    }

  // Return the result if is numeric. Otherwise
  // return the original string.
  if (numeric)
    {
      return [NSString stringWithFormat: @"%f", sum];
    }
  else
    {
      return anEntry;
    }
}

// Process a mathematical term.
- (NSString *) operateTerm: (NSString *)anEntry
{
  int x;
  double result = 0;
  BOOL numeric = YES;
  NSString *factor;
  NSMutableString *data = [NSMutableString stringWithString: anEntry];
  NSMutableString *operations = [NSMutableString string];
  NSMutableArray *factors = [NSMutableArray arrayWithArray:
			      [anEntry componentsSeparatedByCharactersInSet:
			                  multiplication_divisionSet]];

  /*
   * Check that the first and final factors are not an empty string.
   * If one of these is empty, this mean that the first/final character
   * is an operator. And this is not allowed.
   */
  if ( ([[factors objectAtIndex: 0] length] == 0) ||
       ([[factors lastObject] length] == 0) )
    {
      numeric = NO;
    }

  // Get a string with all the operators in term.
  if (numeric)
    {
      for (x = 0; x < [factors count]; x++)
	{
	  if ([data hasPrefix: @"*"] || [data hasPrefix: @"/"])
	    {
	      [operations appendString: [data substringToIndex: 1]];
	      [data deleteCharactersInRange: NSMakeRange (0,1)];
	    }

	  if ([data hasPrefix: [factors objectAtIndex: x]])
	    {
	      [data deletePrefix: [factors objectAtIndex: x]];
	    }
	}
    }

  // Operate the funtions and check that factors/results are numerics.
  if (numeric)
    {
      for (x = 0; x < [factors count]; x++)
	{
	  if ([[factors objectAtIndex: x] hasSuffix: @")"])
	    {
	      factor = [self operateFunction: [factors objectAtIndex: x]];

	      if ([self isNumeric: factor])
		{
		  [factors replaceObjectAtIndex: x
				     withObject: factor];
		}
	      else
		{
		  numeric = NO;
		  break;
		}
	    }
	  else if (![self isNumeric: [factors objectAtIndex: x]])
	    {
	      numeric = NO;
	      break;
	    }
	}
    }

  // Make the operations.
  if (numeric)
    {
      result = [[factors objectAtIndex: 0] doubleValue];

      for (x = 1; x < [factors count]; x++)
	{
	  if ([operations hasPrefix: @"*"])
	    {
	      [operations deletePrefix: @"*"];

	      result *= [[factors objectAtIndex: x] doubleValue];
	    }
	  else if ([operations hasPrefix: @"/"])
	    {
	      [operations deletePrefix: @"/"];

	      result /= [[factors objectAtIndex: x] doubleValue];
	    }
	}
    }

  // Return the result if is numeric. Otherwise
  // return the original string.
  if (numeric)
    {
      return [NSString stringWithFormat: @"%f", result];
    }
  else
    {
      return anEntry;
    }
}

// Process a function.
- (NSString *) operateFunction: (NSString *)anEntry
{
  int j = -1;
  double number = 0;
  BOOL numeric = YES;
  NSMutableString *data = [NSMutableString stringWithString: anEntry];

  // First check the kind of funtion
  if ([data hasPrefix: @"cos("] &&
      [data hasSuffix: @")"])
    {
      [data deletePrefix: @"cos("];
      [data deleteSuffix: @")"];
      
      j = 0;
    }
  else if ([data hasPrefix: @"sin("] &&
	   [data hasSuffix: @")"])
    {
      [data deletePrefix: @"sin("];
      [data deleteSuffix: @")"];
      
      j = 1;
    }
  else if ([data hasPrefix: @"tan("] &&
	   [data hasSuffix: @")"])
    {
      [data deletePrefix: @"tan("];
      [data deleteSuffix: @")"];
      
      j = 2;
    }
  else if ([data hasPrefix: @"sqrt("] &&
	   [data hasSuffix: @")"])
    {
      [data deletePrefix: @"sqrt("];
      [data deleteSuffix: @")"];

      j = 3;
    }
  else if ([data hasPrefix: @"hypot("] &&
	   [data hasSuffix: @")"])
    {
      [data deletePrefix: @"hypot("];
      [data deleteSuffix: @")"];

      j = 4;
    }
  else if ([data hasPrefix: @"leg("] &&
	   [data hasSuffix: @")"])
    {
      [data deletePrefix: @"leg("];
      [data deleteSuffix: @")"];

      j = 5;
    }
  else if ([data hasPrefix: @"rd("] &&
	   [data hasSuffix: @")"])
    {
      [data deletePrefix: @"rd("];
      [data deleteSuffix: @")"];

      j = 6;
    }

  // Operate if there are numeric arguments
  switch (j)
    {
    case 0 ... 3:
      {
	if ([self isNumeric: data])
	  {
	    if (j == 0)
	      {
		// cosine
		number = cos(M_PI*[data doubleValue]/180);
	      }
	    else if (j == 1)
	      {
		// sine
		number = sin(M_PI*[data doubleValue]/180);
	      }
	    else if (j == 2)
	      {
		// tangent
		number = tan(M_PI*[data doubleValue]/180);
	      }
	    else
	      {
		// square root
		number = sqrt([data doubleValue]);
	      }
	  }
	else
	  {
	    numeric = NO;
	  }
      }
      break;
    case 4:
    case 5:
      {
	NSArray *factors;
	factors = [data componentsSeparatedByString: @","];

	if ([self isNumericArray: factors] &&
	    ([factors count] == 2) )
	  {
	    if (j == 4)
	      {
		// hypotenuse
		number = sqrt([[factors objectAtIndex: 0] doubleValue]*
			      [[factors objectAtIndex: 0] doubleValue] +
			      [[factors objectAtIndex: 1] doubleValue]*
			      [[factors objectAtIndex: 1] doubleValue]);
	      }
	    else
	      {
		// leg
		number = sqrt([[factors objectAtIndex: 0] doubleValue]*
			      [[factors objectAtIndex: 0] doubleValue] -
			      [[factors objectAtIndex: 1] doubleValue]*
			      [[factors objectAtIndex: 1] doubleValue]);
	      }
	  }
	else
	  {
	    numeric = NO;
	  }
      }
      break;
    case 6:
      {
	NSArray *factors;
	factors = [data componentsSeparatedByString: @","];

	if ([self isNumericArray: factors] &&
	    ([factors count] == 3) )
	  {
	    // radius
	    number = [[factors objectAtIndex: 1] doubleValue]*
	      [[factors objectAtIndex: 2] doubleValue]/
	      ( [[factors objectAtIndex: 0] doubleValue] +
		[[factors objectAtIndex: 1] doubleValue] );
	  }
	else
	  {
	    numeric = NO;
	  }
      }
      break;
    }
  
  // Return the result if there are numeric arguments. Otherwise
  // return the original string.
  if (numeric)
    {
      return [NSString stringWithFormat: @"%f", number];
    }
  else
    {
      return anEntry;
    }
}

// Check if all entries at the array are numerics or not.
- (BOOL) isNumericArray: (NSArray *)anArray
{
  BOOL numeric = YES;
  NSString *object;
  NSEnumerator *enumerator = [anArray objectEnumerator];

  while ((object = [enumerator nextObject]))
    { 
      if (![self isNumeric: object])
	{
	  numeric = NO;
	  break;
	}
    }

  return numeric;
}

// Check if a string is entirely numeric or not.
- (BOOL) isNumeric: (NSString *)anEntry
{
  BOOL numeric = YES;
  NSCharacterSet *set;

  set = [NSCharacterSet characterSetWithCharactersInString: anEntry];
  
  if (![numbers isSupersetOfSet: set] || ([anEntry length] == 0) )
    {
      numeric = NO;
    }

  return numeric;
}
@end

@implementation ObjectData

- (void) awakeFromNib
{
  // Init instance variables.
  referenceCount = 0;
  system = 0;
  [dataViewer setDataSource: self];
  [dataViewer setDelegate: self];
  objectsList = [NSMutableDictionary dictionary];
  [objectsList retain];

  numberId = nil;

  // Build the conversion factor dictionary.
  NSBundle *mainBundle = [NSBundle mainBundle];
  NSString *path = [mainBundle pathForResource: @"conversionFactors"
					ofType: @"plist"];
  conversionDictionary = [NSMutableDictionary dictionaryWithContentsOfFile:
						path];
  [conversionDictionary retain];

  // Build the menu conversion factor dictionary.
  path = [mainBundle pathForResource: @"menuConversionFactors"
			      ofType: @"plist"];
  menuConversionDictionary = [NSMutableDictionary dictionaryWithContentsOfFile:
						    path];
  [menuConversionDictionary retain];

  // Build character sets needed for mathematical operations.
  numbers = [NSCharacterSet characterSetWithCharactersInString:
			      @".-+0123456789E"];
  [numbers retain];
  plus_minusSet =
    [NSCharacterSet characterSetWithCharactersInString: @"-+"];
  [plus_minusSet retain];
  multiplication_divisionSet =
    [NSCharacterSet characterSetWithCharactersInString: @"*/"];
  [multiplication_divisionSet retain];

  [[NSNotificationCenter defaultCenter]
    addObserver: self
    selector: @selector (setMenuForModule:)
    name: @"moduleDidChangeNotification"
    object: nil];
}

- (void) unitsSystem: (id)sender
{
  /* If user change the units system, update the variable and the menu of
     conversion factors. */
  if (system != [[sender selectedCell] tag])
    {
      system = [[sender selectedCell] tag];
      [self setMenuForModule: nil];
    }
}

- (void) calculate: (id)sender
{
  int cod = [sender code: self];
  [dataViewer deselectAll: sender];
  
  // Delete the content of results viewer if needed.
  if ([cleanResultsViewer state] == NSOnState)
    {
      [resultsViewer setString: @""];
    }
  
  /* Instantiate the corresponding class for the problem and pass all the
     information to solve it. */
  switch (cod)
    {
    case 0:
      {
	id kinematicsParticles = [FLKinematics new];
	[kinematicsParticles setSystem: system];
	[kinematicsParticles setConversions:
                   [conversionDictionary objectForKey: @"kinematicParticles"]];
	[kinematicsParticles setViewer: resultsViewer];
	[kinematicsParticles makeEquationsForData: objectsList];
	[kinematicsParticles release];
      }
      break;
    case 1:
      {
	id staticsParticles = [FLStatics new];
	[staticsParticles setSystem: system];
	[staticsParticles setConversions:
                     [conversionDictionary objectForKey: @"staticParticles"]];
	[staticsParticles setViewer: resultsViewer];
	[staticsParticles setCells: [sender subviews]];
	[staticsParticles withChalkboardWidth: [sender chalkboardWidth]
				       height: [sender chalkboardHeight]];
	[staticsParticles makeEquationsForData: objectsList];
	[staticsParticles release];
      }
      break;
    case 2:
      {
	id dynamicsParticles = [FLDynamics new];
	[dynamicsParticles setSystem: system];
	[dynamicsParticles setConversions:
                    [conversionDictionary objectForKey: @"dynamicParticles"]];
	[dynamicsParticles setViewer: resultsViewer];
	[dynamicsParticles setCells: [sender subviews]];
	[dynamicsParticles withChalkboardWidth: [sender chalkboardWidth]
					height: [sender chalkboardHeight]];
	[dynamicsParticles makeEquationsForData: objectsList];
	[dynamicsParticles release];
      }
      break;
    case 3:
      {
	id calorimetry = [FLCalorimetry new];
	[calorimetry setSystem: system];
	[calorimetry setConversions:
		       [conversionDictionary objectForKey: @"heat"]];
	[calorimetry setViewer: resultsViewer];
	[calorimetry setCells: [sender subviews]];
	[calorimetry withChalkboardWidth: [sender chalkboardWidth]
				  height: [sender chalkboardHeight]];
	[calorimetry makeEquationsForData: objectsList];
	[calorimetry release];
      } 
      break;
    case 4:
      {
	id kinematicsCircular = [FLKinematicsCircularMotion new];
	[kinematicsCircular setSystem: system];
	[kinematicsCircular setConversions:
                   [conversionDictionary objectForKey: @"kinematicCircular"]];
	[kinematicsCircular setViewer: resultsViewer];
	[kinematicsCircular makeEquationsForData: objectsList];
	[kinematicsCircular release];
      }
      break;
    case 5:
      {
	id staticsRigid = [FLStaticsRigidBodies new];
	[staticsRigid setSystem: system];
	[staticsRigid setConversions:
			[conversionDictionary objectForKey: @"staticRigid"]];
	[staticsRigid setViewer: resultsViewer];
	[staticsRigid setCells: [sender subviews]];
	[staticsRigid withChalkboardWidth: [sender chalkboardWidth]
				   height: [sender chalkboardHeight]];
	[staticsRigid makeEquationsForData: objectsList];
	[staticsRigid release];
      }
      break;
    case 6:
      {
	id dynamicsCircular = [FLDynamicsCircularMotion new];
	[dynamicsCircular setSystem: system];
	[dynamicsCircular setConversions:
                 [conversionDictionary objectForKey: @"dynamicsCircular"]];
	[dynamicsCircular setViewer: resultsViewer];
	[dynamicsCircular setCells: [sender subviews]];
	[dynamicsCircular withChalkboardWidth: [sender chalkboardWidth]
				       height: [sender chalkboardHeight]];
	[dynamicsCircular makeEquationsForData: objectsList];
	[dynamicsCircular release];
      }
      break;
    }
}

/* Build the corresponding dictionary to the new element and add this to
   the objectsList dictionary. */
- (void) addObject: (id)new
{
  NSString *gravity;
  NSArray *titleList = nil;
  NSMutableArray *dataList = nil;
  NSMutableArray *valueList = [NSMutableArray array];
  NSMutableDictionary *data;
  
  NSNumber *type;
  referenceCount += 1;
  [dataViewer deselectRow: [dataViewer selectedRow]];
  
  if (system == 0)
    {
      gravity = @"9.81";
    }
  else
    {
      gravity = @"32.2";
    }
  
  switch ([new tag])
    {
      // Objetcs from 1-49 are of kinematics of particles
    case 1:
      {
	// Reference system
        titleList = [NSArray arrayWithObjects: @"tf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 2:
      {
	// Mobile reference system
        titleList = [NSArray arrayWithObjects: _(@"Name"), _(@"Object"), @"xsi",
			     @"ysi", @"vsx", @"vsy", @"xof", @"yof", @"vxof",
			     @"vyof", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", @"0", @"0", @"0", nil];
      }
      break;
    case 3:
      {
	// Mobile
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"ax", @"ay", @"xi",
			     @"yi", @"vxi", @"vyi", @"ti", @"xf", @"yf",
			     @"vxf", @"vyf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 4:
      {
	// Cannon
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"ax", @"ay", @"xi",
			     @"yi", @"vi", @"angi", @"ti", @"xf", @"yf", @"vf",
			     @"angf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 5:
      {
	// Mobile in X
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"ax", @"xi", @"vxi",
			     @"ti", @"xf", @"vxf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", nil];
      }
      break;
    case 6:
      {
	// Mobile in Y
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"ay", @"yi", @"vyi",
			     @"ti", @"yf", @"vyf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", nil];
      }
      break;
    case 7:
      {
	// Mobile reference system in X
        titleList = [NSArray arrayWithObjects: _(@"Name"), _(@"Object"), @"xsi",
			     @"vsx", @"xof", @"vxof", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", nil];
      }
      break;
    case 8:
      {
	// Mobile reference system in Y
        titleList = [NSArray arrayWithObjects: _(@"Name"), _(@"Object"), @"ysi",
			     @"vsy", @"yof", @"vyof", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", nil];
      }
      break;
    case 9:
      {
	// Mobile in X with constant velocity
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"xi", @"xf", @"ti",
			     @"vx", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 10:
      {
	// Mobile in Y with constant velocity
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"yi", @"yf", @"ti",
			     @"vy", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 11:
      {
	// Distance
        titleList = [NSArray arrayWithObjects: _(@"Object 1"), _(@"Object 2"),
			     @"d", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 12:
      {
	// Relative velocity
        titleList = [NSArray arrayWithObjects: _(@"Object 1"), _(@"Object 2"),
			     @"v", @"ang", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 13:
      {
	// Mobile radial
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"a", @"angf", @"xi",
			     @"yi", @"vi", @"ti", @"xf", @"yf", @"vf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", @"0", @"0", @"0", nil];
      }
      break;
    case 14:
      {
	// Distance X or Y
        titleList = [NSArray arrayWithObjects: @"x1 (y1)", @"x2 (y2)",
			     @"x1 - x2 (y1 - y2)", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 15:
      {
	// Point
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"xf", @"yf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
      // Objects from 50-99 are of statics of particles
    case 50:
      {
	// Reference system
        titleList = [NSArray arrayWithObjects: @"g", nil];
        dataList = [NSMutableArray arrayWithObjects: gravity, nil];
      }
      break;
    case 51:
      {
	// Block
        titleList = [NSArray arrayWithObjects: @"m", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 52:
    case 53:
      {
	// Block, inclined plane
        titleList = [NSArray arrayWithObjects: @"m", @"ang", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 54:
    case 55:
      {
	// Pulley and point
        titleList = [NSArray arrayWithObjects: _(@"Name"), nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break; 
    case 56 ... 59:
      {
	// Oblique forces
        titleList = [NSArray arrayWithObjects: @"f", @"ang", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 60 ... 63:
      {
	// Vertical/horizontal forces
        titleList = [NSArray arrayWithObjects: @"f", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 64 ... 71:
      {
	// Frictions
        titleList = [NSArray arrayWithObjects: @"N", @"u", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 72:
      {
	// Resultant
        titleList = [NSArray arrayWithObjects: @"f", @"ang", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 73:
    case 74:
      {
	// Vertical/horizontal resultant
        titleList = [NSArray arrayWithObjects: @"f", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 75 ... 78:
      {
	// Springs
        titleList = [NSArray arrayWithObjects: @"k", @"d", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 79:
      {
	// Relation of angles
	titleList = [NSArray arrayWithObjects: @"ang1", @"ang2", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
      // Objects from 100-149 are of dynamics of particles
    case 100:
      {
	// Reference system
        titleList = [NSArray arrayWithObjects: @"g", @"t", nil];
        dataList = [NSMutableArray arrayWithObjects: gravity, @"0", nil];
      }
      break;
    case 101:
      {
	// Mobile
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"vi", @"angi",
			     @"vf", @"angf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", nil];
      }
      break;
    case 102:
      {
	// Mobile in X
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"vxi", @"vxf",
			     nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 103:
      {
	// Mobile in Y
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"vyi", @"vyf",
			     nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 104:
    case 105:
      {
	// Vertical/horizontal block
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"a", @"vi",
			     @"vf", @"d", _(@"Relative to"), nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"sf", nil];
      }
      break;
    case 106:
    case 107:
      {
	// Block, inclined plane
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"ang", @"a",
			     @"vi", @"vf", @"d", _(@"Relative to"), nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", @"sf", nil];
      }
      break;
    case 108:
      {
	// Pulley
        titleList = [NSArray arrayWithObjects: _(@"Name"), nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 109 ... 112:
      {
	// Oblique forces
        titleList = [NSArray arrayWithObjects: @"f", @"ang", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 113 ... 116:
      {
	// Vertical/horizontal forces
        titleList = [NSArray arrayWithObjects: @"f", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 117 ... 124:
      {
	// Frictions
        titleList = [NSArray arrayWithObjects: @"N", @"u", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 125 ... 128:
      {
	// Oblique contacts
        titleList = [NSArray arrayWithObjects: @"N", @"u", @"ang", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 129 ... 132:
      {
	// Vertical/horizontal contacts
        titleList = [NSArray arrayWithObjects: @"N", @"u", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 133 ... 136:
      {
	// Springs
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"k", @"xi", @"xf",
			     nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 137:
      {
	// Relation of accelerations
        titleList = [NSArray arrayWithObjects: @"a1", @"a2", @"z", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"-1", nil];
      }
      break;
    case 138:
      {
	// Collision
        titleList = [NSArray arrayWithObjects: _(@"Object 1"), _(@"Object 2"),
			     @"e", @"angn", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 139:
      {
	// Energy
        titleList = [NSArray arrayWithObjects: _(@"Object 1"), _(@"Object 2"),
			     _(@"Object 3"), _(@"Object 4"), @"W", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 140:
      {
	// Impulse
        titleList = [NSArray arrayWithObjects: _(@"Object"), @"Imp", @"ang",
			     @"fImp", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 141:
      {
	// Power
        titleList = [NSArray arrayWithObjects: _(@"Object"), @"P", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 143:
      {
	// Relative motion
        titleList = [NSArray arrayWithObjects: _(@"Object"), @"asf", @"ang_asf",
			     @"vfsf", @"ang_vfsf", @"dsf", @"ang_dsf", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", nil];
      }
      break;          
      // Objects from 150-199 are of calorimetry
    case 150:
      {
	// Clock
	titleList = [NSArray arrayWithObjects: @"t", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 151:
    case 153:
      {
	// Applied/extractred heat
	titleList = [NSArray arrayWithObjects: @"Q", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 152:
    case 154:
      {
	// Heat flow/refrigeration
	titleList = [NSArray arrayWithObjects: @"dQ/dt", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 155:
    case 156:
    case 169:
      {
	// Block, liquid and gas
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"c", @"Ti",
			     @"Tf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 157:
      {
	// Linear expansion
	titleList = [NSArray arrayWithObjects: @"k", @"Li", @"Lf", @"Ti", @"Tf",
			     nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 158:
      {
	// Superficial expansion
	titleList = [NSArray arrayWithObjects: @"k", @"Si", @"Sf", @"Ti", @"Tf",
			     nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 159:
      {
	// Volumetric expansion
	titleList = [NSArray arrayWithObjects: @"k", @"Vi", @"Vf", @"Ti", @"Tf",
			     nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 160:
      {
	// Change of state solid-liquid
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"cf",
			     _(@"Sense"), nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0",
				   @">", nil];
      }
      break;
    case 161:
      {
	// Change of state liquid-gas
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"cv",
			     _(@"Sense"), nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @">",
				   nil];
      }
      break;
    case 162:
      {
	// Process
	titleList = [NSArray arrayWithObjects: _(@"Name"), _(@"Object 1"),
			     _(@"Object 2"), _(@"Object 3"), _(@"Object 4"),
			     _(@"Object 5"), nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", nil];
      }
      break;
    case 163:
      {
	// Calorimeter
	titleList = [NSArray arrayWithObjects: _(@"Object 1"), _(@"Object 2"),
			     _(@"Object 3"), _(@"Object 4"), nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 164:
      {
	// Gas at constant pressure
	titleList = [NSArray arrayWithObjects: @"Vi", @"Ti", @"Vf", @"Tf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 165:
      {
	// Gas at constant temperature
	titleList = [NSArray arrayWithObjects: @"Pi", @"Vi", @"Pf", @"Vf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 166:
      {
	// Gas at constant volume
	titleList = [NSArray arrayWithObjects: @"Pi", @"Ti", @"Pf", @"Tf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 167:
      {
	// General gas
	titleList = [NSArray arrayWithObjects: @"Pi", @"Vi", @"Ti", @"Pf",
			     @"Vf", @"Tf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", nil];
      }
      break;
    case 168:
      {
	// Heat exchanger
	titleList = [NSArray arrayWithObjects: @"TRi", @"TRf", @"dR/dt", @"cR",
			     @"TFi", @"TFf", @"dF/dt", @"cF", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", @"0", nil];
      }
      break;
      // Objects from 200-249 are in circular kinematics of particles
    case 200:
      {
	// Reference system
	titleList = [NSArray arrayWithObjects: @"tf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 201:
      {
	// Mobile circular
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"C", @"r", @"aci",
			     @"at", @"angi", @"vi", @"ti", @"angf", @"vf",
			     @"acf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 202:
      {
	// Mobile polar
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"C", @"aa", @"ar",
			     @"angi", @"ri", @"vai", @"vri", @"ti", @"angf",
			     @"rf", @"vaf", @"vrf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", @"0", @"0", @"0", @"0",
				   @"0", @"0", nil];
      }
      break;
    case 203:
      {
	// Angular velocity
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"vangi", @"vangf",
			     nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 204:
      {
	// Angular acceleration
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"aang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 205:
      {
	// Total acceleration
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"atoti", @"angi",
			     @"atotf", @"angf", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 206:
      {
	// Frequency
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"f", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 207:
      {
	// Period
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"T", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 208:
      {
	// Number of laps
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"n", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 209:
      {
	// Center of rotation
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"x", @"y", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 210:
      {
	// Distance
	titleList = [NSArray arrayWithObjects: _(@"Object 1"), _(@"Object 2"),
			     @"d", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 211:
      {
	// Arc length
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"s", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 212:
      {
	// Coordinate
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"x", @"y", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 213:
      {
	// Relative velocity
	titleList = [NSArray arrayWithObjects: _(@"Object 1"), _(@"Object 2"),
			     @"v", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
      // Objects from 250-299 are in static rigid
    case 250:
      {
	// Reference system
	titleList = [NSArray arrayWithObjects: @"g", nil];
	dataList = [NSMutableArray arrayWithObjects: gravity, nil];
      }
      break;
    case 251:
      {
	// Element point
	titleList = [NSArray arrayWithObjects: _(@"Points"), @"x", @"y", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 252:
      {
	// Beam
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"lc", @"ang",
			     nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 253:
      {
	// Solid
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"xc", @"yc",
			     @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 254:
      {
	// Couple
	titleList = [NSArray arrayWithObjects: @"M", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 255:
      {
	// Element of beam
	titleList = [NSArray arrayWithObjects: _(@"Beam"), @"l", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 256:
      {
	// Element of solid
	titleList = [NSArray arrayWithObjects: _(@"Solid"), @"x", @"y", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 257 ... 260:
      {
	// Oblique forces
	titleList = [NSArray arrayWithObjects: @"f", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 261 ... 264:
      {
	// Vertical/horizontal forces
	titleList = [NSArray arrayWithObjects: @"f", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 265 ... 268:
      {
	// Oblique frictions
	titleList = [NSArray arrayWithObjects: @"N", @"ang", @"u", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 269 ... 272:
      {
	// Vertical/horizontal frictions
	titleList = [NSArray arrayWithObjects: @"N", @"u", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 273:
      {
	// General resultant
	titleList = [NSArray arrayWithObjects: @"M", @"f", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 274:
    case 275:
      {
	// Vertical/horizontal resultants
	titleList = [NSArray arrayWithObjects: @"M", @"f", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 276:
      {
	// Points
	titleList = [NSArray arrayWithObjects: _(@"Name"), nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 277 ... 280:
      {
	// Beams of two forces
	titleList = [NSArray arrayWithObjects: @"f", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 281 ... 284:
      {
	// Vertical/horizontal beams of two forces
	titleList = [NSArray arrayWithObjects: @"f", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 285 ... 288:
      {
	// Oblique beams of truss
	titleList = [NSArray arrayWithObjects: @"t", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 289 ... 292:
      {
	// Vertical/horizontal beam of truss
	titleList = [NSArray arrayWithObjects: @"t", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 293:
      {
	// Truss
	titleList = [NSArray arrayWithObjects: _(@"Name"), nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 294:
      {
	// Joint
	titleList = [NSArray arrayWithObjects: _(@"Truss"), nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 295:
      {
	//Relation of angles
	titleList = [NSArray arrayWithObjects: @"ang1", @"ang2", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
      // Objetcs in 300-349 are of dynamics of circular motion
    case 300:
      {
	// Reference system
	titleList = [NSArray arrayWithObjects: @"g", @"t", nil];
	dataList = [NSMutableArray arrayWithObjects: gravity, @"1", nil];
      }
      break;
    case 301:
      {
	// Mobile
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"y", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 302:
      {
	// Mobile linear
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"v", @"x",
			     @"y", @"ang", @"a", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", nil];
      }
      break;
    case 303:
      {
	// Mobile circular
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"vt", @"r",
			     @"y", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 304:
      {
	// Mobile perpendicular
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"vt", @"r",
			     @"at", @"Ft", @"C", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @">", nil];
      }
      break;
    case 305:
      {
	//Energy
	titleList = [NSArray arrayWithObjects: _(@"System i"), _(@"System f"),
			     @"W", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 306:
      {
	// Angular momentum
	titleList = [NSArray arrayWithObjects: _(@"System i"), _(@"System f"),
			     @"M", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 307:
      {
	// Power
	titleList = [NSArray arrayWithObjects: _(@"System i"), _(@"System f"),
			     @"P", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 308:
    case 309:
      {
	// Initial and final system
	titleList = [NSArray arrayWithObjects: _(@"Name"), _(@"Object 1"),
			     _(@"Object 2"), _(@"Object 3"), _(@"Object 4"),
			     nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 310:
      {
	// Sine of angle
	titleList = [NSArray arrayWithObjects: @"y", @"r", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
      // Elements 311, 312, 313 are available.
    case 314:
      {
	// Angles
	titleList = [NSArray arrayWithObjects: @"ang1", @"ang2", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 315:
      {
	// Couple
	titleList = [NSArray arrayWithObjects: @"M", @"f", @"d", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 316:
      {
	// Triangle of accelerations
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"atot", @"ang",
			     nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 317 ... 320:
      {
	// Springs
        titleList = [NSArray arrayWithObjects: _(@"Name"), @"k", @"x", nil];
        dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 321 ... 324:
      {
	// Oblique forces
	titleList = [NSArray arrayWithObjects: @"f", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 325 ... 328:
      {
	// Vertical and horizontal forces
	titleList = [NSArray arrayWithObjects: @"f", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 330:
    case 332:
    case 341:
    case 342:
      {
	// Oblique frictions
	titleList = [NSArray arrayWithObjects: @"N", @"u", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 329:
    case 331:
    case 333:
      {
	// Vertical and horizontal frictions
	titleList = [NSArray arrayWithObjects: @"N", @"u", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 334:
      {
	// Center
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"v", @"ang", @"x",
			     @"y", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
    case 335:
      {
	// Angular velocity
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"vang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 336:
      {
	// Centripetal acceleration
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"ac", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 337:
      {
	// Angular acceleration
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"aang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 338:
      {
	// Max acceleration
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"u", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", nil];
      }
      break;
    case 339:
      {
	// Polar mobile
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"m", @"vt", @"r",
			     @"y", @"vr", @"ar", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", @"0", @"0", nil];
      }
      break;
    case 340:
      {
	// Lineal momentum
	titleList = [NSArray arrayWithObjects: _(@"System i"), _(@"System f"),
			     @"Fx", @"Fy", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   nil];
      }
      break;
    case 343:
      {
	// Inertia
	titleList = [NSArray arrayWithObjects: _(@"System"), @"m", @"r", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    case 344:
      {
	// Absolute velocity
	titleList = [NSArray arrayWithObjects: _(@"Object"), @"angR",
			     _(@"Center"), @"v", @"ang", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", @"0",
				   @"0", nil];
      }
      break;
      // Objetcs in 350-399 are of thermodynamics
    case 350:
      {
	// Heat
	titleList = [NSArray arrayWithObjects: @"Q", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 351:
      {
	// Work
	titleList = [NSArray arrayWithObjects: @"W", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", nil];
      }
      break;
    case 352:
      {
	// Data at some state
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"p", @"t", @"d",
			     @"V", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"-", @"-", @"-",
				   @"-", nil];
      }
      break;
    case 353:
      {
	// Data at some state (satured)
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"p", @"t", @"d",
			     @"V", @"x", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"-", @"-", @"-",
				   @"-", @"-", nil];
      }
      break;
    case 354:
      {
	// Data of a flow
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"p", @"t", @"d",
			     @"v", @"A", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"-", @"-", @"-",
				   @"-", @"-", nil];
      }
      break;
    case 355:
      {
	// Data of a flow (satured)
	titleList = [NSArray arrayWithObjects: _(@"Name"), @"p", @"t", @"d",
			     @"v", @"A", @"x", nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"-", @"-", @"-",
				   @"-", @"-", @"-", nil];
      }
      break;
    case 356:
      {
	// A change
	titleList = [NSArray arrayWithObjects: _(@"System i"), _(@"System f"),
			     _(@"Substance"), nil];
	dataList = [NSMutableArray arrayWithObjects: @"0", @"0", @"0", nil];
      }
      break;
    }

  if ([titleList count] != [dataList count])
    {
      NSLog(@"Error in number of entries.");
    }
  else
    {
      type = [NSNumber numberWithInt: [new tag]];
      data = [NSMutableDictionary dictionaryWithObjectsAndKeys: type, @"Type",
				   titleList, @"Titles",
				   dataList, @"Data",
				   valueList, @"Values", nil];
      ASSIGN(numberId, [NSNumber numberWithInt: referenceCount]);
      [objectsList setObject: data forKey: numberId];
      [new setTag: [numberId intValue]];
      [dataViewer reloadData];
    }
}

// Delete an element in objectsList dictionary.
- (void) deleteObject: (int)code
{
  ASSIGN(numberId, [NSNumber numberWithInt: code]);
  [dataViewer deselectAll: self];
  [objectsList removeObjectForKey: numberId];
  [dataViewer reloadData];
}

// Update the table view with the info of new selected element.
- (void) selectObject: (int)code
{
  [dataViewer deselectRow: [dataViewer selectedRow]];
  ASSIGN(numberId, [NSNumber numberWithInt: code]);
  [dataViewer reloadData];
}

// Delete all elements at objectsList dictionary.
- (void) deleteAllObjects: (BOOL)value
{
  referenceCount = 0;
  [dataViewer deselectAll: self];
  [objectsList removeAllObjects];
  [dataViewer reloadData];
}

// Build a string with the element's info, to display in a tooltip.
- (NSString *) dataOfObject: (NSNumber *)aNumber
{
  int x;
  NSString *field;
  NSString *objectData = @"";
  NSMutableArray *keys, *values;
  
  keys = [[objectsList objectForKey: aNumber] objectForKey: @"Titles"];
  values = [[objectsList objectForKey: aNumber] objectForKey: @"Data"];

  for (x = 0; x < [keys count]; x++)
    {
      field = [NSString stringWithFormat: @"%@  =  %@ \n",
		    [[keys objectAtIndex: x] description],
		  [[values objectAtIndex: x] description]];
      
      objectData = [objectData stringByAppendingString: field];
    }

  return objectData;
}

// Update the contextual menu for the selected system and module.
- (void) setMenuForModule: (NSNotification *)notification
{
  id factor;
  NSMenuItem *item;
  NSMenu *factorsMenu;
  NSEnumerator *conv = nil;

  // Select the appropriate list of conversion factors.
  switch ([chalkboard code: self])
    {
    case 0:
      {
	conv = [[[menuConversionDictionary objectForKey: @"kinematicParticles"]
		  objectAtIndex: system] keyEnumerator];
      }
      break;
    case 1:
      {
	conv = [[[menuConversionDictionary objectForKey: @"staticParticles"]
		  objectAtIndex: system] keyEnumerator];
      }
      break;
    case 2:
      {
	conv = [[[menuConversionDictionary objectForKey: @"dynamicParticles"]
		  objectAtIndex: system] keyEnumerator];
      }
      break;
    case 3:
      {
	conv = [[[menuConversionDictionary objectForKey: @"heat"]
		  objectAtIndex: 0] keyEnumerator];
      }
      break;
    case 4:
      {
	conv = [[[menuConversionDictionary objectForKey: @"kinematicCircular"]
		  objectAtIndex: system] keyEnumerator];
      }
      break;
    case 5:
      {
	conv = [[[menuConversionDictionary objectForKey: @"staticRigid"]
		  objectAtIndex: system] keyEnumerator];
      }
      break;
    case 6:
      {
	conv = [[[menuConversionDictionary objectForKey: @"dynamicsCircular"]
		  objectAtIndex: system] keyEnumerator];
      }
      break;
    default:
      break;
    }

  // Set a transient menu with the selected conversion factors.
  if (conv != nil)
    {
      factorsMenu = [[[NSMenu alloc] initWithTitle: _(@"Factors")] autorelease];
      [factorsMenu setDelegate: self];
      
      while ((factor = [conv nextObject]))
	{
	  item = [factorsMenu addItemWithTitle: [factor description]
			      action: @selector(insertFactor:)
			      keyEquivalent: @""];
	  [item setTarget: self];
	}
      
      [dataViewer setMenu: factorsMenu];
    }
  else
    {
      [dataViewer setMenu: nil];
    }
}

// Add the selected conversion factor in contextual menu.
- (void) insertFactor: (id)sender
{
  int y = [dataViewer selectedRow];

  if (y >= 0)
    {
      NSString *newValue;
      NSCell *cell = [[dataViewer tableColumnWithIdentifier: @"Data"]
		       dataCellForRow: y];
      NSArray *text = [[cell stringValue] componentsSeparatedByString: @"@"];
      NSMutableArray *list = [[objectsList objectForKey: numberId]
				           objectForKey: @"Data"];
      
      newValue = [[text objectAtIndex: 0] stringByTrimmingSpaces];
      newValue = [newValue stringByAppendingString: @" @ "];
      newValue = [newValue stringByAppendingString: [sender title]];
      [list replaceObjectAtIndex: y withObject: newValue];
      [dataViewer reloadData];
      [dataViewer deselectAll: self];
    }
}

// Delegate and data source methods for table view.
- (NSInteger) numberOfRowsInTableView: (NSTableView*)aTableView
{
  return [[[objectsList objectForKey: numberId] objectForKey: @"Titles"] count];
}

- (id) tableView: (NSTableView*)aTableView
       objectValueForTableColumn: (NSTableColumn*)aTableColumn
       row: (NSInteger)rowIndex
{
  id print;
  NSMutableDictionary *object = [objectsList objectForKey: numberId];
  NSMutableArray *list = [object objectForKey: [aTableColumn identifier]];
  print = [list objectAtIndex: rowIndex];
  
  if ([[aTableColumn identifier] isEqualToString: @"Titles"])
    {
      print = [print description];
    }
  
  return print;
}

- (void) tableView: (NSTableView*)aTableView setObjectValue: (id)anObject
         forTableColumn: (NSTableColumn*)aTableColumn row: (NSInteger)rowIndex
{
  NSRange range;
  NSString *factor = nil;
  NSString *entry = [[anObject description] stringByTrimmingSpaces];
  NSMutableDictionary *object = [objectsList objectForKey: numberId];
  NSMutableArray *list = [object objectForKey: [aTableColumn identifier]];

  range = [entry rangeOfString: @"@"];
  if (range.length > 0)
    {
      factor = [entry substringFromIndex: range.location];
      entry = [[entry substringToIndex: range.location]
		stringByTrimmingSpaces];
    }

  if ([[[[object objectForKey: @"Titles"] objectAtIndex: rowIndex]
	 description] hasPrefix: @"ang"])
    {
      entry = [self processAngleEntry: entry];
    }
  else
    {
      entry = [self processEntry: entry];
    }
  
  if (factor != nil)
    {
      entry = [NSString stringWithFormat: @"%@ %@",
			entry,
			factor];
    }

  [list replaceObjectAtIndex: rowIndex
		  withObject: entry];
  [dataViewer reloadData];
}

- (BOOL) tableView: (NSTableView*)aTableView
         shouldEditTableColumn: (NSTableColumn*)aTableColumn
			   row: (NSInteger)rowIndex
{
  [chalkboard controlCursor: self];
  return YES;
}

- (void) tableView: (NSTableView*)aTableView 
   willDisplayCell: (id)aCell 
    forTableColumn: (NSTableColumn*)aTableColumn 
	       row: (NSInteger)rowIndex
{
  [aCell setEditable: YES];
}


// Delegate for contextual menu.
- (void) menuNeedsUpdate: (NSMenu*)menu
{
  [chalkboard controlCursor: self];
}

- (void) dealloc
{
  [objectsList release];
  [conversionDictionary release];
  [menuConversionDictionary release];
  [numbers release];
  [plus_minusSet release];
  [multiplication_divisionSet release];
  [numberId release];
  [super dealloc];
}

@end
