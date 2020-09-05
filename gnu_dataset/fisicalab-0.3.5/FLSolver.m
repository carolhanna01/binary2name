/* 
   Copyright (C) 2012, 2013 German A. Arias

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

#import "FLSolver.h"

@implementation FLSolver

- (id) init
{
  self = [super init];

  if (self)
    {
      numbers = [NSCharacterSet characterSetWithCharactersInString:
				  @".-+0123456789E"];
      [numbers retain];
    }

  return self;
}

- (void) dealloc
{
  [numbers release];
  [super dealloc];
}

// Accessor methods for units system.
- (void) setSystem: (NSUInteger)aNumber
{
  system = aNumber;
}

- (NSUInteger) system
{
  return system;
}

// Accessor methods for conversion factors.
- (void) setConversions: (NSArray *)anArray
{
  conversions = anArray;
}

- (NSDictionary *) conversions
{
  // Return the conversion factors for the selected units system.
  return [conversions objectAtIndex: system];
}

// Accessor methods for text viewer which display the results.
- (void) setViewer: (NSTextView *)aTextview
{
  viewer = aTextview;
}

- (NSTextView *) viewer
{
  return viewer;
}

// Check if string contains a conversion factor.
- (BOOL) hasConversionTheString: (NSString *)data
{
  NSCharacterSet *dataSet = [NSCharacterSet characterSetWithCharactersInString:
					      data];

  if ([dataSet characterIsMember: '@'])
    {
      return YES;
    }
  else
    {
      return NO;
    }
}

// Check if string is a numerical data.
- (BOOL) isNumericDataTheString: (NSString *)data
{
  NSCharacterSet *dataSet = [NSCharacterSet characterSetWithCharactersInString:
					      data];

  if ([numbers isSupersetOfSet: dataSet] && ![data isEqualToString: @"E"])
    {
      return YES;
    }
  else
    {
      return NO;
    }
}

/* Convert the string which represents a scientific notation to a
   numerical data. */
- (NSString *) scientificNotationFor: (double)aNumber
{
  int count = 0;
  double newValue = aNumber;
  NSString *value = nil;

  if ( (newValue > 0) && (newValue < 1) )
    {
      while (newValue < 1)
	{
	  newValue = newValue*10;
	  count++;
	}

      value = [NSString stringWithFormat: @"%.3fE-%d", newValue, count];
    }
  else if ( (newValue > -1) && (newValue < 0) )
    {
      while (newValue > -1)
	{
	  newValue = newValue*10;
	  count++;
	}

      value = [NSString stringWithFormat: @"%.3fE-%d", newValue, count];
    }
  else if (newValue > 1)
    {
      while (newValue >= 10)
	{
	  newValue = newValue/10;
	  count++;
	}
      
      value = [NSString stringWithFormat: @"%.3fE%d", newValue, count];
    }
  else if (newValue < -1)
    {
      while (newValue <= -10)
	{
	  newValue = newValue/10;
	  count++;
	}
      
      value = [NSString stringWithFormat: @"%.3fE%d", newValue, count];
    }
  
  return value;
}

// Print, at viewer, the unknowns alongside the results.
- (void) printUnknowns: (NSArray *)unknowns withResults: (NSArray *)results
{
  [self printUnknowns: unknowns withResults: results withStatus: nil];
}

/* Print, at viewer, the unknowns alongside the results and the additional info
   (i.e: the state of a beam, tension or compression). */
- (void) printUnknowns: (NSArray *)unknowns withResults: (NSArray *)results
         withStatus: (NSArray *)status
{
  int k, length;
  double factor, answer;
  NSString *message, *unit, *st;
  NSArray *components;

  for (k = 0; k < [unknowns count]; k++)
    {
      components = [[unknowns objectAtIndex: k] componentsSeparatedByString:
						  @"@"];
      unit = [components objectAtIndex: 1];

      if (![unit isEqualToString: @"C"] &&
	  ![unit isEqualToString: @"R"] &&
	  ![unit isEqualToString: @"F"])
	{
	  if ([unit isEqualToString: _(@"degrees")])
	    {
	      factor = [[[conversions objectAtIndex: system]
			  objectForKey: @"degrees"] doubleValue];
	    }
	  else
	    {
	      factor = [[[conversions objectAtIndex: system] objectForKey:
				[components objectAtIndex: 1]] doubleValue];
	    }

	  answer = [[results objectAtIndex: k] doubleValue]/factor;
	}
      else
	{
	  if ([unit isEqualToString: @"C"])
	    {
	      answer = [[results objectAtIndex: k] doubleValue] - 273.15;
	    }
	  else
	    {
	      if ([unit isEqualToString: @"R"])
		{
		  answer = 1.8*([[results objectAtIndex: k] doubleValue]
				- 273.15) + 491.67;
		}
	      else
		{
		  answer = 1.8*([[results objectAtIndex: k] doubleValue]
				- 273.15) + 32;
		}
	    }
	}

      if (status == nil)
	{
	  st = @"";
	}
      else
	{
	  st = [status objectAtIndex: k];
	}

      if ([[components objectAtIndex: 0] hasSuffix: @"#E"])
	{
	  message = [NSString stringWithFormat: @" %@  =  %@  %@ %@;  ",
			      [components objectAtIndex: 0],
			      [self scientificNotationFor: answer],
			      [components objectAtIndex: 1],
			      st];
	}
      else
	{
	  message = [NSString stringWithFormat: @" %@  =  %.3f  %@ %@;  ",
			      [components objectAtIndex: 0],
			      answer,
			      [components objectAtIndex: 1],
			      st];
	}
      
      length = [[viewer textStorage] length];
      [viewer replaceCharactersInRange: NSMakeRange(length,0)
			    withString: message];
    }
}

@end
