/* 
   Copyright (C) 2014, 2015 German A. Arias

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

#import <TablesManager.h>

static TablesManager *sharedManager = nil;

// Private methods.

@interface TablesManager ( Private )

- (NSString *) translationFor: (NSString *)aString;
- (NSString *) originalStringFor: (NSString *)aString;

@end

@implementation TablesManager ( Private )

/* This method is used to translate the items of popup menus at
 * properties window. We do this here, because in this way we can
 * maintain the translation in a localizable.strings file.
 */

- (NSString *) translationFor: (NSString *)aString
{
  if ([aString isEqualToString: @"p"])
    {
      return _(@"pressure");
    }
  else if ([aString isEqualToString: @"t"])
    {
      return _(@"temperature");
    }
  else if ([aString isEqualToString: @"density"])
    {
      return _(@"density");
    }
  else if ([aString isEqualToString: @"density-l"])
    {
      return _(@"density-l");
    }
  else if ([aString isEqualToString: @"density-v"])
    {
      return _(@"density-v");
    }
  else if ([aString isEqualToString: @"volume"])
    {
      return _(@"volume");
    }
  else if ([aString isEqualToString: @"volume-l"])
    {
      return _(@"volume-l");
    }
  else if ([aString isEqualToString: @"volume-v"])
    {
      return _(@"volume-v");
    }
  else if ([aString isEqualToString: @"enthalpy"])
    {
      return _(@"enthalpy");
    }
  else if ([aString isEqualToString: @"enthalpy-l"])
    {
      return _(@"enthalpy-l");
    }
  else if ([aString isEqualToString: @"enthalpy-v"])
    {
      return _(@"enthalpy-v");
    }
  else if ([aString isEqualToString: @"enthalpy-delta"])
    {
      return _(@"enthalpy-delta");
    }
  else if ([aString isEqualToString: @"entropy"])
    {
      return _(@"entropy");
    }
  else if ([aString isEqualToString: @"entropy-l"])
    {
      return _(@"entropy-l");
    }
  else if ([aString isEqualToString: @"entropy-v"])
    {
      return _(@"entropy-v");
    }
  else if ([aString isEqualToString: @"entropy-delta"])
    {
      return _(@"entropy-delta");
    }

  return nil;
}

/* This method is used to get the original string. So we can use it
 * to search in dictionaries.
 */

- (NSString *) originalStringFor: (NSString *)aString
{
  if ([aString isEqualToString: _(@"pressure")])
    {
      return @"p";
    }
  else if ([aString isEqualToString: _(@"temperature")])
    {
      return @"t";
    }
  else if ([aString isEqualToString: _(@"density")])
    {
      return @"density";
    }
  else if ([aString isEqualToString: _(@"density-l")])
    {
      return @"density-l";
    }
  else if ([aString isEqualToString: _(@"density-v")])
    {
      return @"density-v";
    }
  else if ([aString isEqualToString: _(@"volume")])
    {
      return @"volume";
    }
  else if ([aString isEqualToString: _(@"volume-l")])
    {
      return @"volume-l";
    }
  else if ([aString isEqualToString: _(@"volume-v")])
    {
      return @"volume-v";
    }
  else if ([aString isEqualToString: _(@"enthalpy")])
    {
      return @"enthalpy";
    }
  else if ([aString isEqualToString: _(@"enthalpy-l")])
    {
      return @"enthalpy-l";
    }
  else if ([aString isEqualToString: _(@"enthalpy-v")])
    {
      return @"enthalpy-v";
    }
  else if ([aString isEqualToString: _(@"enthalpy-delta")])
    {
      return @"enthalpy-delta";
    }
  else if ([aString isEqualToString: _(@"entropy")])
    {
      return @"entropy";
    }
  else if ([aString isEqualToString: _(@"entropy-l")])
    {
      return @"entropy-l";
    }
  else if ([aString isEqualToString: _(@"entropy-v")])
    {
      return @"entropy-v";
    }
  else if ([aString isEqualToString: _(@"entropy-delta")])
    {
      return @"entropy-delta";
    }

  return nil;
}

@end

@implementation TablesManager

// Creation of shared manager.

+ (TablesManager *) sharedTablesManager
{
  if (sharedManager == nil)
    {
      sharedManager = [TablesManager new];
    }

  return sharedManager;
}

- (id) init
{
  self = [super init];

  selectedSubstance = nil;
  substances = [NSMutableDictionary new];

  return self;
}

- (void) dealloc
{
  [substances release];
  [super dealloc];
}

/* Selection of a table. If this is not currently at dictionary, load
   the corresponding data. */

- (void) selectTablesFor: (NSString *)substance
{
  if ([substances objectForKey: substance] == nil)
    {
      NSString *file;
      NSString *dir = [@"Tables/" stringByAppendingString: substance];
      NSBundle *bundle = [NSBundle mainBundle];
      NSMutableDictionary *newTables = [NSMutableDictionary dictionary];

      // Load the files of the substance.
      file = [bundle pathForResource: @"saturation-temperature"
			      ofType: @"plist"
			 inDirectory: dir];

      if (file != nil)
	{
	  NSDictionary *temperature;
	  temperature = [NSDictionary dictionaryWithContentsOfFile: file];
	  [newTables setObject: temperature forKey: @"temperature"];
	}

      file = [bundle pathForResource: @"saturation-pressure"
			      ofType: @"plist"
			 inDirectory: dir];

      if (file != nil)
	{
	  NSDictionary *pressure;
	  pressure = [NSDictionary dictionaryWithContentsOfFile: file];
	  [newTables setObject: pressure forKey: @"pressure"];
	}

      file = [bundle pathForResource: @"superheated"
			      ofType: @"plist"
			 inDirectory: dir];

      if (file != nil)
	{
	  NSDictionary *compressedAndSuperheated;
	  compressedAndSuperheated = [NSDictionary dictionaryWithContentsOfFile:
						     file];
	  [newTables setObject: compressedAndSuperheated
			forKey: @"compressedAndSuperheated"];
	}

      [substances setObject: newTables forKey: substance];
    }

  selectedSubstance = substance;
}

// Return the available data and its units.
- (NSArray *) availableDataForTable: (NSString *)table
{
  NSString *obj, *item;
  NSArray *values;
  NSDictionary *tableData;
  NSMutableArray *data = [NSMutableArray array];
  NSEnumerator *enumerator;

  tableData = [[substances objectForKey: selectedSubstance]
		objectForKey: table];

  enumerator = [[tableData allKeys] objectEnumerator];

  while ((obj = [enumerator nextObject]))
    {
      values = [tableData objectForKey: obj];

      item = [NSString stringWithFormat: @"%@ (%@)",
		[self translationFor: obj],
		[values objectAtIndex: 0]];

      [data addObject: item]; 
    }

  return [NSArray arrayWithArray: data];
}

// Get data for an specific saturation state.

- (NSDictionary *) saturationDataForPressure: (double)pressure
{
  NSDictionary *data = [self saturationDataForValue: pressure
					     ofType: @"p"];

  return data;
}

- (NSDictionary *) saturationDataForTemperature: (double)temperature
{
  NSDictionary *data = [self saturationDataForValue: temperature
					     ofType: @"t"];

  return data;
}

- (NSDictionary *) saturationDataForValue: (double)value
				   ofType: (NSString *)type
{
  BOOL contained = NO;
  NSUInteger x;
  NSMutableDictionary *data;
  NSDictionary *dic = nil;

  // Get the original string.
  type = [self originalStringFor: type];

  /* If type is temperature use the corresponding table, otherwise use the
     pressure table. */
  if ([type isEqualToString: @"t"])
    {
      dic = [[substances objectForKey: selectedSubstance]
	      objectForKey: @"temperature"];
    }
  else
    {
      dic = [[substances objectForKey: selectedSubstance]
	      objectForKey: @"pressure"];
    }

  // If no table available return nil.
  if (dic == nil)
    {
      return nil;
    }

  // If no data for the property, return nil.
  if ([dic objectForKey: type] == nil)
    {
      return nil;
    }

  // Search if the value is contained.
  for (x = 1; x < [[dic objectForKey: type] count]; x++)
    {
      if ([[[dic objectForKey: type] objectAtIndex: x] doubleValue] == value)
	{
	  contained = YES;
	  break;
	}
    }

  data = [NSMutableDictionary dictionary];

  // If the table contains the exact value, return the corresponding row.
  if (contained)
    {
      NSString *obj;
      NSArray *column;
      NSEnumerator *enumerator = [[dic allKeys] objectEnumerator];
 
       while ((obj = [enumerator nextObject]))
	{
	  column = [dic objectForKey: obj];

	  [data setObject: [NSArray arrayWithObjects:
				      [column objectAtIndex: 0],
			              [column objectAtIndex: x], nil]
		   forKey: [self translationFor: obj]];
	}
    }
  // Otherwise interpolate the data.
  else
    {
      BOOL error = NO, increasing = NO;
      double ratio = 0, intplValue;
      NSUInteger index = 0;
      NSString *obj;
      NSArray *column;
      NSEnumerator *enumerator;

      column = [dic objectForKey: type];

      // Check if the column is increasing or decreasing.
      if ([[column objectAtIndex: 1] doubleValue] <
	  [[column lastObject] doubleValue])
	{
	  increasing = YES;
	}

      // Check if the value is not out of the table.
      if (increasing)
	{
	  if ([[column objectAtIndex: 1] doubleValue] > value)
	    {
	      error = YES;
	    }
	  else if ([[column lastObject] doubleValue] < value)
	    {
	      error = YES;
	    }
	}
      else
	{
	  if ([[column objectAtIndex: 1] doubleValue] < value)
	    {
	      error = YES;
	    }
	  else if ([[column lastObject] doubleValue] > value)
	    {
	      error = YES;
	    }
	}

      if (error)
	{
	  return nil;
	}

      // If all OK, then procede to interpolate.
      enumerator = [column objectEnumerator];
      // Jump the cell of units.
      obj = [enumerator nextObject];

      // Find one of the nearest cell in the column.
      while ((obj = [enumerator nextObject]))
	{
	  if (increasing)
	    {
	      if (value < [obj doubleValue])
		{
		  index = [column indexOfObject: obj];
		  ratio = (value - [[column objectAtIndex: index - 1]
				     doubleValue])/
		    ([obj doubleValue] -
		     [[column objectAtIndex: index - 1] doubleValue]);
		  break;
		}
	    }
	  else
	    {
	      if (value > [obj doubleValue])
		{
		  index = [column indexOfObject: obj];
		  ratio = (value - [[column objectAtIndex: index - 1]
				     doubleValue])/
		    ([obj doubleValue] -
		     [[column objectAtIndex: index - 1] doubleValue]);
		  break;
		}
	    }
	}

      enumerator = [[dic allKeys] objectEnumerator];

      while ((obj = [enumerator nextObject]))
	{
	  column = [dic objectForKey: obj];

	  intplValue = [[column objectAtIndex: index - 1] doubleValue] +
	    ratio*([[column objectAtIndex: index] doubleValue] -
		   [[column objectAtIndex: index - 1] doubleValue]);

	  [data setObject: [NSArray arrayWithObjects:
			      [column objectAtIndex: 0],
			      [NSString stringWithFormat: @"%f", intplValue],
			      nil]
		   forKey: [self translationFor: obj]];
	}
    }

  return [NSDictionary dictionaryWithDictionary: data];
}

- (NSDictionary *) saturationDataForPressure: (double)pressure
                                 withQuality: (double)quality
{
  return [self saturationDataForValue: pressure
			       ofType: @"p"
			  withQuality: quality];
}

- (NSDictionary *) saturationDataForTemperature: (double)temperature
                                    withQuality: (double)quality
{
  return [self saturationDataForValue: temperature
			       ofType: @"t"
			  withQuality: quality];
}

- (NSDictionary *) saturationDataForValue: (double)value
                                   ofType: (NSString *)type
                              withQuality: (double)quality
{
  double newValue = 0;
  NSString *st, *units;
  NSMutableDictionary *dic = [NSMutableDictionary dictionaryWithDictionary:
				  [self saturationDataForValue: value
						        ofType: type]];

  /* In all this we assume that different values of the same properties
   * have the same units. This is, of course, what we expect.
   */

  quality /= 100;

  // Density.
  if ([dic objectForKey: _(@"density-l")] != nil &&
      [dic objectForKey: _(@"density-delta")] != nil)
    {
      double denL, denDel;

      denL = [[[dic objectForKey: _(@"density-l")] objectAtIndex: 1]
	       doubleValue];
      denDel = [[[dic objectForKey: _(@"density-delta")] objectAtIndex: 1] 
		 doubleValue];
      units = [[dic objectForKey: _(@"density-l")] objectAtIndex: 0];

      newValue = denL + quality*denDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"density-v")] != nil &&
	   [dic objectForKey: _(@"density-delta")] != nil)
    {
      double denV, denDel;

      denV = [[[dic objectForKey: _(@"density-v")] objectAtIndex: 1]
	       doubleValue];
      denDel = [[[dic objectForKey: _(@"density-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"density-v")] objectAtIndex: 0];

      newValue = denV - (1 - quality)*denDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"density-l")] != nil &&
	   [dic objectForKey: _(@"density-v")] != nil)
    {
      double denL, denV;

      denL = [[[dic objectForKey: _(@"density-l")] objectAtIndex: 1]
	       doubleValue];
      denV = [[[dic objectForKey: _(@"density-v")] objectAtIndex: 1]
	       doubleValue];
      units = [[dic objectForKey: _(@"density-l")] objectAtIndex: 0];

      newValue = quality*denV + (1 - quality)*denL;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else
    {
      units =  @"";
      st = _(@"No available");
    }

  [dic setObject: [NSArray arrayWithObjects: units, st, nil]
	  forKey: _(@"density")];

  if ([dic objectForKey: _(@"density-l")] != nil)
    {
      [dic removeObjectForKey: _(@"density-l")];
    }

  if ([dic objectForKey: _(@"density-v")] != nil)
    {
      [dic removeObjectForKey: _(@"density-v")];
    }

  if ([dic objectForKey: _(@"density-delta")] != nil)
    {
      [dic removeObjectForKey: _(@"density-delta")];
    }


  // Specific volume.
  if ([dic objectForKey: _(@"volume-l")] != nil &&
      [dic objectForKey: _(@"volume-delta")] != nil)
    {
      double volL, volDel;

      volL = [[[dic objectForKey: _(@"volume-l")] objectAtIndex: 1]
	       doubleValue];
      volDel = [[[dic objectForKey: _(@"volume-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"volume-l")] objectAtIndex: 0];

      newValue = volL + quality*volDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"volume-v")] != nil &&
	   [dic objectForKey: _(@"volume-delta")] != nil)
    {
      double volV, volDel;

      volV = [[[dic objectForKey: _(@"volume-v")] objectAtIndex: 1]
	       doubleValue];
      volDel = [[[dic objectForKey: _(@"volume-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"volume-v")] objectAtIndex: 0];

      newValue = volV - (1 - quality)*volDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"volume-l")] != nil &&
	   [dic objectForKey: _(@"volume-v")] != nil)
    {
      double volL, volV;

      volL = [[[dic objectForKey: _(@"volume-l")] objectAtIndex: 1]
	       doubleValue];
      volV = [[[dic objectForKey: _(@"volume-v")] objectAtIndex: 1]
	       doubleValue];
      units = [[dic objectForKey: _(@"volume-l")] objectAtIndex: 0];

      newValue = quality*volV + (1 - quality)*volL;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else
    {
      units = @"";
      st = _(@"No available");
    }

  [dic setObject: [NSArray arrayWithObjects: units, st, nil]
	  forKey: _(@"volume")];

  if ([dic objectForKey: _(@"volume-l")] != nil)
    {
      [dic removeObjectForKey: _(@"volume-l")];
    }

  if ([dic objectForKey: _(@"volume-v")] != nil)
    {
      [dic removeObjectForKey: _(@"volume-v")];
    }

  if ([dic objectForKey: _(@"volume-delta")] != nil)
    {
      [dic removeObjectForKey: _(@"volume-delta")];
    }


  // Enthalpy.
  if ([dic objectForKey: _(@"enthalpy-l")] != nil &&
      [dic objectForKey: _(@"enthalpy-delta")] != nil)
    {
      double entL, entDel;

      entL = [[[dic objectForKey: _(@"enthalpy-l")] objectAtIndex: 1]
	       doubleValue];
      entDel = [[[dic objectForKey: _(@"enthalpy-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"enthalpy-l")] objectAtIndex: 0];

      newValue = entL + quality*entDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"enthalpy-v")] != nil &&
	   [dic objectForKey: _(@"enthalpy-delta")] != nil)
    {
      double entV, entDel;

      entV = [[[dic objectForKey: _(@"enthalpy-v")] objectAtIndex: 1]
	       doubleValue];
      entDel = [[[dic objectForKey: _(@"enthalpy-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"enthalpy-v")] objectAtIndex: 0];

      newValue = entV - (1 - quality)*entDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"enthalpy-l")] != nil &&
	   [dic objectForKey: _(@"enthalpy-v")] != nil)
    {
      double entL, entV;

      entL = [[[dic objectForKey: _(@"enthalpy-l")] objectAtIndex: 1]
	       doubleValue];
      entV = [[[dic objectForKey: _(@"enthalpy-v")] objectAtIndex: 1]
	       doubleValue];
      units = [[dic objectForKey: _(@"enthalpy-l")] objectAtIndex: 0];

      newValue = quality*entV + (1 - quality)*entL;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else
    {
      units = @"";
      st = _(@"No available");
    }

  [dic setObject: [NSArray arrayWithObjects: units, st, nil]
	  forKey: _(@"enthalpy")];

  if ([dic objectForKey: _(@"enthalpy-l")] != nil)
    {
      [dic removeObjectForKey: _(@"enthalpy-l")];
    }

  if ([dic objectForKey: _(@"enthalpy-v")] != nil)
    {
      [dic removeObjectForKey: _(@"enthalpy-v")];
    }

  if ([dic objectForKey: _(@"enthalpy-delta")] != nil)
    {
      [dic removeObjectForKey: _(@"enthalpy-delta")];
    }


  // Entropy.
  if ([dic objectForKey: _(@"entropy-l")] != nil &&
      [dic objectForKey: _(@"entropy-delta")] != nil)
    {
      double entL, entDel;

      entL = [[[dic objectForKey: _(@"entropy-l")] objectAtIndex: 1]
	       doubleValue];
      entDel = [[[dic objectForKey: _(@"entropy-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"entropy-l")] objectAtIndex: 0];

      newValue = entL + quality*entDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"entropy-v")] != nil &&
	   [dic objectForKey: _(@"entropy-delta")] != nil)
    {
      double entV, entDel;

      entV = [[[dic objectForKey: _(@"entropy-v")] objectAtIndex: 1]
	       doubleValue];
      entDel = [[[dic objectForKey: _(@"entropy-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"entropy-v")] objectAtIndex: 0];

      newValue = entV - (1 - quality)*entDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"entropy-l")] != nil &&
	   [dic objectForKey: _(@"entropy-v")] != nil)
    {
      double entL, entV;

      entL = [[[dic objectForKey: _(@"entropy-l")] objectAtIndex: 1]
	       doubleValue];
      entV = [[[dic objectForKey: _(@"entropy-v")] objectAtIndex: 1]
	       doubleValue];
      units = [[dic objectForKey: _(@"entropy-l")] objectAtIndex: 0];

      newValue = quality*entV + (1 - quality)*entL;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else
    {
      units = @"";
      st = _(@"No available");
    }

  [dic setObject: [NSArray arrayWithObjects: units, st, nil]
	  forKey: _(@"entropy")];

  if ([dic objectForKey: _(@"entropy-l")] != nil)
    {
      [dic removeObjectForKey: _(@"entropy-l")];
    }

  if ([dic objectForKey: _(@"entropy-v")] != nil)
    {
      [dic removeObjectForKey: _(@"entropy-v")];
    }

  if ([dic objectForKey: _(@"entropy-delta")] != nil)
    {
      [dic removeObjectForKey: _(@"entropy-delta")];
    }


  // Internal energy.
  if ([dic objectForKey: _(@"energy-l")] != nil &&
      [dic objectForKey: _(@"energy-delta")] != nil)
    {
      double engL, engDel;

      engL = [[[dic objectForKey: _(@"energy-l")] objectAtIndex: 1]
	       doubleValue];
      engDel = [[[dic objectForKey: _(@"energy-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"energy-l")] objectAtIndex: 0];

      newValue = engL + quality*engDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"energy-v")] != nil &&
	   [dic objectForKey: _(@"energy-delta")] != nil)
    {
      double engV, engDel;

      engV = [[[dic objectForKey: _(@"energy-v")] objectAtIndex: 1]
	       doubleValue];
      engDel = [[[dic objectForKey: _(@"energy-delta")] objectAtIndex: 1]
		 doubleValue];
      units = [[dic objectForKey: _(@"energy-v")] objectAtIndex: 0];

      newValue = engV - (1 - quality)*engDel;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else if ([dic objectForKey: _(@"energy-l")] != nil &&
	   [dic objectForKey: _(@"energy-v")] != nil)
    {
      double engL, engV;

      engL = [[[dic objectForKey: _(@"energy-l")] objectAtIndex: 1]
	       doubleValue];
      engV = [[[dic objectForKey: _(@"energy-v")] objectAtIndex: 1]
	       doubleValue];
      units = [[dic objectForKey: _(@"energy-l")] objectAtIndex: 0];

      newValue = quality*engV + (1 - quality)*engL;
      st = [NSString stringWithFormat: @"%f", newValue];
    }
  else
    {
      units = @"";
      st = _(@"No available");
    }

  [dic setObject: [NSArray arrayWithObjects: units, st, nil]
	  forKey: _(@"energy")];

  if ([dic objectForKey: _(@"energy-l")] != nil)
    {
      [dic removeObjectForKey: _(@"energy-l")];
    }

  if ([dic objectForKey: _(@"energy-v")] != nil)
    {
      [dic removeObjectForKey: _(@"energy-v")];
    }

  if ([dic objectForKey: _(@"energy-delta")] != nil)
    {
      [dic removeObjectForKey: _(@"energy-delta")];
    }

  return [NSDictionary dictionaryWithDictionary: dic];
}

// Get data for compressed or superheated state.

- (NSDictionary *) dataForPressure: (double)pressure
		    andTemperature: (double)temp
{
  BOOL contained = NO;
  double ratio = 0, intplValue;
  NSUInteger x, index = 0;
  NSArray *p = nil, *t = nil;
  NSDictionary *pTable = nil;
  NSMutableDictionary *data = [NSMutableDictionary dictionary];
  NSEnumerator *enumerator = nil;
  NSDictionary *dic =  [[substances objectForKey: selectedSubstance]
			 objectForKey: @"compressedAndSuperheated"];

  // If no table available return nil;
  if (dic == nil)
    {
      return nil;
    }

  p = [dic objectForKey: @"p"];


  // Check if the pressure is not out of the table.
  if ([[p objectAtIndex: 1] doubleValue] > pressure ||
      [[p lastObject] doubleValue] < pressure)
    {
      return nil;
    }

  // If all OK, search if tables contain the exact value of pressure.
  for (x = 1; x < [p count]; x++)
    {
      if ([[p objectAtIndex: x] doubleValue] == pressure)
	{
	  contained = YES;
	  /* Here we use 'x - 1' because the pressure array contain an
	   * additional cell (the units).
	   */
	  pTable = [[dic objectForKey: @"tables"] objectAtIndex: x - 1];
	  break;
	}
    }

  // If the exact value of pressure is not contained, interpolate.
  if (!contained)
    {
      NSString *obj;
      NSArray *column1, *column2;
      NSDictionary *p1Table = nil, *p2Table = nil;
      NSMutableDictionary *newTable = [NSMutableDictionary dictionary];

      enumerator = [p objectEnumerator];
      // Jump the cell of units.
      obj = [enumerator nextObject];

      // Find one of the nearest cell in the column.
      while ((obj = [enumerator nextObject]))
	{
	  if (pressure < [obj doubleValue])
	    {
	      index = [p indexOfObject: obj];
	      ratio = (pressure - [[p objectAtIndex: index - 1]
				    doubleValue])/
		([obj doubleValue] -
		 [[p objectAtIndex: index - 1] doubleValue]);

	      /* Get the tables for the interpolation.
	       * Here we use 'index - 1' and 'index - 2' because the
	       * pressure array contain an additional cell (the units).
	       */
	      p1Table = [[dic objectForKey: @"tables"] objectAtIndex:
							 index - 1];
	      p2Table = [[dic objectForKey: @"tables"] objectAtIndex:
							 index - 2];
	      break;
	    }
	}

      // Interpolate the data for the new table.
      enumerator = [[p1Table allKeys] objectEnumerator];

      while ((obj = [enumerator nextObject]))
	{
	  NSMutableArray *newColumn = [NSMutableArray array];
	  column1 = [p1Table objectForKey: obj];
	  column2 = [p2Table objectForKey: obj];

	  // Add units to new column.
	  [newColumn addObject: [column1 objectAtIndex: 0]];

	  for (x = 1; x < [column1 count]; x++)
	    {
	      intplValue = [[column2 objectAtIndex: x] doubleValue] +
		ratio*([[column1 objectAtIndex: x] doubleValue] -
		       [[column2 objectAtIndex: x] doubleValue]);

	      [newColumn addObject: [NSString stringWithFormat: @"%f",
					      intplValue]];
	    }

	  [newTable setObject: newColumn forKey: obj];
	}

      pTable = [NSDictionary dictionaryWithDictionary: newTable];
    }

  // Search if pressure table contain the exact value of temperature.
  contained = NO;
  index = 0;

  // Check if the temperature is not out of the table.
  t = [pTable objectForKey: @"t"];

  if ([[t objectAtIndex: 1] doubleValue] > temp ||
      [[t lastObject] doubleValue] < temp)
    {
      return nil;
    }

  for (x = 1; x < [[pTable objectForKey: @"t"] count]; x++)
    {
      if ([[[pTable objectForKey: @"t"] objectAtIndex: x] doubleValue] ==
	  temp)
	{
	  contained = YES;
	  index = x;
	  break;
	}
    }

  if (contained)
    {
      NSString *obj;
      NSArray *column;
      enumerator = [[pTable allKeys] objectEnumerator];

      while ((obj = [enumerator nextObject]))
	{
	  column = [pTable objectForKey: obj];
	  [data setObject: [NSArray arrayWithObjects:
				      [column objectAtIndex: 0],
			              [column objectAtIndex: index],
				      nil]
		   forKey: obj];
	}
    }
  else
    {
      NSString *obj;
      NSArray *t, *column;

      t = [pTable objectForKey: @"t"];
      enumerator = [t objectEnumerator];

      // Jump the cell of units.
      obj = [enumerator nextObject];

      // Find one of the nearest cell in the column.
      while ((obj = [enumerator nextObject]))
	{
	  if (temp < [obj doubleValue])
	    {
	      index = [t indexOfObject: obj];
	      ratio = (temp - [[t objectAtIndex: index - 1]
				doubleValue])/
		([obj doubleValue] -
		 [[t objectAtIndex: index - 1] doubleValue]);

	      break;
	    }
	}

      // Interpolate the data.
      enumerator = [[pTable allKeys] objectEnumerator];

      while ((obj = [enumerator nextObject]))
	{
	  column = [pTable objectForKey: obj];

	  // Add the data for temperature.
	  if ([obj isEqualToString: @"t"])
	    {
	      [data setObject: [NSArray arrayWithObjects:
					[column objectAtIndex: 0],
					[NSString stringWithFormat:
						    @"%f", temp],
					nil]
		   forKey: obj];
	      continue;
	    }

	  // Calculate the new value.
	  intplValue = [[column objectAtIndex: index - 1] doubleValue] +
	    ratio*([[column objectAtIndex: index] doubleValue] -
		   [[column objectAtIndex: index - 1] doubleValue]);

	  [data setObject: [NSArray arrayWithObjects:
				      [column objectAtIndex: 0],
				      [NSString stringWithFormat:
						  @"%f", intplValue],
				    nil]
		   forKey: obj];
	}
    }

  return [NSDictionary dictionaryWithDictionary: data];
}

- (NSDictionary *) dataForPressure: (double)pressure
			  andValue: (double)value ofType: (NSString *)type
{
  NSMutableDictionary *data = nil;

  return [NSDictionary dictionaryWithDictionary: data];
}

- (NSDictionary *) dataForTemperature: (double)temp
			     andValue: (double)value ofType: (NSString *)type
{
  NSMutableDictionary *data = nil;

  return [NSDictionary dictionaryWithDictionary: data];
}

- (NSDictionary *) dataForValue: (double)valueA ofType: (NSString *)typeA
		       andValue: (double)valueB ofType: (NSString *)typeB
{
  NSMutableDictionary *data = nil;

  return [NSDictionary dictionaryWithDictionary: data];
}

@end
