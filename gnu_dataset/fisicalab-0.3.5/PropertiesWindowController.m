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
#import "PropertiesWindowController.h"

@interface PropertiesWindowController ( Private )

- (NSString *) stringForData: (NSDictionary *)data;

@end

@implementation PropertiesWindowController ( Private )

- (NSString *) stringForData: (NSDictionary *)data
{
  NSUInteger x;
  NSString *obj1, *obj2;
  NSString *line = @"";

  if (data != nil)
    {
      for (x = 0; x < [[data allKeys] count]; x += 2)
	{
	  obj1 = [[data allKeys] objectAtIndex: x];

	  if (x + 1 < [[data allKeys] count])
	    {
	      obj2 = [[data allKeys] objectAtIndex: x + 1];

	      line = [line stringByAppendingString:
			     [NSString stringWithFormat:
					 @"%@ = %@  %@ \t %@ = %@  %@\n",
				       obj1,
				[[data objectForKey: obj1] objectAtIndex: 1],
				[[data objectForKey: obj1] objectAtIndex: 0],
				       obj2,
				[[data objectForKey: obj2] objectAtIndex: 1],
				[[data objectForKey: obj2] objectAtIndex: 0]
			      ]];
	    }
	  else
	    {
	      line = [line stringByAppendingString:
			     [NSString stringWithFormat:
					 @"%@ = %@  %@\n",
				       obj1,
				[[data objectForKey: obj1] objectAtIndex: 1],
				[[data objectForKey: obj1] objectAtIndex: 0]
			      ]];
	    }
	}
    }
  else
    {
      line = [line stringByAppendingString:
		     _(@"The provided data is out of range. \n")];
    }

  line = [line stringByAppendingString: @"============================== \n\n"];

  return line;
}

@end

@implementation PropertiesWindowController

- (void) awakeFromNib
{
  TablesManager *tablesMg = [TablesManager sharedTablesManager];

  // The saturation table is the default.
  [dataB setEnabled: NO];
  [tabview selectTabViewItemAtIndex: 0];

  // By default don't use quality.
  [quality setEnabled: NO];
  [label setEnabled: NO];

  // Water is the default substance.
  [tablesMg selectTablesFor: @"water"];
  [typeA removeAllItems];

  // Add the available data and its units.
  [typeA addItemsWithTitles: [tablesMg availableDataForTable: @"pressure"]];
}

- (void) selectSubstance: (id)sender
{
}

- (void) selectTable: (id)sender
{
  if ([sender selectedRow] == 0)
    {
      [dataB setEnabled: NO];
      [checkBoxTwo setEnabled: YES];
      [tabview selectTabViewItemAtIndex: 0];

      if ([checkBoxTwo state] == NSOnState)
	{
	  [quality setEnabled: YES];
	  [label setEnabled: YES];
	}
    }
  else if ([sender selectedRow] == 1)
    {
      [dataB setEnabled: YES];
      [checkBoxTwo setEnabled: NO];
      [quality setEnabled: NO];
      [label setEnabled: NO];
      [tabview selectTabViewItemAtIndex: 1];
    }
}

- (void) getData: (id)sender
{
  NSUInteger length;
  NSDictionary *result = nil;

  if ([selectedTable selectedRow] == 0)
    {
      double data = [dataA doubleValue];
      NSString *type = [[[typeA stringValue] componentsSeparatedByString: @" "]
			 objectAtIndex: 0];

      if ([checkBoxTwo state] == NSOnState)
	{
	  double q = [quality doubleValue];

	  result = [[TablesManager sharedTablesManager]
		     saturationDataForValue: data
				     ofType: type
				withQuality: q];
	}
      else
	{
	  result = [[TablesManager sharedTablesManager]
		     saturationDataForValue: data
				     ofType: type];
	}
    }
  else if ([selectedTable selectedRow] == 1)
    {
      double data1, data2;
      data1 = [dataA doubleValue];
      data2 = [dataB doubleValue];

      result = [[TablesManager sharedTablesManager]
		     dataForPressure: data1
		      andTemperature: data2];
    }

  if ([checkBox state] == NSOnState)
    {
      [textView setString: [self stringForData: result]];
    }
  else
    {
      length = [[textView textStorage] length];
      [textView replaceCharactersInRange: NSMakeRange(length, 0)
			      withString: [self stringForData: result]];
    }
}

- (void) useQuality: (id)sender
{
  if ([sender state] == NSOnState)
    {
      [quality setEnabled: YES];
      [label setEnabled: YES];
    }
  else
    {
      [quality setEnabled: NO];
      [label setEnabled: NO];
    }
}

@end
