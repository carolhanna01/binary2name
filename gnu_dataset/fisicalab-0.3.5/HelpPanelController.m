/* 
   Copyright (C) 2009, 2010, 2011, 2013 German A. Arias

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

#import "HelpPanelController.h"

@implementation HelpPanelController

- (void) awakeFromNib
{
  [[table window] center];
  directory = [NSBundle mainBundle];

  /* If "introduction" file isn't found, show a message to encourage install
     the documentation package. This could happen if the packagers puts the
     documentation in a separate package and if the user don't
     have installed it.*/
  if (![text readRTFDFromFile:
	       [directory pathForResource: @"introduction" ofType: @"rtfd"]])
    {
      [text readRTFDFromFile:
	      [directory pathForResource: @"doc_warning" ofType: @"rtf"]];
    }
  
  [table selectRow: 0 byExtendingSelection: NO];
}

- (void) outlineViewSelectionDidChange: (NSNotification*)aNotification
{
  BOOL success = YES;
  NSString *resource = nil;
  id item = [[aNotification object] itemAtRow:
				      [[aNotification object] selectedRow]];
  
  if ([item isEqual: _(@"Introduction")])
    {
      resource = @"introduction";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Linear Kinematics of Particles")])
    {
      resource = @"kinematics";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Examples Linear Kinematics")])
    {
      resource = @"kinematicsExamples";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Kinematics of Circular Motion of Particles")])
    {
      resource = @"kinematicsCircular";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Examples Circular Motion Kinematics")])
    {
      resource = @"kinematicsCircularExamples";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Statics of Particles")])
    {
      resource = @"statics";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Examples Statics of Particles")])
    {
      resource = @"staticsExamples";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Statics of Rigid Bodies")])
    {
      resource = @"staticsRigid";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Examples Statics of Rigid Bodies")])
    {
      resource = @"staticsRigidExamples";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Linear Dynamics of Particles")])
    {
      resource = @"dynamics";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Examples Linear Dynamics")])
    {
      resource = @"dynamicsExamples";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Circular Dynamics of particles")])
    {
      resource = @"dynamicsCircular";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Examples Circular Dynamics")])
    {
      resource = @"dynamicsCircularExamples";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Dilatation, Ideal gas and Calorimetry")])
    {
      resource = @"calorimetry";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"Examples of Dilation, Ideal gas ...")])
    {
      resource = @"calorimetryExamples";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }
  else if ([item isEqual: _(@"GNU Free Documentation License")])
    {
      resource = @"COPYING";
      success = [text readRTFDFromFile:
			[directory pathForResource: resource ofType: @"rtfd"]];
    }

  if (!success)
    {
      [text readRTFDFromFile:
	      [directory pathForResource: @"doc_warning" ofType: @"rtf"]];
    }

  [text scrollPoint: NSMakePoint(0.0,0.0)];
}

- (id) outlineView: (NSOutlineView*)outlineView
	     child: (NSInteger)index
	    ofItem: (id)item
{
  if ([item isEqual: _(@"Kinematics")])
    {
      switch(index)
	{
	case 0:
	  return _(@"Linear Kinematics of Particles");
	case 1:
	  return _(@"Examples Linear Kinematics");
	case 2:
	  return _(@"Kinematics of Circular Motion of Particles");
	case 3:
	  return _(@"Examples Circular Motion Kinematics");
	default:
	  break;
	}
    }
  else if ([item isEqual: _(@"Statics")])
    {
      switch(index)
	{
	case 0:
	  return _(@"Statics of Particles");
	case 1:
	  return _(@"Examples Statics of Particles");
	case 2:
	  return _(@"Statics of Rigid Bodies");
	case 3:
	  return _(@"Examples Statics of Rigid Bodies");
	default:
	  break;
	}
    }
  else if ([item isEqual: _(@"Dynamics")])
    {
      switch(index)
	{
	case 0:
	  return _(@"Linear Dynamics of Particles");
	case 1:
	  return _(@"Examples Linear Dynamics");
	case 2:
	  return _(@"Circular Dynamics of particles");
	case 3:
	  return _(@"Examples Circular Dynamics");
	default:
	  break;
	}
    }
  else if ([item isEqual: _(@"Thermodynamics")])
    {
      switch(index)
	{
	case 0:
	  return _(@"Dilatation, Ideal gas and Calorimetry");
	case 1:
	  return _(@"Examples of Dilation, Ideal gas ...");
	default:
	  break;
	}
    }
  else if (item == nil && index == 0)
    {
      return _(@"Introduction");
    }
  else if (item == nil && index == 1)
    {
      return _(@"Kinematics");
    }
  else if (item == nil && index == 2)
    {
      return _(@"Statics");
    }
  else if (item == nil && index == 3)
    {
      return _(@"Dynamics");
    }
  else if (item == nil && index == 4)
    {
      return _(@"Thermodynamics");
    }
  else if (item == nil && index == 5)
    {
      return _(@"GNU Free Documentation License");
    }
  
  return nil;
}

- (BOOL) outlineView: (NSOutlineView*)outlineView isItemExpandable: (id)item
{
  if ([item isEqual: _(@"Kinematics")] ||
      [item isEqual: _(@"Statics")] ||
      [item isEqual: _(@"Dynamics")] ||
      [item isEqual: _(@"Thermodynamics")])
    {
      return YES;
    }
  
  return NO;
}

- (NSInteger) outlineView: (NSOutlineView*)outlineView
        numberOfChildrenOfItem: (id)item
{
  if (item == nil)
    {
      return 6;
    }
  else if ([item isEqual: _(@"Kinematics")])
    {
      return 4;
    }
  else if ([item isEqual: _(@"Statics")])
    {
      return 4;
    }
  else if ([item isEqual: _(@"Dynamics")])
    {
      return 4;
    }
  else if ([item isEqual: _(@"Thermodynamics")])
    {
      return 2;
    }
  
  return 0;
}

- (id) outlineView: (NSOutlineView*)outlineView
       objectValueForTableColumn: (NSTableColumn*)tableColumn
       byItem: (id)item
{
  return item;  
}

- (void) dealloc
{
  [directory release];
  [super dealloc];
}

@end
