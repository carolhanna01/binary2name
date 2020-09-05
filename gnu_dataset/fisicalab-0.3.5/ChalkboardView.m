/* 
   Copyright (C) 2009, 2010, 2011, 2012, 2013, 2015 German A. Arias

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
#import "ObjectData.h"
#import "ChalkboardView.h"

@interface NSObject ( NSToolTipOwner )
- (NSString *)view:(NSView *)view stringForToolTip:(NSToolTipTag)tag
	     point:(NSPoint)point userData:(void *)userData;
@end

@interface ChalkboardView (Private)
- (BOOL) validation: (int)code;
- (void) postModuleNotification;
@end

@implementation ChalkboardView (Private)

/* Validate or not to add an element to the chalkboard. This prevents user to
   mix elements from different modules. */
- (BOOL) validation: (int)code
{
  BOOL decision;
  
  if (objects == -1)
    {
      objects = floor(code/50);
      decision = YES;
    }
  else
    {
      if (objects == floor(code/50))
	{
          decision = YES;
	} 
      else
	{
          decision = NO;
	}
    }

  return decision;
}

// Post a notification when the user change the module.
- (void) postModuleNotification
{

  [[NSNotificationCenter defaultCenter]
    postNotificationName: @"moduleDidChangeNotification"
    object: self
    userInfo: nil];
}
@end

@implementation ChalkboardView

- (id)init
{
  self = [super init];

  if (self)
    {
      selectedCell = nil;
    }

  return self;
}

- (void) dealloc
{
  [selectedCell release];
  [super dealloc];
}

-(void) drawRect: (NSRect) frame
{
  // Draw the black background.
  NSBezierPath *board = [NSBezierPath bezierPathWithRect: [self bounds]];
  [[NSColor blackColor] set];
  [board fill];

  // Highlight the selected cell, if any.
  if (selectedCell != nil)
    {
      [[NSColor yellowColor] set];
      [selectedCell setLineWidth: 2];
      [selectedCell stroke];
    }
}   

// Begins the process to add an element to the chalkboard.
- (void) addObject: (id)sender
{
  BOOL allow;
  moveObject = NO;
  image = nil;
  [[NSCursor arrowCursor] set];
  
  allow = [self validation: [sender tag]];
  
  if (allow)
    {  
      [[NSCursor pointingHandCursor] set];
      image = [sender image];
      objectCode = [sender tag];
      newObject = YES;
    }
}

// Begins the process to solve the problem.
- (void) calculate: (id)sender
{
  newObject = NO;
  moveObject = NO;
  image = nil;
  [[NSCursor arrowCursor] set];
  [information calculate: self];
}

// Returns the identifier of the current module.
- (int) code: (id)sender
{
  return objects;
}

// Removes all elements in the chalkboard.
- (void) clean: (id)sender
{
  int x, amount;
  newObject = NO;
  moveObject = NO;
  image = nil;
  [[NSCursor arrowCursor] set];
  amount = [[self subviews] count];
  
  for (x = 0; x <= amount - 1; x++)
    {
      [[[self subviews] objectAtIndex: x ] setImage: nil];
      [[[self subviews] objectAtIndex: x ] setTag: 0];
      [[[self subviews] objectAtIndex: x ] setToolTip: @""];
    } 
  
  DESTROY(selectedCell);
  [self setNeedsDisplay: YES];
  [information deleteAllObjects: YES];
  objects = -1;
  count = 0;
  [self postModuleNotification];
}

// Add/remove the element at clicked cell.
- (void)clickCell: (id)sender
{
  int key = [[[self window] currentEvent ] modifierFlags];
  
  // Add the new element at clicked cell.
  if ((newObject) && ([sender image] == nil))
    {
      [sender setImage: image];
      [[NSCursor arrowCursor] set];
      [sender setTag: objectCode];
      [information addObject: sender];
      count += 1;
      newObject = NO;
      image = nil;

      // If this is the first element added, set the corresponding menu.
      if (count == 1)
	{
	  [self postModuleNotification];
	}
    }
  // Add the moved element at clicked cell.
  else if ((moveObject) && ([sender image] == nil))
    {
      [sender setImage: image];
      [[NSCursor arrowCursor] set];
      [sender setTag: objectCode];
      moveObject = NO;
      image = nil;
    }
  // Begins a move operation for the element at clicked cell.
  else if ((key == NSControlKeyMask) && ([sender image] != nil)
	   && (image == nil))
    {
      image = [sender image];
      objectCode = [sender tag];
      [sender setImage: nil];
      [sender setTag: 0];
      [sender setToolTip: @""];
      [[NSCursor pointingHandCursor] set];
      moveObject = YES;
    }
  // Delete the element at clicked cell.
  else if ((key == NSShiftKeyMask) && ([sender image] != nil)
	   && (image == nil))
    {
      [sender setImage: nil];
      [information deleteObject: [sender tag]];
      [sender setTag: 0];
      [sender setToolTip: @""];
      count -= 1;

      if (count == 0)
	{
	  objects = -1;
	  [self postModuleNotification];
	}
      
      DESTROY(selectedCell);
      [self setNeedsDisplay: YES];
    }
  
  // Then update the table with the info of the element at clicked cell.
  if (!newObject && !moveObject && ([sender image] != nil))
    {
      [information selectObject: [sender tag]];
      ASSIGN(selectedCell, [NSBezierPath bezierPathWithRect: [sender frame]]);
      [self setNeedsDisplay: YES];
    }
}

// Terminates or cancel any add/move operation and reset the cursor.
- (void) controlCursor: (id) sender
{
  newObject = NO;
  moveObject = NO;
  image = nil;
  [[NSCursor arrowCursor] set];

  if (count == 0)	 
    {	 
      objects = -1;	 
    }
}

// Returns the chalkboard width (number of cells).
- (NSUInteger) chalkboardWidth
{
  return width;
}

// Returns the chalkboard height (number of cells).
- (NSUInteger) chalkboardHeight
{
  return height;
}

// Set the appropriate tooltip for the cell under the cursor.
- (NSString *)view:(NSView *)view stringForToolTip:(NSToolTipTag)tag
	     point:(NSPoint)point userData:(void *)userData
{
  if ([view tag] > 0)
    {
      NSNumber *num = [NSNumber numberWithInt: [view tag]];
      return [information dataOfObject: num];
    }
  else
    {
      return @"";
    }
}

- (void) awakeFromNib
{
  int x, y;
  NSButton *botonCell;
  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];

  // Set the user preferences for the width and height of the chalkboard.
  width = [defaults integerForKey: @"ChalkboardWidth"];
  height = [defaults integerForKey: @"ChalkboardHeight"];

  if ((width < 26) || (width > 100))
    {
      width = 26;
    }

  if ( (height < 18) || (height > 100))
    {
      height = 18;
    }
  
  // Add the cells for the chalkboard.
  for (y = 1; y <= height; y = y + 1)
    {
      for (x = 0; x < width; x = x + 1)
        {
	  botonCell = [[NSButton alloc] initWithFrame:
					  NSMakeRect(x*50, height*50 - y*50,
						     50, 50)];
	  [botonCell setTarget: self];
	  [botonCell setAction: @selector(clickCell:)];
	  [botonCell setTitle: @""];
	  [botonCell setImagePosition: NSImageOnly];
	  [botonCell setBordered: NO];
	  [botonCell addToolTipRect: NSMakeRect(0, 0, 50, 50)
			      owner: self
			   userData: nil];
	  [self addSubview: botonCell];
	  [botonCell release];
        }
    }

  // Adjust the size.
  [self setFrameSize: NSMakeSize(50*width, 50*height)];

  moveObject = NO;
  objects = -1;
  count = 0;
}

@end
