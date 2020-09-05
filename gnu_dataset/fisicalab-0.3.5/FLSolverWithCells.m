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

#import "FLSolverWithCells.h"

@implementation FLSolverWithCells

- (id) init
{
  self = [super init];

  if (self)
    {
      cells = nil;
      chalkboardWidth = 0;
      chalkboardHeight = 0;
    }

  return self;
}

- (void) dealloc
{
  [cells release];
  [super dealloc];
}

// Accessor methods for cells (chalkboard).
- (void) setCells: (NSArray *)anArray
{
  // Retain the array with all the cells at chalkboard.
  ASSIGN(cells, anArray);
}

- (NSArray *) cells
{
  return cells;
}

// Set the width and height of the chalkboard, in cells.
- (void) withChalkboardWidth: (NSUInteger)aWidth height: (NSUInteger)anHeight
{
  chalkboardWidth = aWidth;
  chalkboardHeight = anHeight;
}

// Return the width of chalkboard, in cells.
- (NSUInteger) chalkboardWidth
{
  return chalkboardWidth;
}

// Return the height of chalkboard, in cells.
- (NSUInteger) chalkboardHeight
{
  return chalkboardHeight;
}

@end
