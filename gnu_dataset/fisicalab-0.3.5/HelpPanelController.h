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

#import <AppKit/AppKit.h>

@interface HelpPanelController : NSObject
{
  // Outlets
  id text;
  id table;

  // Instance variables
  NSBundle *directory;
}

// Delegate and source data methods
- (void) outlineViewSelectionDidChange: (NSNotification*)aNotification;

- (id) outlineView: (NSOutlineView*)outlineView child: (NSInteger)index
            ofItem: (id)item;

- (BOOL) outlineView: (NSOutlineView*)outlineView isItemExpandable: (id)item;

- (NSInteger) outlineView: (NSOutlineView*)outlineView
        numberOfChildrenOfItem: (id)item;

- (id) outlineView: (NSOutlineView*)outlineView
       objectValueForTableColumn: (NSTableColumn*)tableColumn
       byItem: (id)item;
@end
