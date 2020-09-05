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

#import <AppKit/AppKit.h>

@interface ObjectData : NSObject
{
  // Outlets
  id dataViewer;
  id resultsViewer;
  id cleanResultsViewer;
  id chalkboard;

  // Instance variables
  int referenceCount, system;
  NSNumber *numberId;
  NSCharacterSet *numbers, *plus_minusSet, *multiplication_divisionSet;
  NSMutableDictionary *objectsList, *conversionDictionary,
    *menuConversionDictionary;
}
- (void) unitsSystem: (id)sender;
- (void) calculate: (id)sender;
- (void) addObject: (id)new;
- (void) deleteObject: (int)code;
- (void) selectObject: (int)code;
- (void) deleteAllObjects: (BOOL)value;
- (NSString *) dataOfObject: (NSNumber *)aNumber;
- (void) setMenuForModule: (NSNotification *)notification;
- (void) insertFactor: (id)sender;

// Delegate and data source methods.
- (NSInteger) numberOfRowsInTableView: (NSTableView*)aTableView;

- (id) tableView: (NSTableView*)aTableView
       objectValueForTableColumn: (NSTableColumn*)aTableColumn
       row: (NSInteger)rowIndex;

- (void) tableView: (NSTableView*)aTableView setObjectValue: (id)anObject
    forTableColumn: (NSTableColumn*)aTableColumn row: (NSInteger)rowIndex;

- (BOOL) tableView: (NSTableView*)aTableView
         shouldEditTableColumn: (NSTableColumn*)aTableColumn
         row: (NSInteger)rowIndex;

- (void) tableView: (NSTableView*)aTableView willDisplayCell: (id)aCell
    forTableColumn: (NSTableColumn*)aTableColumn row: (NSInteger)rowIndex;

// Delegate for contextual menu.
- (void) menuNeedsUpdate: (NSMenu*)menu;
@end
