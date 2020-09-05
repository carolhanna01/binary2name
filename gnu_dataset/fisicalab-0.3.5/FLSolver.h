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

#import <AppKit/AppKit.h>

@interface FLSolver : NSObject
{
  // Instance variables
  NSUInteger system;
  NSArray *conversions;
  NSTextView *viewer;

  //Retained
  NSCharacterSet *numbers;
}
- (void) setSystem: (NSUInteger)aNumber;
- (void) setConversions: (NSArray *)anArray;
- (void) setViewer: (NSTextView *)aTextview;
- (NSUInteger) system;
- (NSDictionary *) conversions;
- (NSTextView *) viewer;
- (BOOL) hasConversionTheString: (NSString *)data;
- (BOOL) isNumericDataTheString: (NSString *)data;
- (NSString *) scientificNotationFor: (double)aNumber;
- (void) printUnknowns: (NSArray *)unknowns withResults: (NSArray *)results;
- (void) printUnknowns: (NSArray *)unknowns withResults: (NSArray *)results
            withStatus: (NSArray *)status;
@end
