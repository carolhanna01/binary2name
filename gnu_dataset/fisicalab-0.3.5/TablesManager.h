/* 
   Copyright (C) 2014 German A. Arias

   This file is part of FísicaLab application

   This application is free software; you can redistribute it and/or
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
 
#import <Foundation/Foundation.h>

@interface TablesManager : NSObject
{
  NSString *selectedSubstance;
  NSMutableDictionary *substances;
}

+ (TablesManager *) sharedTablesManager;

- (void) selectTablesFor: (NSString *)substance;

- (NSArray *) availableDataForTable: (NSString *)table;

- (NSDictionary *) saturationDataForPressure: (double)pressure;

- (NSDictionary *) saturationDataForTemperature: (double)temperature;

- (NSDictionary *) saturationDataForValue: (double)value
                                   ofType: (NSString *)type;

- (NSDictionary *) saturationDataForPressure: (double)pressure
                                 withQuality: (double)quality;

- (NSDictionary *) saturationDataForTemperature: (double)temperature
                                    withQuality: (double)quality;

- (NSDictionary *) saturationDataForValue: (double)value
                                   ofType: (NSString *)type
                              withQuality: (double)quality;

- (NSDictionary *) dataForPressure: (double)pressure
                    andTemperature: (double)temp;

- (NSDictionary *) dataForPressure: (double)pressure
                          andValue: (double)value ofType: (NSString *)type;

- (NSDictionary *) dataForTemperature: (double)temp
                             andValue: (double)value ofType: (NSString *)type;

- (NSDictionary *) dataForValue: (double)valueA ofType: (NSString *)typeA
                       andValue: (double)valueB ofType: (NSString *)typeB;

@end
