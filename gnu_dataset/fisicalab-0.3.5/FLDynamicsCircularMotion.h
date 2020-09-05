/* 
   Copyright (C) 2012, 2013, 2014 German A. Arias

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

@interface FLDynamicsCircularMotion : FLSolverWithCells
{
  NSUInteger sys;
  NSArray *errors;
  NSMutableArray *objectsSystem, *objectSystemGenericType;

  // For internal calculations
  @public

  NSInteger varG, varT;
  double gravityDat, timeDat;
  NSMutableArray *unknowns, *objectsLaw, *objectLawType, *objectsNames,
    *objectsIds, *objectsOthers, *objectsMobilAndSpring, *forcesType,
    *forcesArrays, *postCheck;
  NSString *gravityVar, *timeVar;
  NSMutableDictionary *objectsDictionary;
}
- (void) makeEquationsForData: (NSMutableDictionary *)list;
@end
