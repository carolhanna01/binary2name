/* 
   Project: FisicaLab

   Copyright (C) 2009, 2010, 2011, 2012, 2013,
                 2014, 2015  Free Software Foundation

   Author: German A. Arias <germanandre@gmx.es>

   Created: 2008-09-10 18:56:00 -0600 by german
   
   Application Controller

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

#import "ChalkboardView.h"
#import "AppController.h"

@implementation AppController

- (id) init
{
  if ((self = [super init]))
    {
      preferencesPanel = nil;
      propertiesWindow = nil;
    }
  return self;
}

- (void) applicationWillFinishLaunching: (NSNotification *)aNotification
{
  NSScrollView *staticView, *dynamicsCircularView;

  // Hide the label use only for thermodynamics.
  [system setHidden: YES];

  // Se the user frames for windows.
  [[elements window] setDelegate: self];
  [[elements window] setFrameUsingName: @"Palette"];
  [[chalkboard window] setDelegate: self];
  [[chalkboard window] setFrameUsingName: @"Chalkboard"];
  
  // Set up the static of rigid bodies view.
  [NSBundle loadNibNamed: @"staticRigidBodies.gorm" owner: self];

  staticView = [[NSScrollView alloc] initWithFrame:
				       NSMakeRect(0, 0, 270, 320)];
  [staticView setHasVerticalScroller: YES];
  [staticView setDocumentView: [staticRigidBodies contentView]]; 
  [staticRigidBodiesView addSubview: staticView];

  // Set up the dynamics circular of particles view.
  [NSBundle loadNibNamed: @"dynamicsCircular.gorm" owner: self];

  dynamicsCircularView = [[NSScrollView alloc] initWithFrame:
						 NSMakeRect(0, 0, 270, 320)];
  [dynamicsCircularView setHasVerticalScroller: YES];
  [dynamicsCircularView setDocumentView:
			  [dynamicsCircularParticles contentView]]; 
  [dynamicsCircularParticlesView addSubview: dynamicsCircularView];


  [staticView release];
  [staticRigidBodies release];
  [dynamicsCircularView release];
  [dynamicsCircularParticles release];
}

- (void) applicationDidFinishLaunching: (NSNotification*)aNotification
{
  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];

  if (![defaults boolForKey: @"HasStartedBefore"])
    {
      [NSBundle loadNibNamed: @"firstLaunchPanel" owner: self];
      [firstLaunchPanel center];
      [firstLaunchPanel makeKeyAndOrderFront: self];
    }
}

- (void) showPrefPanel: (id)sender
{
  if (preferencesPanel == nil)
    {
      int width = 0, height = 0, size = 0;
      NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
      [NSBundle loadNibNamed: @"preferences" owner: self];

      [preferencesPanel center];
      [preferencesPanel makeKeyAndOrderFront: self];
      
      width = [defaults integerForKey: @"ChalkboardWidth"];
      height = [defaults integerForKey: @"ChalkboardHeight"];
      size = [defaults integerForKey: @"NSToolTipsFontSize"];
      
      if ((width < 26) || (width > 100))
	{
	  width = 26;
	}
      
      if ( (height < 18) || (height > 100))
	{
	  height = 18;
	}

      if ( (size < 10) || (size > 20))
	{
	  size = 10;
	}

      [widthLabel setIntValue: width];
      [heightLabel setIntValue: height];
      [fontsizeLabel setIntValue: size];
      [widthStepper setIntValue: width];
      [heightStepper setIntValue: height];
      [fontsizeStepper setIntValue: size];
    }
  else
    {
      [preferencesPanel makeKeyAndOrderFront: self];
    }
}

- (void) showHelpPanel: (id)sender
{
  if (helpPanel == nil)
    {
      [NSBundle loadNibNamed: @"help.gorm" owner: self];
      [helpPanel makeKeyAndOrderFront: self];
    }
  else
    {
      [helpPanel makeKeyAndOrderFront: self];
    }
}

- (void) showPropertiesWindow: (id)sender
{
  if (propertiesWindow == nil)
    {
      [NSBundle loadNibNamed: @"properties.gorm" owner: self];
      [propertiesWindow makeKeyAndOrderFront: self];
    }
  else
    {
      [propertiesWindow makeKeyAndOrderFront: self];
    }
}

- (void) selectModule: (id)sender
{
  // Cancel any operation add/move element.
  [chalkboard controlCursor: self];
  
  /* If selected group is thermodynamics, show the label and hide units
     selector. If any other, hide the label and show the units selector.
     This because we only allow system SI in thermodynamics group. */
  if ([sender tag] == 3)
    {
      if ([system isHidden] == YES)
        {
          [system setHidden: NO];
          [unitsSelector setHidden: YES];
        }
    }
  else
    {
      if ([system isHidden] ==  NO)
        {
          [system setHidden: YES];
          [unitsSelector setHidden: NO];
        }
    }
  
  // Select the corresponding group of modules.
  [elements selectTabViewItemAtIndex: [sender tag]];
}

- (void) addToChalkboard: (id)sender
{
  [chalkboard addObject: sender];
}

- (void) windowWillClose: (NSNotification *)aNotification
{
  id window = [aNotification object];
  
  if (window == [chalkboard window])
    {
      [window saveFrameUsingName: @"Chalkboard"];
    }
  
  if (window == [elements window])
    {
      [window saveFrameUsingName: @"Palette"];
    }
}

// Preferences
- (void) changeChalkboardWidth: (id)sender
{
  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  [defaults setInteger: [sender intValue] forKey: @"ChalkboardWidth"];
  [widthLabel setIntValue: [sender intValue]];
}

- (void) changeChalkboardHeight: (id)sender
{
  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  [defaults setInteger: [sender intValue] forKey: @"ChalkboardHeight"];
  [heightLabel setIntValue: [sender intValue]];
}

- (void) changeFontsizeTooltips: (id)sender
{
  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  [defaults setInteger: [sender intValue] forKey: @"NSToolTipsFontSize"];
  [fontsizeLabel setIntValue: [sender intValue]];
}

- (void) restoreDefaults: (id)sender
{
  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  [defaults removeObjectForKey: @"ChalkboardWidth"];
  [defaults removeObjectForKey: @"ChalkboardHeight"];
  [defaults removeObjectForKey: @"NSToolTipsFontSize"];

  [widthLabel setIntValue: 26];
  [heightLabel setIntValue: 18];
  [fontsizeLabel setIntValue: 10];
  [widthStepper setIntValue: 26];
  [heightStepper setIntValue: 18];
  [fontsizeStepper setIntValue: 10];
}

// First launch panel
- (void) notShowAgain: (id)sender
{
  NSUserDefaults *defaults = [NSUserDefaults standardUserDefaults];
  [defaults setBool: YES forKey: @"HasStartedBefore"];
}

@end
