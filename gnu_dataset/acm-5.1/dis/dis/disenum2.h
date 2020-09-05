/*
 *  DIS/x : An implementation of the IEEE 1278.1 protocol
 *  Copyright (C) 1991-1998  Riley Rainey
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software Foundaation,
 *  Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#define DISAppearancePaintUniform			0
#define DISAppearancePaintCamouflage		1
#define DISAppearancePaintMask				1

#define DISAppearanceMobilityNormal		(0 << 1)
#define DISAppearanceMobilityDisabled		(1 << 1)

#define DISAppearanceFirepowerNormal		(0 << 2)
#define DISAppearanceFirepowerDisabled		(1 << 2)

#define DISAppearanceDamageNone			(0 << 3)
#define DISAppearanceDamageSlight		(1 << 3)
#define DISAppearanceDamageModerate		(2 << 3)
#define DISAppearanceDamageDestroyed		(3 << 3)
#define DISAppearanceDamageMask		(3 << 3)

#define DISAppearanceSmokeNone			(0 << 5)
#define DISAppearanceSmokePlume			(1 << 5)
#define DISAppearanceSmokeEngine		(2 << 5)
#define DISAppearanceSmokePlumeAndEngine	(3 << 5)
#define DISAppearanceSmokeMask			(3 << 5)

#define DISAppearanceSmokeTrailingNone		(0 << 7)
#define DISAppearanceSmokeTrailingSmall		(1 << 7)
#define DISAppearanceSmokeTrailingMedium	(2 << 7)
#define DISAppearanceSmokeTrailingLarge		(3 << 7)
#define DISAppearanceSmokeTrailingMask		(3 << 7)

#define DISAppearanceHatchNA			(0 << 9)
#define DISAppearanceHatchClosed		(1 << 9)
#define DISAppearanceHatchPopped		(2 << 9)
#define DISAppearanceHatchPoppedPerson	(3 << 9)
#define DISAppearanceHatchOpen			(4 << 9)
#define DISAppearanceHatchOpenPerson	(5 << 9)
#define DISAppearanceHatchMask			(7 << 9)

#define DISLightsNone				(0 << 12)
#define DISLightsRunning			(1 << 12)
#define DISLightsNavigation			(2 << 12)
#define DISLightsFormation			(3 << 12)
#define DISLightsMask				(3 << 12)

#define DISNoFlames				(0 << 15)
#define DISFlamesPresent			(1 << 15)

#define DISAppearancePlatformNotFrozen		(0 << 21)
#define DISAppearancePlatformFrozen		(1 << 21)
#define DISAppearancePlatformPowerplantOff	(0 << 22)
#define DISAppearancePlatformPowerplantOn	(1 << 22)
#define DISAppearancePlatformActive		(0 << 23)
#define DISAppearancePlatformDeactivated	(1 << 23)

#define DISAppearanceAirAfterburnerOn		(1 << 16)

#define DISAppearanceLandLauncherRaised		(1 << 16)

#define DISAppearanceLandCammouflageDesert	(0 << 17)
#define DISAppearanceLandCammouflageWinter	(1 << 17)
#define DISAppearanceLandCammouflageForest	(2 << 17)
#define DISAppearanceLandCammouflageMask	(3 << 17)

#define DISAppearanceLandConcealed		(1 << 19)
