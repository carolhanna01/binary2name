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

#define DISForceOther		0
#define DISForceFriendly	1
#define DISForceOpposing	2
#define DISForceNeutral		3

#define DISKindOther		0
#define DISKindPlatform		1
#define DISKindMunition		2
#define DISKindLifeForm		3
#define DISKindEnvironmental	4
#define DISKindCultural		5
#define DISKindSupply		6
#define DISKindRadio		7

#define DISDomainOther		0
#define DISDomainLand		1
#define DISDomainAir		2
#define DISDomainSurface	3
#define DISDomainSubsurface	4
#define DISDomainSpace		5

#define DISCategoryLandOther	0
#define DISCategoryLandTank	1
#define DISCategoryLandAFV	2
#define DISCategoryLandAUV	3
#define DISCategorySPA		4
#define DISCategoryTowedArty	5
#define DISCategorySmallWUV	6
#define DISCategoryLargeWUV	7

#define DISTargetDomainOther			0
#define DISTargetDomainAntiAir			1
#define DISTargetDomainAntiArmor		2
#define DISTargetDomainAntiGuidedMunition	3
#define DISTargetDomainAntiRadar		4
#define DISTargetDomainAntiSatellite		5
#define DISTargetDomainAntiShip			6
#define DISTargetDomainAntiSubmarine		7
#define DISTargetDomainBattlefieldSupport	8
#define DISTargetDomainStrategic		8
#define DISTargetDomainMisc			10

#define DISDRMethodOther		0
#define DISDRMethodStatic		1
#define DISDRMethodFPW			2
#define DISDRMethodRPW			3
#define DISDRMethodRVW			4
#define DISDRMethodFVW			5
#define DISDRMethodFPB			6
#define DISDRMethodRPB			7
#define DISDRMethodRVB			8
#define DISDRMethodFVB			9
#define DISDRMethodRPW_2		10
#define DISDRMethodRVW_2		11

#define DISCharSetUnused	0
#define DISCharSetASCII		1

#define DISCapabilityAmmunitionSupply	1
#define DISCapabilityFuelSupply		2
#define DISCapabilityMiscSupply		4
#define DISCapabilityRepair		8

#define DISWarheadOther			0
#define DISWarheadHE			1000
#define DISWarheadHEPlastic		1100
#define DISWarheadHEIncendiary		1200
#define DISWarheadHEFragment		1300
#define DISWarheadHEAntiTank		1400
#define DISWarheadHEBomblets		1500
#define DISWarheadHEShapedCharge	1600
#define DISWarheadSmoke			2000
#define DISWarheadIllumination		3000
#define DISWarheadPractice		4000
#define DISWarheadKinetic		5000
#define DISWarheadUnused		6000
#define DISWarheadNuclear		7000
#define DISWarheadChemGeneral		8000
#define DISWarheadChemBlister		8100
#define DISWarheadChemBlood		8200
#define DISWarheadChemNerve		8300
#define DISWarheadBiologicalGeneral	9000

#define DISFuzeOther			0
#define DISFuzeContact			1000
#define DISFuzeContactInstant		1100
#define DISFuzeContactDelayed		1200
#define DISFuzeContactElectronic	1300
#define DISFuzeContactGraze		1400
#define DISFuzeContactCrush		1500
#define DISFuzeContactHydrostatic	1600
#define DISFuzeContactMechanical	1700
#define DISFuzeContactChemical		1800
#define DISFuzeTimed			2000
#define DISFuzeProximity		3000
#define DISFuzeProximityActiveLaser	3100
#define DISFuzeProximityMagnetic	3200
#define DISFuzeProximityRadar		3300
#define DISFuzeProximityRF		3400
#define DISFuzeProximityProgrammable	3500
#define DISFuzeProximityInfrared	3700
#define DISFuzeCommand			4000
#define DISFuzeCommandElectronicRS	4100
#define DISFuzeAltitude			5000
#define DISFuzeAltitudeRadioAltimeter	5100
#define DISFuzeAltitudeRadioAirBurst	5100
#define DISFuzeDepth			6000
#define DISFuzeAcoustic			7000
#define DISFuzePressure			8000
#define DISFuzePyrotechnic		9000

#define DISDetonationResultOther		0
#define DISDetonationResultEntityImpact		1
#define DISDetonationResultEntityProxDetonation 2
#define DISDetonationResultGroundImpact		3
#define DISDetonationResultGroundProxDetonation 4
#define DISDetonationResultDetonation		5
#define DISDetonationResultNone			6
#define DISDetonationResultHESmall		7
#define DISDetonationResultHEMedium		8
#define DISDetonationResultHELarge		9
#define DISDetonationResultAP			10
#define DISDetonationResultDBSmall		11
#define DISDetonationResultDBMedium		12
#define DISDetonationResultDBLarge		13
#define DISDetonationResultAirHit		17
#define DISDetonationResultBuildingHitSmall		18
#define DISDetonationResultBuildingHitMedium		19
#define DISDetonationResultBuildingHitLarge		20
#define DISDetonationResultMineClearingLineCharge		21

#define DISServiceTypeOther					0
#define DISServiceTypeResupply				1
#define DISServiceTypeRepair				2

#define DISRepairTypeNone					0
#define DISRepairTypeAll					1

#define DISRepairResultOther				0
#define DISRepairResultEnded				1
#define DISRepairResultInvalid				2
#define DISRepairResultInterrupted			3
#define DISRepairResultCancelled			4

#define DISStopReasonOther					0
#define DISStopReasonRecess					1
#define DISStopReasonTermination			2
#define DISStopReasonSystemFailure			3
#define DISStopReasonSecurityViolation		4
#define DISStopReasonEntityReconstitution	5

#define DISAckFlagOther				0
#define DISAckFlagCreateEntity		1
#define DISAckFlagRemoveEntity		2
#define DISAckFlagStartResume		3
#define DISAckFlagStopFreeze		4

#define DISActionOther				0
#define DISActionLocalStorage		1
#define DISActionOutOfAmmunition	2
#define DISActionKIA				3
#define DISActionDamage				4
#define DISActionMobilityDisabled	5
#define DISActionFireDisabled		6

#define DISResponseOther			0
#define DISResponsePending			1
#define DISResponseExecuting		2
#define DISResponsePartiallyComplete	3
#define DISResponseComplete			4

#define DISActivateReasonOther		0
#define DISActivateReasonStart		1
#define DISActivateReasonRestart	2
#define DISActivateReasonEntry		3
#define DISActivateReasonReconstite	4

#define DISActivateResultOther				0
#define DISActivateResultRequestAccepted	1
#define DISActivateResultInvalidParam		2
#define DISActivateResultUnexpectedParam	3

#define DISDeactivateReasonOther			0
#define DISDeactivateReasonEnd				1
#define DISDeactivateReasonWithdrawn		2
#define DISDeactivateReasonDestroyed		3

#define DISDeactivateResultOther			0
#define DISDeactivateResultReqAccepted		1
#define DISDeactivateResultInvalidParam		2
#define DISDeactivateResultUnexpectedReason	3
#define DISDeactivateResultNotActive		4

#define DISFrozenSimulationClock	(1<<0)
#define DISFrozenTransmitPDUs		(1<<1)
#define DISFrozenReceivePDUs		(1<<2)

#define DISInputSourceOther		0
#define DISInputSourcePilot		1
#define DISInputSourceCopilot		2
#define DISInputSourceFirstOfficer	3

#define DISReceiverStateOff		0
#define DISReceiverStateOnNotReceiving	1
#define DISReceiverStateOnReceiving	2

#define DISEmitterFuncUnknown			0
#define DISEmitterFuncLandBasedAirDefense	1
#define DISEmitterFuncBattlefieldAndGroundSurveillance 2
#define DISEmitterFuncNavalSurveillanceAndNavigation 3
#define DISEmitterFuncNavalFireControl		4
#define DISEmitterFuncAirborneSurveillance	5
#define DISEmitterFuncAirborneFireControl	6
#define DISEmitterFuncSpaceborne		7
#define DISEmitterFuncATCInstrumentationAndRanging 8
#define DISEmitterFuncWeather			9
#define DISEmitterFuncMissileGuidance		10
#define DISEmitterFuncJamming			11

#define DISBeamFuncUnknown	0
#define DISBeamFuncSearch 1
#define DISBeamFuncHeightFinder 2
#define DISBeamFuncAcquisition 3
#define DISBeamFuncTracking 4
#define DISBeamFuncAcquisitionAndTracking 5
#define DISBeamFuncCommandGuidance 6
#define DISBeamFuncIlluminator 7
#define DISBeamFuncRangeOnlyRadar 8
#define DISBeamFuncMissileBeacon 9
#define DISBeamFuncMissileFuze	10
#define DISBeamFuncActiveRadarMissileSeeker 11
#define DISBeamFuncJammer		12

#define DISPRITypeUnknown	0
#define DISPRITypeSteady	1
#define DISPRITypeStaggered	2
#define DISPRITypeJitter	3
#define DISPRITypePulseGroup	4
#define DISPRITypeCW		5
#define DISPRITypePulseDoppler	6

#define DISScanTypeUnknown	0
#define DISScanTypeSteady	1
#define DISScanTypeUnidirectional	2
#define DISScanTypeBidirectional	3
#define DISScanTypeConical	4
#define DISScanTypeTwoBar	5
#define DISScanTypeThreeBar	6
#define DISScanTypeFourBar	7
#define DISScanTypeOneBar	8
