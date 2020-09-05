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

#define PDUTypeOther			0
#define PDUTypeEntityState		1
#define PDUTypeFire			2
#define PDUTypeDetonation		3
#define PDUTypeCollision		4
#define PDUTypeServiceRequest		5
#define PDUTypeResupplyOffer		6
#define PDUTypeResupplyReceived		7
#define PDUTypeResupplyCancel		8
#define PDUTypeRepairComplete		9
#define PDUTypeRepairResponse		10
#define PDUTypeCreateEntity		11
#define PDUTypeRemoveEntity		12
#define PDUTypeStartResume		13
#define PDUTypeStopFreeze		14
#define PDUTypeAcknowledge		15
#define PDUTypeActionRequest		16
#define PDUTypeActionResponse		17
#define PDUTypeDataQuery		18
#define PDUTypeSetData			19
#define PDUTypeData			20
#define PDUTypeEventReport		21
#define PDUTypeMessage			22
#define PDUTypeEmission			23
#define PDUTypeLaser			24
#define PDUTypeTransmitter		25
#define PDUTypeSignal			26
#define PDUTypeReceiver			27

#define PDUTypeTransferControl  36

#define PDUFamilyOther                            0
#define PDUFamilyEntityInformation                1
#define PDUFamilyWarfare                          2
#define PDUFamilyLogistics                        3
#define PDUFamilyRadioCommunications              4
#define PDUFamilySimulationManagement             5
#define PDUFamilyDistributedEmissionRegeneration  6


/*
 * CALSPAN experimental PDUs
 */

#define PDUTypeExperimentalRequestControl 150
#define PDUTypeExperimentalGrantControl   151

typedef enum {
	DISProtocolVersionMay92 = 1,
	DISProtocolVersionIEEE1278_93 = 2,
	DISProtocolVersionMay93 = 3,
	DISProtocolVersion2_04 = 4,
	DISProtocolVersionIEEE1278_95 = 5
} DISProtocolVersion;

typedef enum {
	DISForceOther = 0,
	DISForceFriendly = 1,
	DISForceOpposing = 2,
	DISForceNeutral = 3
} DISForce;

typedef enum {
	DISRequestStatusOther = 0,
	DISRequestStatusPending = 1,
	DISRequestStatusExecuting = 2,
	DISRequestStatusPartiallyComplete = 3,
	DISRequestStatusComplete = 4
} DISRequestStatus;

typedef enum {
	DISAcknowledgeFlagCreateEntity = 1,
	DISAcknowledgeFlagRemoveEntity = 2,
	DISAcknowledgeFlagStart = 3,
	DISAcknowledgeFlagStop = 4
} DISAcknowledgeFlag;

typedef enum {
	DISStopReasonOther = 0,
	DISStopReasonRecess = 1,
	DISStopReasonTermination = 2,
	DISStopReasonSystemFailure = 3,
	DISStopReasonSecurityViolation = 4,
	DISStopReasonEntityReconstitution = 5
} DISStopReason;

#define DISFrozenBehaviorRunClock (1<<0)
#define DISFrozenBehaviorTransmit (1<<1)
#define DISFrozenBehaviorReceive  (1<<2)

typedef enum {
	DISTransferTypeOther = 0,
	DISTransferTypeEntityControllerRequest = 1,
	DISTransferTypeEntityRequest = 2,
	DISTransferTypeEntityMutualExchange = 3,
	DISTransferTypeEnvironmentalControllerRequest = 4,
	DISTransferTypeEnvironmentalRequest = 5,
	DISTransferTypeEnvironmentalMutualExchange = 6
} DISTransferType;


/*
 * From IEEE 1278.1-1995
 */

#define ALL_APPLIC            0xFFFF
#define ALL_ENTITIES          0xFFFF
#define ALL_SITES             0xFFFF
#define COLLISION_THRSH_DFLT  0.1
#define DRA_ORIENT_THRSH_DFLT 3.0
#define DRA_POS_THRSH_DFLT    1.0
#define NO_APPLIC             0x0000
#define NO_ENTITY             0x0000
#define NO_SITE               0x0000
#define NOR_FIRE_MISSION      0x0000
#define RQST_ASSIGN_ID        0xFFFE
