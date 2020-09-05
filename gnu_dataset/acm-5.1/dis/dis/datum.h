/*
 *  acm : an aerial combat simulator for X
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
/*
 *  This header file was prepared from values listed in:
 *
 *  "Enumeration and Bit-Encoded Values for use with
 *  IEEE 1278.1 - 1994, Distributed Interactive Simulation --
 *  Application Protocols"
 *
 *  Document Number:    IST-CR-93-46
 *  Date:               March 1994
 *
 *  DATUM IDENTIFIERS
 *
 *  Problem:  Which are fixed and which are variable ???
 */

typedef enum {
	DatumIdentification = 10000,	/*            */
	DatumEntityType = 11000,	/*            */
	DatumConcatenated = 11100,	/*            */
	DatumKind = 11110,			/* u_long     */
	DatumDomain = 11120,		/* u_long     */
	DatumCountry = 11130,		/* u_long     */
	DatumCategory = 11140,		/* u_long     */
	DatumSubcategory = 11150,	/* u_long     */
	DatumSpecific = 11160,		/* u_long     */
	DatumExtra = 11170,			/* u_long     */
	DatumForceID = 11200,		/* u_long     */
	DatumDescription = 11300,	/*            */
	DatumAlternativeEntityType = 12000,		/*            */
	DatumAltKind = 12110,		/* u_long     */
	DatumAltDomain = 12120,		/* u_long     */
	DatumAltCountry = 12130,	/* u_long     */
	DatumAltCategory = 12140,	/* u_long     */
	DatumAltSubcategory = 12150,	/* u_long     */
	DatumAltSpecific = 12160,	/* u_long     */
	DatumAltExtra = 12170,		/* u_long     */
	DatumAltDescription = 12300,	/*            */
	DatumEntityMarking = 13000,	/*            */
	DatumEntityMarkingCharacters = 13100,	/* char[10]   */
	DatumCrewID = 13200,		/* char[10]   */
	DatumTaskOrganization = 14000,	/*            */
	DatumRegimentName = 14200,	/* String     */
	DatumBattalionName = 14300,	/* String     */
	DatumCompanyName = 14400,	/* String     */
	DatumPlatoonName = 14500,	/*            */
	DatumSquadName = 14520,		/*            */
	DatumTeamName = 14540,		/*            */
	DatumBumperNumber = 14600,	/*            */
	DatumVehicleNumber = 14700,	/*            */
	DatumUnitNumber = 14800,	/*            */
	DatumDISIdentity = 15000,	/*            */
	DatumDISSiteID = 15100,		/*            */
	DatumDISHostID = 15200,		/*            */
	DatumDISEntityID = 15300,	/*            */
	DatumLoads = 20000,			/*            */
	DatumCrewMembers = 21000,	/*            */
	DatumCrewMemberID = 21100,	/*            */
	DatumHealth = 21200,		/*            */
	DatumJobAssignment = 21300,	/* String     */
	DatumFuel = 23000,			/*            */
	DatumQuantity = 23100,		/* Liters     */
	DatumQuantityGallons = 23105,	/* Gallons    */
	DatumAmmunition = 24000,	/*            */
	Datum120mmHEATquantity = 24001,		/* Rounds     */
	Datum120mmSABOTquantity = 24002,	/* Rounds     */
	Datum127mmM8quantity = 24003,	/* Rounds     */
	Datum127mmM20quantity = 24004,	/* Rounds     */
	Datum762mmM62quantity = 24005,	/* Rounds     */
	DatumM250UKL8A1quantity = 24006,	/* Grenades   */
	DatumM250UKL8A3quantity = 24007,	/* Grenades   */
	Datum762mmM80quantity = 24008,	/* Rounds     */
	Datum127mmquantity = 24009,	/* Rounds     */
	Datum762mmquantity = 24010,	/* Rounds     */
	DatumMinesquantity = 24060,	/* Mines      */
	DatumAmmunitionType = 24100,	/*            */
	DatumAmmunitionKind = 24110,	/*            */
	DatumAmmunitionDomain = 24120,	/*            */
	DatumAmmunitionCountry = 24130,		/*            */
	DatumAmmunitionCategory = 24140,	/*            */
	DatumAmmunitionSubcategory = 24150,		/*            */
	DatumAmmunitionExtra = 24160,	/*            */
	DatumAmmunitionDescription = 24300,		/*            */
	DatumCargo = 25000,			/*            */
	DatumVehicleMass = 26000,	/* u_long     */
	DatumSupplyQuantity = 27000,	/*            */
	DatumArmament = 28000,		/* Boolean    */
	DatumStatus = 30000,		/*            */
	DatumPosition = 31000,		/*            */
	DatumMilGrid10 = 31100,		/*            */
	DatumGeocentricCoordinates = 31200,		/*            */
	DatumGeocentricCoordinatesX = 31210,	/* u_long     */
	DatumGeocentricCoordinatesY = 31220,	/* u_long     */
	DatumGeocentricCoordinatesZ = 31230,	/* u_long     */
	DatumLatitude = 31300,		/*            */
	DatumLongitude = 31400,		/*            */
	DatumOrientation = 32000,	/*            */
	DatumHullHeadingAngle = 32100,	/* Degrees    */
	DatumHullPitchAngle = 32200,	/*            */
	DatumRollAngle = 32300,		/*            */
	DatumOrientationX = 32500,	/* u_long     */
	DatumOrientationY = 32600,	/* u_long     */
	DatumOrientationZ = 32700,	/* u_long     */
	DatumAppearance = 33000,	/*            */
	DatumAmbientLighting = 33100,	/*            */
	DatumLights = 33101,		/*            */
	DatumPaintScheme = 33200,	/*            */
	DatumSmoke = 33300,			/*            */
	DatumTrailingEffects = 33400,	/*            */
	DatumFlaming = 33500,		/*            */
	DatumMarking = 33600,		/*            */
	DatumMinePlowsAttached = 33710,		/*            */
	DatumMineRollersAttached = 33720,	/*            */
	DatumTankTurretAzimuth = 33730,		/* Degrees    */
	DatumFailuresandMalfunctions = 34000,	/*            */
	DatumAge = 34100,			/* Miles      */
	DatumKilometers = 34110,	/*            */
	DatumDamage = 35000,		/*            */
	DatumCause = 35050,			/*            */
	DatumMobilityKill = 35100,	/*            */
	DatumFire_PowerKill = 35200,	/*            */
	DatumPersonnelCasualties = 35300,	/*            */
	DatumVelocity = 36000,		/*            */
	DatumXVelocity = 36100,		/* Meters/sec */
	DatumYVelocity = 36200,		/* Meters/sec */
	DatumZVelocity = 36300,		/* Meters/sec */
	DatumAcceleration = 37000,	/*            */
	DatumXAcceleration = 37100,	/*            */
	DatumYAcceleration = 37200,	/*            */
	DatumZAcceleration = 37300,	/*            */
	DatumEngineStatus = 38100,	/*            */
	DatumExercise = 40000,		/*            */
	DatumTerrainDatabase = 41000,	/*            */
	DatumMissions = 42000,		/*            */
	DatumMissionID = 42100,		/*            */
	DatumMissionType = 42200,	/*            */
	DatumMissionRequestTimeStamp = 42300,	/*            */
	DatumExerciseDescription = 43000,	/* String     */
	DatumName = 43100,			/* String     */
	DatumEntities = 43200,		/* Integer    */
	DatumVersion = 43300,		/*            */
	DatumEnvironment = 50000,	/*            */
	DatumWeather = 51000,		/*            */
	DatumThermalCondition = 51100,	/*            */
	DatumTime = 52000,			/*            */
	DatumTimeofDayDiscrete = 52100,		/*            */
	DatumTimeofDayContinuous = 52200,	/*            */
	DatumTimeMode = 52300,		/*            */
	DatumTimeScene = 52305,		/*            */
	DatumCurrentHour = 52310,	/*            */
	DatumCurrentMinute = 52320,	/*            */
	DatumCurrentSecond = 52330,	/*            */
	DatumAzimuth = 52340,		/*            */
	DatumMaximumElevation = 52350,	/*            */
	DatumTimeZone = 52360,		/*            */
	DatumTimeSunriseEnabled = 52400,	/*            */
	DatumSunriseHour = 52410,	/*            */
	DatumSunriseMinute = 52420,	/*            */
	DatumSunriseSecond = 52430,	/*            */
	DatumSunriseAzimuth = 52440,	/*            */
	DatumTimeSunsetEnabled = 52500,		/*            */
	DatumSunsetHour = 52510,	/*            */
	DatumSunsetMinute = 52520,	/*            */
	DatumSunsetSecond = 52530,	/*            */
	DatumDate = 52600,			/*            */
	DatumMonth = 52610,			/*            */
	DatumDay = 52620,			/*            */
	DatumYear = 52630,			/*            */
	DatumClouds = 53000,		/*            */
	DatumCloudLayerEnable = 53050,	/*            */
	DatumCloudLayerSelection = 53060,	/*            */
	DatumCloudVisibility = 53100,	/*            */
	DatumCloudBaseAltitude = 53200,		/* Meters     */
	DatumCloudBaseAltitudeFeet = 53250,		/* Feet       */
	DatumCloudCeiling = 53300,	/* Meters     */
	DatumcloudCeilingFeet = 53350,	/* Feet       */
	DatumCharacteristics = 53400,	/*            */
	DatumPrecipitation = 54000,	/*            */
	DatumRain = 54100,			/* Boolean    */
	DatumFog = 55000,			/* Boolean    */
	DatumVisibility = 55100,	/* Meters     */
	DatumVisibilityMiles = 55105,	/* Miles      */
	DatumDensity = 55200,		/*            */
	DatumBase = 55300,			/*            */
	DatumViewLayerFromAbove = 55401,	/*            */
	DatumTransitionRange = 55410,	/*            */
	DatumBottom = 55420,		/* Meters     */
	DatumBottomFeet = 55425,	/* Feet       */
	DatumCeiling = 55430,		/* Meters     */
	DatumCeilingFeet = 55435,	/* Feet       */
	DatumHeavenlyBodies = 56000,	/*            */
	DatumSun = 56100,			/*            */
	DatumSunPosition = 56110,	/*            */
	DatumSunPositionAzimuth = 56120,	/*            */
	DatumSunPositionElevation = 56130,	/*            */
	DatumSunPositionIntensity = 56140,	/*            */
	DatumMoon = 56200,			/*            */
	DatumMoonPosition = 56210,	/*            */
	DatumMoonPositionAzimuth = 56220,	/*            */
	DatumMoonPositionElevation = 56230,		/*            */
	DatumMoonPositionIntensity = 56240,		/*            */
	DatumHorizon = 56310,		/*            */
	DatumHorizonAzimuth = 56320,	/*            */
	DatumHorizonElevation = 56330,	/*            */
	DatumHorizonHeading = 56340,	/*            */
	DatumHorizonIntensity = 56350,	/*            */
	DatumMeteorological = 57000,	/*            */
	DatumMeteorologicalTemperature = 57100,		/*            */
	DatumMeteorologicalHumidity = 57200,	/*            */
	DatumMeteorologicalVisibility = 57300,	/*            */
	DatumMeteorologicalWinds = 57400,	/*            */
	DatumMeteorologicalSpeed = 57410,	/*            */
	DatumMeteorologicalRainsoak = 57500,	/*            */
	DatumHaze = 58000,			/* Boolean    */
	DatumHazeVisibility = 58100,	/* Meters     */
	DatumHazeVisibilityMiles = 58105,	/* Miles      */
	DatumHazeDensity = 58200,	/*            */
	DatumHazeCeiling = 58430,	/* Meters     */
	DatumHazeCeilingFeet = 58435,	/* Feet       */
	DatumCommunications = 60000,	/*            */
	DatumChannelType = 61100,	/*            */
	DatumChannelType1 = 61101,	/*            */
	DatumChannelIdentification = 61200,		/*            */
	DatumAlphaIdentification = 61300,	/*            */
	DatumRadioIdentification = 61400,	/*            */
	DatumLandLineIdentification = 61500,	/*            */
	DatumIntercomIdentification = 61600,	/*            */
	DatumGroupNetworkChannelNumber = 61700,		/*            */
	DatumRadioCommunicationsStatus = 62100,		/*            */
	DatumStationaryRadioTransmittersDefaultTime = 62200,	/* u_long     */
	DatumMovingRadioTransmittersDefaultTime = 62300,	/* u_long     */
	DatumStationaryRadioSignalsDefaultTime = 62400,		/*            */
	DatumMovingRadioSignalDefaultTime = 62500,	/*            */
	DatumRadioInitTransecSecurityKey = 63101,	/* variable   */
	DatumRadioInitInternalNoiseLevel = 63102,	/* variable   */
	DatumRadioInitSquelchThreshold = 63103,		/* variable   */
	DatumRadioInitAntennaLocation = 63104,	/* variable   */
	DatumRadioInitAntennaPatternType = 63105,	/* variable   */
	DatumRadioInitAntennaPatternLength = 63106,		/* variable   */
	DatumRadioInitBeamDefinition = 63107,	/* variable   */
	DatumRadioInitTransmitHeartbeatTime = 63108,	/* variable   */
	DatumRadioInitTransmitDistanceThreshold = 63109,	/* variable   */
	DatumRadioChannelInitLockoutID = 63110,		/* variable   */
	DatumRadioChannelInitHopsetID = 63111,	/* variable   */
	DatumRadioChannelInitPresetFrequency = 63112,	/* variable   */
	DatumRadioChannelInitFrequencySyncTime = 63113,		/* variable   */
	DatumRadioChannelInitComsecKey = 63114,		/* variable   */
	DatumRadioChannelInitAlpha = 63115,		/* variable   */
	DatumAlgorithmParameters = 70000,	/*            */
	DatumDeadReckoningAlgorithm = 71000,	/*            */
	DatumDRALocationThreshold = 71100,	/* u_long     */
	DatumDRAOrientationThreshold = 71200,	/*            */
	DatumDRATimeThreshold = 71300,	/*            */
	DatumSimulationManagementParameters = 72000,	/*            */
	DatumCheckpointInterval = 72100,	/*            */
	DatumTransmitterTimeThreshold = 72600,	/*            */
	DatumReceiverTimeThreshold = 72700,		/*            */
	DatumInteroperabilityMode = 73000,	/*            */
	DatumSIMNETDataCollection = 74000,	/* variable*  */
	DatumEventID = 75000,		/*            */
	DatumSourceSiteID = 75100,	/*            */
	DatumSourceHostID = 75200,	/*            */
	DatumArticulatedPart = 90000,	/*            */
	DatumArticulatedPartID = 90050,		/*            */
	DatumArticulatedPartIndex = 90070,	/*            */
	DatumArticulatedPartPosition = 90100,	/*            */
	DatumArticulatedPartPositionRate = 90200,	/*            */
	DatumArticulatedPartExtension = 90300,	/*            */
	DatumArticulatedPartExtensionRate = 90400,	/*            */
	DatumArticulatedPartX = 90500,	/*            */
	DatumArticulatedPartXRate = 90600,	/*            */
	DatumArticulatedPartY = 90700,	/*            */
	DatumArticulatedPartYRate = 90800,	/*            */
	DatumArticulatedPartZ = 90900,	/*            */
	DatumArticulatedPartZRate = 91000,	/*            */
	DatumArticulatedPartAzimuth = 91100,	/*            */
	DatumArticulatedPartAzimuthRate = 91200,	/*            */
	DatumArticulatedPartElevation = 91300,	/*            */
	DatumArticulatedPartElevationRate = 91400,	/*            */
	DatumArticulatedPartRotation = 91500,	/*            */
	DatumArticulatedPartRotationRate = 91600,	/*            */
	DatumDRAAngularXVelocity = 100001,	/*            */
	DatumDRAAngularYVelocity = 100002,	/*            */
	DatumDRAAngularZVelocity = 100003,	/*            */
	DatumAppearanceTrailingEffects = 100004,	/*            */
	DatumAppearanceHatch = 100005,	/*            */
	DatumAppearanceCharacterSet = 100008,	/*            */
	DatumCapabilityAmmunitionSupplier = 100010,		/*            */
	DatumCapabilityMiscellaneousSupplier = 100011,	/*            */
	DatumCapabilityRepairProvider = 100012,		/*            */
	DatumArticulationParameter = 100014,	/*            */
	DatumArticulationParameterType = 100047,	/*            */
	DatumArticulationParameterValue = 100048,	/*            */
	DatumTimeofDayScene = 100058	/*            */
} dis_datum_type;
