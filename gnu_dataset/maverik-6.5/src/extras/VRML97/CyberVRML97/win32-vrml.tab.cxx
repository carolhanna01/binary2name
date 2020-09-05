/*
   GNU Maverik - a system for managing display and interaction in 
              Virtual Environment applications.
   Copyright (C) 2008  Advanced Interfaces Group

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.

   The authors can be contacted via:
   www   - http://aig.cs.man.ac.uk
   email - maverik@aig.cs.man.ac.uk
   mail  - Advanced Interfaces Group, Room 2.94, Kilburn Building, 
        University of Manchester, Manchester, M13 9PL, UK
*/


/*  A Bison parser, made from vrml.y
    by GNU Bison version 1.28  */

#define YYBISON 1  /* Identify Bison output.  */

#define	NUMBER	257
#define	FLOAT	258
#define	STRING	259
#define	NAME	260
#define	ANCHOR	261
#define	APPEARANCE	262
#define	AUDIOCLIP	263
#define	BACKGROUND	264
#define	BILLBOARD	265
#define	BOX	266
#define	COLLISION	267
#define	COLOR	268
#define	COLOR_INTERP	269
#define	COORDINATE	270
#define	COORDINATE_INTERP	271
#define	CYLINDER_SENSOR	272
#define	NULL_STRING	273
#define	CONE	274
#define	CUBE	275
#define	CYLINDER	276
#define	DIRECTIONALLIGHT	277
#define	FONTSTYLE	278
#define	ERROR	279
#define	EXTRUSION	280
#define	ELEVATION_GRID	281
#define	FOG	282
#define	INLINE	283
#define	MOVIE_TEXTURE	284
#define	NAVIGATION_INFO	285
#define	PIXEL_TEXTURE	286
#define	GROUP	287
#define	INDEXEDFACESET	288
#define	INDEXEDLINESET	289
#define	S_INFO	290
#define	LOD	291
#define	MATERIAL	292
#define	NORMAL	293
#define	POSITION_INTERP	294
#define	PROXIMITY_SENSOR	295
#define	SCALAR_INTERP	296
#define	SCRIPT	297
#define	SHAPE	298
#define	SOUND	299
#define	SPOTLIGHT	300
#define	SPHERE_SENSOR	301
#define	TEXT	302
#define	TEXTURE_COORDINATE	303
#define	TEXTURE_TRANSFORM	304
#define	TIME_SENSOR	305
#define	SWITCH	306
#define	TOUCH_SENSOR	307
#define	VIEWPOINT	308
#define	VISIBILITY_SENSOR	309
#define	WORLD_INFO	310
#define	NORMAL_INTERP	311
#define	ORIENTATION_INTERP	312
#define	POINTLIGHT	313
#define	POINTSET	314
#define	SPHERE	315
#define	PLANE_SENSOR	316
#define	TRANSFORM	317
#define	S_CHILDREN	318
#define	S_PARAMETER	319
#define	S_URL	320
#define	S_MATERIAL	321
#define	S_TEXTURETRANSFORM	322
#define	S_TEXTURE	323
#define	S_LOOP	324
#define	S_STARTTIME	325
#define	S_STOPTIME	326
#define	S_GROUNDANGLE	327
#define	S_GROUNDCOLOR	328
#define	S_SPEED	329
#define	S_AVATAR_SIZE	330
#define	S_BACKURL	331
#define	S_BOTTOMURL	332
#define	S_FRONTURL	333
#define	S_LEFTURL	334
#define	S_RIGHTURL	335
#define	S_TOPURL	336
#define	S_SKYANGLE	337
#define	S_SKYCOLOR	338
#define	S_AXIS_OF_ROTATION	339
#define	S_COLLIDE	340
#define	S_COLLIDETIME	341
#define	S_PROXY	342
#define	S_SIDE	343
#define	S_AUTO_OFFSET	344
#define	S_DISK_ANGLE	345
#define	S_ENABLED	346
#define	S_MAX_ANGLE	347
#define	S_MIN_ANGLE	348
#define	S_OFFSET	349
#define	S_BBOXSIZE	350
#define	S_BBOXCENTER	351
#define	S_VISIBILITY_LIMIT	352
#define	S_AMBIENT_INTENSITY	353
#define	S_NORMAL	354
#define	S_TEXCOORD	355
#define	S_CCW	356
#define	S_COLOR_PER_VERTEX	357
#define	S_CREASE_ANGLE	358
#define	S_NORMAL_PER_VERTEX	359
#define	S_XDIMENSION	360
#define	S_XSPACING	361
#define	S_ZDIMENSION	362
#define	S_ZSPACING	363
#define	S_BEGIN_CAP	364
#define	S_CROSS_SECTION	365
#define	S_END_CAP	366
#define	S_SPINE	367
#define	S_FOG_TYPE	368
#define	S_VISIBILITY_RANGE	369
#define	S_HORIZONTAL	370
#define	S_JUSTIFY	371
#define	S_LANGUAGE	372
#define	S_LEFT2RIGHT	373
#define	S_TOP2BOTTOM	374
#define	IMAGE_TEXTURE	375
#define	S_SOLID	376
#define	S_KEY	377
#define	S_KEYVALUE	378
#define	S_REPEAT_S	379
#define	S_REPEAT_T	380
#define	S_CONVEX	381
#define	S_BOTTOM	382
#define	S_PICTH	383
#define	S_COORD	384
#define	S_COLOR_INDEX	385
#define	S_COORD_INDEX	386
#define	S_NORMAL_INDEX	387
#define	S_MAX_POSITION	388
#define	S_MIN_POSITION	389
#define	S_ATTENUATION	390
#define	S_APPEARANCE	391
#define	S_GEOMETRY	392
#define	S_DIRECT_OUTPUT	393
#define	S_MUST_EVALUATE	394
#define	S_MAX_BACK	395
#define	S_MIN_BACK	396
#define	S_MAX_FRONT	397
#define	S_MIN_FRONT	398
#define	S_PRIORITY	399
#define	S_SOURCE	400
#define	S_SPATIALIZE	401
#define	S_BERM_WIDTH	402
#define	S_CHOICE	403
#define	S_WHICHCHOICE	404
#define	S_FONTSTYLE	405
#define	S_LENGTH	406
#define	S_MAX_EXTENT	407
#define	S_ROTATION	408
#define	S_SCALE	409
#define	S_CYCLE_INTERVAL	410
#define	S_FIELD_OF_VIEW	411
#define	S_JUMP	412
#define	S_TITLE	413
#define	S_TEXCOORD_INDEX	414
#define	S_HEADLIGHT	415
#define	S_TOP	416
#define	S_BOTTOMRADIUS	417
#define	S_HEIGHT	418
#define	S_POINT	419
#define	S_STRING	420
#define	S_SPACING	421
#define	S_TYPE	422
#define	S_RADIUS	423
#define	S_ON	424
#define	S_INTENSITY	425
#define	S_COLOR	426
#define	S_DIRECTION	427
#define	S_SIZE	428
#define	S_FAMILY	429
#define	S_STYLE	430
#define	S_RANGE	431
#define	S_CENTER	432
#define	S_TRANSLATION	433
#define	S_LEVEL	434
#define	S_DIFFUSECOLOR	435
#define	S_SPECULARCOLOR	436
#define	S_EMISSIVECOLOR	437
#define	S_SHININESS	438
#define	S_TRANSPARENCY	439
#define	S_VECTOR	440
#define	S_POSITION	441
#define	S_ORIENTATION	442
#define	S_LOCATION	443
#define	S_CUTOFFANGLE	444
#define	S_WHICHCHILD	445
#define	S_IMAGE	446
#define	S_SCALEORIENTATION	447
#define	S_DESCRIPTION	448
#define	SFBOOL	449
#define	SFFLOAT	450
#define	SFINT32	451
#define	SFTIME	452
#define	SFROTATION	453
#define	SFNODE	454
#define	SFCOLOR	455
#define	SFIMAGE	456
#define	SFSTRING	457
#define	SFVEC2F	458
#define	SFVEC3F	459
#define	MFBOOL	460
#define	MFFLOAT	461
#define	MFINT32	462
#define	MFTIME	463
#define	MFROTATION	464
#define	MFNODE	465
#define	MFCOLOR	466
#define	MFIMAGE	467
#define	MFSTRING	468
#define	MFVEC2F	469
#define	MFVEC3F	470
#define	FIELD	471
#define	EVENTIN	472
#define	EVENTOUT	473
#define	USE	474


#line 11 "vrml.y"
typedef union {
int		ival;
float	fval;
char	*sval;
} YYSTYPE;
#line 61 "vrml.y"


#include <stdio.h>
#include <stdlib.h>

#include "SceneGraph.h"
#include "vrmlnodetype.h"
#include "vrmlsetinfo.h"

float			gColor[3];
float			gVec2f[2];
float			gVec3f[3];
float			gRotation[4];
int				gWidth;
int				gHeight;
int				gComponents;

#define	YYMAXDEPTH	819200

int yyerror(char *s);
int yyparse(void);
int yylex(void);

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		1185
#define	YYFLAG		-32768
#define	YYNTBASE	226

#define YYTRANSLATE(x) ((unsigned)(x) <= 474 ? yytranslate[x] : 517)

static const short yytranslate[] = {     0,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,   221,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
   222,     2,   223,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,   224,     2,   225,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     3,     4,     5,     6,
     7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
    17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
    37,    38,    39,    40,    41,    42,    43,    44,    45,    46,
    47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
    57,    58,    59,    60,    61,    62,    63,    64,    65,    66,
    67,    68,    69,    70,    71,    72,    73,    74,    75,    76,
    77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
    87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
    97,    98,    99,   100,   101,   102,   103,   104,   105,   106,
   107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
   117,   118,   119,   120,   121,   122,   123,   124,   125,   126,
   127,   128,   129,   130,   131,   132,   133,   134,   135,   136,
   137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
   147,   148,   149,   150,   151,   152,   153,   154,   155,   156,
   157,   158,   159,   160,   161,   162,   163,   164,   165,   166,
   167,   168,   169,   170,   171,   172,   173,   174,   175,   176,
   177,   178,   179,   180,   181,   182,   183,   184,   185,   186,
   187,   188,   189,   190,   191,   192,   193,   194,   195,   196,
   197,   198,   199,   200,   201,   202,   203,   204,   205,   206,
   207,   208,   209,   210,   211,   212,   213,   214,   215,   216,
   217,   218,   219,   220
};

#if YYDEBUG != 0
static const short yyprhs[] = {     0,
     0,     2,     4,     6,     9,    13,    14,    16,    18,    20,
    22,    24,    26,    28,    30,    32,    34,    36,    38,    40,
    42,    44,    46,    48,    50,    52,    54,    56,    58,    60,
    62,    64,    66,    68,    70,    72,    74,    76,    78,    80,
    82,    84,    86,    88,    90,    92,    94,    96,    98,   100,
   102,   104,   106,   108,   110,   112,   114,   116,   118,   120,
   122,   124,   126,   128,   130,   134,   139,   142,   143,   147,
   152,   155,   159,   161,   165,   168,   170,   171,   173,   177,
   179,   183,   186,   188,   189,   191,   195,   197,   201,   204,
   206,   207,   209,   213,   215,   219,   222,   224,   225,   227,
   231,   233,   237,   240,   242,   243,   245,   249,   251,   255,
   258,   260,   261,   263,   267,   269,   273,   276,   278,   279,
   281,   285,   288,   289,   291,   293,   296,   299,   301,   304,
   307,   310,   312,   314,   316,   321,   324,   325,   328,   331,
   334,   337,   340,   343,   346,   349,   352,   355,   358,   360,
   365,   368,   369,   371,   374,   377,   380,   383,   386,   389,
   391,   396,   399,   400,   402,   404,   406,   408,   410,   412,
   414,   416,   418,   420,   423,   426,   429,   432,   435,   438,
   441,   444,   447,   450,   452,   457,   460,   461,   463,   466,
   468,   470,   472,   477,   480,   481,   484,   486,   491,   494,
   498,   499,   504,   507,   510,   511,   513,   515,   518,   520,
   522,   525,   528,   530,   535,   538,   539,   542,   544,   549,
   552,   553,   555,   557,   560,   563,   565,   570,   573,   574,
   577,   580,   583,   586,   588,   593,   596,   597,   599,   604,
   607,   608,   611,   614,   616,   621,   624,   625,   628,   631,
   634,   637,   640,   642,   647,   650,   651,   654,   657,   660,
   663,   666,   669,   671,   676,   679,   680,   683,   686,   689,
   692,   695,   697,   702,   705,   706,   708,   711,   714,   717,
   720,   723,   726,   729,   732,   735,   738,   741,   744,   747,
   750,   753,   756,   759,   762,   765,   767,   772,   775,   776,
   778,   780,   782,   784,   787,   790,   793,   796,   799,   802,
   805,   808,   811,   814,   816,   821,   824,   825,   828,   831,
   834,   836,   841,   844,   845,   847,   850,   853,   856,   859,
   862,   865,   868,   871,   874,   877,   881,   884,   885,   887,
   889,   891,   893,   898,   901,   902,   904,   907,   910,   913,
   915,   920,   923,   924,   926,   928,   930,   932,   935,   938,
   941,   944,   947,   950,   953,   956,   959,   962,   965,   968,
   971,   974,   977,   980,   983,   986,   989,   992,   995,   998,
  1000,  1005,  1008,  1009,  1012,  1015,  1018,  1021,  1024,  1027,
  1030,  1033,  1036,  1039,  1043,  1046,  1047,  1049,  1052,  1054,
  1056,  1058,  1063,  1066,  1067,  1069,  1071,  1074,  1077,  1080,
  1085,  1087,  1092,  1095,  1096,  1099,  1102,  1105,  1108,  1111,
  1114,  1116,  1121,  1124,  1125,  1127,  1130,  1133,  1136,  1139,
  1142,  1145,  1148,  1150,  1155,  1158,  1159,  1161,  1163,  1166,
  1169,  1172,  1175,  1178,  1180,  1185,  1188,  1189,  1192,  1194,
  1199,  1202,  1203,  1206,  1209,  1211,  1216,  1219,  1220,  1223,
  1226,  1228,  1233,  1236,  1237,  1239,  1242,  1245,  1248,  1250,
  1255,  1258,  1259,  1262,  1265,  1268,  1271,  1274,  1276,  1281,
  1284,  1285,  1288,  1291,  1294,  1297,  1300,  1303,  1306,  1308,
  1313,  1316,  1317,  1320,  1323,  1326,  1329,  1332,  1335,  1337,
  1342,  1345,  1346,  1349,  1352,  1354,  1359,  1362,  1363,  1366,
  1369,  1372,  1374,  1379,  1382,  1383,  1386,  1389,  1391,  1396,
  1399,  1400,  1402,  1405,  1408,  1411,  1415,  1419,  1423,  1427,
  1431,  1435,  1439,  1443,  1447,  1451,  1455,  1459,  1463,  1467,
  1471,  1475,  1479,  1483,  1487,  1491,  1495,  1499,  1503,  1507,
  1511,  1515,  1519,  1523,  1527,  1531,  1535,  1539,  1543,  1547,
  1551,  1555,  1560,  1565,  1570,  1575,  1580,  1585,  1591,  1596,
  1601,  1606,  1611,  1613,  1618,  1621,  1622,  1625,  1628,  1631,
  1634,  1637,  1640,  1642,  1647,  1650,  1651,  1654,  1657,  1660,
  1663,  1666,  1669,  1672,  1675,  1678,  1681,  1684,  1687,  1690,
  1692,  1697,  1700,  1701,  1704,  1706,  1711,  1714,  1715,  1718,
  1721,  1724,  1726,  1731,  1734,  1735,  1738,  1741,  1744,  1747,
  1750,  1753,  1756,  1759,  1762,  1765,  1767,  1772,  1775,  1776,
  1778,  1781,  1786,  1789,  1791,  1796,  1799,  1800,  1802,  1804,
  1807,  1810,  1813,  1816,  1819,  1822,  1824,  1829,  1832,  1833,
  1836,  1838,  1843,  1846,  1847,  1850,  1853,  1856,  1859,  1861,
  1866,  1869,  1870,  1873,  1876,  1879,  1882,  1885,  1887,  1892,
  1895,  1896,  1899,  1901,  1906,  1909,  1910,  1912,  1915,  1918,
  1921,  1924,  1927,  1929,  1931,  1933,  1938,  1941,  1942,  1945,
  1948,  1951,  1954,  1957,  1959,  1964,  1967,  1968,  1971,  1974,
  1977,  1979,  1984,  1987,  1988,  1990,  1993,  1996,  1998
};

static const short yyrhs[] = {   227,
     0,     1,     0,    25,     0,   235,   227,     0,   235,   221,
   227,     0,     0,   267,     0,   294,     0,   305,     0,   364,
     0,   386,     0,   392,     0,   478,     0,   504,     0,   315,
     0,   326,     0,   415,     0,   419,     0,   440,     0,   448,
     0,   334,     0,   428,     0,   469,     0,   444,     0,   496,
     0,   500,     0,   510,     0,   298,     0,   319,     0,   330,
     0,   343,     0,   351,     0,   377,     0,   381,     0,   436,
     0,   465,     0,   484,     0,   338,     0,   473,     0,   432,
     0,   276,     0,   232,     0,   453,     0,   457,     0,   461,
     0,   516,     0,   290,     0,   355,     0,   407,     0,   508,
     0,   233,     0,   234,     0,   360,     0,   229,     0,   230,
     0,   228,     0,   220,     0,     3,     0,     3,     0,     5,
     0,     4,     0,     3,     0,     4,     0,     3,     0,   239,
   239,   239,     0,   239,   239,   239,   239,     0,   236,   243,
     0,     0,     3,     3,     3,     0,   222,     0,   243,   223,
     0,   239,   239,     0,   239,   239,   239,     0,   241,     0,
   241,   221,   246,     0,   241,   246,     0,   221,     0,     0,
   241,     0,   222,   246,   223,     0,   236,     0,   236,   221,
   248,     0,   236,   248,     0,   221,     0,     0,   236,     0,
   222,   248,   223,     0,   239,     0,   239,   221,   250,     0,
   239,   250,     0,   221,     0,     0,   239,     0,   222,   250,
   223,     0,   238,     0,   238,   221,   252,     0,   238,   252,
     0,   221,     0,     0,   238,     0,   222,   252,   223,     0,
   244,     0,   244,   221,   254,     0,   244,   254,     0,   221,
     0,     0,   244,     0,   222,   254,   223,     0,   245,     0,
   245,   221,   256,     0,   245,   256,     0,   221,     0,     0,
   245,     0,   222,   256,   223,     0,   242,     0,   242,   221,
   258,     0,   242,   258,     0,   221,     0,     0,   242,     0,
   222,   258,   223,     0,   265,   260,     0,     0,    65,     0,
    66,     0,    97,   245,     0,    96,   245,     0,   300,     0,
   194,   238,     0,   261,   253,     0,   262,   253,     0,   263,
     0,   264,     0,     7,     0,   266,   224,   260,   225,     0,
   269,   268,     0,     0,    67,    19,     0,    67,   396,     0,
    67,   220,     0,    69,    19,     0,    69,   369,     0,    69,
   401,     0,    69,   424,     0,    69,   220,     0,    68,    19,
     0,    68,   492,     0,    68,   220,     0,     8,     0,   270,
   224,   268,   225,     0,   274,   272,     0,     0,    66,     0,
   194,   238,     0,    70,   237,     0,   129,   239,     0,    71,
   240,     0,    72,   240,     0,   273,   253,     0,     9,     0,
   275,   224,   272,   225,     0,   288,   277,     0,     0,    77,
     0,    78,     0,    79,     0,    80,     0,    81,     0,    82,
     0,    73,     0,    74,     0,    83,     0,    84,     0,   284,
   251,     0,   285,   247,     0,   278,   253,     0,   279,   253,
     0,   280,   253,     0,   281,   253,     0,   282,   253,     0,
   283,   253,     0,   286,   251,     0,   287,   247,     0,    10,
     0,   289,   224,   277,   225,     0,   292,   291,     0,     0,
   300,     0,    85,   245,     0,   263,     0,   264,     0,    11,
     0,   293,   224,   291,   225,     0,   296,   295,     0,     0,
   174,   245,     0,    12,     0,   297,   224,   295,   225,     0,
   235,   299,     0,   235,   221,   299,     0,     0,    64,   222,
   299,   223,     0,    64,   235,     0,   303,   301,     0,     0,
    88,     0,   300,     0,    86,   237,     0,   263,     0,   264,
     0,    88,   220,     0,   302,   235,     0,    13,     0,   304,
   224,   301,   225,     0,   307,   306,     0,     0,   172,   247,
     0,    14,     0,   308,   224,   306,   225,     0,   313,   310,
     0,     0,   123,     0,   124,     0,   311,   251,     0,   312,
   247,     0,    15,     0,   314,   224,   310,   225,     0,   317,
   316,     0,     0,    89,   237,     0,   128,   237,     0,   163,
   239,     0,   164,   239,     0,    20,     0,   318,   224,   316,
   225,     0,   165,   257,     0,     0,    16,     0,   321,   224,
   320,   225,     0,   324,   323,     0,     0,   311,   251,     0,
   312,   257,     0,    17,     0,   325,   224,   323,   225,     0,
   328,   327,     0,     0,    89,   237,     0,   128,   237,     0,
   162,   237,     0,   169,   239,     0,   164,   239,     0,    22,
     0,   329,   224,   327,   225,     0,   332,   331,     0,     0,
    90,   237,     0,    91,   239,     0,    92,   237,     0,    93,
   239,     0,    94,   239,     0,    95,   239,     0,    18,     0,
   333,   224,   331,   225,     0,   336,   335,     0,     0,   170,
   237,     0,   171,   239,     0,   172,   241,     0,   173,   245,
     0,    99,   239,     0,    23,     0,   337,   224,   335,   225,
     0,   341,   339,     0,     0,   164,     0,   172,    19,     0,
   172,   309,     0,   172,   220,     0,   100,    19,     0,   100,
   411,     0,   100,   220,     0,   101,    19,     0,   101,   488,
     0,   101,   220,     0,   340,   251,     0,   102,   237,     0,
   104,   239,     0,   122,   237,     0,   103,   237,     0,   105,
   237,     0,   106,   236,     0,   107,   239,     0,   108,   236,
     0,   109,   239,     0,    27,     0,   342,   224,   339,   225,
     0,   349,   344,     0,     0,   111,     0,   188,     0,   155,
     0,   113,     0,   110,   237,     0,   102,   237,     0,   127,
   237,     0,   104,   239,     0,   122,   237,     0,   345,   255,
     0,   112,   237,     0,   346,   259,     0,   347,   255,     0,
   348,   257,     0,    26,     0,   350,   224,   344,   225,     0,
   353,   352,     0,     0,   172,   241,     0,   114,   238,     0,
   115,   239,     0,    28,     0,   354,   224,   352,   225,     0,
   358,   356,     0,     0,   117,     0,   175,   238,     0,   116,
   237,     0,   357,   253,     0,   118,   238,     0,   119,   237,
     0,   174,   239,     0,   167,   239,     0,   176,   238,     0,
   120,   237,     0,    24,   224,     0,   359,   356,   225,     0,
   362,   361,     0,     0,   300,     0,   263,     0,   264,     0,
    33,     0,   363,   224,   361,   225,     0,   367,   365,     0,
     0,    66,     0,   366,   253,     0,   125,   237,     0,   126,
   237,     0,   121,     0,   368,   224,   365,   225,     0,   375,
   370,     0,     0,   131,     0,   132,     0,   133,     0,   160,
     0,   172,    19,     0,   172,   309,     0,   172,   220,     0,
   130,    19,     0,   130,   322,     0,   130,   220,     0,   100,
    19,     0,   100,   411,     0,   100,   220,     0,   101,    19,
     0,   101,   488,     0,   101,   220,     0,   102,   237,     0,
   127,   237,     0,   122,   237,     0,   104,   239,     0,   371,
   249,     0,   103,   237,     0,   372,   249,     0,   373,   249,
     0,   374,   249,     0,   105,   237,     0,    34,     0,   376,
   224,   370,   225,     0,   379,   378,     0,     0,   172,    19,
     0,   172,   309,     0,   172,   220,     0,   130,    19,     0,
   130,   322,     0,   130,   220,     0,   103,   237,     0,   371,
   249,     0,   372,   249,     0,    35,   224,     0,   380,   378,
   225,     0,   384,   382,     0,     0,    66,     0,   383,   253,
     0,   263,     0,   264,     0,    29,     0,   385,   224,   382,
   225,     0,   390,   387,     0,     0,   177,     0,   180,     0,
   388,   251,     0,   178,   245,     0,   389,   235,     0,   389,
   222,   227,   223,     0,    37,     0,   391,   224,   387,   225,
     0,   394,   393,     0,     0,    99,   239,     0,   181,   241,
     0,   183,   241,     0,   184,   239,     0,   182,   241,     0,
   185,   239,     0,    38,     0,   395,   224,   393,   225,     0,
   399,   397,     0,     0,    66,     0,    70,   237,     0,    75,
   239,     0,    71,   240,     0,    72,   240,     0,   398,   253,
     0,   125,   237,     0,   126,   237,     0,    30,     0,   400,
   224,   397,   225,     0,   405,   402,     0,     0,    76,     0,
   168,     0,   403,   251,     0,   161,   237,     0,    75,   239,
     0,   404,   253,     0,    98,   239,     0,    31,     0,   406,
   224,   402,   225,     0,   409,   408,     0,     0,   186,   257,
     0,    39,     0,   410,   224,   408,   225,     0,   413,   412,
     0,     0,   311,   251,     0,   312,   257,     0,    57,     0,
   414,   224,   412,   225,     0,   417,   416,     0,     0,   311,
   251,     0,   312,   259,     0,    58,     0,   418,   224,   416,
   225,     0,   422,   420,     0,     0,   192,     0,   421,   243,
     0,   125,   237,     0,   126,   237,     0,    32,     0,   423,
   224,   420,   225,     0,   426,   425,     0,     0,    90,   237,
     0,    92,   237,     0,   134,   244,     0,   135,   244,     0,
    95,   245,     0,    62,     0,   427,   224,   425,   225,     0,
   430,   429,     0,     0,    99,   239,     0,   136,   245,     0,
   172,   241,     0,   171,   239,     0,   189,   245,     0,   170,
   237,     0,   169,   239,     0,    59,     0,   431,   224,   429,
   225,     0,   434,   433,     0,     0,   172,    19,     0,   172,
   309,     0,   172,   220,     0,   130,    19,     0,   130,   322,
     0,   130,   220,     0,    60,     0,   435,   224,   433,   225,
     0,   438,   437,     0,     0,   311,   251,     0,   312,   257,
     0,    40,     0,   439,   224,   437,   225,     0,   442,   441,
     0,     0,   178,   245,     0,   174,   245,     0,    92,   237,
     0,    41,     0,   443,   224,   441,   225,     0,   446,   445,
     0,     0,   311,   251,     0,   312,   251,     0,    42,     0,
   447,   224,   445,   225,     0,   451,   449,     0,     0,    66,
     0,   450,   253,     0,   139,   237,     0,   140,   237,     0,
   218,   195,     6,     0,   218,   196,     6,     0,   218,   197,
     6,     0,   218,   198,     6,     0,   218,   199,     6,     0,
   218,   201,     6,     0,   218,   202,     6,     0,   218,   203,
     6,     0,   218,   204,     6,     0,   218,   205,     6,     0,
   218,   207,     6,     0,   218,   208,     6,     0,   218,   209,
     6,     0,   218,   210,     6,     0,   218,   212,     6,     0,
   218,   214,     6,     0,   218,   215,     6,     0,   218,   216,
     6,     0,   219,   195,     6,     0,   219,   196,     6,     0,
   219,   197,     6,     0,   219,   198,     6,     0,   219,   199,
     6,     0,   219,   201,     6,     0,   219,   202,     6,     0,
   219,   203,     6,     0,   219,   204,     6,     0,   219,   205,
     6,     0,   219,   207,     6,     0,   219,   208,     6,     0,
   219,   209,     6,     0,   219,   210,     6,     0,   219,   212,
     6,     0,   219,   214,     6,     0,   219,   215,     6,     0,
   219,   216,     6,     0,   217,   195,     6,   237,     0,   217,
   196,     6,   239,     0,   217,   197,     6,   236,     0,   217,
   198,     6,   240,     0,   217,   199,     6,   242,     0,   217,
   200,     6,    19,     0,   217,   200,     6,   220,     6,     0,
   217,   201,     6,   241,     0,   217,   203,     6,   238,     0,
   217,   204,     6,   244,     0,   217,   205,     6,   245,     0,
    43,     0,   452,   224,   449,   225,     0,   455,   454,     0,
     0,   137,    19,     0,   137,   271,     0,   137,   220,     0,
   138,    19,     0,   138,   231,     0,   138,   220,     0,    44,
     0,   456,   224,   454,   225,     0,   459,   458,     0,     0,
   173,   245,     0,   171,   239,     0,   189,   245,     0,   141,
   239,     0,   143,   239,     0,   142,   239,     0,   144,   239,
     0,   145,   239,     0,   146,    19,     0,   146,   276,     0,
   146,   401,     0,   146,   220,     0,   147,   237,     0,    45,
     0,   460,   224,   458,   225,     0,   463,   462,     0,     0,
   169,   239,     0,    61,     0,   464,   224,   462,   225,     0,
   467,   466,     0,     0,    90,   237,     0,    92,   237,     0,
    95,   242,     0,    47,     0,   468,   224,   466,   225,     0,
   471,   470,     0,     0,    99,   239,     0,   136,   245,     0,
   148,   239,     0,   172,   241,     0,   190,   239,     0,   173,
   245,     0,   171,   239,     0,   189,   245,     0,   170,   237,
     0,   169,   239,     0,    46,     0,   472,   224,   470,   225,
     0,   476,   474,     0,     0,   149,     0,   475,   235,     0,
   475,   222,   227,   223,     0,   150,   236,     0,    52,     0,
   477,   224,   474,   225,     0,   482,   479,     0,     0,   166,
     0,   152,     0,   480,   253,     0,   151,    19,     0,   151,
   360,     0,   151,   220,     0,   481,   251,     0,   153,   239,
     0,    48,     0,   483,   224,   479,   225,     0,   486,   485,
     0,     0,   165,   255,     0,    49,     0,   487,   224,   485,
   225,     0,   490,   489,     0,     0,   178,   244,     0,   154,
   239,     0,   155,   244,     0,   179,   244,     0,    50,     0,
   491,   224,   489,   225,     0,   494,   493,     0,     0,   156,
   240,     0,    92,   237,     0,    70,   237,     0,    71,   240,
     0,    72,   240,     0,    51,     0,   495,   224,   493,   225,
     0,   498,   497,     0,     0,    92,   237,     0,    53,     0,
   499,   224,   497,   225,     0,   502,   501,     0,     0,   300,
     0,   178,   245,     0,   154,   242,     0,   155,   245,     0,
   193,   242,     0,   179,   245,     0,   263,     0,   264,     0,
    63,     0,   503,   224,   501,   225,     0,   506,   505,     0,
     0,   157,   239,     0,   158,   237,     0,   188,   242,     0,
   187,   245,     0,   194,   238,     0,    54,     0,   507,   224,
   505,   225,     0,   510,   509,     0,     0,   178,   245,     0,
    92,   237,     0,   174,   245,     0,    55,     0,   511,   224,
   509,   225,     0,   514,   512,     0,     0,    36,     0,   513,
   253,     0,   159,   238,     0,    56,     0,   515,   224,   512,
   225,     0
};

#endif

#if YYDEBUG != 0
static const short yyrline[] = { 0,
    89,    90,    91,    95,    96,    97,   101,   102,   103,   104,
   105,   106,   107,   108,   112,   113,   114,   115,   116,   117,
   121,   122,   123,   124,   125,   126,   127,   131,   132,   133,
   134,   135,   136,   137,   138,   139,   140,   144,   145,   146,
   150,   151,   152,   153,   154,   155,   159,   160,   161,   162,
   166,   167,   168,   169,   170,   171,   172,   176,   183,   187,
   194,   198,   206,   207,   211,   221,   232,   233,   238,   247,
   251,   260,   270,   271,   272,   273,   274,   278,   279,   283,
   284,   285,   286,   287,   291,   292,   297,   298,   299,   300,
   301,   305,   306,   310,   311,   312,   313,   314,   318,   319,
   323,   324,   325,   326,   327,   331,   332,   336,   337,   338,
   339,   340,   344,   345,   349,   350,   351,   352,   353,   357,
   358,   368,   369,   373,   380,   387,   394,   401,   402,   407,
   411,   415,   416,   420,   430,   445,   446,   450,   451,   452,
   453,   454,   455,   456,   457,   458,   459,   460,   464,   474,
   489,   490,   494,   501,   505,   509,   513,   517,   521,   528,
   537,   552,   553,   557,   564,   571,   578,   585,   592,   599,
   606,   613,   620,   627,   631,   635,   639,   643,   647,   651,
   655,   659,   663,   670,   680,   695,   696,   700,   701,   705,
   706,   710,   720,   735,   736,   740,   747,   757,   772,   773,
   774,   778,   779,   789,   790,   794,   801,   802,   806,   807,
   808,   809,   816,   826,   841,   842,   846,   850,   860,   875,
   876,   880,   887,   894,   898,   905,   915,   930,   931,   935,
   939,   943,   947,   954,   964,   979,   980,   984,   994,  1009,
  1010,  1014,  1018,  1025,  1035,  1050,  1051,  1055,  1059,  1063,
  1067,  1071,  1078,  1088,  1103,  1104,  1108,  1112,  1116,  1120,
  1124,  1128,  1136,  1146,  1161,  1162,  1166,  1170,  1174,  1178,
  1182,  1189,  1199,  1214,  1215,  1219,  1227,  1228,  1229,  1230,
  1231,  1232,  1233,  1234,  1235,  1236,  1240,  1244,  1248,  1252,
  1256,  1260,  1264,  1268,  1272,  1279,  1289,  1304,  1305,  1309,
  1316,  1323,  1330,  1337,  1341,  1345,  1349,  1353,  1357,  1361,
  1365,  1369,  1373,  1380,  1390,  1405,  1406,  1410,  1414,  1418,
  1425,  1435,  1450,  1451,  1455,  1462,  1466,  1470,  1474,  1478,
  1482,  1486,  1490,  1494,  1501,  1511,  1526,  1527,  1531,  1532,
  1533,  1537,  1547,  1562,  1563,  1567,  1574,  1578,  1582,  1589,
  1599,  1614,  1615,  1619,  1626,  1633,  1640,  1647,  1648,  1649,
  1650,  1651,  1652,  1653,  1654,  1655,  1656,  1657,  1658,  1659,
  1663,  1667,  1671,  1675,  1679,  1683,  1687,  1691,  1695,  1702,
  1712,  1727,  1728,  1732,  1733,  1734,  1735,  1736,  1737,  1738,
  1742,  1746,  1753,  1763,  1778,  1779,  1783,  1790,  1794,  1795,
  1799,  1809,  1824,  1825,  1829,  1837,  1844,  1848,  1852,  1856,
  1863,  1873,  1888,  1889,  1893,  1897,  1901,  1905,  1909,  1913,
  1919,  1929,  1944,  1945,  1949,  1956,  1960,  1964,  1968,  1972,
  1976,  1980,  1987,  1997,  2012,  2013,  2017,  2024,  2031,  2035,
  2039,  2043,  2047,  2054,  2064,  2079,  2080,  2084,  2088,  2098,
  2113,  2114,  2118,  2122,  2129,  2139,  2154,  2155,  2159,  2163,
  2170,  2180,  2195,  2196,  2200,  2207,  2211,  2215,  2222,  2232,
  2247,  2248,  2252,  2256,  2260,  2264,  2268,  2275,  2285,  2301,
  2302,  2306,  2310,  2314,  2318,  2322,  2326,  2330,  2337,  2347,
  2362,  2363,  2367,  2368,  2369,  2370,  2371,  2372,  2377,  2387,
  2401,  2402,  2406,  2410,  2417,  2427,  2442,  2443,  2447,  2451,
  2455,  2462,  2472,  2487,  2488,  2492,  2496,  2503,  2513,  2528,
  2529,  2533,  2540,  2544,  2548,  2557,  2563,  2569,  2575,  2581,
  2595,  2601,  2607,  2613,  2619,  2630,  2636,  2642,  2648,  2662,
  2668,  2674,  2680,  2691,  2697,  2703,  2709,  2715,  2729,  2735,
  2741,  2747,  2753,  2764,  2770,  2776,  2782,  2796,  2802,  2808,
  2814,  2825,  2831,  2837,  2843,  2849,  2856,  2863,  2871,  2885,
  2891,  2897,  2907,  2917,  2933,  2934,  2938,  2939,  2940,  2941,
  2942,  2943,  2947,  2957,  2972,  2973,  2977,  2981,  2985,  2989,
  2993,  2997,  3001,  3005,  3009,  3010,  3011,  3012,  3013,  3020,
  3030,  3045,  3046,  3050,  3057,  3067,  3082,  3083,  3087,  3091,
  3095,  3102,  3112,  3127,  3128,  3132,  3136,  3140,  3144,  3148,
  3152,  3156,  3160,  3164,  3168,  3175,  3185,  3200,  3201,  3205,
  3212,  3216,  3220,  3228,  3238,  3253,  3254,  3258,  3265,  3272,
  3276,  3277,  3278,  3279,  3283,  3291,  3301,  3316,  3317,  3321,
  3326,  3336,  3351,  3352,  3356,  3360,  3364,  3368,  3376,  3386,
  3401,  3402,  3406,  3410,  3414,  3418,  3422,  3430,  3440,  3455,
  3456,  3460,  3467,  3477,  3492,  3493,  3497,  3498,  3502,  3506,
  3510,  3514,  3518,  3519,  3523,  3533,  3548,  3549,  3553,  3557,
  3561,  3565,  3569,  3576,  3586,  3601,  3602,  3606,  3610,  3614,
  3621,  3631,  3646,  3647,  3651,  3658,  3662,  3669,  3679
};
#endif


#if YYDEBUG != 0 || defined (YYERROR_VERBOSE)

static const char * const yytname[] = {   "$","error","$undefined.","NUMBER",
"FLOAT","STRING","NAME","ANCHOR","APPEARANCE","AUDIOCLIP","BACKGROUND","BILLBOARD",
"BOX","COLLISION","COLOR","COLOR_INTERP","COORDINATE","COORDINATE_INTERP","CYLINDER_SENSOR",
"NULL_STRING","CONE","CUBE","CYLINDER","DIRECTIONALLIGHT","FONTSTYLE","ERROR",
"EXTRUSION","ELEVATION_GRID","FOG","INLINE","MOVIE_TEXTURE","NAVIGATION_INFO",
"PIXEL_TEXTURE","GROUP","INDEXEDFACESET","INDEXEDLINESET","S_INFO","LOD","MATERIAL",
"NORMAL","POSITION_INTERP","PROXIMITY_SENSOR","SCALAR_INTERP","SCRIPT","SHAPE",
"SOUND","SPOTLIGHT","SPHERE_SENSOR","TEXT","TEXTURE_COORDINATE","TEXTURE_TRANSFORM",
"TIME_SENSOR","SWITCH","TOUCH_SENSOR","VIEWPOINT","VISIBILITY_SENSOR","WORLD_INFO",
"NORMAL_INTERP","ORIENTATION_INTERP","POINTLIGHT","POINTSET","SPHERE","PLANE_SENSOR",
"TRANSFORM","S_CHILDREN","S_PARAMETER","S_URL","S_MATERIAL","S_TEXTURETRANSFORM",
"S_TEXTURE","S_LOOP","S_STARTTIME","S_STOPTIME","S_GROUNDANGLE","S_GROUNDCOLOR",
"S_SPEED","S_AVATAR_SIZE","S_BACKURL","S_BOTTOMURL","S_FRONTURL","S_LEFTURL",
"S_RIGHTURL","S_TOPURL","S_SKYANGLE","S_SKYCOLOR","S_AXIS_OF_ROTATION","S_COLLIDE",
"S_COLLIDETIME","S_PROXY","S_SIDE","S_AUTO_OFFSET","S_DISK_ANGLE","S_ENABLED",
"S_MAX_ANGLE","S_MIN_ANGLE","S_OFFSET","S_BBOXSIZE","S_BBOXCENTER","S_VISIBILITY_LIMIT",
"S_AMBIENT_INTENSITY","S_NORMAL","S_TEXCOORD","S_CCW","S_COLOR_PER_VERTEX","S_CREASE_ANGLE",
"S_NORMAL_PER_VERTEX","S_XDIMENSION","S_XSPACING","S_ZDIMENSION","S_ZSPACING",
"S_BEGIN_CAP","S_CROSS_SECTION","S_END_CAP","S_SPINE","S_FOG_TYPE","S_VISIBILITY_RANGE",
"S_HORIZONTAL","S_JUSTIFY","S_LANGUAGE","S_LEFT2RIGHT","S_TOP2BOTTOM","IMAGE_TEXTURE",
"S_SOLID","S_KEY","S_KEYVALUE","S_REPEAT_S","S_REPEAT_T","S_CONVEX","S_BOTTOM",
"S_PICTH","S_COORD","S_COLOR_INDEX","S_COORD_INDEX","S_NORMAL_INDEX","S_MAX_POSITION",
"S_MIN_POSITION","S_ATTENUATION","S_APPEARANCE","S_GEOMETRY","S_DIRECT_OUTPUT",
"S_MUST_EVALUATE","S_MAX_BACK","S_MIN_BACK","S_MAX_FRONT","S_MIN_FRONT","S_PRIORITY",
"S_SOURCE","S_SPATIALIZE","S_BERM_WIDTH","S_CHOICE","S_WHICHCHOICE","S_FONTSTYLE",
"S_LENGTH","S_MAX_EXTENT","S_ROTATION","S_SCALE","S_CYCLE_INTERVAL","S_FIELD_OF_VIEW",
"S_JUMP","S_TITLE","S_TEXCOORD_INDEX","S_HEADLIGHT","S_TOP","S_BOTTOMRADIUS",
"S_HEIGHT","S_POINT","S_STRING","S_SPACING","S_TYPE","S_RADIUS","S_ON","S_INTENSITY",
"S_COLOR","S_DIRECTION","S_SIZE","S_FAMILY","S_STYLE","S_RANGE","S_CENTER","S_TRANSLATION",
"S_LEVEL","S_DIFFUSECOLOR","S_SPECULARCOLOR","S_EMISSIVECOLOR","S_SHININESS",
"S_TRANSPARENCY","S_VECTOR","S_POSITION","S_ORIENTATION","S_LOCATION","S_CUTOFFANGLE",
"S_WHICHCHILD","S_IMAGE","S_SCALEORIENTATION","S_DESCRIPTION","SFBOOL","SFFLOAT",
"SFINT32","SFTIME","SFROTATION","SFNODE","SFCOLOR","SFIMAGE","SFSTRING","SFVEC2F",
"SFVEC3F","MFBOOL","MFFLOAT","MFINT32","MFTIME","MFROTATION","MFNODE","MFCOLOR",
"MFIMAGE","MFSTRING","MFVEC2F","MFVEC3F","FIELD","EVENTIN","EVENTOUT","USE",
"','","'['","']'","'{'","'}'","Vrml","VrmlNodes","GroupingNode","InterpolatorNode",
"SensorNode","GeometryNode","LightNode","CommonNode","BindableNode","SFNode",
"SFInt32","SFBool","SFString","SFFloat","SFTime","SFColor","SFRotation","SFImageList",
"SFVec2f","SFVec3f","SFColorList","MFColor","SFInt32List","MFInt32","SFFloatList",
"MFFloat","SFStringList","MFString","SFVec2fList","MFVec2f","SFVec3fList","MFVec3f",
"SFRotationList","MFRotation","AnchorElements","AnchorElementParameterBegin",
"AnchorElementURLBegin","bboxCenter","bboxSize","AnchorElement","AnchorBegin",
"Anchor","AppearanceNodes","AppearanceNode","AppearanceBegin","Appearance","AudioClipElements",
"AudioClipURL","AudioClipElement","AudioClipBegin","AudioClip","BackGroundElements",
"BackGroundBackURL","BackGroundBottomURL","BackGroundFrontURL","BackGroundLeftURL",
"BackGroundRightURL","BackGroundTopURL","BackGroundGroundAngle","BackGroundGroundColor",
"BackGroundSkyAngle","BackGroundSkyColor","BackGroundElement","BackgroundBegin",
"Background","BillboardElements","BillboardElement","BillboardBegin","Billboard",
"BoxElements","BoxElement","BoxBegin","Box","childrenElements","children","CollisionElements",
"CollisionElementProxyBegin","CollisionElement","CollisionBegin","Collision",
"ColorElements","ColorElement","ColorBegin","Color","ColorInterpElements","InterpolateKey",
"InterporlateKeyValue","ColorInterpElement","ColorInterpBegin","ColorInterp",
"ConeElements","ConeElement","ConeBegin","Cone","CoordinateElements","CoordinateBegin",
"Coordinate","CoordinateInterpElements","CoordinateInterpElement","CoordinateInterpBegin",
"CoordinateInterp","CylinderElements","CylinderElement","CylinderBegin","Cylinder",
"CylinderSensorElements","CylinderSensorElement","CylinderSensorBegin","CylinderSensor",
"DirLightElements","DirLightElement","DirLightBegin","DirLight","ElevationGridElements",
"ElevationGridHeight","ElevationGridElement","ElevationGridBegin","ElevationGrid",
"ExtrusionElements","ExtrusionCrossSection","ExtrusionOrientation","ExtrusionScale",
"ExtrusionSpine","ExtrusionElement","ExtrusionBegin","Extrusion","FogElements",
"FogElement","FogBegin","Fog","FontStyleElements","FontStyleJustify","FontStyleElement",
"FontStyleBegin","FontStyle","GroupElements","GroupElement","GroupBegin","Group",
"ImgTexElements","ImgTexURL","ImgTexElement","ImageTextureBegin","ImageTexture",
"IdxFacesetElements","ColorIndex","CoordIndex","NormalIndex","TextureIndex",
"IdxFacesetElement","IdxFacesetBegin","IdxFaceset","IdxLinesetElements","IdxLinesetElement",
"IdxLinesetBegin","IdxLineset","InlineElements","InlineURL","InlineElement",
"InlineBegin","Inline","LodElements","LodRange","LodLevel","LodElement","LodBegin",
"Lod","MaterialElements","MaterialElement","MaterialBegin","Material","MovieTextureElements",
"MovieTextureURL","MovieTextureElement","MovieTextureBegin","MovieTexture","NavigationInfoElements",
"NavigationInfoAvatarSize","NavigationInfoType","NavigationInfoElement","NavigationInfoBegin",
"NavigationInfo","NormalElements","NormalElement","NormalBegin","Normal","NormalInterpElements",
"NormalInterpElement","NormalInterpBegin","NormalInterp","OrientationInterpElements",
"OrientationInterpElement","OrientationInterpBegin","OrientationInterp","PixelTextureElements",
"PixelTextureImage","PixelTextureElement","PixelTextureBegin","PixelTexture",
"PlaneSensorElements","PlaneSensorElement","PlaneSensorBegin","PlaneSensor",
"PointLightNodes","PointLightNode","PointLightBegin","PointLight","PointsetElements",
"PointsetElement","PointsetBegin","Pointset","PositionInterpElements","PositionInterpElement",
"PositionInterpBegin","PositionInterp","ProximitySensorElements","ProximitySensorElement",
"ProximitySensorBegin","ProximitySensor","ScalarInterpElements","ScalarInterpElement",
"ScalarInterpBegin","ScalarInterp","ScriptElements","ScriptURL","ScriptElement",
"ScriptBegin","Script","SharpElements","SharpElement","ShapeBegin","Shape","SoundElements",
"SoundElement","SoundBegin","Sound","SphereElements","SphereElement","SphereBegin",
"Sphere","SphereSensorElements","SphereSensorElement","SphereSensorBegin","SphereSensor",
"SpotLightElements","SpotLightElement","SpotLightBegin","SpotLight","SwitchElements",
"SwitchChoice","SwitchElement","SwitchBegin","Switch","TextElements","TextString",
"TextLength","TextElement","TextBegin","Text","TexCoordElements","TexCoordElement",
"TexCoordBegin","TexCoordinate","TextureTransformElements","TextureTransformElement",
"TexTransformBegin","TexTransform","TimeSensorElements","TimeSensorElement",
"TimeSensorBegin","TimeSensor","TouchSensorElements","TouchSensorElement","TouchSensorBegin",
"TouchSensor","TransformElements","TransformElement","TransformBegin","Transform",
"ViewpointElements","ViewpointElement","ViewpointBegin","Viewpoint","VisibilitySensors",
"VisibilitySensor","VisibilitySensorBegine","WorldInfoElements","WorldInfoInfo",
"WorldInfoElement","WorldInfoBegin","WorldInfo", NULL
};
#endif

static const short yyr1[] = {     0,
   226,   226,   226,   227,   227,   227,   228,   228,   228,   228,
   228,   228,   228,   228,   229,   229,   229,   229,   229,   229,
   230,   230,   230,   230,   230,   230,   230,   231,   231,   231,
   231,   231,   231,   231,   231,   231,   231,   232,   232,   232,
   233,   233,   233,   233,   233,   233,   234,   234,   234,   234,
   235,   235,   235,   235,   235,   235,   235,   236,   237,   238,
   239,   239,   240,   240,   241,   242,   243,   243,    -1,    -1,
   244,   245,   246,   246,   246,   246,   246,   247,   247,   248,
   248,   248,   248,   248,   249,   249,   250,   250,   250,   250,
   250,   251,   251,   252,   252,   252,   252,   252,   253,   253,
   254,   254,   254,   254,   254,   255,   255,   256,   256,   256,
   256,   256,   257,   257,   258,   258,   258,   258,   258,   259,
   259,   260,   260,   261,   262,   263,   264,   265,   265,   265,
   265,   265,   265,   266,   267,   268,   268,   269,   269,   269,
   269,   269,   269,   269,   269,   269,   269,   269,   270,   271,
   272,   272,   273,   274,   274,   274,   274,   274,   274,   275,
   276,   277,   277,   278,   279,   280,   281,   282,   283,   284,
   285,   286,   287,   288,   288,   288,   288,   288,   288,   288,
   288,   288,   288,   289,   290,   291,   291,   292,   292,   292,
   292,   293,   294,   295,   295,   296,   297,   298,   299,   299,
   299,   300,   300,   301,   301,   302,   303,   303,   303,   303,
   303,   303,   304,   305,   306,   306,   307,   308,   309,   310,
   310,   311,   312,   313,   313,   314,   315,   316,   316,   317,
   317,   317,   317,   318,   319,   320,   320,   321,   322,   323,
   323,   324,   324,   325,   326,   327,   327,   328,   328,   328,
   328,   328,   329,   330,   331,   331,   332,   332,   332,   332,
   332,   332,   333,   334,   335,   335,   336,   336,   336,   336,
   336,   337,   338,   339,   339,   340,   341,   341,   341,   341,
   341,   341,   341,   341,   341,   341,   341,   341,   341,   341,
   341,   341,   341,   341,   341,   342,   343,   344,   344,   345,
   346,   347,   348,   349,   349,   349,   349,   349,   349,   349,
   349,   349,   349,   350,   351,   352,   352,   353,   353,   353,
   354,   355,   356,   356,   357,   358,   358,   358,   358,   358,
   358,   358,   358,   358,   359,   360,   361,   361,   362,   362,
   362,   363,   364,   365,   365,   366,   367,   367,   367,   368,
   369,   370,   370,   371,   372,   373,   374,   375,   375,   375,
   375,   375,   375,   375,   375,   375,   375,   375,   375,   375,
   375,   375,   375,   375,   375,   375,   375,   375,   375,   376,
   377,   378,   378,   379,   379,   379,   379,   379,   379,   379,
   379,   379,   380,   381,   382,   382,   383,   384,   384,   384,
   385,   386,   387,   387,   388,   389,   390,   390,   390,   390,
   391,   392,   393,   393,   394,   394,   394,   394,   394,   394,
   395,   396,   397,   397,   398,   399,   399,   399,   399,   399,
   399,   399,   400,   401,   402,   402,   403,   404,   405,   405,
   405,   405,   405,   406,   407,   408,   408,   409,   410,   411,
   412,   412,   413,   413,   414,   415,   416,   416,   417,   417,
   418,   419,   420,   420,   421,   422,   422,   422,   423,   424,
   425,   425,   426,   426,   426,   426,   426,   427,   428,   429,
   429,   430,   430,   430,   430,   430,   430,   430,   431,   432,
   433,   433,   434,   434,   434,   434,   434,   434,   435,   436,
   437,   437,   438,   438,   439,   440,   441,   441,   442,   442,
   442,   443,   444,   445,   445,   446,   446,   447,   448,   449,
   449,   450,   451,   451,   451,   451,   451,   451,   451,   451,
   451,   451,   451,   451,   451,   451,   451,   451,   451,   451,
   451,   451,   451,   451,   451,   451,   451,   451,   451,   451,
   451,   451,   451,   451,   451,   451,   451,   451,   451,   451,
   451,   451,   451,   451,   451,   451,   451,   451,   451,   451,
   451,   451,   452,   453,   454,   454,   455,   455,   455,   455,
   455,   455,   456,   457,   458,   458,   459,   459,   459,   459,
   459,   459,   459,   459,   459,   459,   459,   459,   459,   460,
   461,   462,   462,   463,   464,   465,   466,   466,   467,   467,
   467,   468,   469,   470,   470,   471,   471,   471,   471,   471,
   471,   471,   471,   471,   471,   472,   473,   474,   474,   475,
   476,   476,   476,   477,   478,   479,   479,   480,   481,   482,
   482,   482,   482,   482,   482,   483,   484,   485,   485,   486,
   487,   488,   489,   489,   490,   490,   490,   490,   491,   492,
   493,   493,   494,   494,   494,   494,   494,   495,   496,   497,
   497,   498,   499,   500,   501,   501,   502,   502,   502,   502,
   502,   502,   502,   502,   503,   504,   505,   505,   506,   506,
   506,   506,   506,   507,   508,   509,   509,   510,   510,   510,
   511,   510,   512,   512,   513,   514,   514,   515,   516
};

static const short yyr2[] = {     0,
     1,     1,     1,     2,     3,     0,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     3,     4,     2,     0,     3,     4,
     2,     3,     1,     3,     2,     1,     0,     1,     3,     1,
     3,     2,     1,     0,     1,     3,     1,     3,     2,     1,
     0,     1,     3,     1,     3,     2,     1,     0,     1,     3,
     1,     3,     2,     1,     0,     1,     3,     1,     3,     2,
     1,     0,     1,     3,     1,     3,     2,     1,     0,     1,
     3,     2,     0,     1,     1,     2,     2,     1,     2,     2,
     2,     1,     1,     1,     4,     2,     0,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     1,     4,
     2,     0,     1,     2,     2,     2,     2,     2,     2,     1,
     4,     2,     0,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     1,     4,     2,     0,     1,     2,     1,
     1,     1,     4,     2,     0,     2,     1,     4,     2,     3,
     0,     4,     2,     2,     0,     1,     1,     2,     1,     1,
     2,     2,     1,     4,     2,     0,     2,     1,     4,     2,
     0,     1,     1,     2,     2,     1,     4,     2,     0,     2,
     2,     2,     2,     1,     4,     2,     0,     1,     4,     2,
     0,     2,     2,     1,     4,     2,     0,     2,     2,     2,
     2,     2,     1,     4,     2,     0,     2,     2,     2,     2,
     2,     2,     1,     4,     2,     0,     2,     2,     2,     2,
     2,     1,     4,     2,     0,     1,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     4,     2,     0,     1,
     1,     1,     1,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     1,     4,     2,     0,     2,     2,     2,
     1,     4,     2,     0,     1,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     3,     2,     0,     1,     1,
     1,     1,     4,     2,     0,     1,     2,     2,     2,     1,
     4,     2,     0,     1,     1,     1,     1,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
     4,     2,     0,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     3,     2,     0,     1,     2,     1,     1,
     1,     4,     2,     0,     1,     1,     2,     2,     2,     4,
     1,     4,     2,     0,     2,     2,     2,     2,     2,     2,
     1,     4,     2,     0,     1,     2,     2,     2,     2,     2,
     2,     2,     1,     4,     2,     0,     1,     1,     2,     2,
     2,     2,     2,     1,     4,     2,     0,     2,     1,     4,
     2,     0,     2,     2,     1,     4,     2,     0,     2,     2,
     1,     4,     2,     0,     1,     2,     2,     2,     1,     4,
     2,     0,     2,     2,     2,     2,     2,     1,     4,     2,
     0,     2,     2,     2,     2,     2,     2,     2,     1,     4,
     2,     0,     2,     2,     2,     2,     2,     2,     1,     4,
     2,     0,     2,     2,     1,     4,     2,     0,     2,     2,
     2,     1,     4,     2,     0,     2,     2,     1,     4,     2,
     0,     1,     2,     2,     2,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     4,     4,     4,     4,     4,     4,     5,     4,     4,
     4,     4,     1,     4,     2,     0,     2,     2,     2,     2,
     2,     2,     1,     4,     2,     0,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     1,
     4,     2,     0,     2,     1,     4,     2,     0,     2,     2,
     2,     1,     4,     2,     0,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     4,     2,     0,     1,
     2,     4,     2,     1,     4,     2,     0,     1,     1,     2,
     2,     2,     2,     2,     2,     1,     4,     2,     0,     2,
     1,     4,     2,     0,     2,     2,     2,     2,     1,     4,
     2,     0,     2,     2,     2,     2,     2,     1,     4,     2,
     0,     2,     1,     4,     2,     0,     1,     2,     2,     2,
     2,     2,     1,     1,     1,     4,     2,     0,     2,     2,
     2,     2,     2,     1,     4,     2,     0,     2,     2,     2,
     1,     4,     2,     0,     1,     2,     2,     1,     4
};

static const short yydefact[] = {     0,
     2,   134,   160,   184,   192,   213,   226,   244,   263,   272,
     0,     3,   321,   401,   444,   342,   411,   505,   512,   518,
   573,   583,   600,   626,   612,   668,   634,   673,   694,   701,
   708,   455,   461,   489,   478,   685,     0,     0,     0,    57,
     1,    56,    54,    55,    42,    51,    52,     6,     0,     7,
     0,    41,     0,    47,     0,     8,     0,     9,     0,    15,
     0,    16,     0,    21,     0,    38,     0,    48,   324,    53,
     0,    10,     0,    11,     0,    12,     0,    49,     0,    17,
     0,    18,     0,    22,     0,    40,     0,    19,     0,    24,
     0,    20,     0,    43,     0,    44,     0,    45,     0,    23,
     0,    39,     0,    13,     0,    25,     0,    26,     0,    14,
     0,    50,    27,     0,     0,    46,   335,    59,   699,    62,
    61,     0,   700,   698,     6,     4,   123,   152,   163,   187,
   205,   221,   241,   256,   266,   317,     0,   325,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   324,   338,   396,
   404,   436,   452,   458,   472,   481,   502,   508,   515,   521,
   576,   586,   608,   615,   629,   662,   671,   676,   688,   697,
   704,     0,     5,     0,   124,   125,     0,     0,     0,     0,
     0,     0,   132,   133,   123,   128,   153,     0,     0,     0,
     0,     0,     0,     0,   152,   170,   171,   164,   165,   166,
   167,   168,   169,   172,   173,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   163,     0,   190,   191,
     0,   187,   188,     0,   206,   209,   210,   207,     0,     0,
   205,   222,   223,     0,     0,     0,   221,     0,     0,     0,
   241,     0,     0,     0,     0,     0,     0,     0,   256,     0,
     0,     0,     0,     0,     0,   266,     0,     0,     0,     0,
   317,   327,    60,   329,   330,   334,   332,   331,   326,   333,
   336,    98,    99,   328,   323,   340,   341,   339,     0,   338,
   397,   399,   400,     0,     0,   396,   405,     0,   406,     0,
     0,     0,   404,     0,   437,     0,     0,   438,     0,     0,
     0,   436,     0,     0,     0,   452,     0,     0,     0,   458,
     0,     0,     0,     0,     0,     0,   472,     0,     0,     0,
     0,     0,     0,     0,     0,   481,     0,     0,     0,   502,
     0,     0,     0,     0,   508,     0,     0,     0,   515,   522,
     0,     0,     0,     0,     0,     0,     0,   521,     0,     0,
     0,   576,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   586,     0,     0,     0,     0,   608,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   615,   630,     0,     0,     0,   629,     0,     0,     0,     0,
     0,     0,   662,     0,     0,   671,     0,     0,     0,     0,
     0,   683,   684,   677,     0,   676,     0,     0,     0,     0,
     0,     0,   688,     0,   697,   705,     0,     0,     0,   704,
    72,   201,   203,   127,   126,   129,   135,   130,   131,   122,
   155,    64,    63,   157,   158,   156,   154,   161,   159,   151,
   185,   176,   177,   178,   179,   180,   181,    91,    92,   174,
    77,     0,    78,   175,   182,   183,   162,   189,   193,   186,
   208,   211,   214,   212,   204,   227,   224,   225,   220,   242,
   112,   113,   243,   245,   240,   257,   258,   259,   260,   261,
   262,   264,   255,   271,   267,   268,   269,   270,   273,   265,
   319,   320,   318,   322,   316,    97,    94,     0,   343,   337,
   402,   398,   395,   408,   412,   407,     6,   409,   403,   441,
   443,   440,   445,   439,   442,   435,   453,   454,   456,   451,
   459,   119,     0,   120,   460,   462,   457,   473,   474,   477,
     0,   475,   476,   479,   471,   482,   483,   488,   487,   485,
   484,   486,   490,   480,   503,   504,   506,   501,   511,   510,
   509,   513,   507,   516,   517,   519,   514,   524,   525,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   574,   523,   520,   149,   577,
   579,     0,   578,   197,   580,   234,   253,   314,   296,   380,
     0,   646,   499,   605,   582,   581,     0,    28,     0,    29,
     0,    30,     0,    31,     0,    32,     0,    33,   383,    34,
     0,    35,     0,    36,     0,    37,   584,   575,   590,   592,
   591,   593,   594,   595,   433,   598,   596,     0,   597,   599,
   588,   587,   589,   601,   585,   609,   610,   611,   613,   607,
   616,   617,   618,   625,   624,   622,   619,   621,   623,   620,
   627,   614,    58,   633,   635,     6,   631,   628,   665,   666,
   667,   664,   663,   669,   661,   672,   674,   670,   679,   680,
   678,   682,   681,   686,   675,   689,   690,   692,   691,   693,
   695,   687,   702,   696,   707,   709,   706,   703,   201,     0,
    90,    87,     0,    76,    73,     0,     0,   111,   108,     0,
    97,    96,   100,     0,   118,   115,     0,     0,    71,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   526,
   527,   528,   529,   530,   531,   532,   533,   534,   535,   536,
   537,   538,   539,   540,   541,   542,   543,   544,   545,   546,
   547,   548,   549,   550,   551,   552,   553,   554,   555,   556,
   557,   558,   559,   560,   561,   137,   393,   195,   229,   247,
   275,   299,   353,     0,     0,   354,   355,     0,     0,     0,
     0,   383,   492,   603,   637,   424,     0,   201,   199,   202,
    90,    89,    93,    76,    75,    79,    65,   111,   110,   114,
    95,   410,   118,   117,   121,     0,   562,   563,   564,   565,
   566,   567,     0,   569,   570,   571,   572,     0,     0,     0,
     0,   137,     0,     0,   195,     0,     0,     0,     0,     0,
   229,     0,     0,     0,     0,     0,     0,   247,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   276,
     0,     0,     0,   275,     0,     0,     0,   300,     0,   303,
     0,     0,   302,   301,     0,     0,     0,     0,     0,   299,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   356,
   357,     0,     0,     0,     0,     0,     0,   353,   390,   238,
   387,   389,     0,   388,   218,   384,   386,     0,   385,    84,
    85,   391,   392,   394,   382,     0,     0,     0,   492,     0,
     0,   603,     0,   639,     0,   638,     0,     0,     0,   637,
   425,     0,     0,     0,     0,     0,     0,     0,     0,   424,
   632,   200,    88,    74,   109,   116,    66,   568,   138,   421,
   140,     0,   139,   146,   659,   148,     0,   147,   141,   469,
   350,   145,     0,   142,   143,     0,   144,   150,   136,   196,
   198,   194,   230,   231,   232,   233,   235,   228,   248,   249,
   250,   252,   251,   254,   246,   280,   449,   282,     0,   281,
   283,   651,   285,     0,   284,   287,   290,   288,   291,   292,
   293,   294,   295,   289,   277,   279,   278,   297,   286,   274,
   305,   307,   304,   310,   308,   306,   315,   105,   106,   309,
   311,   312,   313,   298,   364,   366,   365,   367,   369,   368,
   370,   375,   373,   379,   372,   371,   361,   363,   362,   358,
   360,   359,   381,   374,   376,   377,   378,   352,   237,   216,
    83,    80,     0,   496,   498,   497,   493,   495,   494,   500,
   491,   604,   606,   602,   641,   643,   642,   645,   647,   640,
   644,   636,   426,   428,   429,   427,   431,   432,   434,   430,
   423,   414,   654,   345,   464,   447,   649,   104,   101,     0,
     0,     0,     0,     0,   216,    83,    82,    86,     0,     0,
     0,     0,     0,     0,     0,   414,     0,     0,     0,     0,
     0,   654,   346,     0,     0,     0,     0,   345,     0,     0,
   465,     0,    68,   464,     0,     0,   447,     0,     0,   649,
   104,   103,   107,   236,   239,   217,   219,   215,    81,   415,
   416,   419,   417,   418,   420,   422,   413,   656,   657,   655,
   658,   660,   653,   348,   349,   351,   347,   344,   467,   468,
   470,    68,   466,   463,   448,   450,   446,   650,   652,   648,
   102,    67,     0,     0,     0
};

static const short yydefgoto[] = {  1183,
    41,    42,    43,    44,   626,    45,    46,    47,    48,   921,
   119,   273,   122,   434,   453,   736,  1173,  1029,   472,   726,
   454,  1063,   922,   723,   450,   498,   274,  1100,  1030,   730,
   473,   737,   525,   180,   181,   182,   183,   184,   185,    49,
    50,   841,   842,   612,   613,   193,   194,   195,    51,    52,
   206,   207,   208,   209,   210,   211,   212,   213,   214,   215,
   216,   217,    53,    54,   221,   222,    55,    56,   844,   845,
   627,   628,   720,   186,   229,   230,   231,    57,    58,  1104,
  1105,   918,   919,   234,   235,   236,   237,    59,    60,   850,
   851,   629,   630,  1102,   913,   914,   240,   241,    61,    62,
   857,   858,   631,   632,   248,   249,    63,    64,   255,   256,
    65,    66,   872,   873,   874,   633,   634,   885,   886,   887,
   888,   889,   890,   635,   636,   260,   261,    67,    68,   146,
   147,   148,    69,    70,   279,   280,    71,    72,  1126,  1127,
  1128,   973,   974,   903,   799,   800,   906,   907,   908,   637,
   638,   801,   802,   639,   640,   284,   285,   286,    73,    74,
   290,   291,   292,   293,    75,    76,  1115,  1116,   962,   963,
   948,   949,   950,   658,   659,   299,   300,   301,   302,    77,
    78,  1136,  1137,   999,  1000,   305,   306,    79,    80,   309,
   310,    81,    82,  1132,  1133,  1134,   976,   977,   316,   317,
    83,    84,   325,   326,    85,    86,   928,   929,   641,   642,
   329,   330,    87,    88,   334,   335,    89,    90,   338,   339,
    91,    92,   346,   347,   348,    93,    94,   351,   352,    95,
    96,   363,   364,    97,    98,   931,   932,   643,   644,   368,
   369,    99,   100,   380,   381,   101,   102,   384,   385,   386,
   103,   104,   937,   938,   939,   940,   645,   646,  1139,  1140,
  1004,  1005,  1121,  1122,   967,   968,   392,   393,   105,   106,
   395,   396,   107,   108,   405,   406,   109,   110,   412,   413,
   111,   112,   414,   113,   114,   418,   419,   420,   115,   116
};

static const short yypact[] = {  1011,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
  -190,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,    33,   384,   384,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1269,  -124,-32768,
  -121,-32768,   -81,-32768,   -59,-32768,   -56,-32768,   -51,-32768,
   -40,-32768,   -25,-32768,   -16,-32768,   -11,-32768,   474,-32768,
    -7,-32768,    -3,-32768,     5,-32768,    45,-32768,    57,-32768,
   107,-32768,   125,-32768,   130,-32768,   132,-32768,   136,-32768,
   171,-32768,   173,-32768,   187,-32768,   202,-32768,   204,-32768,
   207,-32768,   209,-32768,   221,-32768,   227,-32768,   234,-32768,
   241,-32768,-32768,   252,   255,-32768,-32768,-32768,-32768,-32768,
-32768,   384,-32768,-32768,  1413,-32768,   123,   346,   706,   478,
   513,   167,   167,   615,   351,   262,    33,-32768,   172,    33,
    33,   384,   384,   172,   172,    49,     4,   474,   -32,   342,
   353,   298,   167,   167,   512,   337,   167,    17,   167,   175,
   277,   574,   217,   381,   292,   391,   203,   356,   555,   269,
    -1,   384,-32768,  1068,-32768,-32768,   384,   384,   172,   212,
     4,     4,-32768,-32768,   123,-32768,-32768,    33,   509,   509,
   384,   172,   259,     4,   346,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   265,     4,     4,     4,     4,
     4,     4,    20,    34,    20,    34,   706,   384,-32768,-32768,
   267,   478,-32768,    33,   105,-32768,-32768,-32768,   271,  1413,
   513,-32768,-32768,   273,    20,    34,   167,    20,    44,   275,
   167,    33,   384,    33,   384,   384,   384,   280,   615,   384,
    33,   384,   384,   384,   295,   351,   172,   384,   384,   302,
   262,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,    28,-32768,-32768,-32768,-32768,-32768,-32768,   312,   -32,
-32768,-32768,-32768,   313,     4,   342,-32768,   384,-32768,   316,
    20,  1155,   353,   384,-32768,   384,    33,-32768,   323,    20,
     4,   298,    20,    44,   330,   167,    20,    48,   332,   167,
    33,    33,   384,   384,   384,   334,   512,   384,   384,   384,
    33,   384,   384,   384,   339,   337,    20,    44,   359,   167,
    33,   384,   384,   360,    17,    20,    20,   363,   167,-32768,
    33,    33,   629,   645,   741,   373,     4,   175,    51,   284,
   383,   277,   384,   384,   384,   384,   384,    67,    33,   384,
   384,   384,   387,   574,    33,    33,   384,   388,   217,   384,
   384,   384,   384,    33,   384,   384,   384,   384,   384,   390,
   381,-32768,   485,   392,  1212,   292,    33,   509,   509,    33,
   509,   393,   391,    33,   395,   203,   384,   384,   384,   384,
   384,-32768,-32768,-32768,   396,   356,   384,    33,   384,   384,
   172,   398,   555,   400,   269,-32768,   172,   402,     4,    -1,
-32768,  1413,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,     9,-32768,-32768,
    15,   384,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
    26,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,    41,   406,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,  1413,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,    52,   384,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   384,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   570,
   573,   600,   630,   631,   632,   634,   636,   637,   639,   647,
   648,   649,   650,   652,   653,   658,   669,   671,   672,   673,
   677,   678,   682,   685,   687,   688,   689,   690,   691,   693,
   694,   697,   705,   716,   717,   719,   722,   725,   726,   732,
   733,   734,   742,   744,   746,-32768,-32768,-32768,-32768,-32768,
-32768,   529,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
   531,-32768,-32768,-32768,-32768,-32768,   534,-32768,   535,-32768,
   536,-32768,   543,-32768,   544,-32768,   545,-32768,   371,-32768,
   546,-32768,   548,-32768,   550,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,   551,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,  1413,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  1356,   553,
-32768,    59,   554,-32768,    63,   558,   384,-32768,    68,   568,
    28,-32768,-32768,   569,-32768,    71,   571,   384,-32768,    33,
   384,   485,   509,   384,   152,   384,   172,   384,   384,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   354,-32768,   604,   318,   263,
   565,   456,   530,    33,   113,-32768,-32768,    85,     3,     3,
   572,   371,    11,   624,   429,   610,   575,  1413,-32768,-32768,
     9,-32768,-32768,    15,-32768,-32768,-32768,    26,-32768,-32768,
-32768,-32768,    52,-32768,-32768,   384,-32768,-32768,-32768,-32768,
-32768,-32768,   793,-32768,-32768,-32768,-32768,   101,    39,    58,
   577,   354,   384,   578,   604,    33,    33,   384,   384,   579,
   318,    33,    33,    33,   384,   384,   580,   263,   121,   100,
    33,    33,   384,    33,   485,   384,   485,   384,    33,-32768,
    88,   581,    20,   565,    33,   384,    33,-32768,    33,-32768,
    33,    33,-32768,-32768,   582,    50,    48,    50,    44,   456,
   143,   148,    33,    33,   384,    33,    33,    33,   131,-32768,
-32768,    96,   584,     3,     3,     3,     3,   530,-32768,-32768,
-32768,-32768,   576,-32768,-32768,-32768,-32768,   587,-32768,    23,
-32768,-32768,-32768,-32768,-32768,   147,   103,   593,    11,   384,
   594,   624,   151,-32768,   384,-32768,   595,     4,    20,   429,
-32768,    33,   509,   509,   384,    33,    33,   596,     4,   610,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   588,-32768,-32768,-32768,-32768,   598,-32768,-32768,-32768,
-32768,-32768,   607,-32768,-32768,   612,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   614,-32768,
-32768,-32768,-32768,   627,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,    78,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,   674,   684,
-32768,    65,   635,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   287,   418,   420,   213,   680,   699,-32768,    81,   644,
    44,   646,    34,   651,   684,    23,-32768,-32768,   384,   384,
   384,   384,   384,   384,   654,   287,   384,   384,   384,   384,
   656,   418,-32768,    33,    33,   661,     4,   420,    33,    33,
-32768,   662,   485,   213,    44,   663,   680,    50,   665,   699,
    78,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,   485,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,   872,   873,-32768
};

static const short yypgoto[] = {-32768,
   -47,-32768,-32768,-32768,-32768,-32768,-32768,-32768,  -157,  -376,
    16,   -33,  -122,  -182,  -228,  -259,  -298,  -304,   116,  -624,
  -214, -1023,  -450,  -617,    62,  -494,  -167,  -801,  -883,  -596,
  -300,  -679,   -12,   707,-32768,-32768,   -70,   133,-32768,-32768,
-32768,    54,-32768,-32768,-32768,   704,-32768,-32768,-32768,   542,
   686,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,-32768,
-32768,-32768,-32768,-32768,   679,-32768,-32768,-32768,    60,-32768,
-32768,-32768,  -473,    -4,   676,-32768,-32768,-32768,-32768,  -211,
-32768,-32768,  -659,   667,   189,   226,-32768,-32768,-32768,    64,
-32768,-32768,-32768,-32768,-32768,  -723,   670,-32768,-32768,-32768,
    61,-32768,-32768,-32768,   659,-32768,-32768,-32768,   660,-32768,
-32768,-32768,    43,-32768,-32768,-32768,-32768,    12,-32768,-32768,
-32768,-32768,-32768,-32768,-32768,   657,-32768,-32768,-32768,   772,
-32768,-32768,-32768,   -10,   641,-32768,-32768,-32768,  -206,-32768,
-32768,-32768,-32768,    18,  -706,  -697,-32768,-32768,-32768,-32768,
-32768,   122,-32768,-32768,-32768,   642,-32768,-32768,-32768,-32768,
   638,-32768,-32768,-32768,-32768,-32768,  -191,-32768,-32768,-32768,
   -23,-32768,-32768,-32768,    89,   628,-32768,-32768,-32768,-32768,
-32768,  -205,-32768,-32768,    42,   655,-32768,-32768,-32768,   664,
-32768,-32768,-32768,  -193,-32768,-32768,-32768,-32768,   666,-32768,
-32768,-32768,   621,-32768,-32768,-32768,    25,-32768,-32768,-32768,
   622,-32768,-32768,-32768,   633,-32768,-32768,-32768,   625,-32768,
-32768,-32768,   617,-32768,-32768,-32768,-32768,   618,-32768,-32768,
-32768,   602,-32768,-32768,-32768,    35,-32768,-32768,-32768,   603,
-32768,-32768,-32768,   590,-32768,-32768,-32768,   583,-32768,-32768,
-32768,-32768,    36,-32768,-32768,-32768,-32768,-32768,  -165,-32768,
-32768,    86,  -149,-32768,-32768,-32768,   586,-32768,-32768,-32768,
   589,-32768,-32768,-32768,   597,-32768,-32768,-32768,   567,-32768,
-32768,-32768,   585,  -154,-32768,   562,-32768,-32768,-32768,-32768
};


#define	YYLAST		1633


static const short yytable[] = {   172,
   126,   456,   732,   518,  1032,   683,   684,   435,   263,   532,
   533,   120,   121,   428,   429,   415,   423,   120,   121,   267,
   268,   468,   120,   121,   487,   683,   439,   546,   120,   121,
   493,   174,   263,   117,   416,   118,   120,   121,  1107,   442,
   443,   444,   445,   446,   447,   263,   120,   121,   524,   421,
   120,   121,   120,   121,   120,   121,   824,   964,   609,   219,
   226,   120,   121,   177,   178,   120,   121,   683,   436,   610,
   120,   121,   464,   120,   121,     3,   969,   173,   276,   282,
   120,   121,  1149,   120,   121,   654,   904,   655,   965,   970,
   449,   452,   449,   452,   541,   905,   655,   402,   915,   127,
   815,   915,   128,   916,   812,   264,  1015,   668,   331,   915,
   269,   270,   449,   452,  1050,   449,   915,   502,  1001,   959,
   477,  1067,   479,   480,   481,   223,   228,   484,   910,   486,
   452,   911,   819,   515,   508,   492,   452,   699,   960,   996,
   926,   703,   129,   956,   278,   426,   910,   677,  1002,  1047,
   709,   219,   262,   123,   124,   265,   266,   417,   437,   997,
   226,  1035,   910,   404,   130,  1064,  1038,   131,   449,  1075,
   832,   510,   132,   511,    11,  1049,   263,   449,   971,   607,
   449,   997,   927,   133,   449,   523,   174,   175,   176,   954,
   332,   531,   531,   953,   333,   536,  1002,   538,   134,   540,
   452,   904,  1066,   431,   449,   690,   691,   135,   693,   276,
   905,  1017,   136,   449,   449,   282,   149,   223,   177,   178,
   150,   955,   725,   491,   920,   272,   228,   687,   151,   721,
   649,   650,   651,   652,   653,   724,   821,   661,   497,   461,
   340,   448,  1052,  1061,   523,   809,   728,   671,   496,   673,
   674,   717,   676,   452,  1178,   451,   680,   476,   966,   478,
   415,   731,   220,   227,   719,   471,   485,  1069,   152,   522,
   611,  1028,   735,   271,   523,   278,   455,   972,   523,   811,
   153,   277,   283,   814,   706,  1106,   656,   523,   818,   232,
   233,   823,   424,   425,   394,   614,   467,  1142,  1098,   470,
   403,  1141,   615,   616,   917,   617,   365,  1016,   366,   618,
   619,   367,   512,   341,   342,  1051,   179,   620,   621,  1003,
   961,   238,  1068,    30,   462,   722,   528,   529,   452,   727,
   154,   622,   912,   458,   952,   402,   539,  1129,  1130,  1181,
   998,   303,   307,   623,   624,   327,   549,   336,   155,   923,
  1048,   852,   506,   156,   220,   157,   558,   559,   239,   158,
    37,   514,  1036,   227,   517,   829,  1065,  1039,   521,   488,
  1076,   833,   294,   295,   660,   257,   258,   710,   304,   308,
   666,   667,   328,   715,   337,  1109,   120,   121,   545,   675,
   853,   343,   344,   345,   159,   296,   160,   554,   555,   523,
   738,   404,   689,   504,  1131,   692,   846,   281,   739,   696,
   161,   187,   277,   349,   350,   188,   189,   190,   283,   174,
   838,   839,   840,   707,   854,   162,   855,   163,   530,   238,
   164,   856,   165,   259,   537,   318,   427,   177,   178,   542,
   382,   383,    38,   836,   166,   847,    39,   550,   551,   250,
   167,   177,   178,  1054,  1055,  1056,  1057,   168,   297,   734,
   387,   388,   389,   497,   169,   298,   239,  1110,  1111,  1112,
  1113,  1114,   319,   794,   191,   170,   662,   663,   171,   370,
   848,   849,   390,   438,   831,  1123,   672,   683,  1010,   441,
  1012,   459,   678,   679,   303,   463,   725,   466,   307,   474,
   795,   796,   797,   625,   482,   320,   321,   322,   323,   397,
   398,   432,   433,   700,   701,   702,   371,   834,   327,   489,
   251,   252,   253,   254,   708,   324,   494,   336,   372,   287,
   288,   304,   289,   399,   400,   308,   499,   501,   403,   192,
   505,   174,   798,  1062,  1124,  1125,   391,   513,   401,   373,
   374,   375,   376,   377,   519,   328,   526,   875,   534,   876,
   830,   719,   218,   543,   337,   877,   878,   879,   880,   378,
   379,  1117,  1118,   177,   178,   740,   174,   881,   741,   933,
   934,   935,   882,   547,   552,   725,   729,   556,  1033,   137,
   138,   139,   140,   141,   936,  1119,  1120,   606,   224,   722,
   225,   311,   452,   312,   817,   742,   313,   647,   177,   178,
   883,   664,   669,   523,   681,   826,   685,   694,   828,   697,
   704,   523,   711,   452,   713,   531,   716,   524,   733,   891,
   892,   893,   894,   895,   896,   743,   744,   745,   807,   746,
   142,   747,   748,   884,   749,   314,   315,   143,   144,   145,
   719,   897,   750,   751,   752,   753,   898,   754,   755,   899,
   796,   797,   900,   756,   859,   860,   861,   862,   863,   864,
   865,   866,   867,   868,   757,   941,   758,   759,   760,   942,
   943,   944,   761,   762,   945,  1062,   869,   763,   722,   901,
   764,   452,   765,   766,   767,   768,   769,   497,   770,   771,
   523,   902,   772,   957,   242,   243,   244,   245,   246,   247,
   773,   407,   408,   835,   353,   354,   355,   356,   357,   358,
   359,   774,   775,  1099,   776,   985,   986,   777,   870,  1062,
   778,   779,   992,   993,   946,   947,   871,   780,   781,   782,
  1008,   409,   410,  1011,   360,  1013,   361,   783,   411,   784,
   449,   785,   786,  1022,   787,   827,  1172,   788,   789,   790,
  1084,  1085,   362,   531,   523,   531,   791,   792,   793,   803,
  1080,   804,  1043,   805,   806,   810,   813,   843,   196,   197,
   816,  1090,   198,   199,   200,   201,   202,   203,   204,   205,
   820,   822,   930,   825,  1099,  1172,   924,   951,   958,  1059,
  1144,   978,   981,   987,   994,  1018,  1027,  1072,  1053,   909,
  1060,  1092,  1078,  1159,  1160,  1161,   449,  1070,  1073,  1079,
  1089,  1093,  1086,   560,   561,   562,   563,   564,   565,   566,
  1094,   567,   568,   569,  1175,  1095,  1099,  1096,  1101,   570,
   571,   572,   573,   574,   729,   575,   576,   577,   578,   579,
  1097,   580,   581,   582,   583,  1103,   584,  1108,   585,   586,
   587,   983,   984,  1138,   837,  1135,  1143,   989,   990,   991,
  1145,  1184,  1185,  1182,  1031,  1147,  1006,  1007,  1156,  1009,
  1162,  1151,  1152,  1153,  1014,  1166,  1171,  1176,  1146,  1179,
  1021,   430,  1023,  1148,  1024,   979,  1025,  1026,   440,   657,
   460,  1034,   457,   469,   982,   531,   465,   483,  1041,  1042,
   475,  1044,  1045,  1046,   988,   490,  1020,   495,   995,   275,
   500,  1168,  1077,   925,  1157,  1058,  1091,   503,   975,   516,
   509,  1177,  1037,   729,  1019,   588,   589,   590,   591,   592,
  1174,   593,   594,   595,   596,   597,   544,   598,   599,   600,
   601,   548,   602,  1071,   603,   604,   605,  1083,   980,  1167,
   520,  1087,  1088,   557,   608,   665,  1074,   553,   688,   648,
   682,   670,  1163,   527,  1180,  1082,   531,  1040,   695,   712,
   452,   718,   535,     0,   698,     0,  1150,   452,   452,   452,
  1154,  1155,     0,     0,  1158,   531,   531,   531,     0,   714,
  1081,     0,   705,     0,     0,     0,     0,     0,     0,     0,
    -6,     1,     0,     0,     0,   531,     0,     2,   531,     3,
     4,     5,     0,     6,     0,     7,     0,     8,     9,     0,
     0,     0,     0,    10,    11,    12,     0,     0,    13,    14,
     0,    15,     0,    16,     0,     0,     0,    17,     0,     0,
    18,    19,    20,    21,    22,    23,    24,    25,     0,     0,
     0,    26,    27,    28,    29,    30,    31,    32,    33,    34,
     0,     0,    35,    36,     2,     0,     3,     4,     5,     0,
     6,     0,     7,     0,     8,     9,     0,     0,     0,     0,
    10,    11,     0,     0,     0,    13,    14,     0,    15,     0,
    16,     0,    37,     0,    17,     0,     0,    18,    19,    20,
    21,    22,    23,    24,    25,     0,     0,     0,    26,    27,
    28,    29,    30,    31,    32,    33,    34,     0,     0,    35,
    36,     0,     0,     0,     0,     0,     0,     0,     0,  1164,
  1165,     0,     0,     0,  1169,  1170,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    37,
     0,     2,     0,     3,     4,     5,     0,     6,     0,     7,
     0,     8,     9,     0,     0,     0,     0,    10,    11,     0,
     0,     0,    13,    14,    38,    15,     0,    16,    39,     0,
     0,    17,     0,     0,    18,    19,    20,    21,    22,    23,
    24,    25,     0,     0,     0,    26,    27,    28,    29,    30,
    31,    32,    33,    34,     0,     0,    35,    36,     2,     0,
     3,     4,     5,     0,     6,     0,     7,     0,     8,     9,
    40,     0,     0,     0,    10,    11,     0,     0,     0,    13,
    14,    38,    15,     0,    16,    39,    37,     0,    17,     0,
     0,    18,    19,    20,    21,    22,    23,    24,    25,     0,
     0,     0,    26,    27,    28,    29,    30,    31,    32,    33,
    34,     0,     0,    35,    36,     2,     0,     3,     4,     5,
     0,     6,     0,     7,     0,     8,     9,    40,     0,   422,
     0,    10,    11,     0,     0,     0,    13,    14,     0,    15,
     0,    16,     0,    37,     0,    17,     0,     0,    18,    19,
    20,    21,    22,    23,    24,    25,     0,     0,     0,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    38,     0,
    35,    36,    39,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    37,     0,     2,     0,     3,     4,     5,     0,     6,     0,
     7,     0,     8,     9,    40,     0,   507,     0,    10,    11,
     0,     0,     0,    13,    14,    38,    15,     0,    16,    39,
     0,     0,    17,     0,     0,    18,    19,    20,    21,    22,
    23,    24,    25,     0,     0,     0,    26,    27,    28,    29,
    30,    31,    32,    33,    34,     0,     0,    35,    36,     2,
     0,     3,     4,     5,     0,     6,     0,     7,     0,     8,
     9,    40,     0,   686,     0,    10,    11,     0,     0,     0,
    13,    14,    38,    15,     0,    16,    39,    37,     0,    17,
     0,     0,    18,    19,    20,    21,    22,    23,    24,    25,
     0,     0,     0,    26,    27,    28,    29,    30,    31,    32,
    33,    34,     0,     0,    35,    36,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,    40,   125,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    37,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    38,
     0,     0,     0,    39,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,    40,   808,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    38,     0,     0,     0,
    39,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,    40
};

static const short yycheck[] = {   122,
    48,   216,   497,   304,   888,     3,   383,   190,     5,   314,
   315,     3,     4,   181,   182,   170,   174,     3,     4,   142,
   143,   236,     3,     4,   253,     3,   194,   328,     3,     4,
   259,    64,     5,   224,    36,     3,     3,     4,  1062,   207,
   208,   209,   210,   211,   212,     5,     3,     4,   308,   172,
     3,     4,     3,     4,     3,     4,   736,    19,     8,   130,
   131,     3,     4,    96,    97,     3,     4,     3,   191,    19,
     3,     4,   230,     3,     4,     9,    19,   125,   149,   150,
     3,     4,  1106,     3,     4,    19,   793,    30,    50,    32,
   213,   214,   215,   216,   323,   793,    30,   168,    14,   224,
   725,    14,   224,    19,   722,   139,    19,   367,    92,    14,
   144,   145,   235,   236,    19,   238,    14,   285,    19,    19,
   243,    19,   245,   246,   247,   130,   131,   250,    16,   252,
   253,    19,   729,   301,   292,   258,   259,   397,    38,    19,
   130,   401,   224,   823,   149,   179,    16,   376,    49,    19,
   410,   222,   137,    38,    39,   140,   141,   159,   192,    39,
   231,    19,    16,   168,   224,    19,    19,   224,   291,    19,
    19,   294,   224,   296,    24,   899,     5,   300,   121,   347,
   303,    39,   172,   224,   307,   308,    64,    65,    66,   814,
   174,   314,   315,   811,   178,   318,    49,   320,   224,   322,
   323,   908,   926,   188,   327,   388,   389,   224,   391,   280,
   908,   871,   224,   336,   337,   286,   224,   222,    96,    97,
   224,   818,   451,   257,   222,   222,   231,   385,   224,   221,
   353,   354,   355,   356,   357,   221,   731,   360,   272,   224,
    66,   222,   902,   221,   367,   719,   221,   370,   221,   372,
   373,   419,   375,   376,  1138,   222,   379,   242,   220,   244,
   415,   221,   130,   131,   422,   222,   251,   927,   224,   222,
   220,   222,   221,   225,   397,   280,   215,   220,   401,   221,
   224,   149,   150,   221,   407,   221,   220,   410,   221,   123,
   124,   221,   177,   178,    92,    12,   235,  1099,   221,   238,
   168,   221,    19,    20,   220,    22,    90,   220,    92,    26,
    27,    95,   297,   139,   140,   220,   194,    34,    35,   220,
   220,   133,   220,    55,   220,   448,   311,   312,   451,   452,
   224,    48,   220,   218,   808,   406,   321,   125,   126,  1141,
   220,   153,   154,    60,    61,   157,   331,   159,   224,   800,
   220,    89,   291,   224,   222,   224,   341,   342,   133,   224,
    92,   300,   220,   231,   303,   742,   220,   220,   307,   254,
   220,   220,    75,    76,   359,   114,   115,   411,   153,   154,
   365,   366,   157,   417,   159,    99,     3,     4,   327,   374,
   128,   217,   218,   219,   224,    98,   224,   336,   337,   522,
   523,   406,   387,   288,   192,   390,    89,    66,   531,   394,
   224,    66,   280,   137,   138,    70,    71,    72,   286,    64,
    67,    68,    69,   408,   162,   224,   164,   224,   313,   241,
   224,   169,   224,   172,   319,    99,   225,    96,    97,   324,
   149,   150,   174,   748,   224,   128,   178,   332,   333,    99,
   224,    96,    97,   904,   905,   906,   907,   224,   161,   507,
    70,    71,    72,   497,   224,   168,   241,   181,   182,   183,
   184,   185,   136,   103,   129,   224,   361,   362,   224,    99,
   163,   164,    92,   225,   744,    66,   371,     3,   865,   225,
   867,   225,   377,   378,   306,   225,   725,   225,   310,   225,
   130,   131,   132,   220,   225,   169,   170,   171,   172,   154,
   155,     3,     4,   398,   399,   400,   136,   746,   330,   225,
   170,   171,   172,   173,   409,   189,   225,   339,   148,   177,
   178,   306,   180,   178,   179,   310,   225,   225,   406,   194,
   225,    64,   172,   920,   125,   126,   156,   225,   193,   169,
   170,   171,   172,   173,   225,   330,   225,   102,   225,   104,
   743,   719,    85,   225,   339,   110,   111,   112,   113,   189,
   190,   154,   155,    96,    97,     6,    64,   122,     6,   151,
   152,   153,   127,   225,   225,   814,   471,   225,   889,   116,
   117,   118,   119,   120,   166,   178,   179,   225,    86,   722,
    88,    90,   725,    92,   727,     6,    95,   225,    96,    97,
   155,   225,   225,   736,   225,   738,   225,   225,   741,   225,
   225,   744,   225,   746,   225,   748,   225,   887,   223,   100,
   101,   102,   103,   104,   105,     6,     6,     6,   686,     6,
   167,     6,     6,   188,     6,   134,   135,   174,   175,   176,
   808,   122,     6,     6,     6,     6,   127,     6,     6,   130,
   131,   132,   133,     6,   100,   101,   102,   103,   104,   105,
   106,   107,   108,   109,     6,    66,     6,     6,     6,    70,
    71,    72,     6,     6,    75,  1062,   122,     6,   811,   160,
     6,   814,     6,     6,     6,     6,     6,   731,     6,     6,
   823,   172,     6,   826,    90,    91,    92,    93,    94,    95,
     6,   157,   158,   747,   141,   142,   143,   144,   145,   146,
   147,     6,     6,  1028,     6,   848,   849,     6,   164,  1106,
     6,     6,   855,   856,   125,   126,   172,     6,     6,     6,
   863,   187,   188,   866,   171,   868,   173,     6,   194,     6,
   873,     6,   224,   876,   224,   740,  1133,   224,   224,   224,
   943,   944,   189,   886,   887,   888,   224,   224,   224,   224,
   938,   224,   895,   224,   224,   223,   223,   174,    73,    74,
   223,   949,    77,    78,    79,    80,    81,    82,    83,    84,
   223,   223,   169,   223,  1099,  1172,   225,   223,     6,   224,
  1101,   225,   225,   225,   225,   225,   225,   930,   225,   794,
   224,   224,   935,  1118,  1119,  1120,   939,   225,   225,   225,
   225,   224,   945,   195,   196,   197,   198,   199,   200,   201,
   224,   203,   204,   205,  1135,   224,  1141,   224,   165,   195,
   196,   197,   198,   199,   729,   201,   202,   203,   204,   205,
   224,   207,   208,   209,   210,   172,   212,   223,   214,   215,
   216,   846,   847,   165,   749,   186,   223,   852,   853,   854,
   225,     0,     0,  1172,   887,   225,   861,   862,   225,   864,
   225,  1110,  1111,  1112,   869,   225,   225,   225,  1103,   225,
   875,   185,   877,  1105,   879,   842,   881,   882,   195,   358,
   222,   890,   217,   237,   845,  1028,   231,   249,   893,   894,
   241,   896,   897,   898,   851,   256,   874,   261,   858,   148,
   280,  1128,   933,   802,  1116,   908,   950,   286,   840,   302,
   293,  1137,   891,   818,   873,   195,   196,   197,   198,   199,
  1134,   201,   202,   203,   204,   205,   326,   207,   208,   209,
   210,   330,   212,   929,   214,   215,   216,   942,   843,  1127,
   306,   946,   947,   339,   348,   364,   932,   335,   386,   352,
   381,   369,  1122,   310,  1140,   940,  1099,   892,   393,   413,
  1103,   420,   317,    -1,   396,    -1,  1109,  1110,  1111,  1112,
  1113,  1114,    -1,    -1,  1117,  1118,  1119,  1120,    -1,   415,
   939,    -1,   406,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     0,     1,    -1,    -1,    -1,  1138,    -1,     7,  1141,     9,
    10,    11,    -1,    13,    -1,    15,    -1,    17,    18,    -1,
    -1,    -1,    -1,    23,    24,    25,    -1,    -1,    28,    29,
    -1,    31,    -1,    33,    -1,    -1,    -1,    37,    -1,    -1,
    40,    41,    42,    43,    44,    45,    46,    47,    -1,    -1,
    -1,    51,    52,    53,    54,    55,    56,    57,    58,    59,
    -1,    -1,    62,    63,     7,    -1,     9,    10,    11,    -1,
    13,    -1,    15,    -1,    17,    18,    -1,    -1,    -1,    -1,
    23,    24,    -1,    -1,    -1,    28,    29,    -1,    31,    -1,
    33,    -1,    92,    -1,    37,    -1,    -1,    40,    41,    42,
    43,    44,    45,    46,    47,    -1,    -1,    -1,    51,    52,
    53,    54,    55,    56,    57,    58,    59,    -1,    -1,    62,
    63,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1124,
  1125,    -1,    -1,    -1,  1129,  1130,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
    -1,     7,    -1,     9,    10,    11,    -1,    13,    -1,    15,
    -1,    17,    18,    -1,    -1,    -1,    -1,    23,    24,    -1,
    -1,    -1,    28,    29,   174,    31,    -1,    33,   178,    -1,
    -1,    37,    -1,    -1,    40,    41,    42,    43,    44,    45,
    46,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    -1,    -1,    62,    63,     7,    -1,
     9,    10,    11,    -1,    13,    -1,    15,    -1,    17,    18,
   220,    -1,    -1,    -1,    23,    24,    -1,    -1,    -1,    28,
    29,   174,    31,    -1,    33,   178,    92,    -1,    37,    -1,
    -1,    40,    41,    42,    43,    44,    45,    46,    47,    -1,
    -1,    -1,    51,    52,    53,    54,    55,    56,    57,    58,
    59,    -1,    -1,    62,    63,     7,    -1,     9,    10,    11,
    -1,    13,    -1,    15,    -1,    17,    18,   220,    -1,   222,
    -1,    23,    24,    -1,    -1,    -1,    28,    29,    -1,    31,
    -1,    33,    -1,    92,    -1,    37,    -1,    -1,    40,    41,
    42,    43,    44,    45,    46,    47,    -1,    -1,    -1,    51,
    52,    53,    54,    55,    56,    57,    58,    59,   174,    -1,
    62,    63,   178,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    92,    -1,     7,    -1,     9,    10,    11,    -1,    13,    -1,
    15,    -1,    17,    18,   220,    -1,   222,    -1,    23,    24,
    -1,    -1,    -1,    28,    29,   174,    31,    -1,    33,   178,
    -1,    -1,    37,    -1,    -1,    40,    41,    42,    43,    44,
    45,    46,    47,    -1,    -1,    -1,    51,    52,    53,    54,
    55,    56,    57,    58,    59,    -1,    -1,    62,    63,     7,
    -1,     9,    10,    11,    -1,    13,    -1,    15,    -1,    17,
    18,   220,    -1,   222,    -1,    23,    24,    -1,    -1,    -1,
    28,    29,   174,    31,    -1,    33,   178,    92,    -1,    37,
    -1,    -1,    40,    41,    42,    43,    44,    45,    46,    47,
    -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    57,
    58,    59,    -1,    -1,    62,    63,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   220,   221,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,
    -1,    -1,    -1,   178,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,   220,   221,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,
   178,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,   220
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/share/bison.simple"
/* This file comes from bison-1.28.  */

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330,
   Boston, MA 02111-1307, USA.  */

/* As a special exception, when this file is copied by Bison into a
   Bison output file, you may use that output file without restriction.
   This special exception was added by the Free Software Foundation
   in version 1.24 of Bison.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

#ifndef YYSTACK_USE_ALLOCA
#ifdef alloca
#define YYSTACK_USE_ALLOCA
#else /* alloca not defined */
#ifdef __GNUC__
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi) || (defined (__sun) && defined (__i386))
#define YYSTACK_USE_ALLOCA
#include <alloca.h>
#else /* not sparc */
/* We think this test detects Watcom and Microsoft C.  */
/* This used to test MSDOS, but that is a bad idea
   since that symbol is in the user namespace.  */
#if (defined (_MSDOS) || defined (_MSDOS_)) && !defined (__TURBOC__)
#if 0 /* No need for malloc.h, which pollutes the namespace;
	 instead, just don't use alloca.  */
#include <malloc.h>
#endif
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
/* I don't know what this was needed for, but it pollutes the namespace.
   So I turned it off.   rms, 2 May 1997.  */
/* #include <malloc.h>  */
 #pragma alloca
#define YYSTACK_USE_ALLOCA
#else /* not MSDOS, or __TURBOC__, or _AIX */
#if 0
#ifdef __hpux /* haible@ilog.fr says this works for HPUX 9.05 and up,
		 and on HPUX 10.  Eventually we can turn this on.  */
#define YYSTACK_USE_ALLOCA
#define alloca __builtin_alloca
#endif /* __hpux */
#endif
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc */
#endif /* not GNU C */
#endif /* alloca not defined */
#endif /* YYSTACK_USE_ALLOCA not defined */

#ifdef YYSTACK_USE_ALLOCA
#define YYSTACK_ALLOC alloca
#else
#define YYSTACK_ALLOC malloc
#endif

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	goto yyacceptlab
#define YYABORT 	goto yyabortlab
#define YYERROR		goto yyerrlab1
/* Like YYERROR except do call yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto yyerrlab
#define YYRECOVERING()  (!!yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    { yychar = (token), yylval = (value);			\
      yychar1 = YYTRANSLATE (yychar);				\
      YYPOPSTACK;						\
      goto yybackup;						\
    }								\
  else								\
    { yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, &yylloc, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval, &yylloc)
#endif
#else /* not YYLSP_NEEDED */
#ifdef YYLEX_PARAM
#define YYLEX		yylex(&yylval, YYLEX_PARAM)
#else
#define YYLEX		yylex(&yylval)
#endif
#endif /* not YYLSP_NEEDED */
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Define __yy_memcpy.  Note that the size argument
   should be passed with type unsigned int, because that is what the non-GCC
   definitions require.  With GCC, __builtin_memcpy takes an arg
   of type size_t, but it can handle unsigned int.  */

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __yy_memcpy(TO,FROM,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (to, from, count)
     char *to;
     char *from;
     unsigned int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__yy_memcpy (char *to, char *from, unsigned int count)
{
  register char *t = to;
  register char *f = from;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 217 "/usr/share/bison.simple"

/* The user can define YYPARSE_PARAM as the name of an argument to be passed
   into yyparse.  The argument should have type void *.
   It should actually point to an object.
   Grammar actions can access the variable by casting it
   to the proper pointer type.  */

#ifdef YYPARSE_PARAM
#ifdef __cplusplus
#define YYPARSE_PARAM_ARG void *YYPARSE_PARAM
#define YYPARSE_PARAM_DECL
#else /* not __cplusplus */
#define YYPARSE_PARAM_ARG YYPARSE_PARAM
#define YYPARSE_PARAM_DECL void *YYPARSE_PARAM;
#endif /* not __cplusplus */
#else /* not YYPARSE_PARAM */
#define YYPARSE_PARAM_ARG
#define YYPARSE_PARAM_DECL
#endif /* not YYPARSE_PARAM */

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
#ifdef YYPARSE_PARAM
int yyparse (void *);
#else
int yyparse (void);
#endif
#endif

int
yyparse(YYPARSE_PARAM_ARG)
     YYPARSE_PARAM_DECL
{
  register int yystate;
  register int yyn;
  register short *yyssp;
  register YYSTYPE *yyvsp;
  int yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *yyss = yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *yyvs = yyvsa;	/*  to allow yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;

#define YYPOPSTACK   (yyvsp--, yyssp--, yylsp--)
#else
#define YYPOPSTACK   (yyvsp--, yyssp--)
#endif

  int yystacksize = YYINITDEPTH;
  int yyfree_stacks = 0;

#ifdef YYPURE
  int yychar;
  YYSTYPE yylval;
  int yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE yylloc;
#endif
#endif

  YYSTYPE yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int yylen;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss - 1;
  yyvsp = yyvs;
#ifdef YYLSP_NEEDED
  yylsp = yyls;
#endif

/* Push a new state, which is found in  yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
yynewstate:

  *++yyssp = yystate;

  if (yyssp >= yyss + yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *yyvs1 = yyvs;
      short *yyss1 = yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *yyls1 = yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = yyssp - yyss + 1;

#ifdef yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if yyoverflow is a macro.  */
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yyls1, size * sizeof (*yylsp),
		 &yystacksize);
#else
      yyoverflow("parser stack overflow",
		 &yyss1, size * sizeof (*yyssp),
		 &yyvs1, size * sizeof (*yyvsp),
		 &yystacksize);
#endif

      yyss = yyss1; yyvs = yyvs1;
#ifdef YYLSP_NEEDED
      yyls = yyls1;
#endif
#else /* no yyoverflow */
      /* Extend the stack our own way.  */
      if (yystacksize >= YYMAXDEPTH)
	{
	  yyerror("parser stack overflow");
	  if (yyfree_stacks)
	    {
	      free (yyss);
	      free (yyvs);
#ifdef YYLSP_NEEDED
	      free (yyls);
#endif
	    }
	  return 2;
	}
      yystacksize *= 2;
      if (yystacksize > YYMAXDEPTH)
	yystacksize = YYMAXDEPTH;
#ifndef YYSTACK_USE_ALLOCA
      yyfree_stacks = 1;
#endif
      yyss = (short *) YYSTACK_ALLOC (yystacksize * sizeof (*yyssp));
      __yy_memcpy ((char *)yyss, (char *)yyss1,
		   size * (unsigned int) sizeof (*yyssp));
      yyvs = (YYSTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yyvsp));
      __yy_memcpy ((char *)yyvs, (char *)yyvs1,
		   size * (unsigned int) sizeof (*yyvsp));
#ifdef YYLSP_NEEDED
      yyls = (YYLTYPE *) YYSTACK_ALLOC (yystacksize * sizeof (*yylsp));
      __yy_memcpy ((char *)yyls, (char *)yyls1,
		   size * (unsigned int) sizeof (*yylsp));
#endif
#endif /* no yyoverflow */

      yyssp = yyss + size - 1;
      yyvsp = yyvs + size - 1;
#ifdef YYLSP_NEEDED
      yylsp = yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Stack size increased to %d\n", yystacksize);
#endif

      if (yyssp >= yyss + yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Entering state %d\n", yystate);
#endif

  goto yybackup;
 yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      yychar = YYLEX;
    }

  /* Convert token to internal form (in yychar1) for indexing tables with */

  if (yychar <= 0)		/* This means end of input. */
    {
      yychar1 = 0;
      yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Now at end of input.\n");
#endif
    }
  else
    {
      yychar1 = YYTRANSLATE(yychar);

#if YYDEBUG != 0
      if (yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", yychar, yytname[yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, yychar, yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  yyn += yychar1;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != yychar1)
    goto yydefault;

  yyn = yytable[yyn];

  /* yyn is what to do for this token type in this state.
     Negative => reduce, -yyn is rule number.
     Positive => shift, yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrlab;

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", yychar, yytname[yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (yyerrstatus) yyerrstatus--;

  yystate = yyn;
  goto yynewstate;

/* Do the default action for the current state.  */
yydefault:

  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;

/* Do a reduction.  yyn is the number of a rule to reduce with.  */
yyreduce:
  yylen = yyr2[yyn];
  if (yylen > 0)
    yyval = yyvsp[1-yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       yyn, yyrline[yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = yyprhs[yyn]; yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", yytname[yyrhs[i]]);
      fprintf (stderr, " -> %s\n", yytname[yyr1[yyn]]);
    }
#endif


  switch (yyn) {

case 2:
#line 90 "vrml.y"
{YYABORT;;
    break;}
case 3:
#line 91 "vrml.y"
{YYABORT;;
    break;}
case 58:
#line 177 "vrml.y"
{
			AddSFInt32(yyvsp[0].ival);
		;
    break;}
case 60:
#line 188 "vrml.y"
{
			AddSFString(yyvsp[0].sval);
		;
    break;}
case 61:
#line 195 "vrml.y"
{
			AddSFFloat(yyvsp[0].fval);
		;
    break;}
case 62:
#line 199 "vrml.y"
{
			yyval.fval = (float)yyvsp[0].ival;
			AddSFFloat((float)yyvsp[0].ival);
		;
    break;}
case 64:
#line 207 "vrml.y"
{yyval.fval = (float)yyvsp[0].ival;;
    break;}
case 65:
#line 212 "vrml.y"
{
			gColor[0] = yyvsp[-2].fval;
			gColor[1] = yyvsp[-1].fval;
			gColor[2] = yyvsp[0].fval;
			AddSFColor(gColor);
	    ;
    break;}
case 66:
#line 222 "vrml.y"
{
			gRotation[0] = yyvsp[-3].fval;
			gRotation[1] = yyvsp[-2].fval;
			gRotation[2] = yyvsp[-1].fval;
			gRotation[3] = yyvsp[0].fval;
			AddSFRotation(gRotation);
		;
    break;}
case 67:
#line 232 "vrml.y"
{;
    break;}
case 69:
#line 239 "vrml.y"
{
			gWidth = yyvsp[-2].ival;
			gHeight = yyvsp[-1].ival;
			gComponents = yyvsp[0].ival;
	    ;
    break;}
case 71:
#line 252 "vrml.y"
{
			gVec2f[0] = yyvsp[-1].fval;
			gVec2f[1] = yyvsp[0].fval;
			AddSFVec2f(gVec2f);
		;
    break;}
case 72:
#line 261 "vrml.y"
{
			gVec3f[0] = yyvsp[-2].fval;
			gVec3f[1] = yyvsp[-1].fval;
			gVec3f[2] = yyvsp[0].fval;
			AddSFVec3f(gVec3f);
		;
    break;}
case 80:
#line 283 "vrml.y"
{;
    break;}
case 81:
#line 284 "vrml.y"
{;
    break;}
case 82:
#line 285 "vrml.y"
{;
    break;}
case 85:
#line 291 "vrml.y"
{;
    break;}
case 86:
#line 292 "vrml.y"
{;
    break;}
case 87:
#line 297 "vrml.y"
{;
    break;}
case 88:
#line 298 "vrml.y"
{;
    break;}
case 89:
#line 299 "vrml.y"
{;
    break;}
case 92:
#line 305 "vrml.y"
{;
    break;}
case 93:
#line 306 "vrml.y"
{;
    break;}
case 94:
#line 310 "vrml.y"
{;
    break;}
case 95:
#line 311 "vrml.y"
{;
    break;}
case 96:
#line 312 "vrml.y"
{;
    break;}
case 99:
#line 318 "vrml.y"
{;
    break;}
case 100:
#line 319 "vrml.y"
{;
    break;}
case 124:
#line 374 "vrml.y"
{
			PushNode(VRML_NODETYPE_ANCHOR_PARAMETER, GetCurrentNodeObject());
		;
    break;}
case 125:
#line 381 "vrml.y"
{
			PushNode(VRML_NODETYPE_ANCHOR_URL, GetCurrentNodeObject());
		;
    break;}
case 126:
#line 388 "vrml.y"
{
			((GroupingNode *)GetCurrentNodeObject())->setBoundingBoxCenter(gVec3f);
		;
    break;}
case 127:
#line 395 "vrml.y"
{
			((GroupingNode *)GetCurrentNodeObject())->setBoundingBoxSize(gVec3f);
		;
    break;}
case 129:
#line 403 "vrml.y"
{
			((AnchorNode *)GetCurrentNodeObject())->setDescription(yyvsp[0].sval);
		;
    break;}
case 130:
#line 408 "vrml.y"
{
			PopNode();
		;
    break;}
case 131:
#line 412 "vrml.y"
{
			PopNode();
		;
    break;}
case 134:
#line 421 "vrml.y"
{   
			AnchorNode	*anchor = new AnchorNode();
			anchor->setName(GetDEFName());
			AddNode(anchor);
			PushNode(VRML_NODETYPE_ANCHOR, anchor);
		;
    break;}
case 135:
#line 431 "vrml.y"
{
			AnchorNode *anchor = (AnchorNode *)GetCurrentNodeObject();
			anchor->initialize();
			PopNode();
		;
    break;}
case 149:
#line 465 "vrml.y"
{
			AppearanceNode	*appearance = new AppearanceNode();
			appearance->setName(GetDEFName());
			AddNode(appearance);
			PushNode(VRML_NODETYPE_APPEARANCE, appearance);
		;
    break;}
case 150:
#line 475 "vrml.y"
{
			AppearanceNode	*appearance = (AppearanceNode *)GetCurrentNodeObject();
			appearance->initialize();
			PopNode();
		;
    break;}
case 153:
#line 495 "vrml.y"
{
			PushNode(VRML_NODETYPE_AUDIOCLIP_URL, GetCurrentNodeObject());
		;
    break;}
case 154:
#line 502 "vrml.y"
{
			((AudioClipNode *)GetCurrentNodeObject())->setDescription(yyvsp[0].sval);
		;
    break;}
case 155:
#line 506 "vrml.y"
{
			((AudioClipNode *)GetCurrentNodeObject())->setLoop(yyvsp[0].ival);
		;
    break;}
case 156:
#line 510 "vrml.y"
{
			((AudioClipNode *)GetCurrentNodeObject())->setPitch(yyvsp[0].fval);
		;
    break;}
case 157:
#line 514 "vrml.y"
{
			((AudioClipNode *)GetCurrentNodeObject())->setStartTime(yyvsp[0].fval);
		;
    break;}
case 158:
#line 518 "vrml.y"
{
			((AudioClipNode *)GetCurrentNodeObject())->setStopTime(yyvsp[0].fval);
		;
    break;}
case 159:
#line 522 "vrml.y"
{
			PopNode();
		;
    break;}
case 160:
#line 529 "vrml.y"
{
			AudioClipNode	*audioClip = new AudioClipNode();
			audioClip->setName(GetDEFName());
			AddNode(audioClip);
			PushNode(VRML_NODETYPE_AUDIOCLIP, audioClip);
		;
    break;}
case 161:
#line 538 "vrml.y"
{
			AudioClipNode *audioClip = (AudioClipNode *)GetCurrentNodeObject();
			audioClip->initialize();
			PopNode();
		;
    break;}
case 164:
#line 558 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_BACKURL, GetCurrentNodeObject());
		;
    break;}
case 165:
#line 565 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_BOTTOMURL, GetCurrentNodeObject());
		;
    break;}
case 166:
#line 572 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_FRONTURL, GetCurrentNodeObject());
		;
    break;}
case 167:
#line 579 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_LEFTURL, GetCurrentNodeObject());
		;
    break;}
case 168:
#line 586 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_RIGHTURL, GetCurrentNodeObject());
		;
    break;}
case 169:
#line 593 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_TOPURL, GetCurrentNodeObject());
		;
    break;}
case 170:
#line 600 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_GROUNDANGLE, GetCurrentNodeObject());
		;
    break;}
case 171:
#line 607 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_GROUNDCOLOR, GetCurrentNodeObject());
		;
    break;}
case 172:
#line 614 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_SKYANGLE, GetCurrentNodeObject());
		;
    break;}
case 173:
#line 621 "vrml.y"
{
			PushNode(VRML_NODETYPE_BACKGROUND_SKYCOLOR, GetCurrentNodeObject());
		;
    break;}
case 174:
#line 628 "vrml.y"
{
			PopNode();
		;
    break;}
case 175:
#line 632 "vrml.y"
{
			PopNode();
		;
    break;}
case 176:
#line 636 "vrml.y"
{
			PopNode();
		;
    break;}
case 177:
#line 640 "vrml.y"
{
			PopNode();
		;
    break;}
case 178:
#line 644 "vrml.y"
{
			PopNode();
		;
    break;}
case 179:
#line 648 "vrml.y"
{
			PopNode();
		;
    break;}
case 180:
#line 652 "vrml.y"
{
			PopNode();
		;
    break;}
case 181:
#line 656 "vrml.y"
{
			PopNode();
		;
    break;}
case 182:
#line 660 "vrml.y"
{
			PopNode();
		;
    break;}
case 183:
#line 664 "vrml.y"
{
			PopNode();
		;
    break;}
case 184:
#line 671 "vrml.y"
{
			BackgroundNode *bg = new BackgroundNode();
			bg->setName(GetDEFName());
			AddNode(bg);
			PushNode(VRML_NODETYPE_BACKGROUND, bg);
		;
    break;}
case 185:
#line 681 "vrml.y"
{
			BackgroundNode *bg = (BackgroundNode *)GetCurrentNodeObject();
			bg->initialize();
			PopNode();
		;
    break;}
case 189:
#line 702 "vrml.y"
{
			((BillboardNode *)GetCurrentNodeObject())->setAxisOfRotation(gVec3f);
		;
    break;}
case 192:
#line 711 "vrml.y"
{   
			BillboardNode *billboard = new BillboardNode();
			billboard->setName(GetDEFName());
			AddNode(billboard);
			PushNode(VRML_NODETYPE_BILLBOARD, billboard);
		;
    break;}
case 193:
#line 721 "vrml.y"
{
			BillboardNode *billboard = (BillboardNode *)GetCurrentNodeObject();
			billboard->initialize();
			PopNode();
		;
    break;}
case 196:
#line 741 "vrml.y"
{
			((BoxNode *)GetCurrentNodeObject())->setSize(gVec3f);
		;
    break;}
case 197:
#line 748 "vrml.y"
{
			BoxNode *box = new BoxNode();
			box->setName(GetDEFName());
			AddNode(box);
			PushNode(VRML_NODETYPE_BOX, box);
		;
    break;}
case 198:
#line 758 "vrml.y"
{
			BoxNode *box = (BoxNode *)GetCurrentNodeObject();
			box->initialize();
			PopNode();
		;
    break;}
case 206:
#line 795 "vrml.y"
{
			PushNode(VRML_NODETYPE_COLLISION_PROXY, GetCurrentNodeObject());
		;
    break;}
case 208:
#line 803 "vrml.y"
{
			((CollisionNode *)GetCurrentNodeObject())->setCollide(yyvsp[0].ival);
		;
    break;}
case 212:
#line 810 "vrml.y"
{
			PopNode();							
		;
    break;}
case 213:
#line 817 "vrml.y"
{   
			CollisionNode *collision = new CollisionNode();
			collision->setName(GetDEFName());
			AddNode(collision);
			PushNode(VRML_NODETYPE_BOX, collision);
		;
    break;}
case 214:
#line 827 "vrml.y"
{
			CollisionNode *collision = (CollisionNode *)GetCurrentNodeObject();
			collision->initialize();
			PopNode();
		;
    break;}
case 218:
#line 851 "vrml.y"
{
			ColorNode *color = new ColorNode();
			color->setName(GetDEFName());
			AddNode(color);
			PushNode(VRML_NODETYPE_COLOR, color);
		;
    break;}
case 219:
#line 861 "vrml.y"
{
			ColorNode *color = (ColorNode *)GetCurrentNodeObject();
			color->initialize();
			PopNode();
		;
    break;}
case 222:
#line 881 "vrml.y"
{
			PushNode(VRML_NODETYPE_INTERPOLATOR_KEY, GetCurrentNodeObject());
		;
    break;}
case 223:
#line 888 "vrml.y"
{
			PushNode(VRML_NODETYPE_INTERPOLATOR_KEYVALUE, GetCurrentNodeObject());
		;
    break;}
case 224:
#line 895 "vrml.y"
{
			PopNode();
		;
    break;}
case 225:
#line 899 "vrml.y"
{
			PopNode();
		;
    break;}
case 226:
#line 906 "vrml.y"
{
			ColorInterpolatorNode *colInterp = new ColorInterpolatorNode();
			colInterp->setName(GetDEFName());
			AddNode(colInterp);
			PushNode(VRML_NODETYPE_COLORINTERPOLATOR, colInterp);
		;
    break;}
case 227:
#line 916 "vrml.y"
{
			ColorInterpolatorNode *colInterp = (ColorInterpolatorNode *)GetCurrentNodeObject();
			colInterp->initialize();
			PopNode();
		;
    break;}
case 230:
#line 936 "vrml.y"
{
			((ConeNode *)GetCurrentNodeObject())->setSide(yyvsp[0].ival);
		;
    break;}
case 231:
#line 940 "vrml.y"
{
			((ConeNode *)GetCurrentNodeObject())->setBottom(yyvsp[0].ival);
		;
    break;}
case 232:
#line 944 "vrml.y"
{
			((ConeNode *)GetCurrentNodeObject())->setBottomRadius(yyvsp[0].fval);
		;
    break;}
case 233:
#line 948 "vrml.y"
{
			((ConeNode *)GetCurrentNodeObject())->setHeight(yyvsp[0].fval);
		;
    break;}
case 234:
#line 955 "vrml.y"
{
			ConeNode *cone = new ConeNode();
			cone->setName(GetDEFName());
			AddNode(cone);
			PushNode(VRML_NODETYPE_CONE, cone);
		;
    break;}
case 235:
#line 965 "vrml.y"
{
			ConeNode *cone = (ConeNode *)GetCurrentNodeObject();
			cone->initialize();
			PopNode();
		;
    break;}
case 238:
#line 985 "vrml.y"
{
			CoordinateNode *coord = new CoordinateNode();
			coord->setName(GetDEFName());
			AddNode(coord);
			PushNode(VRML_NODETYPE_COORDINATE, coord);
		;
    break;}
case 239:
#line 995 "vrml.y"
{
			CoordinateNode *coord = (CoordinateNode *)GetCurrentNodeObject();
			coord->initialize();
			PopNode();
		;
    break;}
case 242:
#line 1015 "vrml.y"
{
			PopNode();
		;
    break;}
case 243:
#line 1019 "vrml.y"
{
			PopNode();
		;
    break;}
case 244:
#line 1026 "vrml.y"
{
			CoordinateInterpolatorNode *coordInterp = new CoordinateInterpolatorNode();
			coordInterp->setName(GetDEFName());
			AddNode(coordInterp);
			PushNode(VRML_NODETYPE_COORDINATEINTERPOLATOR, coordInterp);
		;
    break;}
case 245:
#line 1036 "vrml.y"
{
			CoordinateInterpolatorNode *coordInterp = (CoordinateInterpolatorNode *)GetCurrentNodeObject();
			coordInterp->initialize();
			PopNode();
		;
    break;}
case 248:
#line 1056 "vrml.y"
{
			((CylinderNode *)GetCurrentNodeObject())->setSide(yyvsp[0].ival);
		;
    break;}
case 249:
#line 1060 "vrml.y"
{
			((CylinderNode *)GetCurrentNodeObject())->setBottom(yyvsp[0].ival);
		;
    break;}
case 250:
#line 1064 "vrml.y"
{
			((CylinderNode *)GetCurrentNodeObject())->setTop(yyvsp[0].ival);
		;
    break;}
case 251:
#line 1068 "vrml.y"
{
			((CylinderNode *)GetCurrentNodeObject())->setRadius(yyvsp[0].fval);
		;
    break;}
case 252:
#line 1072 "vrml.y"
{
			((CylinderNode *)GetCurrentNodeObject())->setHeight(yyvsp[0].fval);
		;
    break;}
case 253:
#line 1079 "vrml.y"
{
			CylinderNode *cylinder = new CylinderNode();
			cylinder->setName(GetDEFName());
			AddNode(cylinder);
			PushNode(VRML_NODETYPE_CYLINDER, cylinder);
		;
    break;}
case 254:
#line 1089 "vrml.y"
{
			CylinderNode *cylinder = (CylinderNode *)GetCurrentNodeObject();
			cylinder->initialize();
			PopNode();
		;
    break;}
case 257:
#line 1109 "vrml.y"
{
			((CylinderSensorNode *)GetCurrentNodeObject())->setAutoOffset(yyvsp[0].ival);
		;
    break;}
case 258:
#line 1113 "vrml.y"
{
			((CylinderSensorNode *)GetCurrentNodeObject())->setDiskAngle(yyvsp[0].fval);
		;
    break;}
case 259:
#line 1117 "vrml.y"
{
			((CylinderSensorNode *)GetCurrentNodeObject())->setEnabled(yyvsp[0].ival);
		;
    break;}
case 260:
#line 1121 "vrml.y"
{
			((CylinderSensorNode *)GetCurrentNodeObject())->setMaxAngle(yyvsp[0].fval);
		;
    break;}
case 261:
#line 1125 "vrml.y"
{
			((CylinderSensorNode *)GetCurrentNodeObject())->setMinAngle(yyvsp[0].fval);
		;
    break;}
case 262:
#line 1129 "vrml.y"
{
			((CylinderSensorNode *)GetCurrentNodeObject())->setOffset(yyvsp[0].fval);
		;
    break;}
case 263:
#line 1137 "vrml.y"
{
			CylinderSensorNode *cysensor = new CylinderSensorNode();
			cysensor->setName(GetDEFName());
			AddNode(cysensor);
			PushNode(VRML_NODETYPE_CYLINDERSENSOR, cysensor);
		;
    break;}
case 264:
#line 1147 "vrml.y"
{
			CylinderSensorNode *cysensor = (CylinderSensorNode *)GetCurrentNodeObject();
			cysensor->initialize();
			PopNode();
		;
    break;}
case 267:
#line 1167 "vrml.y"
{
			((DirectionalLightNode *)GetCurrentNodeObject())->setOn(yyvsp[0].ival);
		;
    break;}
case 268:
#line 1171 "vrml.y"
{
			((DirectionalLightNode *)GetCurrentNodeObject())->setIntensity(yyvsp[0].fval);
		;
    break;}
case 269:
#line 1175 "vrml.y"
{
			((DirectionalLightNode *)GetCurrentNodeObject())->setColor(gColor);
		;
    break;}
case 270:
#line 1179 "vrml.y"
{
			((DirectionalLightNode *)GetCurrentNodeObject())->setDirection(gVec3f);
		;
    break;}
case 271:
#line 1183 "vrml.y"
{
			((DirectionalLightNode *)GetCurrentNodeObject())->setAmbientIntensity(yyvsp[0].fval);
		;
    break;}
case 272:
#line 1190 "vrml.y"
{
			DirectionalLightNode *dirLight = new DirectionalLightNode();
			dirLight->setName(GetDEFName());
			AddNode(dirLight);
			PushNode(VRML_NODETYPE_DIRECTIONALLIGHT, dirLight);
		;
    break;}
case 273:
#line 1200 "vrml.y"
{
			DirectionalLightNode *dirLight = (DirectionalLightNode *)GetCurrentNodeObject();
			dirLight->initialize();
			PopNode();
		;
    break;}
case 276:
#line 1220 "vrml.y"
{
			PushNode(VRML_NODETYPE_ELEVATIONGRID_HEIGHT, GetCurrentNodeObject());
		;
    break;}
case 286:
#line 1237 "vrml.y"
{
			PopNode();
		;
    break;}
case 287:
#line 1241 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setCCW(yyvsp[0].ival);
		;
    break;}
case 288:
#line 1245 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setCreaseAngle(yyvsp[0].fval);
		;
    break;}
case 289:
#line 1249 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setSolid(yyvsp[0].ival);
		;
    break;}
case 290:
#line 1253 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setColorPerVertex(yyvsp[0].ival);
		;
    break;}
case 291:
#line 1257 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setNormalPerVertex(yyvsp[0].ival);
		;
    break;}
case 292:
#line 1261 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setXDimension(yyvsp[0].ival);
		;
    break;}
case 293:
#line 1265 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setXSpacing(yyvsp[0].fval);
		;
    break;}
case 294:
#line 1269 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setZDimension(yyvsp[0].ival);
		;
    break;}
case 295:
#line 1273 "vrml.y"
{
			((ElevationGridNode *)GetCurrentNodeObject())->setZSpacing(yyvsp[0].fval);
		;
    break;}
case 296:
#line 1280 "vrml.y"
{
			ElevationGridNode *elev = new ElevationGridNode();
			elev->setName(GetDEFName());
			AddNode(elev);
			PushNode(VRML_NODETYPE_ELEVATIONGRID, elev);
		;
    break;}
case 297:
#line 1290 "vrml.y"
{
			ElevationGridNode *elev = (ElevationGridNode *)GetCurrentNodeObject();
			elev->initialize();
			PopNode();
		;
    break;}
case 300:
#line 1310 "vrml.y"
{
			PushNode(VRML_NODETYPE_EXTRUSION_CROSSSECTION, GetCurrentNodeObject());
		;
    break;}
case 301:
#line 1317 "vrml.y"
{
			PushNode(VRML_NODETYPE_EXTRUSION_ORIENTATION, GetCurrentNodeObject());
		;
    break;}
case 302:
#line 1324 "vrml.y"
{
			PushNode(VRML_NODETYPE_EXTRUSION_SCALE, GetCurrentNodeObject());
		;
    break;}
case 303:
#line 1331 "vrml.y"
{
			PushNode(VRML_NODETYPE_EXTRUSION_SPINE, GetCurrentNodeObject());
		;
    break;}
case 304:
#line 1338 "vrml.y"
{
			((ExtrusionNode *)GetCurrentNodeObject())->setBeginCap(yyvsp[0].ival);
		;
    break;}
case 305:
#line 1342 "vrml.y"
{
			((ExtrusionNode *)GetCurrentNodeObject())->setCCW(yyvsp[0].ival);
		;
    break;}
case 306:
#line 1346 "vrml.y"
{
			((ExtrusionNode *)GetCurrentNodeObject())->setConvex(yyvsp[0].ival);
		;
    break;}
case 307:
#line 1350 "vrml.y"
{
			((ExtrusionNode *)GetCurrentNodeObject())->setCreaseAngle(yyvsp[0].fval);
		;
    break;}
case 308:
#line 1354 "vrml.y"
{
			((ExtrusionNode *)GetCurrentNodeObject())->setSolid(yyvsp[0].ival);
		;
    break;}
case 309:
#line 1358 "vrml.y"
{
			PopNode();
		;
    break;}
case 310:
#line 1362 "vrml.y"
{
			((ExtrusionNode *)GetCurrentNodeObject())->setEndCap(yyvsp[0].ival);
		;
    break;}
case 311:
#line 1366 "vrml.y"
{
			PopNode();
		;
    break;}
case 312:
#line 1370 "vrml.y"
{
			PopNode();
		;
    break;}
case 313:
#line 1374 "vrml.y"
{
			PopNode();
		;
    break;}
case 314:
#line 1381 "vrml.y"
{
			ExtrusionNode *ex = new ExtrusionNode();
			ex->setName(GetDEFName());
			AddNode(ex);
			PushNode(VRML_NODETYPE_EXTRUSION, ex);
		;
    break;}
case 315:
#line 1391 "vrml.y"
{
			ExtrusionNode *ex = (ExtrusionNode *)GetCurrentNodeObject();
			ex->initialize();
			PopNode();
		;
    break;}
case 318:
#line 1411 "vrml.y"
{
			((FogNode *)GetCurrentNodeObject())->setColor(gColor);
		;
    break;}
case 319:
#line 1415 "vrml.y"
{
			((FogNode *)GetCurrentNodeObject())->setFogType(yyvsp[0].sval);
		;
    break;}
case 320:
#line 1419 "vrml.y"
{
			((FogNode *)GetCurrentNodeObject())->setVisibilityRange(yyvsp[0].fval);
		;
    break;}
case 321:
#line 1426 "vrml.y"
{
			FogNode *fog= new FogNode();
			fog->setName(GetDEFName());
			AddNode(fog);
			PushNode(VRML_NODETYPE_FOG, fog);
		;
    break;}
case 322:
#line 1436 "vrml.y"
{
			FogNode *fog= (FogNode *)GetCurrentNodeObject();
			fog->initialize();
			PopNode();
		;
    break;}
case 325:
#line 1456 "vrml.y"
{
			PushNode(VRML_NODETYPE_FONTSTYLE_JUSTIFY, GetCurrentNodeObject());
		;
    break;}
case 326:
#line 1463 "vrml.y"
{
			((FontStyleNode *)GetCurrentNodeObject())->setFamily(yyvsp[0].sval);
		;
    break;}
case 327:
#line 1467 "vrml.y"
{
			((FontStyleNode *)GetCurrentNodeObject())->setHorizontal(yyvsp[0].ival);
		;
    break;}
case 328:
#line 1471 "vrml.y"
{
			PopNode();
		;
    break;}
case 329:
#line 1475 "vrml.y"
{
			((FontStyleNode *)GetCurrentNodeObject())->setLanguage(yyvsp[0].sval);
		;
    break;}
case 330:
#line 1479 "vrml.y"
{
			((FontStyleNode *)GetCurrentNodeObject())->setLeftToRight(yyvsp[0].ival);
		;
    break;}
case 331:
#line 1483 "vrml.y"
{
			((FontStyleNode *)GetCurrentNodeObject())->setSize(yyvsp[0].fval);
		;
    break;}
case 332:
#line 1487 "vrml.y"
{
			((FontStyleNode *)GetCurrentNodeObject())->setSpacing(yyvsp[0].fval);
		;
    break;}
case 333:
#line 1491 "vrml.y"
{
			((FontStyleNode *)GetCurrentNodeObject())->setStyle(yyvsp[0].sval);
		;
    break;}
case 334:
#line 1495 "vrml.y"
{
			((FontStyleNode *)GetCurrentNodeObject())->setTopToBottom(yyvsp[0].ival);
		;
    break;}
case 335:
#line 1502 "vrml.y"
{
			FontStyleNode *fs = new FontStyleNode();
			fs->setName(GetDEFName());
			AddNode(fs);
			PushNode(VRML_NODETYPE_FONTSTYLE, fs);
		;
    break;}
case 336:
#line 1512 "vrml.y"
{
			FontStyleNode *fs = (FontStyleNode *)GetCurrentNodeObject();
			fs->initialize();
			PopNode();
		;
    break;}
case 342:
#line 1538 "vrml.y"
{   
			GroupNode *group = new GroupNode();
			group->setName(GetDEFName());
			AddNode(group);
			PushNode(VRML_NODETYPE_GROUP, group);
		;
    break;}
case 343:
#line 1548 "vrml.y"
{
			GroupNode *group = (GroupNode *)GetCurrentNodeObject();
			group->initialize();
			PopNode();
		;
    break;}
case 346:
#line 1568 "vrml.y"
{
			PushNode(VRML_NODETYPE_IMAGETEXTURE_URL, GetCurrentNodeObject());
		;
    break;}
case 347:
#line 1575 "vrml.y"
{
			PopNode();
		;
    break;}
case 348:
#line 1579 "vrml.y"
{
			((ImageTextureNode *)GetCurrentNodeObject())->setRepeatS(yyvsp[0].ival);
		;
    break;}
case 349:
#line 1583 "vrml.y"
{
			((ImageTextureNode *)GetCurrentNodeObject())->setRepeatT(yyvsp[0].ival);
		;
    break;}
case 350:
#line 1590 "vrml.y"
{
			ImageTextureNode *imgTexture = new ImageTextureNode();
			imgTexture->setName(GetDEFName());
			AddNode(imgTexture);
			PushNode(VRML_NODETYPE_IMAGETEXTURE, imgTexture);
		;
    break;}
case 351:
#line 1600 "vrml.y"
{
			ImageTextureNode *imgTexture = (ImageTextureNode *)GetCurrentNodeObject();
			imgTexture->initialize();
			PopNode();
		;
    break;}
case 354:
#line 1620 "vrml.y"
{
			PushNode(VRML_NODETYPE_COLOR_INDEX, GetCurrentNodeObject());
		;
    break;}
case 355:
#line 1627 "vrml.y"
{
			PushNode(VRML_NODETYPE_COORDINATE_INDEX, GetCurrentNodeObject());
		;
    break;}
case 356:
#line 1634 "vrml.y"
{
			PushNode(VRML_NODETYPE_NORMAL_INDEX, GetCurrentNodeObject());
		;
    break;}
case 357:
#line 1641 "vrml.y"
{
			PushNode(VRML_NODETYPE_TEXTURECOODINATE_INDEX, GetCurrentNodeObject());
		;
    break;}
case 370:
#line 1660 "vrml.y"
{
			((IndexedFaceSetNode *)GetCurrentNodeObject())->setCCW(yyvsp[0].ival);
		;
    break;}
case 371:
#line 1664 "vrml.y"
{
			((IndexedFaceSetNode *)GetCurrentNodeObject())->setConvex(yyvsp[0].ival);
		;
    break;}
case 372:
#line 1668 "vrml.y"
{
			((IndexedFaceSetNode *)GetCurrentNodeObject())->setSolid(yyvsp[0].ival);
		;
    break;}
case 373:
#line 1672 "vrml.y"
{
			((IndexedFaceSetNode *)GetCurrentNodeObject())->setCreaseAngle(yyvsp[0].fval);
		;
    break;}
case 374:
#line 1676 "vrml.y"
{
			PopNode();
		;
    break;}
case 375:
#line 1680 "vrml.y"
{
			((IndexedFaceSetNode *)GetCurrentNodeObject())->setColorPerVertex(yyvsp[0].ival);
		;
    break;}
case 376:
#line 1684 "vrml.y"
{
			PopNode();
		;
    break;}
case 377:
#line 1688 "vrml.y"
{
			PopNode();
		;
    break;}
case 378:
#line 1692 "vrml.y"
{
			PopNode();
		;
    break;}
case 379:
#line 1696 "vrml.y"
{
			((IndexedFaceSetNode *)GetCurrentNodeObject())->setNormalPerVertex(yyvsp[0].ival);
		;
    break;}
case 380:
#line 1703 "vrml.y"
{
			IndexedFaceSetNode	*idxFaceset = new IndexedFaceSetNode();
			idxFaceset->setName(GetDEFName());
			AddNode(idxFaceset);
			PushNode(VRML_NODETYPE_INDEXEDFACESET, idxFaceset);
		;
    break;}
case 381:
#line 1713 "vrml.y"
{
			IndexedFaceSetNode *idxFaceset = (IndexedFaceSetNode *)GetCurrentNodeObject();
			idxFaceset->initialize();
			PopNode();
		;
    break;}
case 390:
#line 1739 "vrml.y"
{
			((IndexedLineSetNode *)GetCurrentNodeObject())->setColorPerVertex(yyvsp[0].ival);
		;
    break;}
case 391:
#line 1743 "vrml.y"
{
			PopNode();
		;
    break;}
case 392:
#line 1747 "vrml.y"
{
			PopNode();
		;
    break;}
case 393:
#line 1754 "vrml.y"
{
			IndexedLineSetNode	*idxLineset = new IndexedLineSetNode();
			idxLineset->setName(GetDEFName());
			AddNode(idxLineset);
			PushNode(VRML_NODETYPE_INDEXEDLINESET, idxLineset);
		;
    break;}
case 394:
#line 1764 "vrml.y"
{
			IndexedLineSetNode *idxLineset = (IndexedLineSetNode *)GetCurrentNodeObject();
			idxLineset->initialize();
			PopNode();
		;
    break;}
case 397:
#line 1784 "vrml.y"
{
			PushNode(VRML_NODETYPE_INLINE_URL, GetCurrentNodeObject());
		;
    break;}
case 398:
#line 1791 "vrml.y"
{
			PopNode();
		;
    break;}
case 401:
#line 1800 "vrml.y"
{   
			InlineNode *inlineNode = new InlineNode();
			inlineNode->setName(GetDEFName());
			AddNode(inlineNode);
			PushNode(VRML_NODETYPE_INLINE, inlineNode);
		;
    break;}
case 402:
#line 1810 "vrml.y"
{
			InlineNode *inlineNode = (InlineNode *)GetCurrentNodeObject();
			//inlineNode->initialize();
			PopNode();
		;
    break;}
case 405:
#line 1830 "vrml.y"
{
			PushNode(VRML_NODETYPE_LOD_RANGE, GetCurrentNodeObject());
		;
    break;}
case 406:
#line 1838 "vrml.y"
{
			PushNode(VRML_NODETYPE_LOD_LEVEL, GetCurrentNodeObject());
		;
    break;}
case 407:
#line 1845 "vrml.y"
{
			PopNode();							
		;
    break;}
case 408:
#line 1849 "vrml.y"
{
			((LodNode *)GetCurrentNodeObject())->setCenter(gVec3f);
		;
    break;}
case 409:
#line 1853 "vrml.y"
{
			PopNode();							
		;
    break;}
case 410:
#line 1857 "vrml.y"
{
			PopNode();							
		;
    break;}
case 411:
#line 1864 "vrml.y"
{   
			LodNode	*lod = new LodNode();
			lod->setName(GetDEFName());
			AddNode(lod);
			PushNode(VRML_NODETYPE_INLINE, lod);
		;
    break;}
case 412:
#line 1874 "vrml.y"
{
			LodNode	*lod = (LodNode *)GetCurrentNodeObject();
			lod->initialize();
			PopNode();
		;
    break;}
case 415:
#line 1894 "vrml.y"
{
			((MaterialNode *)GetCurrentNodeObject())->setAmbientIntensity(yyvsp[0].fval);
		;
    break;}
case 416:
#line 1898 "vrml.y"
{
			((MaterialNode *)GetCurrentNodeObject())->setDiffuseColor(gColor);
		;
    break;}
case 417:
#line 1902 "vrml.y"
{
			((MaterialNode *)GetCurrentNodeObject())->setEmissiveColor(gColor);
		;
    break;}
case 418:
#line 1906 "vrml.y"
{
			((MaterialNode *)GetCurrentNodeObject())->setShininess(yyvsp[0].fval);
		;
    break;}
case 419:
#line 1910 "vrml.y"
{
			((MaterialNode *)GetCurrentNodeObject())->setSpecularColor(gColor);
		;
    break;}
case 420:
#line 1914 "vrml.y"
{
			((MaterialNode *)GetCurrentNodeObject())->setTransparency(yyvsp[0].fval);
		;
    break;}
case 421:
#line 1920 "vrml.y"
{
			MaterialNode *material = new MaterialNode();
			material->setName(GetDEFName());
			AddNode(material);
			PushNode(VRML_NODETYPE_MATERIAL, material);
		;
    break;}
case 422:
#line 1930 "vrml.y"
{
			MaterialNode *material = (MaterialNode *)GetCurrentNodeObject();
			material->initialize();
			PopNode();
		;
    break;}
case 425:
#line 1950 "vrml.y"
{
			PushNode(VRML_NODETYPE_MOVIETEXTURE_URL, GetCurrentNodeObject());
		;
    break;}
case 426:
#line 1957 "vrml.y"
{
			((MovieTextureNode *)GetCurrentNodeObject())->setLoop(yyvsp[0].ival);
		;
    break;}
case 427:
#line 1961 "vrml.y"
{
			((MovieTextureNode *)GetCurrentNodeObject())->setSpeed(yyvsp[0].fval);
		;
    break;}
case 428:
#line 1965 "vrml.y"
{
			((MovieTextureNode *)GetCurrentNodeObject())->setStartTime(yyvsp[0].fval);
		;
    break;}
case 429:
#line 1969 "vrml.y"
{
			((MovieTextureNode *)GetCurrentNodeObject())->setStopTime(yyvsp[0].fval);
		;
    break;}
case 430:
#line 1973 "vrml.y"
{
			PopNode();
		;
    break;}
case 431:
#line 1977 "vrml.y"
{
			((MovieTextureNode *)GetCurrentNodeObject())->setRepeatS(yyvsp[0].ival);
		;
    break;}
case 432:
#line 1981 "vrml.y"
{
			((MovieTextureNode *)GetCurrentNodeObject())->setRepeatT(yyvsp[0].ival);
		;
    break;}
case 433:
#line 1988 "vrml.y"
{
			MovieTextureNode *movieTexture = new MovieTextureNode();
			movieTexture->setName(GetDEFName());
			AddNode(movieTexture);
			PushNode(VRML_NODETYPE_MOVIETEXTURE, movieTexture);
		;
    break;}
case 434:
#line 1998 "vrml.y"
{
			MovieTextureNode *movieTexture = (MovieTextureNode *)GetCurrentNodeObject();
			movieTexture->initialize();
			PopNode();
		;
    break;}
case 437:
#line 2018 "vrml.y"
{
			PushNode(VRML_NODETYPE_NAVIGATIONINFO_AVATARSIZE, GetCurrentNodeObject());
		;
    break;}
case 438:
#line 2025 "vrml.y"
{
			PushNode(VRML_NODETYPE_NAVIGATIONINFO_TYPE, GetCurrentNodeObject());
		;
    break;}
case 439:
#line 2032 "vrml.y"
{
			PopNode();
		;
    break;}
case 440:
#line 2036 "vrml.y"
{
			((NavigationInfoNode *)GetCurrentNodeObject())->setHeadlight(yyvsp[0].ival);
		;
    break;}
case 441:
#line 2040 "vrml.y"
{
			((NavigationInfoNode *)GetCurrentNodeObject())->setSpeed(yyvsp[0].fval);
		;
    break;}
case 442:
#line 2044 "vrml.y"
{
			PopNode();
		;
    break;}
case 443:
#line 2048 "vrml.y"
{
			((NavigationInfoNode *)GetCurrentNodeObject())->setVisibilityLimit(yyvsp[0].fval);
		;
    break;}
case 444:
#line 2055 "vrml.y"
{
			NavigationInfoNode *navInfo = new NavigationInfoNode();
			navInfo->setName(GetDEFName());
			AddNode(navInfo);
			PushNode(VRML_NODETYPE_NAVIGATIONINFO, navInfo);
		;
    break;}
case 445:
#line 2065 "vrml.y"
{
			NavigationInfoNode *navInfo = (NavigationInfoNode *)GetCurrentNodeObject();
			navInfo->initialize();
			PopNode();
		;
    break;}
case 449:
#line 2089 "vrml.y"
{
			NormalNode *normal = new NormalNode();
			normal->setName(GetDEFName());
			AddNode(normal);
			PushNode(VRML_NODETYPE_NORMAL, normal);
		;
    break;}
case 450:
#line 2099 "vrml.y"
{
			NormalNode *normal = (NormalNode *)GetCurrentNodeObject();
			normal->initialize();
			PopNode();
		;
    break;}
case 453:
#line 2119 "vrml.y"
{
			PopNode();
		;
    break;}
case 454:
#line 2123 "vrml.y"
{
			PopNode();
		;
    break;}
case 455:
#line 2130 "vrml.y"
{
			NormalInterpolatorNode *normInterp = new NormalInterpolatorNode();
			normInterp->setName(GetDEFName());
			AddNode(normInterp);
			PushNode(VRML_NODETYPE_NORMALINTERPOLATOR, normInterp);
		;
    break;}
case 456:
#line 2140 "vrml.y"
{
			NormalInterpolatorNode *normInterp = (NormalInterpolatorNode *)GetCurrentNodeObject();
			normInterp->initialize();
			PopNode();
		;
    break;}
case 459:
#line 2160 "vrml.y"
{
			PopNode();
		;
    break;}
case 460:
#line 2164 "vrml.y"
{
			PopNode();
		;
    break;}
case 461:
#line 2171 "vrml.y"
{
			OrientationInterpolatorNode *oriInterp = new OrientationInterpolatorNode();
			oriInterp->setName(GetDEFName());
			AddNode(oriInterp);
			PushNode(VRML_NODETYPE_ORIENTATIONINTERPOLATOR, oriInterp);
		;
    break;}
case 462:
#line 2181 "vrml.y"
{
			OrientationInterpolatorNode *oriInterp = (OrientationInterpolatorNode *)GetCurrentNodeObject();
			oriInterp->initialize();
			PopNode();
		;
    break;}
case 465:
#line 2201 "vrml.y"
{
			PushNode(VRML_NODETYPE_PIXELTEXTURE_IMAGE, GetCurrentNodeObject());
		;
    break;}
case 466:
#line 2208 "vrml.y"
{
			PopNode();
		;
    break;}
case 467:
#line 2212 "vrml.y"
{
			((PixelTextureNode *)GetCurrentNodeObject())->setRepeatS(yyvsp[0].ival);
		;
    break;}
case 468:
#line 2216 "vrml.y"
{
			((PixelTextureNode *)GetCurrentNodeObject())->setRepeatT(yyvsp[0].ival);
		;
    break;}
case 469:
#line 2223 "vrml.y"
{
			PixelTextureNode *pixTexture = new PixelTextureNode();
			pixTexture->setName(GetDEFName());
			AddNode(pixTexture);
			PushNode(VRML_NODETYPE_PIXELTEXTURE, pixTexture);
		;
    break;}
case 470:
#line 2233 "vrml.y"
{
			PixelTextureNode *pixTexture = (PixelTextureNode *)GetCurrentNodeObject();
			pixTexture->initialize();
			PopNode();
		;
    break;}
case 473:
#line 2253 "vrml.y"
{
			((PlaneSensorNode *)GetCurrentNodeObject())->setAutoOffset(yyvsp[0].ival);
		;
    break;}
case 474:
#line 2257 "vrml.y"
{
			((PlaneSensorNode *)GetCurrentNodeObject())->setEnabled(yyvsp[0].ival);
		;
    break;}
case 475:
#line 2261 "vrml.y"
{
			((PlaneSensorNode *)GetCurrentNodeObject())->setMaxPosition(gVec2f);
		;
    break;}
case 476:
#line 2265 "vrml.y"
{
			((PlaneSensorNode *)GetCurrentNodeObject())->setMinPosition(gVec2f);
		;
    break;}
case 477:
#line 2269 "vrml.y"
{
			((PlaneSensorNode *)GetCurrentNodeObject())->setOffset(gVec3f);
		;
    break;}
case 478:
#line 2276 "vrml.y"
{
			PlaneSensorNode *psensor = new PlaneSensorNode();
			psensor->setName(GetDEFName());
			AddNode(psensor);
			PushNode(VRML_NODETYPE_PLANESENSOR, psensor);
		;
    break;}
case 479:
#line 2286 "vrml.y"
{
			PlaneSensorNode *psensor = (PlaneSensorNode *)GetCurrentNodeObject();
			psensor->initialize();
			PopNode();
		;
    break;}
case 482:
#line 2307 "vrml.y"
{
			((PointLightNode *)GetCurrentNodeObject())->setAmbientIntensity(yyvsp[0].fval);
		;
    break;}
case 483:
#line 2311 "vrml.y"
{
			((PointLightNode *)GetCurrentNodeObject())->setAttenuation(gVec3f);
		;
    break;}
case 484:
#line 2315 "vrml.y"
{
			((PointLightNode *)GetCurrentNodeObject())->setColor(gColor);
		;
    break;}
case 485:
#line 2319 "vrml.y"
{
			((PointLightNode *)GetCurrentNodeObject())->setIntensity(yyvsp[0].fval);
		;
    break;}
case 486:
#line 2323 "vrml.y"
{
			((PointLightNode *)GetCurrentNodeObject())->setLocation(gVec3f);
		;
    break;}
case 487:
#line 2327 "vrml.y"
{
			((PointLightNode *)GetCurrentNodeObject())->setOn(yyvsp[0].ival);
		;
    break;}
case 488:
#line 2331 "vrml.y"
{
			((PointLightNode *)GetCurrentNodeObject())->setRadius(yyvsp[0].fval);
		;
    break;}
case 489:
#line 2338 "vrml.y"
{
			PointLightNode *pointLight = new PointLightNode();
			pointLight->setName(GetDEFName());
			AddNode(pointLight);
			PushNode(VRML_NODETYPE_POINTLIGHT, pointLight);
		;
    break;}
case 490:
#line 2348 "vrml.y"
{
			PointLightNode *pointLight = (PointLightNode *)GetCurrentNodeObject();
			pointLight->initialize();
			PopNode();
		;
    break;}
case 499:
#line 2378 "vrml.y"
{
			PointSetNode *pset = new PointSetNode();
			pset->setName(GetDEFName());
			AddNode(pset);
			PushNode(VRML_NODETYPE_POINTSET, pset);
		;
    break;}
case 500:
#line 2388 "vrml.y"
{
			PointSetNode *pset = (PointSetNode *)GetCurrentNodeObject();
			pset->initialize();
			PopNode();
		;
    break;}
case 503:
#line 2407 "vrml.y"
{
			PopNode();
		;
    break;}
case 504:
#line 2411 "vrml.y"
{
			PopNode();
		;
    break;}
case 505:
#line 2418 "vrml.y"
{
			PositionInterpolatorNode *posInterp = new PositionInterpolatorNode();
			posInterp->setName(GetDEFName());
			AddNode(posInterp);
			PushNode(VRML_NODETYPE_POSITIONINTERPOLATOR, posInterp);
		;
    break;}
case 506:
#line 2428 "vrml.y"
{
			PositionInterpolatorNode *posInterp = (PositionInterpolatorNode *)GetCurrentNodeObject();
			posInterp->initialize();
			PopNode();
		;
    break;}
case 509:
#line 2448 "vrml.y"
{
			((ProximitySensorNode *)GetCurrentNodeObject())->setCenter(gVec3f);
		;
    break;}
case 510:
#line 2452 "vrml.y"
{
			((ProximitySensorNode *)GetCurrentNodeObject())->setSize(gVec3f);
		;
    break;}
case 511:
#line 2456 "vrml.y"
{
			((ProximitySensorNode *)GetCurrentNodeObject())->setEnabled(yyvsp[0].ival);
		;
    break;}
case 512:
#line 2463 "vrml.y"
{
			ProximitySensorNode *psensor = new ProximitySensorNode();
			psensor->setName(GetDEFName());
			AddNode(psensor);
			PushNode(VRML_NODETYPE_PROXIMITYSENSOR, psensor);
		;
    break;}
case 513:
#line 2473 "vrml.y"
{
			ProximitySensorNode *psensor = (ProximitySensorNode *)GetCurrentNodeObject();
			psensor->initialize();
			PopNode();
		;
    break;}
case 516:
#line 2493 "vrml.y"
{
			PopNode();
		;
    break;}
case 517:
#line 2497 "vrml.y"
{
			PopNode();
		;
    break;}
case 518:
#line 2504 "vrml.y"
{
			ScalarInterpolatorNode *scalarInterp = new ScalarInterpolatorNode();
			scalarInterp->setName(GetDEFName());
			AddNode(scalarInterp);
			PushNode(VRML_NODETYPE_SCALARINTERPOLATOR, scalarInterp);
		;
    break;}
case 519:
#line 2514 "vrml.y"
{
			ScalarInterpolatorNode *scalarInterp = (ScalarInterpolatorNode *)GetCurrentNodeObject();
			scalarInterp->initialize();
			PopNode();
		;
    break;}
case 522:
#line 2534 "vrml.y"
{
			PushNode(VRML_NODETYPE_SCRIPT_URL, GetCurrentNodeObject());
		;
    break;}
case 523:
#line 2541 "vrml.y"
{
			PopNode();
		;
    break;}
case 524:
#line 2545 "vrml.y"
{
			((ScriptNode *)GetCurrentNodeObject())->setDirectOutput(yyvsp[0].ival);
		;
    break;}
case 525:
#line 2549 "vrml.y"
{
			((ScriptNode *)GetCurrentNodeObject())->setMustEvaluate(yyvsp[0].ival);
		;
    break;}
case 526:
#line 2558 "vrml.y"
{
			SFBool *value = new SFBool();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 527:
#line 2564 "vrml.y"
{
			SFFloat *value = new SFFloat();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 528:
#line 2570 "vrml.y"
{
			SFInt32 *value = new SFInt32();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 529:
#line 2576 "vrml.y"
{
			SFTime *value = new SFTime();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 530:
#line 2582 "vrml.y"
{
			SFRotation *value = new SFRotation();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 531:
#line 2596 "vrml.y"
{
			SFColor *value = new SFColor();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 532:
#line 2602 "vrml.y"
{
			SFImage *value = new SFImage();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 533:
#line 2608 "vrml.y"
{
			SFString *value = new SFString();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 534:
#line 2614 "vrml.y"
{
			SFVec2f *value = new SFVec2f();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 535:
#line 2620 "vrml.y"
{
			SFVec3f *value = new SFVec3f();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 536:
#line 2631 "vrml.y"
{
			MFFloat *value = new MFFloat();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 537:
#line 2637 "vrml.y"
{
			MFInt32 *value = new MFInt32();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 538:
#line 2643 "vrml.y"
{
			MFTime *value = new MFTime();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 539:
#line 2649 "vrml.y"
{
			MFRotation *value = new MFRotation();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 540:
#line 2663 "vrml.y"
{
			MFColor *value = new MFColor();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 541:
#line 2669 "vrml.y"
{
			MFString *value = new MFString();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 542:
#line 2675 "vrml.y"
{
			MFVec2f *value = new MFVec2f();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 543:
#line 2681 "vrml.y"
{
			MFVec3f *value = new MFVec3f();
			((ScriptNode *)GetCurrentNodeObject())->addEventIn(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 544:
#line 2692 "vrml.y"
{
			SFBool *value = new SFBool();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 545:
#line 2698 "vrml.y"
{
			SFFloat *value = new SFFloat();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 546:
#line 2704 "vrml.y"
{
			SFInt32 *value = new SFInt32();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 547:
#line 2710 "vrml.y"
{
			SFTime *value = new SFTime();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 548:
#line 2716 "vrml.y"
{
			SFRotation *value = new SFRotation();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 549:
#line 2730 "vrml.y"
{
			SFColor *value = new SFColor();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 550:
#line 2736 "vrml.y"
{
			SFImage *value = new SFImage();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 551:
#line 2742 "vrml.y"
{
			SFString *value = new SFString();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 552:
#line 2748 "vrml.y"
{
			SFVec2f *value = new SFVec2f();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 553:
#line 2754 "vrml.y"
{
			SFVec3f *value = new SFVec3f();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 554:
#line 2765 "vrml.y"
{
			MFFloat *value = new MFFloat();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 555:
#line 2771 "vrml.y"
{
			MFInt32 *value = new MFInt32();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 556:
#line 2777 "vrml.y"
{
			MFTime *value = new MFTime();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 557:
#line 2783 "vrml.y"
{
			MFRotation *value = new MFRotation();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 558:
#line 2797 "vrml.y"
{
			MFColor *value = new MFColor();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 559:
#line 2803 "vrml.y"
{
			MFString *value = new MFString();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 560:
#line 2809 "vrml.y"
{
			MFVec2f *value = new MFVec2f();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 561:
#line 2815 "vrml.y"
{
			MFVec3f *value = new MFVec3f();
			((ScriptNode *)GetCurrentNodeObject())->addEventOut(yyvsp[0].sval, value);
			delete[] yyvsp[0].sval;
		;
    break;}
case 562:
#line 2826 "vrml.y"
{
			SFBool *value = new SFBool(yyvsp[0].ival);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 563:
#line 2832 "vrml.y"
{
			SFFloat *value = new SFFloat(yyvsp[0].fval);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 564:
#line 2838 "vrml.y"
{
			SFInt32 *value = new SFInt32(yyvsp[0].ival);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 565:
#line 2844 "vrml.y"
{
			SFTime *value = new SFTime(yyvsp[0].fval);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 566:
#line 2850 "vrml.y"
{
			SFRotation *value = new SFRotation(gRotation);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 567:
#line 2857 "vrml.y"
{
			SFNode *value = new SFNode();
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 568:
#line 2864 "vrml.y"
{
			Node *node = GetParserObject()->findNodeByName(yyvsp[0].sval);
			SFNode *value = new SFNode(node);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-2].sval, value);
			delete[] yyvsp[-2].sval; delete[] yyvsp[0].sval;
		;
    break;}
case 569:
#line 2872 "vrml.y"
{
			SFColor *value = new SFColor(gColor);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 570:
#line 2886 "vrml.y"
{
			SFString *value = new SFString(yyvsp[0].sval);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 571:
#line 2892 "vrml.y"
{
			SFVec2f *value = new SFVec2f(gVec2f);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 572:
#line 2898 "vrml.y"
{
			SFVec3f *value = new SFVec3f(gVec3f);
			((ScriptNode *)GetCurrentNodeObject())->addField(yyvsp[-1].sval, value);
			delete[] yyvsp[-1].sval;
		;
    break;}
case 573:
#line 2908 "vrml.y"
{
			ScriptNode *script = new ScriptNode();
			script->setName(GetDEFName());
			AddNode(script);
			PushNode(VRML_NODETYPE_SCRIPT, script);
		;
    break;}
case 574:
#line 2918 "vrml.y"
{
			ScriptNode *script = (ScriptNode *)GetCurrentNodeObject();
			script->initialize();
			PopNode();
		;
    break;}
case 583:
#line 2948 "vrml.y"
{
			ShapeNode *shape = new ShapeNode();
			shape->setName(GetDEFName());
			AddNode(shape);
			PushNode(VRML_NODETYPE_SHAPE, shape);
		;
    break;}
case 584:
#line 2958 "vrml.y"
{
			ShapeNode *shape = (ShapeNode *)GetCurrentNodeObject();
			shape->initialize();
			PopNode();
		;
    break;}
case 587:
#line 2978 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setDirection(gVec3f);
		;
    break;}
case 588:
#line 2982 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setIntensity(yyvsp[0].fval);
		;
    break;}
case 589:
#line 2986 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setLocation(gVec3f);
		;
    break;}
case 590:
#line 2990 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setMinBack(yyvsp[0].fval);
		;
    break;}
case 591:
#line 2994 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setMaxFront(yyvsp[0].fval);
		;
    break;}
case 592:
#line 2998 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setMinBack(yyvsp[0].fval);
		;
    break;}
case 593:
#line 3002 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setMinFront(yyvsp[0].fval);
		;
    break;}
case 594:
#line 3006 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setPriority(yyvsp[0].fval);
		;
    break;}
case 599:
#line 3014 "vrml.y"
{
			((SoundNode *)GetCurrentNodeObject())->setSpatialize(yyvsp[0].ival);
		;
    break;}
case 600:
#line 3021 "vrml.y"
{
			SoundNode *sound = new SoundNode();
			sound->setName(GetDEFName());
			AddNode(sound);
			PushNode(VRML_NODETYPE_SOUND, sound);
		;
    break;}
case 601:
#line 3031 "vrml.y"
{
			SoundNode *sound = (SoundNode *)GetCurrentNodeObject();
			sound->initialize();
			PopNode();
		;
    break;}
case 604:
#line 3051 "vrml.y"
{
			((SphereNode *)GetCurrentNodeObject())->setRadius(yyvsp[0].fval);
		;
    break;}
case 605:
#line 3058 "vrml.y"
{
			SphereNode *sphere = new SphereNode();
			sphere->setName(GetDEFName());
			AddNode(sphere);
			PushNode(VRML_NODETYPE_SPHERE, sphere);
		;
    break;}
case 606:
#line 3068 "vrml.y"
{
			SphereNode *sphere = (SphereNode *)GetCurrentNodeObject();
			sphere->initialize();
			PopNode();
		;
    break;}
case 609:
#line 3088 "vrml.y"
{
			((SphereSensorNode *)GetCurrentNodeObject())->setAutoOffset(yyvsp[0].ival);
		;
    break;}
case 610:
#line 3092 "vrml.y"
{
			((SphereSensorNode *)GetCurrentNodeObject())->setEnabled(yyvsp[0].ival);
		;
    break;}
case 611:
#line 3096 "vrml.y"
{
			((SphereSensorNode *)GetCurrentNodeObject())->setOffset(gRotation);
		;
    break;}
case 612:
#line 3103 "vrml.y"
{
			SphereSensorNode *spsensor = new SphereSensorNode();
			spsensor->setName(GetDEFName());
			AddNode(spsensor);
			PushNode(VRML_NODETYPE_SPHERESENSOR, spsensor);
		;
    break;}
case 613:
#line 3113 "vrml.y"
{
			SphereSensorNode *spsensor = (SphereSensorNode *)GetCurrentNodeObject();
			spsensor->initialize();
			PopNode();
		;
    break;}
case 616:
#line 3133 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setAmbientIntensity(yyvsp[0].fval);
		;
    break;}
case 617:
#line 3137 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setAttenuation(gVec3f);
		;
    break;}
case 618:
#line 3141 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setBeamWidth(yyvsp[0].fval);
		;
    break;}
case 619:
#line 3145 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setColor(gColor);
		;
    break;}
case 620:
#line 3149 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setCutOffAngle(yyvsp[0].fval);
		;
    break;}
case 621:
#line 3153 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setDirection(gVec3f);
		;
    break;}
case 622:
#line 3157 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setIntensity(yyvsp[0].fval);
		;
    break;}
case 623:
#line 3161 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setLocation(gVec3f);
		;
    break;}
case 624:
#line 3165 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setOn(yyvsp[0].ival);
		;
    break;}
case 625:
#line 3169 "vrml.y"
{
			((SpotLightNode *)GetCurrentNodeObject())->setRadius(yyvsp[0].fval);
		;
    break;}
case 626:
#line 3176 "vrml.y"
{
			SpotLightNode *spotLight = new SpotLightNode();
			spotLight->setName(GetDEFName());
			AddNode(spotLight);
			PushNode(VRML_NODETYPE_SPOTLIGHT, spotLight);
		;
    break;}
case 627:
#line 3186 "vrml.y"
{
			SpotLightNode *spotLight = (SpotLightNode *)GetCurrentNodeObject();
			spotLight->initialize();
			PopNode();
		;
    break;}
case 630:
#line 3206 "vrml.y"
{
			PushNode(VRML_NODETYPE_SWITCH_CHOICE, GetCurrentNodeObject());
		;
    break;}
case 631:
#line 3213 "vrml.y"
{
			PopNode();							
		;
    break;}
case 632:
#line 3217 "vrml.y"
{
			PopNode();							
		;
    break;}
case 633:
#line 3221 "vrml.y"
{
			((SwitchNode *)GetCurrentNodeObject())->setWhichChoice(yyvsp[0].ival);
		;
    break;}
case 634:
#line 3229 "vrml.y"
{   
			SwitchNode *switchNode = new SwitchNode();
			switchNode->setName(GetDEFName());
			AddNode(switchNode);
			PushNode(VRML_NODETYPE_SWITCH, switchNode);
		;
    break;}
case 635:
#line 3239 "vrml.y"
{
			SwitchNode *switchNode = (SwitchNode *)GetCurrentNodeObject();
			switchNode->initialize();
			PopNode();
		;
    break;}
case 638:
#line 3259 "vrml.y"
{
			PushNode(VRML_NODETYPE_TEXT_STRING, GetCurrentNodeObject());
		;
    break;}
case 639:
#line 3266 "vrml.y"
{
			PushNode(VRML_NODETYPE_TEXT_LENGTH, GetCurrentNodeObject());
		;
    break;}
case 640:
#line 3273 "vrml.y"
{
			PopNode();
		;
    break;}
case 644:
#line 3280 "vrml.y"
{
			PopNode();
		;
    break;}
case 645:
#line 3284 "vrml.y"
{
			((TextNode *)GetCurrentNodeObject())->setMaxExtent(yyvsp[0].fval);
		;
    break;}
case 646:
#line 3292 "vrml.y"
{
			TextNode *text = new TextNode();
			text->setName(GetDEFName());
			AddNode(text);
			PushNode(VRML_NODETYPE_TEXT, text);
		;
    break;}
case 647:
#line 3302 "vrml.y"
{
			TextNode *text = (TextNode *)GetCurrentNodeObject();
			text->initialize();
			PopNode();
		;
    break;}
case 651:
#line 3327 "vrml.y"
{
			TextureCoordinateNode *texCoord = new TextureCoordinateNode();
			texCoord->setName(GetDEFName());
			AddNode(texCoord);
			PushNode(VRML_NODETYPE_TEXTURECOODINATE, texCoord);
		;
    break;}
case 652:
#line 3337 "vrml.y"
{
			TextureCoordinateNode *texCoord = (TextureCoordinateNode *)GetCurrentNodeObject();
			texCoord->initialize();
			PopNode();
		;
    break;}
case 655:
#line 3357 "vrml.y"
{
			((TextureTransformNode *)GetCurrentNodeObject())->setCenter(gVec2f);
		;
    break;}
case 656:
#line 3361 "vrml.y"
{
			((TextureTransformNode *)GetCurrentNodeObject())->setRotation(yyvsp[0].fval);
		;
    break;}
case 657:
#line 3365 "vrml.y"
{
			((TextureTransformNode *)GetCurrentNodeObject())->setScale(gVec2f);
		;
    break;}
case 658:
#line 3369 "vrml.y"
{
			((TextureTransformNode *)GetCurrentNodeObject())->setTranslation(gVec2f);
		;
    break;}
case 659:
#line 3377 "vrml.y"
{
			TextureTransformNode *textureTransform = new TextureTransformNode();
			textureTransform->setName(GetDEFName());
			AddNode(textureTransform);
			PushNode(VRML_NODETYPE_TEXTURETRANSFORM, textureTransform);
		;
    break;}
case 660:
#line 3387 "vrml.y"
{
			TextureTransformNode *textureTransform = (TextureTransformNode *)GetCurrentNodeObject();
			textureTransform->initialize();
			PopNode();
		;
    break;}
case 663:
#line 3407 "vrml.y"
{
			((TimeSensorNode *)GetCurrentNodeObject())->setCycleInterval(yyvsp[0].fval);
		;
    break;}
case 664:
#line 3411 "vrml.y"
{
			((TimeSensorNode *)GetCurrentNodeObject())->setEnabled(yyvsp[0].ival);
		;
    break;}
case 665:
#line 3415 "vrml.y"
{
			((TimeSensorNode *)GetCurrentNodeObject())->setLoop(yyvsp[0].ival);
		;
    break;}
case 666:
#line 3419 "vrml.y"
{
			((TimeSensorNode *)GetCurrentNodeObject())->setStartTime(yyvsp[0].fval);
		;
    break;}
case 667:
#line 3423 "vrml.y"
{
			((TimeSensorNode *)GetCurrentNodeObject())->setStopTime(yyvsp[0].fval);
		;
    break;}
case 668:
#line 3431 "vrml.y"
{
			TimeSensorNode *tsensor = new TimeSensorNode();
			tsensor->setName(GetDEFName());
			AddNode(tsensor);
			PushNode(VRML_NODETYPE_TIMESENSOR, tsensor);
		;
    break;}
case 669:
#line 3441 "vrml.y"
{
			TimeSensorNode *tsensor = (TimeSensorNode *)GetCurrentNodeObject();
			tsensor->initialize();
			PopNode();
		;
    break;}
case 672:
#line 3461 "vrml.y"
{
			((TouchSensorNode *)GetCurrentNodeObject())->setEnabled(yyvsp[0].ival);
		;
    break;}
case 673:
#line 3468 "vrml.y"
{
			TouchSensorNode *touchSensor = new TouchSensorNode();
			touchSensor->setName(GetDEFName());
			AddNode(touchSensor);
			PushNode(VRML_NODETYPE_TOUCHSENSOR, touchSensor);
		;
    break;}
case 674:
#line 3478 "vrml.y"
{
			TouchSensorNode *touchSensor = (TouchSensorNode *)GetCurrentNodeObject();
			touchSensor->initialize();
			PopNode();
		;
    break;}
case 678:
#line 3499 "vrml.y"
{
			((TransformNode *)GetCurrentNodeObject())->setCenter(gVec3f);
		;
    break;}
case 679:
#line 3503 "vrml.y"
{
			((TransformNode *)GetCurrentNodeObject())->setRotation(gRotation);
		;
    break;}
case 680:
#line 3507 "vrml.y"
{
			((TransformNode *)GetCurrentNodeObject())->setScale(gVec3f);
		;
    break;}
case 681:
#line 3511 "vrml.y"
{
			((TransformNode *)GetCurrentNodeObject())->setScaleOrientation(gRotation);
		;
    break;}
case 682:
#line 3515 "vrml.y"
{
			((TransformNode *)GetCurrentNodeObject())->setTranslation(gVec3f);
		;
    break;}
case 685:
#line 3524 "vrml.y"
{
			TransformNode *transform = new TransformNode();
			transform->setName(GetDEFName());
			AddNode(transform);
			PushNode(VRML_NODETYPE_TRANSFORM, transform);
		;
    break;}
case 686:
#line 3534 "vrml.y"
{
			TransformNode *transform = (TransformNode *)GetCurrentNodeObject();
			transform->initialize();
			PopNode();
		;
    break;}
case 689:
#line 3554 "vrml.y"
{
			((ViewpointNode *)GetCurrentNodeObject())->setFieldOfView(yyvsp[0].fval);
		;
    break;}
case 690:
#line 3558 "vrml.y"
{
			((ViewpointNode *)GetCurrentNodeObject())->setJump(yyvsp[0].ival);
		;
    break;}
case 691:
#line 3562 "vrml.y"
{
			((ViewpointNode *)GetCurrentNodeObject())->setOrientation(gRotation);
		;
    break;}
case 692:
#line 3566 "vrml.y"
{
			((ViewpointNode *)GetCurrentNodeObject())->setPosition(gVec3f);
		;
    break;}
case 693:
#line 3570 "vrml.y"
{
			((ViewpointNode *)GetCurrentNodeObject())->setDescription(yyvsp[0].sval);
		;
    break;}
case 694:
#line 3577 "vrml.y"
{
			ViewpointNode *viewpoint = new ViewpointNode();
			viewpoint->setName(GetDEFName());
			AddNode(viewpoint);
			PushNode(VRML_NODETYPE_VIEWPOINT, viewpoint);
		;
    break;}
case 695:
#line 3587 "vrml.y"
{
			ViewpointNode *viewpoint = (ViewpointNode *)GetCurrentNodeObject();
			viewpoint->initialize();
			PopNode();
		;
    break;}
case 698:
#line 3607 "vrml.y"
{
			((VisibilitySensorNode *)GetCurrentNodeObject())->setCenter(gVec3f);
		;
    break;}
case 699:
#line 3611 "vrml.y"
{
			((VisibilitySensorNode *)GetCurrentNodeObject())->setEnabled(yyvsp[0].ival);
		;
    break;}
case 700:
#line 3615 "vrml.y"
{
			((VisibilitySensorNode *)GetCurrentNodeObject())->setSize(gVec3f);
		;
    break;}
case 701:
#line 3622 "vrml.y"
{
			VisibilitySensorNode *vsensor = new VisibilitySensorNode();
			vsensor->setName(GetDEFName());
			AddNode(vsensor);
			PushNode(VRML_NODETYPE_VISIBILITYSENSOR, vsensor);
		;
    break;}
case 702:
#line 3632 "vrml.y"
{
			VisibilitySensorNode *vsensor = (VisibilitySensorNode *)GetCurrentNodeObject();
			vsensor->initialize();
			PopNode();
		;
    break;}
case 705:
#line 3652 "vrml.y"
{
			PushNode(VRML_NODETYPE_WORLDINFO_INFO, GetCurrentNodeObject());
		;
    break;}
case 706:
#line 3659 "vrml.y"
{
			PopNode();
		;
    break;}
case 707:
#line 3663 "vrml.y"
{
			((WorldInfoNode *)GetCurrentNodeObject())->setTitle(yyvsp[0].sval);
		;
    break;}
case 708:
#line 3670 "vrml.y"
{
			WorldInfoNode *worldInfo = new WorldInfoNode();
			worldInfo->setName(GetDEFName());
			AddNode(worldInfo);
			PushNode(VRML_NODETYPE_WORLDINFO, worldInfo);
		;
    break;}
case 709:
#line 3680 "vrml.y"
{
			WorldInfoNode *worldInfo = (WorldInfoNode *)GetCurrentNodeObject();
			worldInfo->initialize();
			PopNode();
		;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 543 "/usr/share/bison.simple"

  yyvsp -= yylen;
  yyssp -= yylen;
#ifdef YYLSP_NEEDED
  yylsp -= yylen;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++yyvsp = yyval;

#ifdef YYLSP_NEEDED
  yylsp++;
  if (yylen == 0)
    {
      yylsp->first_line = yylloc.first_line;
      yylsp->first_column = yylloc.first_column;
      yylsp->last_line = (yylsp-1)->last_line;
      yylsp->last_column = (yylsp-1)->last_column;
      yylsp->text = 0;
    }
  else
    {
      yylsp->last_line = (yylsp+yylen-1)->last_line;
      yylsp->last_column = (yylsp+yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTBASE] + *yyssp;
  if (yystate >= 0 && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTBASE];

  goto yynewstate;

yyerrlab:   /* here on detecting error */

  if (! yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++yynerrs;

#ifdef YYERROR_VERBOSE
      yyn = yypact[yystate];

      if (yyn > YYFLAG && yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -yyn if nec to avoid negative indexes in yycheck.  */
	  for (x = (yyn < 0 ? -yyn : 0);
	       x < (sizeof(yytname) / sizeof(char *)); x++)
	    if (yycheck[x + yyn] == x)
	      size += strlen(yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (yyn < 0 ? -yyn : 0);
		       x < (sizeof(yytname) / sizeof(char *)); x++)
		    if (yycheck[x + yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      yyerror(msg);
	      free(msg);
	    }
	  else
	    yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	yyerror("parse error");
    }

  goto yyerrlab1;
yyerrlab1:   /* here on error raised explicitly by an action */

  if (yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of input */
      if (yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", yychar, yytname[yychar1]);
#endif

      yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto yyerrhandle;

yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  yyn = yydefact[yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (yyn) goto yydefault;
#endif

yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (yyssp == yyss) YYABORT;
  yyvsp--;
  yystate = *--yyssp;
#ifdef YYLSP_NEEDED
  yylsp--;
#endif

#if YYDEBUG != 0
  if (yydebug)
    {
      short *ssp1 = yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

yyerrhandle:

  yyn = yypact[yystate];
  if (yyn == YYFLAG)
    goto yyerrdefault;

  yyn += YYTERROR;
  if (yyn < 0 || yyn > YYLAST || yycheck[yyn] != YYTERROR)
    goto yyerrdefault;

  yyn = yytable[yyn];
  if (yyn < 0)
    {
      if (yyn == YYFLAG)
	goto yyerrpop;
      yyn = -yyn;
      goto yyreduce;
    }
  else if (yyn == 0)
    goto yyerrpop;

  if (yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++yyvsp = yylval;
#ifdef YYLSP_NEEDED
  *++yylsp = yylloc;
#endif

  yystate = yyn;
  goto yynewstate;

 yyacceptlab:
  /* YYACCEPT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 0;

 yyabortlab:
  /* YYABORT comes here.  */
  if (yyfree_stacks)
    {
      free (yyss);
      free (yyvs);
#ifdef YYLSP_NEEDED
      free (yyls);
#endif
    }
  return 1;
}
#line 3687 "vrml.y"

