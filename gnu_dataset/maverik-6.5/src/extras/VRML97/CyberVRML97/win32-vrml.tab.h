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

typedef union {
int		ival;
float	fval;
char	*sval;
} YYSTYPE;
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


extern YYSTYPE yylval;
