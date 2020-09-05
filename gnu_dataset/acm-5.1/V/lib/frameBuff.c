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

#if defined(mips) && !defined(ultrix) && !defined(sgi)

#include </usr/include/sys/types.h>
#include </usr/include/sys/param.h>
#include </usr/include/sys/grafreg.h>
#include </usr/include/sys/ipc.h>
#include </usr/include/sys/shm.h>

static unsigned long *mips_buf_word_addr, mips_pixel[256];
static unsigned long mips_amask[4] =
{0x00000000, 0xFF000000, 0xFFFF0000, 0xFFFFFF00};
static unsigned long mips_bmask[4] =
{0xFFFFFFFF, 0x00FFFFFF, 0x0000FFFF, 0x000000FF};
static int mips_addval[4] =
{-4, -3, -2, -1};

static int x_origin = 0, y_origin = 0;

void
FrameBufferIOInit()
{

	register unsigned long value;
	int       buf_id, reg_id;
	char     *buf_addr, *reg_addr;
	register int i, j, k;
	extern char *shmat();

	value = 0;

	for (i = 0; i < 256; ++i) {
		mips_pixel[i] = (i << 24) | (i << 16) | (i << 8) | i;
	}

	buf_id = shmget(GBFCKEY, GRAPHICS_FRAME_SIZE, 0666);
	reg_id = shmget(GREGKEY, GRAPHICS_REG_SIZE, 0666);

	buf_addr = shmat(buf_id, GRAPHICS_FRAME_ADDR, 0);
	reg_addr = shmat(reg_id, GRAPHICS_REG_ADDR, 0);

	mips_buf_word_addr = (unsigned long *) buf_addr;
}

void
FrameBufferSetOrigin(x, y)
int       x, y;
{

	x_origin = x;
	y_origin = y;

}

void
FrameBufferRun(x, y, length, pixel)
register int x, y, length, pixel;
{

	register unsigned long value = mips_pixel[pixel], i, j;
	register unsigned long *p, *pend;

	x += x_origin;
	y += y_origin;

	p = mips_buf_word_addr + (y * 512 + (x >> 2));

	i = x % 4;

	if (i + length < 4) {
		j = (*p & mips_amask[i]) | (value & mips_bmask[i]);
		i += length;
		*p = (*p & mips_bmask[i]) | (j & mips_amask[i]);
		return;
	}

	if (i != 0) {
		*p = (*p & mips_amask[i]) | (value & mips_bmask[i]);
		length += mips_addval[i];
		++p;
	}

	pend = p + (length >> 2);
	for (; p < pend;) {
		*p++ = value;
	}

	if ((i = length % 4) != 0) {
		*p = (*p & mips_bmask[i]) | (value & mips_amask[i]);
	}

}

#endif							/* defined(mips) ... */

#ifdef WIN32

/*#define WIN32_LEAN_AND_MEAN */
#include <objbase.h>
#include <windows.h>
#include <windowsx.h>
#include <ddraw.h>
#include <stdlib.h>
#include <stdarg.h>
#include <assert.h>

#include <Vlib.h>
#include <string.h>

long      (*g_remap) (int, int, long) = NULL;
void      (*g_FrameBufferRun) (int, int x, int y, int length, unsigned long pixel) = NULL;
void      (*g_getVideoConfig) (int, int *depth, int *width, int *height) = NULL;

void
SetAlibCallbacks(long (*remap) (int, int, long),
				 void      (*FrameBufferRun) (int, int x, int y, int length, unsigned long pixel),
				 void      (*config) (int, int *, int *, int *))
{
	g_remap = remap;
	g_FrameBufferRun = FrameBufferRun;
	g_getVideoConfig = config;
}

extern LPDIRECTDRAW GetDirectDrawInterface(int iWndIndex);
extern LPDIRECTDRAWSURFACE GetBackBuffer(int iWndIndex);
extern LPDIRECTDRAWSURFACE GetFrontBuffer(int iWndIndex);
extern    printf(const char *,...);

/*  defined in ddraw.cpp */

extern LPDIRECTDRAW lpDD;
extern LPDIRECTDRAWSURFACE lpBack;
extern LPDIRECTDRAWSURFACE lpFront;

//#define RGB(r,g,b)  (0x3f3f3fL & ((long)(b) << 16 | (g) << 8 | (r)))
long      _remappalette(int color_index, long rgb_value);
void      _setcolor(), _moveto(int, int), _lineto(int, int);

typedef struct {
	char     *name;
	XColor    value;
} ColorLookup;

#define xx(x)	((x) << 8) | 0xff

static ColorLookup quickColors[] =
{
	{"black",
	 {0, 0, 0, RGB(0, 0, 0), 0, 0}},
	{"white",
	 {0xffff, 0xffff, 0xffff, RGB(255, 255, 255), 0, 0}},
	{"lightgray",
	 {xx(200), xx(200), xx(200), 0, 0, 0}},
	{"gray44",
	 {xx(28 << 2), xx(28 << 2), xx(28 << 2), 0, 0, 0}},
	{"gray33",
	 {xx(20 << 2), xx(20 << 2), xx(20 << 2), 0, 0, 0}},
	{"red",
	 {xx(255), 0, 0, 0, 0, 0}},
	{"skyblue",
	 {xx(135), xx(206), xx(255), 0, 0, 0}},
	{"cyan",
	 {xx(0), xx(255), xx(255), 0, 0, 0}},
	{"yellow",
	 {xx(255), xx(255), xx(0), 0, 0, 0}},
	{"magenta",
	 {xx(255), xx(0), xx(255), 0, 0, 0}},
	 {"tan",
	 {xx(210), xx(180), xx(140), 0, 0, 0}},
	 {"linen",
	 {xx(250), xx(240), xx(230), 0, 0, 0}},
	 {"azure",
	 {xx(240), xx(255), xx(255), 0, 0, 0}},
	 {"MistyRose",
	 {xx(255), xx(228), xx(225), 0, 0, 0}}
};

/*
 *  The 16 standard bitmap colors
 */
static ColorLookup bitmapColors[] =
{
	{"black",
	 {0, 0, 0, RGB(0, 0, 0), 0, 0}},
	{"dark red",
	 {xx(128), xx(0), xx(0), RGB(128, 0, 0), 0, 0}},
	{"dark green",
	 {xx(0), xx(128), xx(0), RGB(0, 128, 0), 0, 0}},
	{"dark yellow",
	 {xx(128), xx(128), xx(0), RGB(128, 128, 0), 0, 0}},
	{"dark blue",
	 {xx(0), xx(0), xx(128), RGB(0, 0, 128), 0, 0}},
	{"dark magenta",
	 {xx(128), xx(0), xx(128), RGB(128, 0, 128), 0, 0}},
	{"dark cyan",
	 {xx(0), xx(128), xx(128), RGB(0, 128, 128), 0, 0}},
	{"light gray",
	 {xx(192), xx(192), xx(192), RGB(192, 192, 192), 0, 0}},
	{"medium gray",
	 {xx(128), xx(128), xx(128), RGB(128, 128, 128), 0, 0}},
	{"red",
	 {xx(255), xx(0), xx(0), RGB(255, 0, 0), 0, 0}},
	{"green",
	 {xx(0), xx(255), xx(0), RGB(0, 255, 0), 0, 0}},
	{"yellow",
	 {xx(255), xx(255), xx(0), RGB(255, 255, 0), 0, 0}},
	{"blue",
	 {xx(0), xx(0), xx(255), RGB(0, 0, 255), 0, 0}},
	{"magenta",
	 {xx(255), xx(0), xx(255), RGB(255, 0, 255), 0, 0}},
	{"cyan",
	 {xx(0), xx(255), xx(255), RGB(0, 255, 255), 0, 0}},
	{"white",
	 {xx(255), xx(255), xx(255), RGB(255, 255, 255), 0, 0}}
};

/*

   index red green blue
   ----- --- ----- -----

   0 0 0 0 black

   1 128 0 0 dark red

   2 0 128 0 dark green

   3 128 128 0 dark yellow

   4 0 0 128 dark blue

   5 128 0 128 dark magenta

   6 0 128 128 dark cyan

   7 192 192 192 light gray

   8 192 220 192 money green

   9 166 202 240 sky blue

    

   246 255 251 240 cream

   247 160 160 164 light gray

   248 128 128 128 medium gray

   249 255 0 0 red

   250 0 255 0 green

   251 255 255 0 yellow

   252 0 0 255 blue

   253 255 0 255 magenta

   254 0 255 255 cyan

   255 255 255 255 white

 */

static struct {
	int       flag;
	unsigned long color;
} color_map[256];

static int color_count = 0;

struct _videoconfig {
	int       bitsperpixel;
	int       numxpixels, numypixels;
	int       numcolors;
};

static struct _videoconfig vc =
{8, 640, 480, 256};
int       _getvideoconfig(struct _videoconfig *);

int
DisplayPlanes(dpy, s)
Display  *dpy;
int       s;
{
	if (color_count == 0)
		_getvideoconfig(&vc);
	return vc.bitsperpixel;
}

int
DisplayWidth(dpy, s)
Display  *dpy;
int       s;
{
	if (color_count == 0)
		_getvideoconfig(&vc);
	return vc.numxpixels;
}

int
DisplayHeight(dpy, s)
Display  *dpy;
int       s;
{
	if (color_count == 0)
		_getvideoconfig(&vc);
	return vc.numypixels;
}

void
FrameBufferGetGeometry(flag, width, height)
int       flag;
unsigned int *width;
unsigned int *height;
{
	if (color_count == 0)
		_getvideoconfig(&vc);

	*width = vc.numxpixels;
	*height = vc.numypixels;
	if ((color_count = vc.numcolors) > 256) {
		color_count = 256;
	}
}

void
FrameBufferIOInit()
{
	long      old;
	int       i, n;

	for (i = 0; i < 256; ++i) {
		if (i < 8) {
			color_map[i].flag = 1;
			color_map[i].color = bitmapColors[i].value.rgb;
			old = _remappalette((short) i, color_map[i].color);
		}
		else if (i >= 248) {
			n = i - 248 + 8;
			color_map[i].flag = 1;
			color_map[i].color = bitmapColors[n].value.rgb;
			old = _remappalette((short) i, color_map[i].color);
		}
		else {
			color_map[i].flag = 0;
		}
	}

	//   color_map[10].flag = color_map[11].flag = 1;
	//   color_map[10].color = quickColors[0].value.rgb;
	//   color_map[11].color = quickColors[1].value.rgb;
	//   old = _remappalette((short) 10, color_map[10].color);
	//   old = _remappalette((short) 11, color_map[11].color);
}

void
FrameBufferSetOrigin(x, y)
int       x, y;
{
}

//  This algorithm is quite imperfect

int
PCParseColor(name, pcolor)
char     *name;
XColor   *pcolor;
{
	int       qcnt = sizeof(quickColors) / sizeof(quickColors[0]);
	long      x;
	char     *p;

	pcolor->flags = 0;

	if (*name == '#') {
		++name;
		x = strtol(name, &p, 16);
		if (strlen(name) == 3) {
			pcolor->red = ((x & 0xf00) << 4) | 0x0fff;
			pcolor->green = ((x & 0x0f0) << 8) | 0x0fff;
			pcolor->blue = ((x & 0x00f) << 12) | 0x0fff;
		}
		else if (strlen(name) == 6) {
			pcolor->red = ((x & 0xff0000) >> 8) | 0x00ff;
			pcolor->green = ((x & 0x00ff00) << 0) | 0x00ff;
			pcolor->blue = ((x & 0x0000ff) << 8) | 0x00ff;
		}
		else {
			fprintf(stderr, "Invalid color spec #%s\n", name);
			return 0;
		}
	}
	else {
		for (x = 0; x < qcnt; ++x) {
			if (_stricmp(quickColors[x].name, name) == 0) {
				*pcolor = quickColors[x].value;
				goto done;
			}
		}
		return 0;
	}

  done:
	pcolor->rgb = RGB(
						 (pcolor->red >> 8),
						 (pcolor->green >> 8),
						 (pcolor->blue >> 8));
	return 1;
}

void
FrameBufferRun(int x, int y, int length, int pixel)
{
	unsigned long value;

	if (vc.bitsperpixel > 8) {
		value = color_map[pixel].color;
	}
	else {
		value = pixel;
	}
	(*g_FrameBufferRun) (0, x, y, length, value);
}

#if 0
void
MyDrawSegment(int iWndIndex, int x, int y, int length, unsigned long pixel_index)
{
	DDSURFACEDESC desc;
	HRESULT   hr;
	BYTE     *pBuf;
	int       iWidth, iHeight, iPitch, iDepth;

	// Lazy initialization

	if (!lpDD) {
		lpDD = GetDirectDrawInterface(0);
	}

	if (!lpBack) {
		lpBack = GetBackBuffer(0);
		lpFront = GetFrontBuffer(0);
	}

	desc.dwSize = sizeof(desc);
	IDirectDrawSurface_GetSurfaceDesc(lpBack, &desc);
	iWidth = desc.dwWidth;
	iHeight = desc.dwHeight;
	iPitch = desc.lPitch;

	// Get some info about back buffer

	if (!(desc.ddpfPixelFormat.dwFlags & DDPF_RGB)) {
		iDepth = 0;				// not RGB

	}
	else {
		iDepth = desc.ddpfPixelFormat.dwRGBBitCount;
	}
//      ASSERT(iDepth >= 8);

	// Get RGB masks
	static DWORD dwRMask, dwGMask, dwBMask;

	// For each mask, check its width in bits
	// and see how many bits from the LSB (lease significant
	// bit) end it's shifted
	static DWORD dwRShift, dwGShift, dwBShift;
	static DWORD dwRBits, dwGBits, dwBBits;

	if (got_shifts == 0 && iDepth > 8) {

		dwRShift = dwRBits = 0;
		dwGShift = dwGBits = 0;
		dwBShift = dwBBits = 0;

//              pBB->GetRGBMasks(dwRMask, dwGMask, dwBMask);
		if (!(desc.ddpfPixelFormat.dwFlags & DDPF_RGB)) {
			r = g = b = 0;
		}
		else {
			dwRMask = m_SurfDesc.ddpfPixelFormat.dwRBitMask;
			dwGMask = m_SurfDesc.ddpfPixelFormat.dwGBitMask;
			dwBMask = m_SurfDesc.ddpfPixelFormat.dwBBitMask;
		}

		DWORD     d = dwRMask;

		while ((d & 0x1) == 0) {
			d = d >> 1;
			dwRShift++;
		}
		while (d & 0x01) {
			d = d >> 1;
			dwRBits++;
		}

		d = dwGMask;
		while ((d & 0x1) == 0) {
			d = d >> 1;
			dwGShift++;
		}
		while (d & 0x01) {
			d = d >> 1;
			dwGBits++;
		}

		d = dwBMask;
		while ((d & 0x1) == 0) {
			d = d >> 1;
			dwBShift++;
		}
		while (d & 0x01) {
			d = d >> 1;
			dwBBits++;
		}

		got_shifts = 1;
	}

	// WARNING: Don't try stepping until Unlock.
	//      BYTE* pBuf = (BYTE*) pBB->Lock();
	hr = IDirectDrawSurface_Lock(lpBack,
								 NULL,
								 &desc,
								 DDLOCK_SURFACEMEMORYPTR | DDLOCK_WAIT,
								 NULL);

	if (FAILED(hr)) {
		pBuf = NULL;
	}
	else {
		pBuf = m_SurfDesc.lpSurface;
	}

	if (pBuf) {

		// Compute where line segment starts
		int       n = length;
		DWORD     dwOffset = y * iPitch;	// In bytes

		int       ir, ig, ib;

		if (iDepth > 8) {
			ir = (pixel_index & 0xff);
			ig = ((pixel_index >> 8) & 0xff);
			ib = ((pixel_index >> 16) & 0xff);
		}

		// Draw pixels directly to buffer
		switch (iDepth) {
		case 8:
			{
				BYTE     *p = pBuf + dwOffset + x;

				while (n--)
					*p++ = (BYTE) pixel_index;
			}
			break;

		case 16:
			{
				DWORD     dw = (ir >> (8 - dwRBits)) << dwRShift
				| (ig >> (8 - dwGBits)) << dwGShift
				| (ib >> (8 - dwBBits)) << dwBShift;
				WORD      w = (WORD) dw;
				WORD     *p = (WORD *) (pBuf + dwOffset) + x;

				while (n--)
					*p++ = w;
			}
			break;

		case 24:
			{
				DWORD     dw = (ir >> (8 - dwRBits)) << dwRShift
				| (ig >> (8 - dwGBits)) << dwGShift
				| (ib >> (8 - dwBBits)) << dwBShift;
				DWORD    *p = (DWORD *) (pBuf + dwOffset) + x;

				while (n--)
					*p++ = dw;
			}
			break;

		case 32:
			{
				DWORD     dw = (ir >> (8 - dwRBits)) << dwRShift
				| (ig >> (8 - dwGBits)) << dwGShift
				| (ib >> (8 - dwBBits)) << dwBShift;
				DWORD    *p = (DWORD *) (pBuf + dwOffset) + x;

				while (n--)
					*p++ = dw;
			}
			break;

		default:
			break;
		}

//              pBB->Unlock();
		hr = IDirectDrawSurface_Unlock(lpBack, desc.lpSurface);
		ASSERT(SUCCEEDED(hr));
	}
}
#endif


XSetForeground(    Display* dpy,
    GC gc,
    unsigned long  foreground)
{
	return 0;
}


XFlush(dpy)
Display  *dpy;
{
	return 0;
}


XFillRectangle(   Display*            display,
    Drawable            d,
    GC                  gc,
    int                 x,
    int                 y,
    unsigned int        width,
    unsigned int        height)
{
	return 0;
}

int
XAllocColor(dpy, cmap, color)
Display  *dpy;
Colormap  cmap;
XColor   *color;
{
	short     i;
	long      old;

	color->rgb = RGB(
						(color->red >> 8),
						(color->green >> 8),
						(color->blue >> 8));

	for (i = 10; i < color_count; ++i) {
		if (color_map[i].flag == 0) {
			color_map[i].flag = 1;
			color_map[i].color = color->rgb;
			old = _remappalette(i, color->rgb);
			break;
		}
		else if (color_map[i].flag == 1 &&
				 color_map[i].color == color->rgb) {
			break;
		}
	}

	if (i == color_count) {
		return 0;
	}

	color->pixel = i;
/*      printf ("color %d = RGB %x\n", color->pixel, color->rgb); */

	return 1;
}

int
_getvideoconfig(struct _videoconfig *p)
{
	int       depth, width, height;

	(g_getVideoConfig) (0, &depth, &width, &height);
	p->numypixels = height;
	p->numxpixels = width;
	p->bitsperpixel = depth;
	p->numcolors = 1 << depth;
	color_count = p->numcolors;
	color_count = (color_count > 256) ? 256 : color_count;
	return 0;
}

int
_my_getvideoconfig(struct _videoconfig *p)
{
	int       depth, width, height;

	(g_getVideoConfig) (0, &depth, &width, &height);
	p->numypixels = height;
	p->numxpixels = width;
	p->bitsperpixel = depth;
	p->numcolors = 1 << depth;
	color_count = p->numcolors;
	color_count = (color_count > 256) ? 256 : color_count;
	return 0;
}

long
_remappalette(int color_index, long rgb_value)
{
	if (vc.bitsperpixel > 8) {
		return rgb_value;
	}
	return (*g_remap) (0, color_index, rgb_value);
}

Status
XAllocColorCells(    Display*           display,
    Colormap            colormap,
    Bool                contig,
    unsigned long*      plane_masks_return,
    unsigned int        nplanes,
    unsigned long*      pixels_return,
    unsigned int        npixels)
{
	return 0;
}

XSetPlaneMask(Display* display, GC gc, unsigned long plane_mask)
{
	return 0;
}


XStoreColors(Display* display, Colormap colormap, XColor* color, int ncolors)
{
	return 0;
}

Pixmap
XCreatePixmap(    Display*            display,
    Drawable            d,
    unsigned int        width,
    unsigned int        height,
    unsigned int        depth
	)
{
	return 0;
}


XFreePixmap(Display*  display,
    Pixmap pixmap )
{
	return 0;
}


XCopyArea(    Display*            display,
    Drawable            src,
    Drawable            dest,
    GC                  gc,
    int                 src_x,
    int                 src_y,
    unsigned int        width,
    unsigned int        height,
    int                 dest_x,
    int                 dest_y)
{
	return 0;
}

#endif							/* WIN32 */
