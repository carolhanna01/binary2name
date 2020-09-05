/*
 * // SpeedX //
 *
 *  medernac@isty-info.uvsq.fr
 *
 *  Copyright (C) 2000 
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */

/*
   Version : 0.1.2c

   License : GPL  

   X11, window creation 
 */

//#include <math.h>

#include "main.h"

#include "music.h"

#include "world.h"

/* MIT-SHM disponible */

int XShmGetEventBase(Display * dpy);

int mit_shm;

int factor;

int bpl;

int bitmap_pad;

int bitmap_bit_order;

char *screen_buffer, *screen_buffer2;

char *sky;

void init_display(void)
{

	int i;

	printf(". Drawing Initialised ...");

	fflush(stdout);

	if ((display = XOpenDisplay(DisplayName)) == NULL) {

		printf("ERREUR! (Display invalide : %s)\n", DisplayName);

		exit(1);

	}
	screen = DefaultScreen(display);

	depth = DefaultDepth(display, screen);

	/* That seems to be more efficient than DefaultDepth */

	/* Need to verify on other machines. */

	/* We look at the number of bits per pixel in the ZPixmap */

	/* for a taken "depth" . */

	bpp = 0;

	for (i = 0; i < ((_XPrivDisplay) display)->nformats; i++) {

		ScreenFormat sf =
		((_XPrivDisplay) display)->pixmap_format[i];

		if (sf.depth == (int) depth) {

			bpp = sf.bits_per_pixel;

		}
	}

	bitmap_bit_order = BitmapBitOrder(display);

	switch (bpp) {

	case 8:

		factor = 1;

		bitmap_pad = 8;

		break;

	case 15:

	case 16:

		factor = 2;

		bitmap_pad = 16;

		break;

	case 24:

		factor = 3;

		bitmap_pad = 32;

		break;

	case 32:

		factor = 4;

		bitmap_pad = 32;

		break;

	default:

		factor = 0;

		printf("Error : (This graphics mode is not managed ..)\n");

		exit(1);

		break;

	}

	visual = DefaultVisual(display, screen);

	root_width = DisplayWidth(display, screen);

	root_height = DisplayHeight(display, screen);

	root = DefaultRootWindow(display);

	printf("OK\n");

}

/*
   Window and image area creation
 */

void init_win()
{

	unsigned long win_mask;

	XSetWindowAttributes win_attr;

	XSizeHints win_hint;

	printf(". Window Initialised...");

	fflush(stdout);

/* MIT SHM ? */

	mit_shm = 0;

#if MITSHM

	{

		int major, minor;

		Bool pix;

		mit_shm = XShmQueryVersion(display, &major, &minor, &pix);

/*

   if (mit_shm) {

   fprintf(stderr, "Extension MIT-SHM version %d.%d : OK\n", major, minor);

   }

   else {

   fprintf(stderr, "Pas d'extension MIT-SHM\n");

   }

 */

		CompletionType = XShmGetEventBase(display) + ShmCompletion;

#ifdef NOSHM

		mit_shm = 0;

		//fprintf(stderr, "__ NOSHM __\n");

#endif

		/* 
		   Need to be fixed :
		   No MIT-SHM at 2 player mode
		   We need to pass each screen 
		   to each functions
		 */

		if (multi) {

			mit_shm = 0;

		}
/*

   if (autorun) {

   mit_shm = 0;

   }

 */

	}

#endif

/* No edge on the Window */

	win_mask = CWBackPixel | CWBorderPixel;

	win_attr.border_pixel = BlackPixel(display, screen);

	win_attr.background_pixel = BlackPixel(display, screen);

	if (dble == 0)
	  if (multi == 0) {
		win =
		    XCreateWindow(display, root, root_width >> 1,
				  root_height >> 2, WIDTH, HEIGHT, BORDER,
				  depth, InputOutput, CopyFromParent,
				  win_mask, &win_attr);
	  } else {
	  		win =
		    XCreateWindow(display, root, root_width >> 2,
				  root_height >> 1, (WIDTH ),
				(HEIGHT ), BORDER, depth, InputOutput,
				  CopyFromParent, win_mask, &win_attr);
		printf("MULTI 1\n");
	  }
	else
	  if (multi == 0) {
		win =
		    XCreateWindow(display, root, root_width >> 2,
				  root_height >> 2, (WIDTH * 2),
				(HEIGHT * 2), BORDER, depth, InputOutput,
				  CopyFromParent, win_mask, &win_attr);
		printf("MULTI 0\n");
	  } else {
	  		win =
		    XCreateWindow(display, root, root_width >> 2,
				  root_height >> 2, (WIDTH ),
				(HEIGHT ), BORDER, depth, InputOutput,
				  CopyFromParent, win_mask, &win_attr);
		printf("MULTI 1\n");
	  }
	multi = 0 ;
	/*
	if (multi)
		win2 =
		    XCreateWindow(display, root, root_width >> 2,
				  root_height >> 2, WIDTH, HEIGHT, BORDER,
				  depth, InputOutput, CopyFromParent,
				  win_mask, &win_attr);
	*/
	win_hint.flags = PPosition | PMinSize | PMaxSize;

	win_hint.x = 0;

	win_hint.y = 0;

	if (dble == 0) {

		win_hint.max_width = win_hint.min_width = WIDTH;

		win_hint.max_height = win_hint.min_height = HEIGHT;

	} else {

		win_hint.max_width = win_hint.min_width = WIDTH * 2;

		win_hint.max_height = win_hint.min_height = HEIGHT * 2;

	}

	XSetWMNormalHints(display, win, &win_hint);

	if (multi)
		XSetWMNormalHints(display, win2, &win_hint);

	if (depth == 8) {

		cmap = XCreateColormap(display, win, visual, AllocAll);

		XSetWindowColormap(display, win, cmap);

		if (multi)
			XSetWindowColormap(display, win2, cmap);

/*

   if (DEBUG)

   perror("cmap OK");

 */

	}
	XMapWindow(display, win);

	if (multi)
		XMapWindow(display, win2);

	XFlush(display);

	XSelectInput(display, win,
		     KeyPressMask | KeyReleaseMask | FocusChangeMask);

	if (multi)
		XSelectInput(display, win2,
			     KeyPressMask | KeyReleaseMask |
			     FocusChangeMask);

	gcVal.foreground = 0;

	gcVal.background = WhitePixel(display, screen);

	gcMask = GCForeground | GCBackground;

	gc = XCreateGC(display, win, gcMask, &gcVal);

/* Mouse arrow */

	{

		Cursor c;

		c = XCreateFontCursor(display, XC_circle);

		XDefineCursor(display, win, c);

	}

/* Autorepeat On for the keyboard */

	XAutoRepeatOn(display);

	init_table_perspective();

	if (multi) {

		XStoreName(display, win, "SpeedX : 1");

		XStoreName(display, win2, "SpeedX : 2");

	} else
		XStoreName(display, win, "SpeedX");

	if (depth == 8) {

		XColor xc;

		unsigned int r, g, b;

/*

   if (DEBUG)

   perror("creation xim");

 */

		/*

		   Colormap allocation 

		 */

		for (b = 0; b < 8; b++) {

			for (g = 0; g < 8; g++) {

				for (r = 0; r < 4; r++) {

					/* Each RGB is on 16 bits */

					xc.pixel = b | g << 3 | r << 6;

					xc.red = r << 14;

					xc.green = g << 13;

					xc.blue = b << 13;

					xc.flags =
					    DoRed | DoGreen | DoBlue;

					XStoreColor(display, cmap, &xc);

				}

			}

		}

	}
	printf("OK\n");

}

void init_buffers(void)
{

	int taille_img;

	printf(". Buffers Initialisation ...");

	fflush(stdout);

/* XImage Buffer */

	bpl = factor * WIDTH;

	if (dble != 0)
		bpl = 2 * bpl;

	screen_buffer =
	    (char *) calloc(factor * (WIDTH) * (HEIGHT), sizeof(char));

	if (dble != 0)
		screen_buffer2 =
		    (char *) calloc(4 * factor * (WIDTH) * (HEIGHT),
				    sizeof(char));

	sky = (char *) calloc(factor * (WIDTH) * (HEIGHT), sizeof(char));

	bool = 1;

	if (dble == 0) {

#if MITSHM

		if (mit_shm) {

			xim =
			    XShmCreateImage(display, CopyFromParent,
					    (unsigned int) depth, ZPixmap,
					    0, shminfo, WIDTH, HEIGHT);

			//xim = XShmCreateImage(display, CopyFromParent, (unsigned int) depth, ZPixmap, 0, shminfo + 1, WIDTH, HEIGHT);

		} else
#endif

		{

			xim =
			    XCreateImage(display, CopyFromParent,
					 (unsigned int) depth, ZPixmap, 0,
					 screen_buffer, WIDTH, HEIGHT,
					 bitmap_pad, bpl);

		}

	} else {

		//printf("Double\n");

#if MITSHM

		if (mit_shm) {

			xim =
			    XShmCreateImage(display, CopyFromParent,
					    (unsigned int) depth, ZPixmap,
					    0, shminfo, WIDTH * 2,
					    HEIGHT * 2);

			//xim = XShmCreateImage(display, CopyFromParent, (unsigned int) depth, ZPixmap, 0, shminfo + 1, WIDTH * 2, HEIGHT * 2);

		} else
#endif

		{

			xim =
			    XCreateImage(display, CopyFromParent,
					 (unsigned int) depth, ZPixmap, 0,
					 screen_buffer2, WIDTH * 2,
					 HEIGHT * 2, bitmap_pad, bpl);

		}

	}

	/* sky */

	switch (rand() % 2) {

	case 0:

		taille_img = init(&sky, 0, depth);

		break;

	case 1:

		taille_img = init(&sky, 1, depth);

		break;

	}

/* MIT-SHM */

#if MITSHM

	if (mit_shm) {

		key_t clef;

		int id;

		int size = -1;

		size = factor * WIDTH * HEIGHT;

		if (dble)
			size *= 4;

		/*clef = ('S' << 24) || ('p' << 16) || ('d' << 8) || ('X'); */

		{
			int key_id = 1;		/* Must begin at 1, 0 is reserved for the world */
			do {
				clef = ftok(".", key_id);
				key_id++;
			}
			while ((id = shmget(clef, size, IPC_CREAT | 0777)) == -1);
		}

		shminfo[0].shmid = id;

		if (dble == 0) {

			if (screen_buffer != NULL)
				free(screen_buffer);

			xim->data = screen_buffer = shminfo[0].shmaddr =
			    (void *) shmat(id, 0, 0);

			/*

			   if (screen_buffer == NULL)

			   perror("shmat");

			 */

		} else {

			if (screen_buffer2 != NULL)
				free(screen_buffer2);

			xim->data = screen_buffer2 = shminfo[0].shmaddr =
			    (void *) shmat(id, 0, 0);

			/*

			   if (screen_buffer2 == NULL)

			   perror("shmat");

			 */

		}

		shminfo[0].readOnly = False;

		if (!XShmAttach(display, shminfo)) {

			//perror("XShmAttach");

		}
		XSync(display, 0);

		shmctl(shminfo[0].shmid, IPC_RMID, 0);

		XShmPutImage(display, win, gc, xim, 0, 0, 0, 0, WIDTH,
			     HEIGHT, False);	//True

		//fprintf(stderr, "XShmPutImage : premier passage\n");

	}
#endif

	printf("OK\n");

}

int main(int argc, char *argv[])
{

	OptionParsing(argc, argv);

	srand((unsigned int) time(NULL));

	init_display();

	init_keyboard(display);

	init_win();

	init_buffers();

	init_world();
	switch (player) {
	case -1:
		printf("Sorry ! Every cars have a pilot.\n");
		printf("Come back later !.\n");
		exit(0);
		break;
	case 0:
		printf("You are the server.\n");
		break;
	default:
		printf("You are a client [IPC].\n");
		break;
	}

#if LIBMIKMOD
	init_music();
#endif
	printf("   * depth = %d\n", depth);

	printf("   * bpp   = %d\n", bpp);

	if (autorun != 1) {

		loop(display);

	} else
		auto_run();

/* FREE */

	{

#if MITSHM

		if (mit_shm) {

			XShmDetach(display, shminfo);

			shmctl(shminfo[0].shmid, IPC_RMID, 0);

			shmdt(shminfo[0].shmaddr);

		}
#endif

		if (!mit_shm)
			free(screen_buffer);

		XDestroyWindow(display, win);

		XCloseDisplay(display);

		free(sky);

		Free_sprite();

		liberation();

	}

	return 0;

}
