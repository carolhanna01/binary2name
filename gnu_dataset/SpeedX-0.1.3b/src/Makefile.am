SUBDIRS = background cars num road scene

CFLAGS = -Wall -Wunused -O3 -I/usr/local/include/ -I/usr/X11R6/include/
LDFLAGS = -L/usr/local/lib/ -L/usr/X11R6/lib/
LDADD = -lz -lpng -lX11 -lXext -lm

bin_PROGRAMS = speedx
speedx_SOURCES = \
	course.c \
	effects.c \
	game.c \
	init.c \
	keyboard.c \
	loop.c \
	main.c \
	mem.c \
	menu.c \
	music.c \
	param.c \
	perf.c \
	perspective.c \
	physics.c \
	read_image.c \
	road.c \
	sprite.c \
	world.c \
	Xh.h \
	car.h \
	color.h \
	const.h \
	course.h \
	effects.h \
	game.h \
	init.h \
	keyboard.h \
	loop.h \
	main.h \
	mem.h \
	menu.h \
	music.h \
	param.h \
	perf.h \
	perspective.h \
	physics.h \
	read_image.h \
	road.h \
	sprite.h \
	std.h \
	world.h
