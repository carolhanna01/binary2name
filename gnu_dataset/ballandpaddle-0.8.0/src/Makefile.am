bin_PROGRAMS = ballandpaddle
ballandpaddle_SOURCES = main.cpp globals.h splash.cpp splash.h mainmenu.h mainmenu.cpp ball.h ball.cpp blocktype.h blocktype.cpp block.h block.cpp paddle.cpp paddle.h powerup.h powerup.cpp laser.h laser.cpp gamestatemanager.h gamestatemanager.cpp gamestate.h optionsmenu.h optionsmenu.cpp helpscreen.h helpscreen.cpp highscoremenu.h highscoremenu.cpp game.h game.cpp levelsetmenu.h levelsetmenu.cpp soundmanager.h soundmanager.cpp gameover.h gameover.cpp utilities.cpp utilities.h fontmanager.h fontmanager.cpp settingsmanager.h settingsmanager.cpp extension.h extension.cpp poweruptype.h poweruptype.cpp
CPPFLAGS=-DDATADIR='"$(datadir)"' -DPREFIX='"$(prefix)"' @CPPFLAGS@

localedir = $(datadir)/locale
AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"
LDADD = @LIBINTL@

