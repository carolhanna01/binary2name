%define ver      	0.7.2
%define RELEASE 	1
%define rel     	%{?CUSTOM_RELEASE} %{!?CUSTOM_RELEASE:%RELEASE}
%define prefix		/usr

Summary: GCompris / I Have Inderstood.
Name: 		gcompris
Version: 	%ver
Release: 	%rel
Copyright: 	GPL
Group: 		Amusements/Games
Source:		ftp://gcompris.sourceforge.net/gcompris-%{PACKAGE_VERSION}.tgz
BuildRoot: 	/var/tmp/gcompris-%{PACKAGE_VERSION}-root
URL: 		http://savannah.gnu.org
Docdir: 	%{prefix}/share/doc/gcompris-%{PACKAGE_VERSION}
Requires:	gnome-libs libxml gdk-pixbuf-gnomecanvas gdk-pixbuf
Packager:       Bruno Coudoin <bcoudoin.coudoin@free.fr>

%description
GCompris / I Have Understood is an educationnal game for children starting at 3.
Today several Boards are implemented:
* Click on the animals => learn the mouse/click usage
* Type the falling letters => learn the keyboard usage
* Falling Dices
* Falling words
* Basic algebra
* Time learning with an analog clock
* Puzzle game with famous paintings
* Drive Plane to catch clouds in increasing number
* Balance the scales

The Game is included in the Gnome Desktop under the Game menu.

You should install it only if you have children using this computer.
You will also need to install the gnome-libs package,  gdk-pixbuf-gnomecanvas,
gdk-pixbuf and the libxml.

%description -l fr
GCompris / J'ai Compris est un logiciel �ducatif pour les enfants 
� partir de 3 ans.

Aujourd'hui, plusieurs tableau sont impl�ment�s:
* Cliquer sur les animaux => apprentissage du clique et de la souris
* Entrer les lettre qui tombent => Apprentissage du clavier
* Les d�s qui tombent
* Les mots qui tombent
* Alg�bre simple
* Apprentissage de la lecture de l'heure sur une horloge analogique
* Puzzle avec des tableaux c�l�bres
* Pilote un avion pour attraper les nuages dans l'ordre
* Equilibre la balance

Le jeux est inclus dans le bureau Gnome sous le menu Jeux.

A installer si vous avez des enfants utilisant cet ordinateur.
Le packetage gnome-libs, libxml, gdk-pixbuf-gnomecanvas,
gdk-pixbuf doit etre install�.

%changelog
* Fri May 25 2001  Bruno Coudoin  <bruno.coudoin@free.fr>
- GCompris: Release 0.6.1
- WARNING: THE TRANSLATION IS BROKEN.
- WARNING: THE HIGHLIGHTING IS BROKEN
- ALL: Replace the imlib for images by the gdk-pixbuf library
- gcompris.spec.in: Added back the gcompris icon pixmap
- clickgame.c: Limit the number of fish to 5
- clickgame.c: Simplified the code by removing one hashtable.
- shapegame.c: Changed the board order to have the most difficult
               at the higher level (as suggested by Yann Dirson)
	       Added the method shuffle_shape_list()
- gcompris.c (end_game): reset the timer in the status bar
- Makefile.am: Added DESTDIR prefix in the install hooks
- ALL: Boards are now separated from gcompris in src/
- boards/*: All boards are defined as independant plugins
- gcompris/*: Added the necessary glue to load the plugins
- configure.in: Removed PACKAGE_PIXMAPS_DIR
- data/*: The data directory has been renamed in boards
- pixmaps/*: The pixmaps directory has been moved to board/gcompris
             This will be the default gcompris icons. Other icons
             have been moved in a dir specific to their boards.
- boards/planegame.c: Set a max speed for the plane because children
		      have difficulties to manipulate it.
- docs/C/gcompris.texi: Update of the doc to explain the new plugin
                        mecanism

* Wed Apr 11 2001  Bruno Coudoin  <bruno.coudoin@free.fr>
- GCompris: Release 0.4.2
- gcompris.c: adjusted main window size calculation.
              it fixes a bug was making gcompris hardly
	      unusable under 640x400
	      Changed to not use the Gnome standard menu for loading
	      help. Implemented my own to be able to be relocatable.
	      This hack does not implement I18N for the help file.
- fi.po: Created by Jyrki Kuoppala <jkp@kaapeli.fi>
- data/wordsgame/*.fi:Created by Jyrki Kuoppala <jkp@kaapeli.fi>
- gcompris.soundlist: deleted. I now use direct sound call instead
	                      of gnome .soundlist API
- sounds/*: now sounds are local to gcompris.
- configure.in: Changed to use local relative pixmaps and sounds
	                instead of gnome api
- src/*.c: Changed to use relative pixmaps and sounds
	
%prep
%setup -q

%build
%ifarch alpha
  MYARCH_FLAGS="--host=alpha-redhat-linux"
%endif

CFLAGS="$RPM_OPT_FLAGS" ./configure --quiet $MYARCH_FLAGS --prefix=%{prefix} \
	 --localstatedir=/var/lib

if [ "$SMP" != "" ]; then
  make -j$SMP MAKE="make -j$SMP"
else
  make
fi

%install
rm -rf $RPM_BUILD_ROOT

make prefix=$RPM_BUILD_ROOT%{prefix} install >install.log 2>&1

%clean
rm -rf $RPM_BUILD_ROOT

%post 

%postun 

%files
%defattr(-, root, root)

%doc ABOUT-NLS AUTHORS COPYING ChangeLog INSTALL NEWS README TODO THANKS
%attr(-, root, games) %{prefix}/bin/*
%{prefix}/share/gnome/apps/Games/*
%{prefix}/share/pixmaps/*
%{prefix}/share/gcompris/sounds/*
%{prefix}/share/gcompris/boards/*
%{prefix}/lib/gcompris/*
%{prefix}/share/locale/*/*/*
%{prefix}/share/gnome/help/gcompris/*/*
%{prefix}/lib/menu/*


