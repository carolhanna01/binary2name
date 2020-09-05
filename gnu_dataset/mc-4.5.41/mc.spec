# Note that this is NOT a relocatable package
%define ver     4.5.41
%define  RELEASE 1
%define  rel     %{?CUSTOM_RELEASE} %{!?CUSTOM_RELEASE:%RELEASE}
%define prefix  /usr

Summary:   Midnight Commander visual shell
Name:      mc
Version:   %ver
Release:   %rel
Copyright: GPL
Group:     System Environment/Shells
Source0:   ftp://ftp.nuclecu.unam.mx/linux/local/devel/mc-%{PACKAGE_VERSION}.tar.gz
URL:       http://www.gnome.org/mc/
BuildRoot: /var/tmp/mc-%{PACKAGE_VERSION}-root
Requires:  pam >= 0.59
Prereq:    /sbin/chkconfig

%description
Midnight Commander is a visual shell much like a file manager, only with way
more features.  It is text mode, but also includes mouse support if you are
running GPM.  Its coolest feature is the ability to ftp, view tar, zip
files, and poke into RPMs for specific files.  :-)

%package -n gmc
Summary:  Midnight Commander visual shell (GNOME version)
Requires: mc >= %{PACKAGE_VERSION}
BuildPrereq: stylesheets
Group:    Interface/Desktops

%description -n gmc
Midnight Commander is a visual shell much like a file manager, only with
way more features.  This is the GNOME version. It's coolest feature is the
ability to ftp, view tar, zip files and poke into RPMs for specific files.
The GNOME version of Midnight Commander is not yet finished though. :-(
 
%package -n mcserv
Summary:  Midnight Commander file server
Group:    System Environment/Daemons
Requires: portmap
%description -n mcserv
mcserv is the server program for the Midnight Commander networking file
system. It provides access to the host file system to clients running the
Midnight file system (currently, only the Midnight Commander file manager).

%prep
%setup -q

%build
CFLAGS="$RPM_OPT_FLAGS" LDFLAGS="-s" ./configure \
	--prefix=%{prefix} --with-included-slang \
	--with-gnome \
	--without-debug \
        --sysconfdir=/etc

if [ "$SMP" != "" ]; then
  (make "MAKE=make -k -j $SMP"; exit 0)
  make
else
  make
fi

%install
echo $RPM_BUILD_ROOT
rm -rf $RPM_BUILD_ROOT
install -d $RPM_BUILD_ROOT/etc/{rc.d/init.d,pam.d,profile.d,X11/wmconfig}


make prefix=$RPM_BUILD_ROOT%{prefix} sysconfdir=$RPM_BUILD_ROOT/etc install
(cd icons; make prefix=$RPM_BUILD_ROOT%{prefix} install_icons)
install lib/mcserv.init $RPM_BUILD_ROOT/etc/rc.d/init.d/mcserv

install lib/mcserv.pamd $RPM_BUILD_ROOT/etc/pam.d/mcserv
install lib/{mc.sh,mc.csh} $RPM_BUILD_ROOT/etc/profile.d
install lib/mc.global $RPM_BUILD_ROOT/etc


# clean up this setuid problem for now
chmod 755 $RPM_BUILD_ROOT%{prefix}/lib/mc/bin/cons.saver

%clean
rm -rf $RPM_BUILD_ROOT

%post   -n mcserv
/sbin/chkconfig --add mcserv

%postun -n mcserv
/sbin/chkconfig --del mcserv

%files
%defattr(-, root, root)

%doc FAQ COPYING NEWS README
%{prefix}/bin/mc
%{prefix}/bin/mcedit
%{prefix}/bin/mcmfmt
%{prefix}/lib/mc/mc.ext
%{prefix}/lib/mc/mc.hint
%{prefix}/lib/mc/mc.hlp
%{prefix}/lib/mc/mc.lib
%{prefix}/lib/mc/mc.menu
%{prefix}/lib/mc/bin/cons.saver
%{prefix}/lib/mc/extfs/*
%{prefix}/lib/mc/syntax/*
%{prefix}/lib/mc/term/*
%{prefix}/man/man1/*
%config /etc/profile.d/*
%config /etc/mc.global
%dir %{prefix}/lib/mc
%dir %{prefix}/lib/mc/bin

%files -n mcserv
%defattr(-, root, root)

%config /etc/pam.d/mcserv
%config /etc/rc.d/init.d/mcserv
%attr(-, root, man)  %{prefix}/man/man8/mcserv.8
%{prefix}/bin/mcserv

%files -n gmc
%defattr(-, root, root)
%{prefix}/bin/gmc
%{prefix}/bin/plain-gmc
%{prefix}/share/idl/*
%{prefix}/lib/mc/layout
%{prefix}/lib/mc/mc-gnome.ext
%{prefix}/share/pixmaps/mc/*
%{prefix}/share/mime-info/mc.keys
%{prefix}/share/gnome/help/gmc/*
%{prefix}/share/locale/*
%config /etc/CORBA/servers/*
%dir %{prefix}/lib/mc/desktop-scripts

%changelog
* Thu Sep 09 1999 Elliot Lee <sopwith@redhat.com>
- Include .idl files in the package.

* Sat Sep 04 1999 Gregory McLean <gregm@comstar.net>
- Added a build prereq so that rpms get built with documentation ;)

* Mon Jul 12 1999 Kjartan Maraas  <kmaraas@online.no>
- added help and locale files to %files

* Tue Jun 22 1999 Vladimir Kondratiev <vkondra@iil.intel.com>
- added syntax files to %files

* Wed May 26 1999 Cody Russell <bratsche@dfw.net>
- chmod cons.saver at $RPM_BUILD_ROOT%{prefix}/lib rather than at
  $RPM_BUILD_ROOT/usr/lib. We can now install to somewhere other than /usr.

* Sun Apr 18 1999 Gregory McLean <gregm@comstar.net>
- Updated the specfile, removed some kludges.

* Thu Aug 20 1998 Michael Fulbright <msf@redhat.com>
- rebuilt against gnome-libs 0.27 and gtk+-1.1

* Thu Jul 09 1998 Michael Fulbright <msf@redhat.com>
- made cons.saver not setuid

* Sun Apr 19 1998 Marc Ewing <marc@redhat.com>
- removed tkmc

* Wed Apr 8 1998 Marc Ewing <marc@redhat.com>
- add /usr/lib/mc/layout to gmc

* Tue Dec 23 1997 Tomasz K�oczko <kloczek@rudy.mif.pg.gda.pl>
- added --without-debug to configure,
- modification in %build and %install and cosmetic modification in packages
  headers,
- added %%{PACKAGE_VERSION} macro to Buildroot,
- removed "rm -rf $RPM_BUILD_ROOT" from %prep.
- removed Packager field.

* Thu Dec 18 1997 Michele Marziani <marziani@fe.infn.it>
- Merged spec file with that from RedHat-5.0 distribution
  (now a Hurricane-based distribution is needed)
- Added patch for RPM script (didn't always work with rpm-2.4.10)
- Corrected patch for mcserv init file (chkconfig init levels)
- Added more documentation files on termcap, terminfo, xterm

* Thu Oct 30 1997 Michael K. Johnson <johnsonm@redhat.com>

- Added dependency on portmap

* Wed Oct 29 1997 Michael K. Johnson <johnsonm@redhat.com>

- fixed spec file.
- Updated to 4.1.8

* Sun Oct 26 1997 Tomasz K�oczko <kloczek@rudy.mif.pg.gda.pl>

- updated to 4.1.6
- added %attr macros in %files,
- a few simplification in %install,
- removed glibc patch,
- fixed installing /etc/X11/wmconfig/tkmc.

* Thu Oct 23 1997 Michael K. Johnson <johnsonm@redhat.com>

- updated to 4.1.5
- added wmconfig

* Wed Oct 15 1997 Erik Troan <ewt@redhat.com>

- chkconfig is for mcserv package, not mc one

* Tue Oct 14 1997 Erik Troan <ewt@redhat.com>

- patched init script for chkconfig
- don't turn on the service by default

* Fri Oct 10 1997 Michael K. Johnson <johnsonm@redhat.com>

- Converted to new PAM conventions.
- Updated to 4.1.3
- No longer needs glibc patch.

* Thu May 22 1997 Michele Marziani <marziani@fe.infn.it>

- added support for mc alias in /etc/profile.d/mc.csh (for csh and tcsh)
- lowered number of SysV init scripts in /etc/rc.d/rc[0,1,6].d
  (mcserv needs to be killed before inet)
- removed all references to $RPM_SOURCE_DIR
- restored $RPM_OPT_FLAGS when compiling
- minor cleanup of spec file: redundant directives and comments removed

* Sun May 18 1997 Michele Marziani <marziani@fe.infn.it>

- removed all references to non-existent mc.rpmfs
- added mcedit.1 to the %files section
- reverted to un-gzipped man pages (RedHat style)
- removed double install line for mcserv.pamd

* Tue May 13 1997 Tomasz K�oczko <kloczek@rudy.mif.pg.gda.pl>

- added new rpmfs script,
- removed mcfn_install from mc (adding mc() to bash enviroment is in
  /etc/profile.d/mc.sh),
- /etc/profile.d/mc.sh changed to %config,
- removed /usr/lib/mc/bin/create_vcs,
- removed /usr/lib/mc/term.

* Wed May 9 1997 Tomasz K�oczko <kloczek@rudy.mif.pg.gda.pl>

- changed source url,
- fixed link mcedit to mc,

* Tue May 7 1997 Tomasz K�oczko <kloczek@rudy.mif.pg.gda.pl>

- new version 3.5.27,
- %dir /usr/lib/mc/icons and icons removed from tkmc,
- added commented xmc part.

* Tue Apr 22 1997 Tomasz K�oczko <kloczek@rudy.mif.pg.gda.pl>

- FIX spec:
   - added URL field,
   - in mc added missing /usr/lib/mc/mc.ext, /usr/lib/mc/mc.hint,
     /usr/lib/mc/mc.hlp, /usr/lib/mc/mc.lib, /usr/lib/mc/mc.menu.

* Fri Apr 18 1997 Tomasz K�oczko <kloczek@rudy.mif.pg.gda.pl>

- added making packages: tkmc, mcserv (xmc not work yet),
- gziped man pages,
- added /etc/pamd.d/mcserv PAM config file.
- added instaling icons,
- added /etc/profile.d/mc.sh,
- in %doc added NEWS README,
- removed /usr/lib/mc/FAQ,
- added mcserv.init script for mcserv (start/stop on level 86).
