# solfege.redhat9.spec.  Generated from solfege.redhat9.spec.in by configure.
Summary: Eartraining program for GNOME
Summary(de): Geh�rbildungssoftware f�r GNOME
Name: solfege
Version: 3.8.2
Release: 1.redhat9
Vendor: Tom Cato Amundsen (tca@gnu.org)
Source: http://download.sourceforge.net/solfege/%{name}-%{version}.tar.gz
URL: http://www.solfege.org/
Copyright: GPL, see file COPYING for details
Group: Applications/Multimedia
Requires: gnome-python2 >= 1.99.11, pygtk2 >= 1.99.11, gnome-python2-gtkhtml2 >= 1.99.11
BuildRoot: %{_tmppath}/%name-%{version}

%description
Solfege is an ear training program for X written in python, using the
GTK+ and GNOME libraries. This is a development release, things might
be broken. See INSTALL file if you have problems running or installing
Solfege. Report your problems to solfege-devel@lists.sourceforge.net

Eartraining is a big subject with many connections to music theory and
performance of music, so I won't even try to make "a complete
computerbased ear training course". But I hope someone find this
software useful.

%description -l de
Solfege ist eine Geh�rbildungssoftware geschrieben in Python f�r X, die
GTK+ und GNOME Bibliotheken verwendet. Es handelt sich hierbei um eine
Entwicklerversion, manches mag noch Fehler haben. Dem INSTALL Text k�nnen
Informationen entnommen werden, falls Probleme bei der Installation oder
der Ausf�hrung auftreten sollten. Probleme k�nnen solfege-devel@lists.sourceforge.net
mitgeteilt werden.

Geh�rbildung stellt eine gro�e Anforderung an viele Bereiche der
Musiktheorie dar, weshalb ich nicht versucht habe, einen vollst�ndigen
Computerkurs Geh�rbildung zu erstellen. Ich hoffe aber, dass so
mancher diese Software hilfreich findet.

%prep
%setup

%build
./configure --with-gtkhtml\
  --prefix=%{_prefix} \
  --mandir=%{_mandir} \
  --sysconfdir=%{_sysconfdir}
make

%install
rm -rf %{buildroot}
make nopycompile=YES install \
  prefix=%{buildroot}%{_prefix} \
  mandir=%{buildroot}%{_mandir} \
  sysconfdir=%{buildroot}%{_sysconfdir}
install -D solfege.desktop \
  %{buildroot}%{_datadir}/gnome/apps/Applications/solfege.desktop
install -D graphics/solfege.png \
  %{buildroot}%{_datadir}/pixmaps/solfege.png

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root)
%doc AUTHORS INSTALL FAQ README TODO COPYING changelog
%config %{_sysconfdir}/solfege
%{_mandir}/man1/%{name}.1*
%{_bindir}/%{name}*
%{_libdir}/%{name}
%{_datadir}/%{name}
%{_datadir}/locale
%{_datadir}/pixmaps
%{_datadir}/gnome
