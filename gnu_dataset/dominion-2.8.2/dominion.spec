Summary: Dominion -- an empire-style multiplayer role playing game
Name: dominion
Packager: mark@galassi.org
%define version 2.8.2
%define release 0
Version: %{version}
Release: %{release}
Vendor: The Dominion Team
Distribution: games
Copyright: Copyright (C) 1988-2003 The Dominion Team, GPL
Source: %{name}-%{version}.tar.gz
Group: Games
%define mybuildroot /var/tmp/%{name}-build
%define installroot /install-tmp
BuildRoot: %{mybuildroot}

%description
  Dominion -- empire-style multiplayer role playing game.  Players
  run a nation with its economy and military.


%prep
%setup -c
echo "dude, mybuildroot is " %{mybuildroot}
echo "dude, installroot is " %{installroot}
echo "dude, RPM_BUILD_ROOT is " $RPM_BUILD_ROOT

%build
cd %{name}-%{version}; ./configure --prefix=/usr; make

%install
cd %{name}-%{version}; make install prefix=%{mybuildroot}/usr

%post

%postun

%files
%defattr(-, root, root)
%doc %{name}-%{version}/{README,NEWS,ChangeLog,AUTHORS}
/usr/*
