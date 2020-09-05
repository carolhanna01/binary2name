Summary: GNU ccAudio - a C++ class framework for processing audio files.
Name: ccaudio
Version: 1.2.0
Release: 1.ost1
Group: Development/Libraries
URL: http://www.gnu.org/software/ccaudio
Source: ftp://ftp.gnu.org/gnu/ccaudio/ccaudio-%{PACKAGE_VERSION}.tar.gz
Prefix: %{_prefix}
Copyright: GPL
BuildRoot: %{_tmppath}/ccaudio-root
Requires: commoncpp2 >= 1.0.0
Packager: David Sugar <dyfet@ostel.com>

%package devel
Group: Development/Libraries
Summary: header files and static link library for GNU ccAudio.
Requires: ccaudio, commoncpp2-devel

%description
The GNU ccAudio package offers a highly portable C++ class framework for
developing applications which manipulate audio streams and various
disk based audio file formats.  At the moment ccaudio is primarly a class
framework for handling .au, .wav (RIFF), and various .raw audio encoding
formats under Posix and win32 systems, though it may expand to become a
general purpose audio and soundcard support library.  Support for
controlling CD audio devices has recently been added as well as support
for codecs and other generic audio processing services.

%description devel
This package provides the header files and documentation for building
applications that use GNU ccAudio. 

%prep
rm -rf $RPM_BUILD_ROOT

%setup -n ccaudio-%{PACKAGE_VERSION}
CXXFLAGS="$RPM_OPT_FLAGS" ./configure --prefix=%{_prefix}

%build
uname -a|grep SMP && make -j 2 || make

%install
mkdir -p $RPM_BUILD_ROOT/%{_mandir}/man3

#make docs
make prefix=$RPM_BUILD_ROOT/%{_prefix} \
	mandir=$RPM_BUILD_ROOT/%{_mandir} install
make prefix=$RPM_BUILD_ROOT/%{_prefix} \
        mandir=$RPM_BUILD_ROOT/%{_mandir} man
strip $RPM_BUILD_ROOT/%{_prefix}/lib/libcc*.so.*

#mkdir -p $RPM_BUILD_ROOT/%{_mandir}/man3
#cp doc/man3/*.3cc $RPM_BUILD_ROOT/%{_mandir}/man3

%files
%defattr(-,root,root,0755)
%{_prefix}/lib/libcc*so*
%{_prefix}/lib/ccaudio1

%files devel
%defattr(-,root,root,0755)
%doc AUTHORS COPYING NEWS README TODO ChangeLog 
%{_prefix}/lib/libcc*a
%{_prefix}/include/cc++2/cc++/audio.h
%{_mandir}/man3/*.3cc*

%clean
rm -rf $RPM_BUILD_ROOT

%post
/sbin/ldconfig -n ${exec_prefix}/lib

%postun
/sbin/ldconfig -n ${exec_prefix}/lib

