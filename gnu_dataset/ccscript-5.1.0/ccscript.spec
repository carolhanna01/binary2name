#
# spec file for package ccscript
#
# Copyright (C) 2014 David Sugar, Tycho Softworks.
# Copyright (C) 2015 Cherokees of Idaho.
#
# All modifications and additions to the file contributed by third parties
# remain the property of their copyright owners, unless otherwise agreed
# upon. The license for this file, and modifications and additions to the
# file, is the same license as for the pristine package itself (unless the
# license for the pristine package is not an Open Source License, in which
# case the license is the MIT License). An "Open Source License" is a
# license that conforms to the Open Source Definition (Version 1.9)
# published by the Open Source Initiative.

# Please submit bugfixes or comments via http://bugs.opensuse.org/
#


%define libname libccscript5
Name:           ccscript
Version:        5.1.0
Release:        1
Summary:        C++ framework for threaded scripting engine
License:        LGPL-3.0+
Group:          Development/Libraries/C and C++
Url:            http://www.gnu.org/software/commoncpp/commoncpp.html
Source:         %{name}-%{version}.tar.gz
BuildRequires:  cmake
BuildRequires:  pkgconfig
BuildRequires:  ucommon-devel >= 7.0.0
BuildRoot:      %{_tmppath}/%{name}-%{version}-build

%description
GNU ccScript is a C++ class framework for creating a virtual machine
execution system for use with and as a scripting/assembler language for
state-transition driven realtime systems.

%package -n %{libname}
Summary:        C++ framework for threaded scripting engine
Group:          Development/Libraries/C and C++

%description -n %{libname}
GNU ccScript is a C++ class framework for creating a virtual machine
execution system for use with and as a scripting/assembler language for
state-transition driven realtime systems.

%package devel
Summary:        Headers and static link library for GNU ccScript
Group:          Development/Libraries/C and C++
Requires:       %{libname}%{?_isa} = %{version}-%{release}
Requires:       ucommon-devel%{?_isa} >= 6.0.0

%description devel
This package provides the header files, link libraries, and
documentation for building applications that use GNU ccScript.

%prep
%setup -q

%build
%cmake \
    -DCMAKE_INSTALL_SYSCONFDIR:PATH=%{_sysconfdir} \
    -DCMAKE_INSTALL_LOCALSTATEDIR:PATH=%{_localstatedir}

make %{?_smp_mflags}

%install
%cmake_install

%files -n %{libname}
%defattr(-,root,root,-)
%doc AUTHORS COPYING ChangeLog NEWS README
%{_libdir}/*.so.*

%files devel
%defattr(-,root,root,-)
%{_libdir}/*.so
%{_libdir}/pkgconfig/*.pc
%{_includedir}/ccscript.h

%post -n %{libname} -p /sbin/ldconfig

%postun -n %{libname} -p /sbin/ldconfig

%changelog

