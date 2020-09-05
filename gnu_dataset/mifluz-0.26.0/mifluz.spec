Name: mifluz 
Summary: Full text indexing
Version: 0.26.0
Release: 1
Source: http://www.senga.org/mifluz/current/%{name}-%{version}.tar.gz
Group: Development/Libraries
URL: http://www.senga.org/mifluz/html/download.html
BuildRoot: /var/tmp/%{name}-buildroot
Copyright: GPL
Prefix: /usr

%description
The purpose of mifluz is to provide a C++ library to build and query a
full text inverted index. It is dynamically updatable, scalable (up to
1Tb indexes), uses a controlled amount of memory, shares index files
and memory cache among processes or threads and compresses index files
to 50% of the raw data. The structure of the index is configurable at
runtime and allows inclusion of relevance ranking information. The
query functions do not require to load all the occurences of a
searched term.  They consume very few resources and many searches can
be run in parallel.

%prep
%setup -q

%build

%configure
make

%install
make DESTDIR=$RPM_BUILD_ROOT install

%clean
rm -rf ${RPM_BUILD_ROOT}

%post -p /sbin/ldconfig

%postun -p /sbin/ldconfig

%files
%defattr(-,root,root)
%doc ChangeLog
%{_sysconfdir}/*
%{_bindir}/*
%{_libdir}/*
%{_includedir}/*
%{_mandir}/man*/*

%changelog
* Fri Oct 27 2000  Loic Dachary  <loic@senga.org>
        - add sysconfdir

* Fri Oct 13 2000  Loic Dachary  <loic@senga.org>
        - create
