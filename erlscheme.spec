%global realname erlscheme

Name: %{realname}
Version: 0.1
Release: 1%{?dist}
Summary: Scheme implementation for the Erlang VM
License: Apache-2.0
Group: Development/Languages
URL: http://github.com/mikpe/%{realname}
Source0: %{realname}-%{version}.tar.xz
BuildRoot: %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)
BuildArch: noarch

Requires: erlang-compiler%{?_isa}
Requires: erlang-erts%{?_isa}
Requires: erlang-kernel%{?_isa}
Requires: erlang-stdlib%{?_isa}

%description
ErlScheme is an implementation of the Scheme programming language,
running on the Erlang/OTP virtual machine.

%prep
%setup -q -n %{realname}-%{version}

%build
make

%install
rm -rf %{buildroot}
make datadir=%{_datadir} bindir=%{_bindir} DESTDIR=%{buildroot} install

%clean
rm -rf %{buildroot}

%files
%defattr(-,root,root,-)
%doc LICENSE README.md
%dir %{_datadir}/%{realname}
%dir %{_datadir}/%{realname}/ebin
%dir %{_datadir}/%{realname}/scm
%{_datadir}/%{realname}/ebin/*.beam
%{_datadir}/%{realname}/scm/*.scm
%{_bindir}/erlscheme

%changelog
* Tue Jul 01 2014 Mikael Pettersson <mikpelinux@gmail.com> - 0.1-1
- Initial build.
