/*
  Author:   Pieter Van den Abeele
  E-mail:   pvdabeel@mac.com
  Copyright (c) 2005-2025, Pieter Van den Abeele

  Distributed under the terms of the LICENSE file in the root directory of this
  project.
*/


/** <module> PREFERENCE
The preferences module contains build specific preferences
*/

:- module(preference, []).

:- dynamic preference:known_broken/1.
:- dynamic preference:positive_use/1.
:- dynamic preference:negative_usa/1.
:- dynamic preference:use_expand_hidden/1.
:- dynamic preference:masked/1.

% ***********************
% PREFERENCE declarations
% ***********************


%! preference:env_cflags(?Cflags)
%
% Fact which defines the CFLAGS environment variable

preference:env_cflags('-O3 -march=native -pipe').


%! preference:env_cxxflags(?Cxxflags)
%
% Fact which defines the CXXFLAGS environment variable

preference:env_cxxflags('-O3 -march=native -pipe').


%! preference:env_chost(?Chost)
%
% Fact which defines the CHOST environment variable

preference:env_chost('x86_64-pc-linux-gnu').


%! preference:env_makeopts(?Makeopts)
%
% Fact which defines the MAKEOPTS variable

preference:env_makeopts('-j36').


%! preference:env_features(?Features)
%
% Fact which defines the FEATURES variable

preference:env_features('sign -ccache -buildpkg -sandbox -usersandbox -ebuild-locks parallel-fetch parallel-install').


%! preference:accept_keywords(?Keyword)
%
% Fact which defines the ACCEPT_KEYWORDS variable

preference:accept_keywords(stable(amd64)).
preference:accept_keywords(unstable(amd64)).



%! preference:use(?Use)
%
% Fact which defines the USE flags to be used

%preference:use(['X','a52','aac','aacplus','aalib','abi_x86_64','account','acl','aio','alsa','alsa_cards_ens1371','amd64','apache2_modules_auth_basic','apache2_modules_authn_core','apache2_modules_authn_file','apache2_modules_authz_core','apache2_modules_authz_host','apache2_modules_dir','apache2_modules_mime','apache2_modules_socache_shmcb','apache2_modules_unixd','apng','avahi','bzip2','cairo','calligra_features_karbon','calligra_features_sheets','calligra_features_words','cli','collectd_plugins_df','collectd_plugins_interface','collectd_plugins_irq','collectd_plugins_load','collectd_plugins_memory','collectd_plugins_rrdtool','collectd_plugins_swap','collectd_plugins_syslog','container','cpu_flags_x86_aes','cpu_flags_x86_avx','cpu_flags_x86_avx2','cpu_flags_x86_avx512bw','cpu_flags_x86_avx512cd','cpu_flags_x86_avx512dq','cpu_flags_x86_avx512f','cpu_flags_x86_avx512vl','cpu_flags_x86_f16c','cpu_flags_x86_fma3','cpu_flags_x86_mmx','cpu_flags_x86_mmxext','cpu_flags_x86_pclmul','cpu_flags_x86_popcnt','cpu_flags_x86_sse','cpu_flags_x86_sse2','cpu_flags_x86_sse3','cpu_flags_x86_sse4_1','cpu_flags_x86_sse4_2','cpu_flags_x86_ssse3','crypt','cvs','cxx','dbus','directfb','dri','dts','elibc_glibc','elogind','fbcondecor','fontconfig','fortran','gdbm','gif','git','glitz','gmp','gpg','gpm','http','iconv','icu','imap','input_devices_evdev','input_devices_keyboard','input_devices_mouse','input_devices_vmmouse','ipv6','jpeg','jpeg2k','json','kernel_linux','libkms','libtirpc','md5sum','mdnsresponder-compat','messages','mmx','mmxext','mp3','ncurses','nova','npm','nptl','object','opengl','openmp','openssl','pam','pcre','pcre16','pkcs11','png','policykit','pop','proxy','python','python_single_target_python3_7','python_targets_python3_7','qemu','readline','ruby_targets_ruby25','seccomp','sidebar','smime','smp','smtp','split-usr','sqlite','sse','sse2','sse3','sse4','sse4_2','ssh','ssl','ssse3','svg','tcpd','threads','tiff','truetype','unicode','userland_GNU','video_cards_vesa','video_cards_vga','video_cards_vmware','x264','x265','x86emu','xa','xattr','xcb','xkb','xlib-xcb','xvid','zeroconf','zlib']).

preference:positive_use('a52').
preference:positive_use('aac').
preference:positive_use('aacplus').
preference:positive_use('aalib').
preference:positive_use('abi_x86_64').
preference:positive_use('account').
preference:positive_use('acl').
preference:positive_use('aio').
preference:positive_use('alsa').
preference:positive_use('alsa_cards_ens1371').
preference:positive_use('amd64').
preference:positive_use('apache2_modules_auth_basic').
preference:positive_use('apache2_modules_authn_core').
preference:positive_use('apache2_modules_authn_file').
preference:positive_use('apache2_modules_authz_core').
preference:positive_use('apache2_modules_authz_host').
preference:positive_use('apache2_modules_dir').
preference:positive_use('apache2_modules_mime').
preference:positive_use('apache2_modules_socache_shmcb').
preference:positive_use('apache2_modules_unixd').
preference:positive_use('addc').
preference:positive_use('apng').
preference:positive_use('avahi').
preference:positive_use('bzip2').
preference:positive_use('cairo').
preference:positive_use('calligra_features_karbon').
preference:positive_use('calligra_features_sheets').
preference:positive_use('calligra_features_words').
preference:positive_use('cli').
preference:positive_use('collectd_plugins_df').
preference:positive_use('collectd_plugins_interface').
preference:positive_use('collectd_plugins_irq').
preference:positive_use('collectd_plugins_load').
preference:positive_use('collectd_plugins_memory').
preference:positive_use('collectd_plugins_rrdtool').
preference:positive_use('collectd_plugins_swap').
preference:positive_use('collectd_plugins_syslog').
preference:positive_use('container').
preference:positive_use('cpu_flags_x86_aes').
preference:positive_use('cpu_flags_x86_avx').
preference:positive_use('cpu_flags_x86_avx2').
preference:positive_use('cpu_flags_x86_avx512bw').
preference:positive_use('cpu_flags_x86_avx512cd').
preference:positive_use('cpu_flags_x86_avx512dq').
preference:positive_use('cpu_flags_x86_avx512f').
preference:positive_use('cpu_flags_x86_avx512vl').
preference:positive_use('cpu_flags_x86_f16c').
preference:positive_use('cpu_flags_x86_fma3').
preference:positive_use('cpu_flags_x86_mmx').
preference:positive_use('cpu_flags_x86_mmxext').
preference:positive_use('cpu_flags_x86_pclmul').
preference:positive_use('cpu_flags_x86_popcnt').
preference:positive_use('cpu_flags_x86_sse').
preference:positive_use('cpu_flags_x86_sse2').
preference:positive_use('cpu_flags_x86_sse3').
preference:positive_use('cpu_flags_x86_sse4_1').
preference:positive_use('cpu_flags_x86_sse4_2').
preference:positive_use('cpu_flags_x86_ssse3').
preference:positive_use('crypt').
preference:positive_use('cvs').
preference:positive_use('cxx').
preference:positive_use('dbus').
preference:positive_use('directfb').
preference:positive_use('dri').
preference:positive_use('dts').
preference:positive_use('elibc_glibc').
preference:positive_use('elogind').
preference:positive_use('fbcondecor').
preference:positive_use('fontconfig').
preference:positive_use('fortran').
preference:positive_use('gdbm').
preference:positive_use('gif').
preference:positive_use('git').
preference:positive_use('glitz').
preference:positive_use('gmp').
preference:positive_use('gpg').
preference:positive_use('gpm').
preference:positive_use('http').
preference:positive_use('iconv').
preference:positive_use('icu').
preference:positive_use('imap').
preference:positive_use('input_devices_evdev').
preference:positive_use('input_devices_keyboard').
preference:positive_use('input_devices_mouse').
preference:positive_use('input_devices_vmmouse').
preference:positive_use('ipv6').
preference:positive_use('jpeg').
preference:positive_use('jpeg2k').
preference:positive_use('json').
preference:positive_use('kernel_linux').
preference:positive_use('libkms').
preference:positive_use('libtirpc').
preference:positive_use('md5sum').
preference:positive_use('mdnsresponder-compat').
preference:positive_use('messages').
preference:positive_use('mmx').
preference:positive_use('mmxext').
preference:positive_use('mp3').
preference:positive_use('ncurses').
preference:positive_use('nova').
preference:positive_use('npm').
preference:positive_use('nptl').
preference:positive_use('object').
preference:positive_use('opengl').
preference:positive_use('openmp').
preference:positive_use('openssl').
preference:positive_use('pam').
preference:positive_use('pcre').
preference:positive_use('pcre16').
preference:positive_use('pkcs11').
preference:positive_use('png').
preference:positive_use('policykit').
preference:positive_use('pop').
preference:positive_use('proxy').
preference:positive_use('python').
preference:positive_use('python_single_target_python3_7').
preference:positive_use('python_targets_python3_7').
preference:positive_use('qemu').
preference:positive_use('readline').
preference:positive_use('ruby_targets_ruby25').
preference:positive_use('seccomp').
preference:positive_use('sidebar').
preference:positive_use('smime').
preference:positive_use('smp').
preference:positive_use('smtp').
preference:positive_use('split-usr').
preference:positive_use('sqlite').
preference:positive_use('sse').
preference:positive_use('sse2').
preference:positive_use('sse3').
preference:positive_use('sse4').
preference:positive_use('sse4_2').
preference:positive_use('ssh').
preference:positive_use('ssl').
preference:positive_use('ssse3').
preference:positive_use('svg').
preference:positive_use('tcpd').
preference:positive_use('threads').
preference:positive_use('tiff').
preference:positive_use('truetype').
preference:positive_use('unicode').
preference:positive_use('userland_GNU').
preference:positive_use('video_cards_vesa').
preference:positive_use('video_cards_vga').
preference:positive_use('video_cards_vmware').
preference:positive_use('x264').
preference:positive_use('x265').
preference:positive_use('x86emu').
preference:positive_use('xa').
preference:positive_use('xattr').
preference:positive_use('xcb').
preference:positive_use('xkb').
preference:positive_use('xlib-xcb').
preference:positive_use('xvid').
preference:positive_use('zeroconf').
preference:positive_use('zlib').

preference:negative_use('test').
preference:negative_use('static-libs').


%! preference:use_expand_hidden(?Use)
%
% The printer does not print the expanding USE declared as hidden

preference:use_expand_hidden('abi_mips').
preference:use_expand_hidden('abi_ppc').
preference:use_expand_hidden('abi_riscv').
preference:use_expand_hidden('abi_s390').
preference:use_expand_hidden('abi_x86').
preference:use_expand_hidden('cpu_flags_arm').
preference:use_expand_hidden('cpu_flags_ppc').



%! preference:masked(?Repository://?Entry)
%
% Fact which masks a Repository entry

% Disabled for now (performance)
%
% The prover uses the dynamic 'proven:broken/1' to mark some entries as broken
% preference:masked(Repository://Entry) :- prover:broken(Repository://Entry).

preference:masked(portage://'dev-perl/Filesys-SmbClient-3.200.0-r4').


%! preference:set(?Name,?List)
%
% A sample set for testing merging different packages

preference:set('@prolog',[ 'dev-lang/swi-prolog', 'portage://dev-lang/qu-prolog-10.8', 'portage-9999','>gentoo-sources-2.5' ]).


%! preference:world(?List)
%
% A sample world list for testing merging different packages

preference:world([ 'acct-group/avahi-0-r1','acct-group/input-0-r1','acct-group/kvm-0-r1','acct-group/locate-0-r1','acct-group/man-0-r1','acct-group/netdev-0-r1','acct-group/portage-0','acct-group/render-0-r1','acct-group/sshd-0-r1','acct-group/utmp-0-r1','acct-user/avahi-0-r1','acct-user/man-1-r1','acct-user/portage-0','acct-user/sshd-0-r1','app-admin/eselect-1.4.17','app-admin/logrotate-3.18.0','app-admin/metalog-20200113-r1','app-admin/perl-cleaner-2.28','app-admin/sudo-1.9.5_p2-r1','app-arch/afio-2.5.1-r2','app-arch/bzip2-1.0.8-r1','app-arch/gzip-1.10','app-arch/libarchive-3.5.1','app-arch/pbzip2-1.1.13','app-arch/tar-1.33','app-arch/unzip-6.0_p25-r1','app-arch/xz-utils-5.2.5','app-arch/zstd-1.4.8-r1','app-crypt/gnupg-2.2.27','app-crypt/gpgme-1.15.1','app-crypt/libb2-0.98.1-r3','app-crypt/openpgp-keys-gentoo-release-20200704','app-crypt/p11-kit-0.23.22','app-crypt/rhash-1.4.1','app-doc/xmltoman-0.4-r1','app-editors/nano-5.5','app-editors/vim-8.2.0814-r100','app-editors/vim-core-8.2.0814','app-emulation/open-vm-tools-11.2.5_p17337674','app-emulation/virt-what-1.20','app-eselect/eselect-fontconfig-1.1-r1','app-eselect/eselect-iptables-20200508','app-eselect/eselect-lib-bin-symlink-0.1.1-r1','app-eselect/eselect-pinentry-0.7.1','app-eselect/eselect-python-20200719','app-eselect/eselect-rust-20200419','app-eselect/eselect-vi-1.2','app-misc/c_rehash-1.7-r1','app-misc/editor-wrapper-4-r1','app-misc/mime-types-9','app-misc/pax-utils-1.2.9','app-misc/screen-4.8.0-r1','app-portage/cpuid2cpuflags-11','app-portage/elt-patches-20201205','app-portage/esearch-1.3-r3','app-portage/gemato-16.2','app-portage/genlop-0.30.10-r2','app-portage/gentoolkit-0.5.0-r2','app-portage/portage-utils-0.90.1','app-portage/repoman-3.0.2','app-portage/splat-0.08-r1','app-shells/bash-5.1_p4','app-shells/zsh-5.8','app-text/ansifilter-2.18','app-text/build-docbook-catalog-1.21','app-text/docbook-xml-dtd-4.2-r3','app-text/docbook-xml-dtd-4.1.2-r7','app-text/docbook-xsl-stylesheets-1.79.1-r2','app-text/manpager-1','app-text/opensp-1.5.2-r6','app-text/po4a-0.62','app-text/sgml-common-0.6.3-r7','app-text/xmlto-0.0.28-r3','app-vim/gentoo-syntax-20201216','dev-db/sqlite-3.34.0','dev-lang/nasm-2.15.05','dev-lang/perl-5.32.1','dev-lang/python-3.10.0_alpha5','dev-lang/python-3.9.1-r1','dev-lang/python-3.7.9-r2','dev-lang/python-exec-2.4.6-r4','dev-lang/python-exec-conf-2.4.6','dev-lang/rust-1.49.0','dev-lang/spidermonkey-78.7.1','dev-lang/swig-4.0.2','dev-lang/tcl-8.6.11','dev-libs/elfutils-0.183','dev-libs/expat-2.2.10','dev-libs/fribidi-1.0.9','dev-libs/gmp-6.2.1','dev-libs/icu-68.2','dev-libs/isl-0.23-r1','dev-libs/jsoncpp-1.9.4','dev-libs/libassuan-2.5.4','dev-libs/libbsd-0.10.0','dev-libs/libdaemon-0.14-r3','dev-libs/libdnet-1.14-r2','dev-libs/libedit-20191211.3.1','dev-libs/libevdev-1.11.0','dev-libs/libevent-2.1.12','dev-libs/libffi-3.3-r2','dev-libs/libksba-1.5.0','dev-libs/liblinear-242','dev-libs/libltdl-2.4.6','dev-libs/libmspack-0.10.1_alpha','dev-libs/libpcre-8.44','dev-libs/libpipeline-1.5.3','dev-libs/libtasn1-4.16.0','dev-libs/libunistring-0.9.10','dev-libs/libuv-1.40.0','dev-libs/libxml2-2.9.10-r4','dev-libs/libxslt-1.1.34-r1','dev-libs/libyaml-0.2.5','dev-libs/lzo-2.10','dev-libs/mpc-1.2.1','dev-libs/mpfr-4.1.0','dev-libs/npth-1.6-r1','dev-libs/nspr-4.29','dev-libs/openssl-1.1.1i','dev-libs/popt-1.18','dev-libs/xmlsec-1.2.31','dev-perl/Authen-SASL-2.160.0-r2','dev-perl/Date-Manip-6.820.0','dev-perl/DBD-SQLite-1.660.0','dev-perl/DBI-1.643.0','dev-perl/Devel-Size-0.830.0','dev-perl/Digest-HMAC-1.30.0-r2','dev-perl/Encode-Locale-1.50.0','dev-perl/Error-0.170.290','dev-perl/ExtUtils-Config-0.8.0','dev-perl/ExtUtils-Helpers-0.26.0','dev-perl/ExtUtils-InstallPaths-0.12.0','dev-perl/File-Listing-6.70.0','dev-perl/HTML-Parser-3.720.0','dev-perl/HTML-Tagset-3.200.0-r1','dev-perl/HTTP-Cookies-6.40.0','dev-perl/HTTP-Daemon-6.60.0','dev-perl/HTTP-Date-6.20.0-r1','dev-perl/HTTP-Message-6.130.0','dev-perl/HTTP-Negotiate-6.10.0-r1','dev-perl/IO-HTML-1.1.0','dev-perl/IO-Socket-INET6-2.720.0-r1','dev-perl/IO-Socket-SSL-2.66.0','dev-perl/libwww-perl-6.270.0','dev-perl/Locale-gettext-1.70.0','dev-perl/LWP-MediaTypes-6.20.0-r1','dev-perl/LWP-Protocol-https-6.70.0','dev-perl/MailTools-2.190.0','dev-perl/MIME-Charset-1.12.2','dev-perl/Module-Build-0.422.400','dev-perl/Module-Build-Tiny-0.39.0','dev-perl/Mozilla-CA-20999999','dev-perl/Net-Daemon-0.480.0-r2','dev-perl/Net-HTTP-6.170.0','dev-perl/Net-SSLeay-1.880.0','dev-perl/PlRPC-0.202.0-r3','dev-perl/Pod-Parser-1.630.0-r1','dev-perl/SGMLSpm-1.1-r1','dev-perl/Socket6-0.280.0','dev-perl/Sub-Name-0.210.0','dev-perl/TermReadKey-2.370.0','dev-perl/Text-CharWidth-0.40.0-r1','dev-perl/Text-WrapI18N-0.60.0-r1','dev-perl/TimeDate-2.330.0','dev-perl/Try-Tiny-0.300.0','dev-perl/Unicode-LineBreak-2019.1.0','dev-perl/URI-1.730.0','dev-perl/WWW-RobotRules-6.20.0-r1','dev-perl/XML-Parser-2.440.0','dev-perl/YAML-Tiny-1.730.0','dev-python/certifi-10001-r1','dev-python/chardet-4.0.0','dev-python/cython-0.29.21-r1','dev-python/idna-2.10-r1','dev-python/jinja-2.11.3','dev-python/lxml-4.6.2-r1','dev-python/mako-1.1.4','dev-python/markupsafe-1.1.1-r1','dev-python/PySocks-1.7.1-r1','dev-python/pyyaml-5.4.1','dev-python/requests-2.25.1-r1','dev-python/setuptools_scm-5.0.1','dev-util/cmake-3.19.4','dev-util/desktop-file-utils-0.26-r1','dev-util/glib-utils-2.66.4','dev-util/google-perftools-2.8','dev-util/gperf-3.1','dev-util/gtk-doc-am-1.33.1','dev-util/intltool-0.51.0-r2','dev-util/itstool-2.0.6-r1','dev-util/meson-format-array-0','dev-util/pkgconfig-0.29.2','dev-util/re2c-2.0.3','dev-vcs/cvsps-2.2_beta1-r1','gui-libs/display-manager-init-1.0-r2','mail-mta/nullmailer-2.2-r1','media-fonts/dejavu-2.37','media-fonts/encodings-1.0.5-r1','media-fonts/font-adobe-100dpi-1.0.3-r2','media-fonts/font-adobe-75dpi-1.0.3-r2','media-fonts/font-alias-1.0.4','media-fonts/font-misc-misc-1.1.2-r2','media-fonts/font-util-1.3.2-r1','media-gfx/graphite2-1.3.14','media-gfx/graphviz-2.44.1-r1','media-libs/fontconfig-2.13.1-r2','media-libs/freeglut-3.2.1','media-libs/freetype-2.10.4','media-libs/giflib-5.2.1-r1','media-libs/glu-9.0.1','media-libs/imlib2-1.7.1','media-libs/libepoxy-1.5.5','media-libs/libglvnd-1.3.2-r2','media-libs/libid3tag-0.15.1b-r4','media-libs/libjpeg-turbo-2.0.6','media-libs/libpng-1.6.37-r2','media-libs/tiff-4.2.0','net-analyzer/mtr-0.94','net-analyzer/nmap-7.91-r1','net-dns/avahi-0.8-r2','net-dns/libidn2-2.3.0','net-firewall/iptables-1.8.7','net-libs/gnutls-3.6.15','net-libs/libmnl-1.0.4','net-libs/libnsl-1.3.0-r1','net-libs/libpcap-1.10.0','net-libs/libssh2-1.9.0_p20200614','net-libs/rpcsvc-proto-1.4.2','net-misc/dhcpcd-9.4.0','net-misc/iputils-20210202','net-misc/keychain-2.8.5','net-misc/netifrc-0.7.3','net-misc/openssh-8.4_p1-r3','net-misc/rsync-3.2.3-r1','net-misc/sntpd-3.0-r1','net-misc/wget-1.21.1','perl-core/File-Temp-0.230.900','sys-apps/acl-2.2.53-r1','sys-apps/baselayout-2.7-r1','sys-apps/busybox-1.33.0','sys-apps/coreutils-8.32-r1','sys-apps/debianutils-4.11.2','sys-apps/diffutils-3.7-r1','sys-apps/fbset-2.1','sys-apps/file-5.39-r3','sys-apps/findutils-4.8.0','sys-apps/gawk-5.1.0','sys-apps/grep-3.6','sys-apps/groff-1.22.4','sys-apps/help2man-1.48.1','sys-apps/hwids-20201207','sys-apps/install-xattr-0.8','sys-apps/iproute2-5.10.0','sys-apps/kbd-2.4.0','sys-apps/kmod-28','sys-apps/less-563-r1','sys-apps/man-db-2.9.4','sys-apps/man-pages-5.10','sys-apps/man-pages-posix-2017a','sys-apps/mlocate-0.26-r3','sys-apps/net-tools-2.10','sys-apps/openrc-0.42.1-r1','sys-apps/opentmpfiles-0.2','sys-apps/pciutils-3.7.0','sys-apps/sandbox-2.20','sys-apps/sed-4.8','sys-apps/sysvinit-2.98-r1','sys-apps/texinfo-6.7','sys-apps/util-linux-2.36.2','sys-apps/which-2.21','sys-auth/nss-mdns-0.14.1','sys-auth/passwdqc-1.4.0-r1','sys-block/parted-3.4','sys-boot/efibootmgr-17','sys-devel/autoconf-2.69-r5','sys-devel/autoconf-2.13-r1','sys-devel/autoconf-archive-2019.01.06','sys-devel/autoconf-wrapper-13-r1','sys-devel/automake-1.16.3-r1','sys-devel/automake-wrapper-11','sys-devel/bc-1.07.1-r3','sys-devel/binutils-2.35.2','sys-devel/binutils-config-5.3.2','sys-devel/flex-2.6.4-r1','sys-devel/gcc-10.2.0-r5','sys-devel/gcc-config-2.3.3','sys-devel/gettext-0.21','sys-devel/gnuconfig-20210107','sys-devel/libtool-2.4.6-r6','sys-devel/llvm-11.0.1','sys-devel/llvm-common-11.0.1','sys-devel/m4-1.4.18-r1','sys-devel/make-4.3','sys-devel/patch-2.7.6-r4','sys-devel/prelink-20151030-r1','sys-fs/dosfstools-4.2','sys-fs/eudev-3.2.10','sys-fs/fuse-2.9.9-r1','sys-fs/fuse-common-3.10.1','sys-fs/udev-init-scripts-34','sys-kernel/installkernel-gentoo-2','sys-kernel/linux-headers-5.10','sys-libs/e2fsprogs-libs-1.46.1','sys-libs/efivar-37','sys-libs/glibc-2.32-r7','sys-libs/gpm-1.20.7-r2','sys-libs/libcap-2.48','sys-libs/libtermcap-compat-2.0.8-r4','sys-libs/libunwind-1.5.0-r1','sys-libs/libutempter-1.2.1','sys-libs/mtdev-1.1.6','sys-libs/ncurses-6.2_p20210123','sys-libs/ncurses-compat-6.2','sys-libs/pam-1.5.1','sys-libs/readline-8.1','sys-libs/timezone-data-2021a','sys-libs/zlib-1.2.11-r3','sys-process/cronbase-0.3.7-r6','sys-process/cronie-1.5.5-r1','sys-process/htop-3.0.5','sys-process/parallel-20210122','sys-process/procps-3.3.17','sys-process/psmisc-23.4','virtual/acl-0-r2','virtual/awk-1','virtual/cron-0-r2','virtual/dev-manager-0-r2','virtual/editor-0-r3','virtual/glu-9.0-r2','virtual/jpeg-100','virtual/libc-1-r1','virtual/libelf-3','virtual/libiconv-0-r2','virtual/libintl-0-r2','virtual/libudev-232-r3','virtual/logger-0-r1','virtual/man-0-r4','virtual/mta-1-r1','virtual/opengl-7.0-r2','virtual/os-headers-0-r2','virtual/package-manager-1','virtual/pager-0','virtual/perl-Carp-1.500.0-r3','virtual/perl-Compress-Raw-Bzip2-2.93.0','virtual/perl-Compress-Raw-Zlib-2.93.0','virtual/perl-CPAN-Meta-2.150.10-r4','virtual/perl-CPAN-Meta-YAML-0.18.0-r6','virtual/perl-Digest-SHA-6.20.0-r1','virtual/perl-Encode-3.60.0','virtual/perl-ExtUtils-CBuilder-0.280.234','virtual/perl-ExtUtils-Install-2.140.0-r3','virtual/perl-ExtUtils-Manifest-1.720.0-r1','virtual/perl-ExtUtils-ParseXS-3.400.0-r1','virtual/perl-File-Path-2.160.0-r1','virtual/perl-File-Spec-3.780.0-r1','virtual/perl-File-Temp-0.230.900','virtual/perl-Getopt-Long-2.510.0','virtual/perl-IO-1.430.0','virtual/perl-IO-Compress-2.93.0','virtual/perl-IO-Socket-IP-0.390.0-r3','virtual/perl-JSON-PP-4.40.0','virtual/perl-libnet-3.110.0-r4','virtual/perl-MIME-Base64-3.150.0-r7','virtual/perl-Module-Metadata-1.0.37','virtual/perl-parent-0.238.0','virtual/perl-Parse-CPAN-Meta-2.150.10-r4','virtual/perl-Perl-OSType-1.10.0-r4','virtual/perl-Pod-Parser-1.630.0-r8','virtual/perl-podlators-4.140.0','virtual/perl-Socket-2.29.0','virtual/perl-Storable-3.210.0','virtual/perl-Test-Harness-3.420.0-r3','virtual/perl-Text-ParseWords-3.300.0-r7','virtual/perl-Time-Local-1.280.0-r1','virtual/perl-version-0.992.400-r1','virtual/perl-XSLoader-0.300.0-r3','virtual/pkgconfig-2','virtual/service-manager-1','virtual/ssh-0','virtual/tmpfiles-0-r1','virtual/ttf-fonts-1-r1','virtual/udev-217-r2','virtual/yacc-0','x11-apps/bdftopcf-1.1','x11-apps/iceauth-1.0.8-r1','x11-apps/luit-20190106','x11-apps/mkfontscale-1.2.1','x11-apps/rgb-1.0.6-r1','x11-apps/setxkbmap-1.3.2','x11-apps/xauth-1.1','x11-apps/xinit-1.4.1-r1','x11-apps/xkbcomp-1.4.4','x11-apps/xmessage-1.0.5-r1','x11-apps/xrandr-1.5.1','x11-apps/xrdb-1.2.0','x11-base/xcb-proto-1.14.1','x11-base/xorg-drivers-1.20-r2','x11-base/xorg-proto-2020.1','x11-base/xorg-server-1.20.10-r3','x11-drivers/xf86-input-evdev-2.10.6','x11-drivers/xf86-input-vmmouse-13.1.0-r1','x11-drivers/xf86-video-vesa-2.5.0','x11-drivers/xf86-video-vmware-13.3.0','x11-libs/cairo-1.16.0-r4','x11-libs/gdk-pixbuf-2.42.2','x11-libs/libfontenc-1.1.4','x11-libs/libICE-1.0.10','x11-libs/libpciaccess-0.16','x11-libs/libSM-1.2.3-r1','x11-libs/libX11-1.7.0','x11-libs/libXau-1.0.9-r1','x11-libs/libXaw-1.0.13-r2','x11-libs/libxcb-1.14','x11-libs/libXdmcp-1.1.3','x11-libs/libXext-1.3.4','x11-libs/libXfixes-5.0.3-r3','x11-libs/libXfont2-2.0.4','x11-libs/libXft-2.3.3','x11-libs/libXi-1.7.10','x11-libs/libxkbfile-1.1.0','x11-libs/libXmu-1.1.3','x11-libs/libXpm-3.5.13','x11-libs/libXrandr-1.5.2','x11-libs/libXrender-0.9.10-r2','x11-libs/libxshmfence-1.3-r2','x11-libs/libXt-1.2.1','x11-libs/libXxf86vm-1.1.4-r2','x11-libs/pango-1.42.4-r2','x11-libs/pixman-0.40.0','x11-libs/xtrans-1.4.0','x11-misc/compose-tables-1.7.0','x11-misc/shared-mime-info-2.1','x11-misc/util-macros-1.19.3','x11-misc/xbitmaps-1.1.2-r1','x11-misc/xkeyboard-config-2.31','x11-terms/xterm-366','x11-wm/fluxbox-1.3.7-r4' ]).
