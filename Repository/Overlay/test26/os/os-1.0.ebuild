# Copyright 2013 Pieter Van den Abeele
# Distributed under the terms of the GNU General Public License v2
# $Header$

DESCRIPTION="Description of test package os"
HOMEPAGE="http://www.portage-ng.org/"
SRC_URI=""

LICENSE="GPL-2"
SLOT="0"
KEYWORDS="alpha amd64 arm hppa ia64 m68k mips ppc ppc64 s390 sh sparc x86 sparc-fbsd x86-fbsd"
IUSE=""

DEPEND="|| (test26/linux test26/bsd test26/windows)"
RDEPEND=""

src_unpack() {
	echo "unpacking"
}

src_compile() {
	echo "compiling"
}

src_install() {
	echo "installing"
}