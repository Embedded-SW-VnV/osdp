# OSDP (OCaml SDP) is an OCaml frontend library to semi-definite
# programming (SDP) solvers.
# Copyright (C) 2012, 2014  P. Roux and P.L. Garoche
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([OSDP], [0.4.3], [ploc@garoche.net])
AC_CONFIG_SRCDIR([src/sdp.ml])

# default prefix is /usr/local
AC_PREFIX_DEFAULT(/usr/local)

# Configure options
AC_ARG_WITH([csdp],
  [AS_HELP_STRING([--without-csdp], [disable support for CSDP])],
  [],
  [with_csdp=check])
AC_ARG_WITH([csdp-inc-path],
  [AS_HELP_STRING([--with-csdp-inc-path=<path>],
    [specify path for CSDP headers])],
  [CSDP_INCPATH=$with_csdp_inc_path],
  [CSDP_INCPATH=no])
AC_ARG_WITH([csdp-lib-path],
  [AS_HELP_STRING([--with-csdp-lib-path=<path>],
    [specify path for CSDP library])],
  [CSDP_LIBPATH=$with_csdp_lib_path],
  [CSDP_LIBPATH=no])

AC_ARG_WITH([mosek],
  [AS_HELP_STRING([--without-mosek], [disable support for MOSEK])],
  [],
  [with_mosek=check])
AC_ARG_WITH([mosek-inc-path],
  [AS_HELP_STRING([--with-mosek-inc-path=<path>],
    [specify path for MOSEK headers])],
  [MOSEK_INCPATH=$with_mosek_inc_path],
  [MOSEK_INCPATH=no])
AC_ARG_WITH([mosek-lib-path],
  [AS_HELP_STRING([--with-mosek-lib-path=<path>],
    [specify path for MOSEK library])],
  [MOSEK_LIBPATH=$with_mosek_lib_path],
  [MOSEK_LIBPATH=no])
MOSEK_LIB=no

AC_ARG_WITH([sdpa],
  [AS_HELP_STRING([--without-sdpa], [disable support for SDPA])],
  [],
  [with_sdpa=check])
AC_ARG_WITH([sdpa-path],
  [AS_HELP_STRING([--with-sdpa-path=<path>],
    [specify path for SDPA binary])],
  [SDPA_PATH=$with_sdpa_path],
  [SDPA_PATH=no])

AC_ARG_WITH([sdpa-gmp],
  [AS_HELP_STRING([--without-sdpa-gmp], [disable support for SDPA-GMP])],
  [],
  [with_sdpa_gmp=check])
AC_ARG_WITH([sdpa-gmp-path],
  [AS_HELP_STRING([--with-sdpa-gmp-path=<path>],
    [specify path for SDPA-GMP binary])],
  [SDPA_GMP_PATH=$with_sdpa_gmp_path],
  [SDPA_GMP_PATH=no])

AC_ARG_WITH([sdpa-dd],
  [AS_HELP_STRING([--without-sdpa-dd], [disable support for SDPA-DD])],
  [],
  [with_sdpa_dd=check])
AC_ARG_WITH([sdpa-dd-path],
  [AS_HELP_STRING([--with-sdpa-dd-path=<path>],
    [specify path for SDPA-DD binary])],
  [SDPA_DD_PATH=$with_sdpa_dd_path],
  [SDPA_DD_PATH=no])

AC_ARG_WITH([camlp4],
  [AS_HELP_STRING([--without-camlp4], [disable support for Camlp4 parsers])],
  [],
  [with_camlp4=check])

AC_ARG_WITH([glpk],
  [AS_HELP_STRING([--without-glpk], [disable support for GLPK])],
  [],
  [with_glpk=check])

# Check for programs
AC_CHECK_TOOL([OCAMLC], [ocamlc], [no])
AS_IF([test "x$OCAMLC" = xno], [AC_MSG_ERROR([Cannot find ocamlc.])])

AC_MSG_CHECKING(OCaml version)
ocamlc_version=`$OCAMLC -v | grep version | rev | cut -d \  -f 1 | rev`
major=`echo $ocamlc_version | cut -d . -f 1`
minor=`echo $ocamlc_version | cut -d . -f 2`
if test "$major" -eq 4 -a "$minor" -lt 1 -o "$major" -lt 4; then
  AC_MSG_ERROR([Ocaml version must be at least 4.01. You have version $ocamlc_version.])
fi
AC_MSG_RESULT($ocamlc_version)

AC_CHECK_TOOL([OCAMLOPT], [ocamlopt], [no])
AS_IF([test "x$OCAMLOPT" = xno], [AC_MSG_ERROR([Cannot find ocamlopt.])])

AC_CHECK_TOOL([OCAMLBUILD], [ocamlbuild], [no])
AS_IF([test "x$OCAMLBUILD" = xno], [AC_MSG_ERROR([Cannot find ocamlbuild.])])

AC_CHECK_TOOL([OCAMLFIND], [ocamlfind], [no])
AS_IF([test "x$OCAMLFIND" = xno], [AC_MSG_ERROR([Cannot find ocamlfind.])])

if test "x$with_camlp4" != xno; then
  AC_CHECK_TOOL([OCAMLLEX], [ocamllex], [no])
  if test "x$OCAMLLEX" = xno; then
    AS_IF([test "x$with_camlp4" = xcheck],
      [AC_MSG_WARN([Cannot find Ocamllex, will compile without it but installing it and reruning this configure script is highly recommended.])],
      [AC_MSG_ERROR([Cannot find Ocamllex.])])
    with_camlp4=no
  fi
fi
if test "x$with_camlp4" != xno; then
  AC_CHECK_TOOL([OCAMLYACC], [ocamlyacc], [no])
  if test "x$OCAMLYACC" = xno; then
    AS_IF([test "x$with_camlp4" = xcheck],
      [AC_MSG_WARN([Cannot find Ocamlyacc, will compile without it but installing it and reruning this configure script is highly recommended.])],
      [AC_MSG_ERROR([Cannot find Ocamlyacc.])])
    with_camlp4=no
  fi
fi
if test "x$with_camlp4" != xno; then
  AC_CHECK_TOOL([CAMLP4], [camlp4o], [no])
  if test "x$CAMLP4" = xno; then
    AS_IF([test "x$with_camlp4" = xcheck],
      [AC_MSG_WARN([Cannot find Camlp4, will compile without it but installing Camlp4 (package camlp4 on Debian based systems) and reruning this configure script is highly recommended.])],
      [AC_MSG_ERROR([Cannot find Camlp4.])])
    with_camlp4=no
  fi
fi
if test "x$with_camlp4" != xno; then
  with_camlp4=yes
  CAMLP4_PARSERS="pa_osdp.cma"
else
  CAMLP4_PARSERS=""
fi
AC_SUBST(with_camlp4)
AC_SUBST(CAMLP4_PARSERS)

if test "x$with_sdpa" != xno; then
  AC_MSG_CHECKING(for SDPA)
  if test "x$SDPA_PATH" = xno; then
    SDPA_PATH=`command -v sdpa || echo no`
  fi
  if test "x$SDPA_PATH" = xno; then
    if test "x$with_sdpa" = "xcheck"; then
      with_sdpa=no
    else
      AC_MSG_ERROR([Unable to find SDPA in \$PATH, please provide path with option --with-sdpa-path=<path>.])
    fi
  fi
  AC_MSG_RESULT($SDPA_PATH)
fi
if test "x$with_sdpa" != xno; then with_sdpa=yes ; fi
AC_SUBST(SDPA_PATH)

if test "x$with_sdpa_gmp" != xno; then
  AC_MSG_CHECKING(for SDPA-GMP)
  if test "x$SDPA_GMP_PATH" = xno; then
    SDPA_GMP_PATH=`command -v sdpa_gmp || echo no`
  fi
  if test "x$SDPA_GMP_PATH" = xno; then
    if test "x$with_sdpa_gmp" = "xcheck"; then
      with_sdpa_gmp=no
    else
      AC_MSG_ERROR([Unable to find SDPA-GMP in \$PATH, please provide path with option --with-sdpa-gmp-path=<path>.])
    fi
  fi
  AC_MSG_RESULT($SDPA_GMP_PATH)
fi
if test "x$with_sdpa_gmp" != xno; then with_sdpa_gmp=yes ; fi
AC_SUBST(SDPA_GMP_PATH)

if test "x$with_sdpa_dd" != xno; then
  AC_MSG_CHECKING(for SDPA-DD)
  if test "x$SDPA_DD_PATH" = xno; then
    SDPA_DD_PATH=`command -v sdpa_dd || echo no`
  fi
  if test "x$SDPA_DD_PATH" = xno; then
    if test "x$with_sdpa_dd" = "xcheck"; then
      with_sdpa_dd=no
    else
      AC_MSG_ERROR([Unable to find SDPA-DD in \$PATH, please provide path with option --with-sdpa-dd-path=<path>.])
    fi
  fi
  AC_MSG_RESULT($SDPA_DD_PATH)
fi
if test "x$with_sdpa_dd" != xno; then with_sdpa_dd=yes ; fi
AC_SUBST(SDPA_DD_PATH)

# Check for libraries
if test "x$with_csdp" != xno; then
  AC_MSG_CHECKING(for CSDP headers)
  if test "x$CSDP_INCPATH" = xno; then
    csdp_path=`command -v csdp || echo no`
    if test "x$csdp_path" != xno; then
      CSDP_INCPATH=`echo "$csdp_path" | sed -e 's|[[^/]]*/csdp$||'`
      CSDP_INCPATH="${CSDP_INCPATH}include/csdp"
    fi
  fi
  if ! test -f ${CSDP_INCPATH}/declarations.h; then
    if test "x$with_csdp" = "xcheck"; then
      with_csdp=no
      CSDP_INCPATH=no
    else
      AC_MSG_ERROR([Unable to find CSDP header declarations.h, please provide path with option --with-csdp-inc-path=<path>.])
    fi
  fi
  AC_MSG_RESULT($CSDP_INCPATH)
fi
if test "x$with_csdp" != xno; then
  AC_MSG_CHECKING(for CSDP library)
  if test "x$CSDP_LIBPATH" = xno; then
    csdp_path=`command -v csdp || echo no`
    if test "x$csdp_path" != xno; then
      CSDP_LIBPATH=`echo "$csdp_path" | sed -e 's|[[^/]]*/csdp$||'`
      CSDP_LIBPATH="${CSDP_LIBPATH}lib/csdp"
    fi
  fi
  if ! test -f ${CSDP_LIBPATH}/libsdp.a; then
    if test "x$with_csdp" = "xcheck"; then
      with_csdp=no
      CSDP_LIBPATH=no
    else
      AC_MSG_ERROR([Unable to find CSDP library libsdp.a, please provide path with option --with-csdp-lib-path=<path>.])
    fi
  fi
  AC_MSG_RESULT($CSDP_LIBPATH)
fi
if test "x$with_csdp" != xno; then with_csdp=yes ; fi
AC_SUBST(with_csdp)
AC_SUBST(CSDP_INCPATH)
AC_SUBST(CSDP_LIBPATH)

if test "x$with_mosek" != xno; then
  AC_MSG_CHECKING(for MOSEK headers)
  if test "x$MOSEK_INCPATH" = xno; then
    mosek_path=`command -v mosek || echo no`
    if test "x$mosek_path" != xno; then
      MOSEK_INCPATH=`echo "$mosek_path" | sed -e 's|[[^/]]*/mosek$||'`
      MOSEK_INCPATH="${MOSEK_INCPATH}h"
    fi
  fi
  if ! test -f ${MOSEK_INCPATH}/mosek.h; then
    if test "x$with_mosek" = "xcheck"; then
      with_mosek=no
      MOSEK_INCPATH=no
    else
      AC_MSG_ERROR([Unable to find MOSEK header mosek.h, please provide path with option --with-mosek-inc-path=<path>.])
    fi
  fi
  AC_MSG_RESULT($MOSEK_INCPATH)
fi
if test "x$with_mosek" != xno; then
  AC_MSG_CHECKING(for MOSEK library)
  if test "x$MOSEK_LIBPATH" = xno; then
    mosek_path=`command -v mosek || echo no`
    if test "x$mosek_path" != xno; then
      MOSEK_LIBPATH=`echo "$mosek_path" | sed -e 's|[[^/]]*/mosek$||'`
      MOSEK_LIBPATH="${MOSEK_LIBPATH}bin"
    fi
  fi
  if test -f ${MOSEK_LIBPATH}/libmosek.so; then
    MOSEK_LIB=mosek
  else if test -f ${MOSEK_LIBPATH}/libmosek64.so; then
    MOSEK_LIB=mosek64
  else
    if test "x$with_mosek" = "xcheck"; then
      with_mosek=no
      MOSEK_LIBPATH=no
    else
      AC_MSG_ERROR([Unable to find MOSEK library libmosek{,64}.so, please provide path with option --with-mosek-lib-path=<path>.])
    fi
  fi; fi
  AC_MSG_RESULT($MOSEK_LIBPATH)
fi
if test "x$with_mosek" != xno; then with_mosek=yes ; fi
AC_SUBST(with_mosek)
AC_SUBST(MOSEK_INCPATH)
AC_SUBST(MOSEK_LIBPATH)
AC_SUBST(MOSEK_LIB)

if test "x$with_csdp" = xno -a "x$with_mosek" = xno -a "x$with_sdpa" = xno -a "x$with_sdpa_gmp" = xno; then
  AC_MSG_WARN($PACKAGE_NAME will be compiled without any SDP solver which is quite useless.)
  AC_MSG_WARN(You should at least install either CSDP (from sources, c.f., INSTALL) or MOSEK or SDPA or SDPA-GMP.)
fi

if test "x$with_glpk" != xno; then
  AC_MSG_CHECKING(for ocaml-glpk)
  GLPK_PATH=`ocamlfind query glpk`
  if test $? -ne 0 ; then
    AS_IF([test "x$with_glpk" = xcheck],
      [AC_MSG_WARN([Cannot find Ocaml-Glpk, will compile without it but it is also possible to install Ocaml-Glpk and rerun this configure script.])],
      [AC_MSG_ERROR([Cannot find Ocaml-Glpk.])])
    with_glpk=no
  else
    AC_MSG_RESULT($GLPK_PATH)
  fi
fi
if test "x$with_glpk" != xno; then
  with_glpk=yes
  REQUIRES_GLPK=" glpk"
  TAGS_GLPK=", package(glpk)"
  MLPACK_GLPK="Monomial_glpk"
  INCLUDE_MONOMIAL_GLPK="include Monomial_glpk"
else
  REQUIRES_GLPK=""
  TAGS_GLPK=""
  MLPACK_GLPK=""
  INCLUDE_MONOMIAL_GLPK=""
fi
AC_SUBST(with_glpk)
AC_SUBST(REQUIRES_GLPK)
AC_SUBST(TAGS_GLPK)
AC_SUBST(MLPACK_GLPK)
AC_SUBST(INCLUDE_MONOMIAL_GLPK)

AC_CHECK_LIB([m], [fesetround], [],
  [AC_MSG_ERROR([Could not find function fesetround in math library.])])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([math.h fenv.h])

# Create files
AC_CONFIG_FILES([Makefile src/Makefile src/myocamlbuild.ml src/config.h src/META src/_tags src/osdp.mlpack src/osdp_top.ml src/monomial_glpk_opt.ml src/sdpa_paths.ml])
AC_OUTPUT
chmod a-w Makefile src/Makefile src/myocamlbuild.ml src/config.h src/META src/_tags src/osdp.mlpack src/osdp_top.ml src/monomial_glpk_opt.ml src/sdpa_paths.ml

echo ""
echo "*** Configuration for $PACKAGE_NAME $PACKAGE_VERSION:"
echo ""
echo "  Compiled with CSDP ......... : $with_csdp"
if test "x$with_csdp" = xyes; then
  echo "    CSDP_INCPATH ............. : $CSDP_INCPATH"
  echo "    CSDP_LIBPATH ............. : $CSDP_LIBPATH"
fi
echo "  Compiled with MOSEK ........ : $with_mosek"
if test "x$with_mosek" = xyes; then
  echo "    MOSEK_INCPATH ............ : $MOSEK_INCPATH"
  echo "    MOSEK_LIBPATH ............ : $MOSEK_LIBPATH"
  echo "    MOSEK_LIB ................ : $MOSEK_LIB"
fi
echo "  Compiled with SDPA ......... : $with_sdpa"
if test "x$with_sdpa" = xyes; then
  echo "    SDPA_PATH ................ : $SDPA_PATH"
fi
echo "  Compiled with SDPA-GMP ..... : $with_sdpa_gmp"
if test "x$with_sdpa_gmp" = xyes; then
  echo "    SDPA_GMP_PATH ............ : $SDPA_GMP_PATH"
fi
echo "  Compiled with SDPA-DD ...... : $with_sdpa_dd"
if test "x$with_sdpa_dd" = xyes; then
  echo "    SDPA_DD_PATH ............. : $SDPA_DD_PATH"
fi
echo "  Compiled with Camlp4 ....... : $with_camlp4"
echo "  Compiled with GLPK ......... : $with_glpk"
echo ""
echo "  Now type 'make' to build $PACKAGE $VERSION"
echo "  then 'make install' to install."
echo ""

if test "x$with_csdp" = xno -a "x$with_mosek" = xno -a "x$with_sdpa" = xno -a "x$with_sdpa_gmp" = xno; then
  AC_MSG_WARN($PACKAGE_NAME will be compiled without any SDP solver which is quite useless.)
  AC_MSG_WARN(You should at least install either CSDP (from sources, c.f., INSTALL) or MOSEK or SDPA or SDPA-GMP.)
fi
