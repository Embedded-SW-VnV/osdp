#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([OSDP], [0.1], [ploc@garoche.net])
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

if test "x$with_csdp" = xno -a "x$with_mosek" = xno; then
  AC_MSG_WARN($PACKAGE_NAME will be compiled without any SDP solver)
  AC_MSG_WARN(this is quite useless. You should at least install either CSDP or MOSEK.)
fi

# Create files
AC_CONFIG_FILES([src/Makefile src/myocamlbuild.ml src/config.h])
AC_OUTPUT
chmod a-w src/Makefile src/myocamlbuild.ml src/config.h

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
echo ""
echo "  Now type 'make' to build $PACKAGE $VERSION"
echo "  then 'make install' to install."
echo ""
