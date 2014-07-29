#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_INIT([OSDP], [0.1], [ploc@garoche.net])
AC_CONFIG_SRCDIR([Makefile.in])


# default prefix is /usr/local
AC_PREFIX_DEFAULT(/usr/local)

AC_ARG_WITH([glpk-path],
	[AS_HELP_STRING([--with-glpk-path],
	      [specify the path of OCaml-GLPK library. glpk.cmxa should be in glpk-path @<:@default=$(ocamlfind query glpk)@:>@])],
	[AS_IF([test "x$with_glpk_path" = xno],
		[AC_MSG_ERROR([OCaml-GLPK library is needed])],
		[test "x$with_glpk_path" = xyes],
		[GLPK_PATH=$(ocamlfind query glpk)],
		[GLPK_PATH=$with_glpk_path]
	)],
	[GLPK_PATH=$(ocamlfind query glpk)]
)
AC_SUBST(GLPK_PATH)

AC_ARG_WITH([csdp-lib-path],
	[AS_HELP_STRING([--with-csdp-lib-path],
	      [specify the path of CSDP library. libsdp.a should be in csdp-lib-path @<:@default=~/prefix/lib/csdp@:>@])],
	[AS_IF([test "x$with_csdp_lib_path" = xno],
		[AC_MSG_ERROR([CSDP library is needed])],
		[test "x$with_csdp_lib_path" = xyes],
		[CSDP_LIBPATH=~/prefix/lib/csdp],
		[CSDP_LIBPATH=$with_csdp_lib_path]
	)],
	[CSDP_LIBPATH=~/prefix/lib/csdp]
)
AC_SUBST(CSDP_LIBPATH)

CSDP_INCPATH=~/prefix/include/csdp
AC_ARG_WITH([csdp-inc-path],
	[AS_HELP_STRING([--with-csdp-inc-path],
	      [specify the path of CSDP lirary includes. @<:@default=$CSDP_INCPATH@:>@])],
	[AS_IF([test "x$with_csdp_inc_path" = xno],
		[AC_MSG_ERROR([CSDP library is needed])],
		[test "x$with_csdp_inc_path" = xyes],
		[],
		[CSDP_INCPATH=$with_csdp_inc_path]
	)],
	[]
)
AC_SUBST(CSDP_INCPATH)


# Checks for programs. ocaml311 ocamlbuild and solvers
AC_PATH_PROG([OCAMLC],[ocamlc],[:])
AC_MSG_CHECKING(OCaml version)
ocamlc_version=`$OCAMLC -v | grep version | rev| cut -d \  -f 1 | rev`
major=`echo $ocamlc_version | cut -d . -f 1`
minor=`echo $ocamlc_version | cut -d . -f 2`
if (test "$major" -lt 3 -a "$minor" -lt 11 ); then
  AC_MSG_ERROR([Ocaml version must be at least 3.11. You have version $ocamlc_version])
fi
AC_MSG_RESULT(valid ocaml version detected: $ocamlc_version)

AC_PATH_PROG([OCAMLBUILD],[ocamlbuild],[:])

# Checks for libraries. OCaml-GLPK
AC_MSG_CHECKING(Ocaml-GLPK library)
   glpk_lib=`find $GLPK_PATH -iname glpk.cmxa -nowarn | grep -m 1 -o "glpk.cmxa"`
   if (test "x$glpk_lib" = xglpk.cmxa ); then
      glpk_lib_full=`find $GLPK_PATH -iname glpk.cmxa -nowarn | grep -m 1 "glpk.cmxa"`
      AC_MSG_RESULT(library detected: $glpk_lib_full )
   else	
      AC_MSG_ERROR([OCaml-GLPK library not installed in $GLPK_PATH])
   fi

# Checks for libraries. CSDP
AC_MSG_CHECKING(CSDP library)
   csdp_lib=`find $CSDP_LIBPATH -iname libsdp.a -nowarn | grep -m 1 -o "libsdp.a"`
   if (test "x$csdp_lib" = xlibsdp.a ); then
      csdp_lib_full=`find $CSDP_LIBPATH -iname libsdp.a -nowarn | grep -m 1 "libsdp.a"`
      AC_MSG_RESULT(library detected: $csdp_lib_full )
   else	
      AC_MSG_ERROR([CSDP library not installed in $CSDP_LIBPATH])
   fi
   csdp_inc=`find $CSDP_INCPATH -iname declarations.h -nowarn | grep -m 1 -o "declarations.h"`
   if (!(test "x$csdp_inc" = xdeclarations.h )); then
      AC_MSG_ERROR([CSDP library include not installed in $CSDP_INCPATH])
   fi

AC_DEFUN([AC_DEFINE_DIR], [
  prefix_NONE=
  exec_prefix_NONE=
  test "x$prefix" = xNONE && prefix_NONE=yes && prefix=$ac_default_prefix
  test "x$exec_prefix" = xNONE && exec_prefix_NONE=yes && exec_prefix=$prefix
dnl In Autoconf 2.60, ${datadir} refers to ${datarootdir}, which in turn
dnl refers to ${prefix}.  Thus we have to use `eval' twice.
  eval ac_define_dir="\"[$]$2\""
  eval ac_define_dir="\"$ac_define_dir\""
  AC_SUBST($1, "$ac_define_dir")
  AC_DEFINE_UNQUOTED($1, "$ac_define_dir", [$3])
  test "$prefix_NONE" && prefix=NONE
  test "$exec_prefix_NONE" && exec_prefix=NONE
])

AC_DEFINE_DIR([abs_datadir], [datadir])

# Instanciation
AC_CONFIG_FILES([Makefile
		 myocamlbuild.ml])

AC_OUTPUT


# summary
dnl AC_MSG_NOTICE(******** Configuration ********)
