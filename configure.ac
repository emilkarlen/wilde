AC_INIT([Wilde Web/DB Library],
	       [0.1],
	       emil@member.fsf.org,
	       [wilde-hs])
AC_CONFIG_AUX_DIR([auxiliary])
AM_INIT_AUTOMAKE([1.8 gnu dist-bzip2])

# Name, version, license.
AC_SUBST(AUTHOR,'Emil Karlen')
AC_SUBST(AUTHOR_EMAIL,'emil@member.fsf.org')
AC_SUBST(LICENSE,'GPL')
AC_SUBST(RELEASE,'1')

# Common programs.
# Av nån anledning tror autoreconf att det finns C-program här.
dnl AC_PROG_CC
AC_PROG_SED
AC_PROG_AWK
AC_PROG_MKDIR_P
AC_PATH_PROGS(BASH, bash sh)
AC_PATH_PROGS(AWK_PATH, gawk awk)
dnl AM_PATH_PYTHON
AC_ARG_VAR(RM,[rm. (Default is `rm'.)])
AC_CHECK_PROGS(RM, rm)

# Haskell

# GHC Haskell Compiler
AC_ARG_VAR(GHC,[GHC Haskell Compiler.])
AC_CHECK_PROGS(GHC, ghc)

# Haddock
AC_ARG_VAR(HADDOCK,[Haddock Haskell Doc Compiler.])
AC_CHECK_PROGS(HADDOCK, haddock)

# Makefiles used in the build/install process to generate
AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([testsuite/tests/Makefile])
AC_CONFIG_FILES([testsuite/testapp/setup/apache/wilde-hs-cgi.conf])

# Produce output.
AC_OUTPUT
