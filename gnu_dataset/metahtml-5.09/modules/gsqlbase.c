/* gsqlbase.c: -*- C -*-  The basis for all SQL DB modules. */

/*  Copyright (c) 1998 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Thu May 28 22:03:49 1998.  */

#define COMPILING_MODULE 1

#include "../libmhtml/gsql.c"

#if defined (__cplusplus)
extern "C"
{
#endif

void
module_initialize (void)
{
  static int called = 0;

  if (!called)
    {
      register int i;
      Symbol *sym, *funcnames;
      char *symsof = "modules::syms-of-database";

#if defined (COMPILING_MYSQLFUNCS)
      symsof = "modules::syms-of-mysql";
#endif

#if defined (COMPILING_MSQLFUNCS)
      symsof = "modules::syms-of-msql";
#endif

#if defined (COMPILING_PGSQLFUNCS)
      symsof = "modules::syms-of-pgsql";
#endif

      called++;
      funcnames = symbol_intern (symsof);

      /* Install the names and pointers. */
      for (i = 0; func_table[i].tag != (char *)NULL; i++)
	{
	  sym = symbol_intern_in_package
	    (mhtml_function_package, func_table[i].tag);
	  symbol_add_value (funcnames, func_table[i].tag);
	  sym->type = symtype_FUNCTION;
	  sym->values = (char **)(&func_table[i]);
	}
    }
}

#if defined (__CYGWIN32__)
#  include <cygwin32/cygwin_dll.h>
int WINAPI dllEntry(HANDLE hDll, DWORD reason, LPVOID reserved)
{
  return (TRUE);
}
#endif /* __CYGWIN32__ */

#if !defined (__cplusplus)
#  if !defined (Solaris)
     void _init (void) { module_initialize (); }
#  endif
#else
}
#endif

