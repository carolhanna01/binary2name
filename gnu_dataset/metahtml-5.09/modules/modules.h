/* modules.h: -*- C -*-  DESCRIPTIVE TEXT. */

/*  Copyright (c) 1998 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Sat Jun 27 11:25:42 1998.  */

#if !defined (_MODULES_H_)
#define _MODULES_H_

#include "language.h"

#if defined (__cplusplus)
extern "C" {
#endif

#define MODULE_INITIALIZE(packstring, ftab)				\
void									\
module_initialize (void)						\
{									\
  static int called = 0;						\
  if (!called)								\
    {									\
      register int i;							\
      Symbol *sym, *funcnames;						\
      char symname[256];						\
									\
      called++;								\
      sprintf (symname, "modules::syms-of-%s", packstring);		\
      funcnames = symbol_intern (symname);				\
									\
      /* Install the names and pointers. */				\
      for (i = 0; ftab[i].tag != (char *)NULL; i++)			\
	{								\
	  sym = symbol_intern_in_package				\
		(mhtml_function_package, ftab[i].tag);			\
	  symbol_add_value (funcnames, ftab[i].tag);			\
	  sym->type = symtype_FUNCTION;					\
	  sym->values = (char **)(&ftab[i]);				\
	}								\
    }									\
}

extern void module_initialize (void);

#if !defined (__cplusplus) && !defined (Solaris)
void _init (void) { module_initialize (); }
#endif

#if defined (__CYGWIN32__)
#  include <cygwin32/cygwin_dll.h>
int WINAPI dllEntry(HANDLE hDll, DWORD reason, LPVOID reserved)
{
  return (TRUE);
}
#endif /* __CYGWIN32__ */

#if defined (__cplusplus)
}
#endif

#endif /* MODULES_H */
