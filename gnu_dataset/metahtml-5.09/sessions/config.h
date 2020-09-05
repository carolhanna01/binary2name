/* config.h: -*- C -*-  DESCRIPTIVE TEXT. */

/*  Copyright (c) 1998 Brian J. Fox
    Author: Brian J. Fox (bfox@ai.mit.edu) Fri Apr 10 16:36:02 1998.  */
#include "../config.h"
#if defined (NO_EXTRAS)
#  undef MHTML_CRYPTOGRAPHY
#  undef MSQL_DATABASE
#  undef MYSQL_DATABASE
#  undef ODBC_DATABASE
#endif /* NO_EXTRAS */
