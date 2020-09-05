/* test_all_opts.c test */

/* test all kinds of options */

#include <stdlib.h>
#include <stdio.h>

#include "test_all_opts_cmd.h"

static struct gengetopt_args_info args_info;

int
main (int argc, char **argv)
{  
  if (test_all_opts_cmd_parser (argc, argv, &args_info) != 0)
    exit(1) ;

  return 0;
}
