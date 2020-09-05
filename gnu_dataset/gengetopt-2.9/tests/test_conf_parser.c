/* test_conf_parser.c test */

/* test all kinds of options and the conf file parser */

#include <stdlib.h>
#include <stdio.h>

#include "test_conf_parser_cmd.h"

static struct gengetopt_args_info args_info;

int
main (int argc, char **argv)
{  
  if (test_conf_parser_cmd_parser (argc, argv, &args_info) != 0)
    exit(1) ;

  if (test_conf_parser_cmd_parser_configfile
      (args_info.conf_file_arg, &args_info, 1) != 0) // override cmd options
    exit(1);

  printf ("value of required: %s\n", args_info.required_arg);
  printf ("value of string: %s\n", args_info.string_arg);
  printf ("value of no-short: %d\n", args_info.no_short_given);
  printf ("value of int: %d\n", args_info.int_arg);
  printf ("value of float: %f\n", args_info.float_arg);

  return 0;
}
