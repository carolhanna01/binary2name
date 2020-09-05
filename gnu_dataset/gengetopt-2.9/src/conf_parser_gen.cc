/**
 * Copyright (C) 1999, 2000, 2001, 2002  Free Software Foundation, Inc.
 *
 * This file is part of GNU gengetopt 
 *
 * GNU gengetopt is free software; you can redistribute it and/or modify 
 * it under the terms of the GNU General Public License as published by 
 * the Free Software Foundation; either version 2, or (at your option) 
 * any later version. 
 *
 * GNU gengetopt is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of 
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
 * Public License for more details. 
 *
 * You should have received a copy of the GNU General Public License along 
 * with gengetopt; see the file COPYING. If not, write to the Free Software 
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
 */

#include <stdio.h>

#include "conf_parser_gen.h"

#include "ggo_options.h"
#include "my_sstream.h"

#include "argsdef.h"

#include "string_opt_arg.h"
#include "int_opt_arg.h"
#include "long_opt_arg.h"
#include "float_opt_arg.h"
#include "flag_opt_arg.h"

#include "config_parser_update_option.h"

#define SET_GEN_STRUCTURE(gen) (opt->multiple ? gen.set_structure (string (opt->var_arg) + "_new") : gen.set_structure (ARGS_STRUCT));

void
ConfigParserGenerator::generate_handle_single_option(ostream &stream,
                                                     unsigned int indent)
{
  const string argstr ("farg");
  struct gengetopt_option * opt;
  string_opt_arg_gen_class str_gen;
  str_gen.set_optarg (argstr);
  int_opt_arg_gen_class int_gen;
  int_gen.set_optarg (argstr);
  long_opt_arg_gen_class long_gen;
  long_gen.set_optarg (argstr);
  float_opt_arg_gen_class float_gen;
  float_gen.set_optarg (argstr);
  flag_opt_arg_gen_class flag_gen;

  foropt
    {
      ostringstream opt_stream;
      ostringstream check_stream;
      require_check = true;

      switch (opt->type) 
        {
        case ARG_NO:
          require_check = false;
          break;
        case ARG_FLAG:
          require_check = false;
          flag_gen.set_opt_var (opt->var_arg);
          flag_gen.generate_flag_opt_arg (opt_stream, indent);
          break;
        case ARG_STRING:
          str_gen.set_opt_var (opt->var_arg);
          SET_GEN_STRUCTURE(str_gen);
          str_gen.generate_string_opt_arg (opt_stream, indent);
          break;
        case ARG_INT:
          int_gen.set_cast ("");
          int_gen.set_opt_var (opt->var_arg);
          SET_GEN_STRUCTURE(int_gen);
          int_gen.generate_int_opt_arg (opt_stream, indent);
          break;
        case ARG_SHORT:
          int_gen.set_cast ("(short)");
          int_gen.set_opt_var (opt->var_arg);
          SET_GEN_STRUCTURE(int_gen);
          int_gen.generate_int_opt_arg (opt_stream, indent);
          break;
        case ARG_LONG:
          long_gen.set_cast ("");
          long_gen.set_opt_var (opt->var_arg);
          SET_GEN_STRUCTURE(long_gen);
          long_gen.generate_long_opt_arg (opt_stream, indent);
          break;
        case ARG_FLOAT:
          float_gen.set_cast ("(float)");
          float_gen.set_opt_var (opt->var_arg);
          SET_GEN_STRUCTURE(float_gen);
          float_gen.generate_float_opt_arg (opt_stream, indent);
          break;
        case ARG_DOUBLE:
          float_gen.set_cast ("");
          float_gen.set_opt_var (opt->var_arg);
          SET_GEN_STRUCTURE(float_gen);
          float_gen.generate_float_opt_arg (opt_stream, indent);
          break;
        case ARG_LONGDOUBLE:
          float_gen.set_cast ("(long double)");
          float_gen.set_opt_var (opt->var_arg);
          SET_GEN_STRUCTURE(float_gen);
          float_gen.generate_float_opt_arg (opt_stream, indent);
          break;
        case ARG_LONGLONG:
          long_gen.set_cast ("(long long)");
          long_gen.set_opt_var (opt->var_arg);
          SET_GEN_STRUCTURE(long_gen);
          long_gen.generate_long_opt_arg (opt_stream, indent);
          break;
        default:
          fprintf (stderr, "gengetopt: bug found in %s:%d\n", __FILE__,
                   __LINE__);
          abort ();
        }

      set_option_name (opt->var_arg);
      set_long_option (opt->long_opt);
      update_opt_string = opt_stream.str ();
      generate_config_parser_handle_option (stream, indent);
    }
}

void
ConfigParserGenerator::generate_handle_configoption(ostream &stream,
                                                    unsigned int indent)
{
  if (require_check)
    {
      config_parser_update_option_gen_class update_opt_gen;
      update_opt_gen.set_gen_exit (gen_exit);
      update_opt_gen.set_update_arg (update_opt_string);
      update_opt_gen.generate_config_parser_update_option (stream, indent);
    }
  else
    stream << update_opt_string;
}
