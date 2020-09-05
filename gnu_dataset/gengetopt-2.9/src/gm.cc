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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>

#include <fstream>

extern "C"
{
#include "argsdef.h"
#include "global_opts.h"
};

#include "ggo_options.h"

#include "gm.h"
#include "my_sstream.h"

#include "string_opt_arg.h"
#include "int_opt_arg.h"
#include "long_opt_arg.h"
#include "float_opt_arg.h"
#include "flag_opt_arg.h"
#include "required_option.h"
#include "generic_option.h"
#include "group_option.h"
#include "group_counter.h"
#include "generic_option_group.h"
#include "handle_help.h"
#include "handle_version.h"
#include "no_short_option.h"
#include "handle_unamed.h"
#include "print_group.h"
#include "config_parser_header.h"
#include "multiple_opt_list.h"
#include "multiple_option.h"
#include "multiple_fill_array.h"

#include "conf_parser_gen.h"

using std::ofstream;
using std::endl;
using std::string;

extern char * gengetopt_package;
extern char * gengetopt_version;
extern char * gengetopt_purpose;
extern int gengetopt_strdup_text_length;
extern char *gengetopt_strdup_text[];

extern groups_collection_t gengetopt_groups;

static char *create_filename (char *name, char *ext);
static ofstream *open_fstream (char *filename);
static char *canonize_names(const char * name);
static const string strip_path(const string &);
static const string to_upper(const string &);

char *
create_filename (char *name, char *ext)
{
  char *filename ;

  filename = (char *) malloc (strlen (name) + strlen (ext) + 2);
  /* 2 = 1 for the . and one for the '\0' */
  if (! filename)
    {
      fprintf (stderr, "Error in memory allocation! %s %d\n",
               __FILE__, __LINE__);
      abort ();
    }

  sprintf (filename, "%s.%s", name, ext);

  return filename ;
}

ofstream *
open_fstream (char *filename)
{
  ofstream *fstream = new ofstream (filename);
  
  if ( ! (*fstream) )
    {
      fprintf( stderr, "Error creating %s\n", filename ) ;
      abort() ;
    }

  return fstream;
}

CmdlineParserCreator::CmdlineParserCreator (char *function_name, 
                                            bool unamed_options_,
                                            char *filename_, 
                                            char *header_ext, char *c_ext,
                                            bool long_help_, 
                                            bool no_handle_help_,
                                            bool no_handle_version_,
                                            bool no_handle_error_,
                                            bool conf_parser_,
                                            const string &comment_) :
  filename (filename_), comment (comment_),
  unamed_options (unamed_options_), 
  long_help (long_help_), no_handle_help (no_handle_help_),
  no_handle_version (no_handle_version_), no_handle_error (no_handle_error_),
  conf_parser (conf_parser_),
  tab_indentation (0)
{
  parser_function_name = canonize_names (function_name);
  c_filename = create_filename (filename, c_ext);
  header_filename = create_filename (filename, header_ext);

  // header_gen_class
  const string header_file_name = strip_path (filename);
  set_header_file_name (header_file_name);
  header_gen_class::set_generator_version (VERSION);
  const string ifndefname = to_upper (strip_path (header_file_name));
  set_ifndefname (canonize_names (ifndefname.c_str ()));
  header_gen_class::set_parser_name (parser_function_name);
  if (gengetopt_package)
    set_if_package_notdefined (gengetopt_package);
  if (gengetopt_version)
    set_if_version_notdefined (gengetopt_version);

  if (conf_parser)
    {
      // the config file parser prototype
      ostringstream conf_parser_proto;
      config_parser_header_gen_class cf_gen;
      cf_gen.set_parser_name (parser_function_name);
      conf_parser_proto << endl;
      cf_gen.generate_config_parser_header (conf_parser_proto);
      conf_parser_proto << endl;
      set_config_parser_proto (conf_parser_proto.str ());
    }

  // c_source_gen_class
  set_command_line (comment);
  c_source_gen_class::set_generator_version (VERSION);
  c_source_gen_class::set_parser_name (parser_function_name);
  set_source_name (filename);

  set_gen_exit (no_handle_error ? "return" : "exit");
}

void
CmdlineParserCreator::generateBreak(ostream &stream, unsigned int indent)
{
  string indent_str (indent, ' ');
  
  stream << endl;
  stream << indent_str;
  stream << "break;";
}

void
CmdlineParserCreator::do_update_arg (struct gengetopt_option *opt,
                                     ostream &stream,
                                     unsigned int indent) 
{
  const string argstr ("optarg");
  string_opt_arg_gen_class str_gen;
  str_gen.set_optarg (argstr);
  int_opt_arg_gen_class int_gen;
  int_gen.set_optarg (argstr);
  long_opt_arg_gen_class long_gen;
  long_gen.set_optarg (argstr);
  float_opt_arg_gen_class float_gen;
  float_gen.set_optarg (argstr);
  flag_opt_arg_gen_class flag_gen;

  if (opt->multiple)
    {
      int_gen.set_structure (string (opt->var_arg) + "_new");
      str_gen.set_structure (string (opt->var_arg) + "_new");
      long_gen.set_structure (string (opt->var_arg) + "_new");
      float_gen.set_structure (string (opt->var_arg) + "_new");
    }
  else
    {
      int_gen.set_structure (ARGS_STRUCT);
      str_gen.set_structure (ARGS_STRUCT);
      long_gen.set_structure (ARGS_STRUCT);
      float_gen.set_structure (ARGS_STRUCT);
    }

  switch (opt->type)
    {
    case ARG_NO:
      stream << "break;";
      break;
    case ARG_FLAG:
      flag_gen.set_opt_var (opt->var_arg);
      flag_gen.generate_flag_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    case ARG_STRING:
      str_gen.set_opt_var (opt->var_arg);
      str_gen.generate_string_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    case ARG_INT:
      int_gen.set_cast ("");
      int_gen.set_opt_var (opt->var_arg);
      int_gen.generate_int_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    case ARG_SHORT:
      int_gen.set_cast ("(short)");
      int_gen.set_opt_var (opt->var_arg);
      int_gen.generate_int_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    case ARG_LONG:
      long_gen.set_cast ("");
      long_gen.set_opt_var (opt->var_arg);
      long_gen.generate_long_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    case ARG_FLOAT:
      float_gen.set_cast ("(float)");
      float_gen.set_opt_var (opt->var_arg);
      float_gen.generate_float_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    case ARG_DOUBLE:
      float_gen.set_cast ("");
      float_gen.set_opt_var (opt->var_arg);
      float_gen.generate_float_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    case ARG_LONGDOUBLE:
      float_gen.set_cast ("(long double)");
      float_gen.set_opt_var (opt->var_arg);
      float_gen.generate_float_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    case ARG_LONGLONG:
      long_gen.set_cast ("(long long)");
      long_gen.set_opt_var (opt->var_arg);
      long_gen.generate_long_opt_arg (stream, indent);
      generateBreak(stream, indent);
      break;
    default:
      fprintf (stderr, "gengetopt: bug found in %s:%d\n", __FILE__,
               __LINE__);
      abort ();
    }
}

int
CmdlineParserCreator::generate ()
{
  int head_result;

  head_result = generate_header_file ();
  if (head_result)
    return head_result;

  return generate_source ();
}

int
CmdlineParserCreator::generate_header_file ()
{
  if (gengetopt_options == 0) 
    {
      fprintf (stderr, "gengetopt: none option given\n");
      return 1;
    }

  /* ****************************************************** */
  /* HEADER FILE******************************************* */
  /* ****************************************************** */

  ofstream *output_file = open_fstream (header_filename);
  generate_header (*output_file);
  output_file->close ();
  delete output_file;

  return 0;
}

void 
CmdlineParserCreator::generate_option_arg(ostream &stream, 
                                          unsigned int indent)
{
  struct gengetopt_option * opt;
  string indent_str (indent, ' ');
  bool first = true;

  foropt
    if (opt->type != ARG_NO) {
      switch (opt->type) {
      case ARG_FLAG:
      case ARG_STRING:
      case ARG_INT:
      case ARG_SHORT:
      case ARG_LONG:
      case ARG_FLOAT:
      case ARG_DOUBLE:
      case ARG_LONGDOUBLE:
      case ARG_LONGLONG:
        if (! first)
          stream << indent_str;
        else
          first = false;

        stream << arg_types[opt->type];
	if (opt->multiple)
	  stream << "* ";
	else
	  stream << " ";
        break;
      default: fprintf (stderr, "gengetopt: bug found in %s:%d!!\n",
                        __FILE__, __LINE__);
        abort ();
      }
      
      if (opt->type == ARG_FLAG)
        stream << opt->var_arg << "_flag";
      else
        stream << opt->var_arg << "_arg";
      
      stream << ";\t/* " << opt->desc;
      
      if (opt->default_given)
        {
          stream << " (default='" << opt->default_string << "')";
        }
      
      if (opt->type == ARG_FLAG)
        {
          if (opt->flagstat)
            stream << " (default=on)";
          else
            stream << " (default=off)";
        }
      
      stream << ".  */\n";
    }
}

void 
CmdlineParserCreator::generate_option_given(ostream &stream, 
                                            unsigned int indent)
{
  struct gengetopt_option * opt;
  string indent_str (indent, ' ');
  bool first = true;

  foropt
    {
      switch (opt->type) {
      case ARG_NO:
      case ARG_FLAG:
      case ARG_STRING:
      case ARG_INT:
      case ARG_SHORT:
      case ARG_LONG:
      case ARG_FLOAT:
      case ARG_DOUBLE:
      case ARG_LONGDOUBLE:
      case ARG_LONGLONG: break;
      default:
        fprintf (stderr, "gengetopt: bug found in %s:%d!!\n",
                 __FILE__, __LINE__);
        abort ();
      }
      if (! first)
        stream << indent_str;
      else
        first = false;

      stream << "int " << opt->var_arg << 
        "_given ;\t/* Whether " << opt->long_opt << " was given.  */\n";
    }

  if (unamed_options)
    {
      stream << endl;
      stream << indent_str;
      stream << "char **inputs ; /* unamed options */\n" ;
      stream << indent_str;
      stream << "unsigned inputs_num ; /* unamed options number */" ;
    }
}

void 
CmdlineParserCreator::generate_print_purpose(ostream &stream, 
                                             unsigned int indent)
{
  if (gengetopt_purpose != NULL) 
    {
      string indent_str (indent, ' ');

      char *ptr;
      ptr = gengetopt_purpose;
      stream << indent_str;
      stream << "\"Purpose:\\n\"";
      stream << endl;
      stream << indent_str;
      stream << "\"  ";
      for (; *ptr!='\0'; ptr++) 
        {
          if (*ptr == '\n') 
            {
              stream << "\\n\"";
              stream << endl;
              stream << indent_str;
              stream << "\"  ";
            } else {
              stream << *ptr;
            }
        }
      stream << "\\n\""; // close the last line
      stream << endl;
      stream << indent_str;
      stream << "\"\\n\"\n"; // generate the last \n
    }
}

string
CmdlineParserCreator::generate_usage_string()
{
  struct gengetopt_option * opt;
  ostringstream usage;

  usage << "Usage: %s ";

  if ( long_help )
    {
      foropt
	if (opt->required) /* required options */
		switch (opt->type) {
		case ARG_INT:
		case ARG_SHORT:
		case ARG_LONG:
		case ARG_FLOAT:
		case ARG_DOUBLE:
		case ARG_LONGDOUBLE:
		case ARG_LONGLONG:
		case ARG_STRING: 
        	if (opt->short_opt)
                  {
                    usage << "-" << opt->short_opt 
                          << arg_names[opt->type] << "|";
                  }
                usage << "--" << opt->long_opt << "=" << 
                  arg_names[opt->type] << " ";
                break;
		default: 
                  fprintf (stderr, "gengetopt: bug found in %s:%d!!\n",
                           __FILE__, __LINE__);
                  abort ();
		}
      foropt
	if (!opt->required)
          switch (opt->type) {
          case ARG_NO:
          case ARG_FLAG: 
            usage << "[";
            if (opt->short_opt)
              {
                usage << "-" << opt->short_opt << "|";
              }
            usage << "--" << opt->long_opt << "] ";
            break;
          case ARG_INT:
          case ARG_SHORT:
          case ARG_LONG:
          case ARG_FLOAT:
          case ARG_DOUBLE:
          case ARG_LONGDOUBLE:
          case ARG_LONGLONG:
          case ARG_STRING: 
            if (opt->short_opt)
              {
                usage << "-" << opt->short_opt 
                      << arg_names[opt->type] << "|";
              }
            usage << "--" << opt->long_opt << "=" << 
              arg_names[opt->type] << " ";
            break;
          default: fprintf (stderr, "gengetopt: bug found in %s:%d!!\n",
                            __FILE__, __LINE__);
            abort ();
          }
    } 
  else 
    { /* if not long help we generate it as GNU standards */
      usage << "[OPTIONS]...";
    }

  if ( unamed_options )
    usage << " [FILES]...";

  usage << "\\n";

  return usage.str ();
}

void 
CmdlineParserCreator::generate_help_option_print(ostream &stream, 
                                                 unsigned int indent)
{
  long max_long, max_short, w;
  struct gengetopt_option * opt;
  string indent_str (indent, ' ');

  /* calculate columns */
  max_long = max_short = 0;
  foropt {
    w = 3 + strlen (opt->long_opt);
    if (opt->type == ARG_FLAG || opt->type == ARG_NO)
      {
        if (w > max_long) max_long = w;
        if (2 > max_short) max_short = 2;
      }
    else
      {
        w += strlen (arg_names[opt->type]);
        if (w > max_long) max_long = w;
        w = (3 + strlen (arg_names[opt->type]));
        if (w > max_short) max_short = w;
      }
  }
  /* print justified options */
  char *prev_group = 0;
  print_group_gen_class print_group;

  foropt
    {
      if (opt->group_value &&
          (! prev_group || strcmp (opt->group_value, prev_group) != 0))
        {
          stream << indent_str;
          print_group.set_group_name (opt->group_value);
          print_group.generate_print_group (stream, indent);
          stream << endl;

          prev_group = opt->group_value;
        }

      stream << indent_str;
      stream << "printf(\"";
      if (opt->type == ARG_FLAG || opt->type == ARG_NO)
        {
          if (opt->short_opt) 
            stream << "   -" << opt->short_opt;
          else
            stream << "     ";
          for (w = 2; w < max_short; w++) 
            stream << " ";
          stream << "  --" << opt->long_opt;
          for (w = 2+strlen(opt->long_opt); w < max_long; w++)
            stream << " ";
          stream << "  " << opt->desc;
          if (opt->type == ARG_FLAG)
            {
              if (opt->flagstat)
                stream << " (default=on)";
              else
                stream << " (default=off)";
            }
          stream << "\\n\");\n";
        }
      else
        {
          if (opt->short_opt)
            stream << "   -" << opt->short_opt << arg_names[opt->type];
          else
            {
              int type_len = strlen(arg_names[opt->type]);
              
              stream << "      ";
              for (w = 1; w < type_len; w++) 
                stream << " ";
            }
          for (w = 2+strlen(arg_names[opt->type]); w < max_short; w++)
            stream << " ";
          stream << "  --" << opt->long_opt << "=" << arg_names[opt->type];
          for (w = 3+strlen(opt->long_opt)+
                 strlen(arg_names[opt->type]); w < max_long; w++)
            stream << " ";
          stream << "  " << opt->desc;
          if (opt->default_given)
            {
              stream << " (default='" << opt->default_string << "')";
            }
          stream << "\\n\");\n";
        }
    }
}

void
CmdlineParserCreator::generate_strdup(ostream &stream, unsigned int indent)
{
  struct gengetopt_option * opt;
  bool gen_strdup = unamed_options;

  if (! gen_strdup)
    {
      foropt
	if (opt->type == ARG_STRING) {
          gen_strdup = 1;
          break;
	}
    }

  stream << endl;
  if (gen_strdup) 
    {
      stream << "#ifndef HAVE_STRDUP\n";
      for (int i = 1; i <= gengetopt_strdup_text_length; ++i)
        stream << gengetopt_strdup_text[i] << endl;
      stream << "#endif /* HAVE_STRDUP */\n";
    }
}

void 
CmdlineParserCreator::generate_given_init(ostream &stream, 
                                          unsigned int indent)
{
  struct gengetopt_option * opt;
  string indent_str (indent, ' ');

  /* now we initialize "given" fields */
  foropt
    {
      stream << indent_str;
      stream << ARGS_STRUCT << "->" << opt->var_arg << "_given = 0 ;";
      stream << endl;
    }
}

void
CmdlineParserCreator::generate_struct_def(ostream &stream, unsigned int indent)
{
  struct gengetopt_option * opt;
  string indent_str (indent, ' ');
  multiple_opt_list_gen_class multiple_opt;
  bool generated_counter = false;

  /* define linked-list structs for multiple options */
  foropt
    {
      if (opt->multiple)
	{
          if (! generated_counter)
            {
              stream << indent_str;
              stream << "int i;        /* Counter */" << endl;
              generated_counter = true;
            }
          stream << indent_str;
	  multiple_opt.set_type (arg_types[opt->type]);
	  multiple_opt.set_arg_name (opt->var_arg);
	  multiple_opt.generate_multiple_opt_list (stream, indent);
          stream << endl;
	}
    }
}

void
CmdlineParserCreator::generate_multiple_fill_array(ostream &stream, unsigned int indent)
{
  struct gengetopt_option * opt;
  string indent_str (indent, ' ');
  multiple_fill_array_gen_class filler;

  /* copy linked list into the array */
  foropt
    {
      if (opt->multiple)
	{
	  stream << indent_str;
	  filler.set_type (arg_types[opt->type]);
	  filler.set_option_var_name (opt->var_arg);
	  filler.generate_multiple_fill_array (stream, indent);
          stream << endl;
	}
    }
}

void
CmdlineParserCreator::generate_clear_arg(ostream &stream, unsigned int indent)
{
  struct gengetopt_option * opt;
  string indent_str (indent, ' ');

  /* now we initialize "given" fields */
  foropt
    {
      if (opt->type == ARG_STRING)
        {
          stream << indent_str;

          if (opt->default_given)
            stream << ARGS_STRUCT << "->" << opt->var_arg 
                   << "_arg = strdup(\"" << opt->default_string 
                   << "\") ;\\";
          else
            stream << ARGS_STRUCT << "->" << opt->var_arg 
                   << "_arg = NULL; \\";

          stream << endl;
        }
      else if (opt->type == ARG_FLAG)
        {
          stream << indent_str;

          stream << ARGS_STRUCT << "->" << opt->var_arg << "_flag";
          stream << " = " << opt->flagstat << ";\\";

          stream << endl;
        }
      else if (opt->type != ARG_NO && opt->default_given)
        {
          stream << indent_str;

          stream << ARGS_STRUCT << "->" << opt->var_arg << "_arg";
          stream << " = " << opt->default_string << " ;\\";

          stream << endl;
        }
    }
}

void
CmdlineParserCreator::generate_init_unamed(ostream &stream,
                                           unsigned int indent)
{
  if ( unamed_options )
    {
      string indent_str (indent, ' ');

      stream << endl;
      stream << indent_str;
      stream << ARGS_STRUCT << "->inputs = NULL;\n";
      stream << indent_str;
      stream << ARGS_STRUCT << "->inputs_num = 0;\n";
    }
}

void
CmdlineParserCreator::generate_long_option_struct(ostream &stream, 
                                                  unsigned int indent)
{
  string indent_str (indent, ' ');
  struct gengetopt_option * opt;

  foropt
    {
      if (opt->short_opt == 'h' || opt->short_opt == 'V')
        continue;

      stream << indent_str;
      
      stream << "{ \"" << opt->long_opt << "\",\t" 
             << (opt->type == ARG_NO || opt->type == ARG_FLAG ? 0 : 1) 
             << ", NULL, ";

      if (opt->short_opt)
        stream << "\'" << opt->short_opt << "\'";
      else 
        stream << "0";

      stream << " }," << endl;
    }
}

string
CmdlineParserCreator::generate_getopt_string()
{
  struct gengetopt_option * opt;
  ostringstream getopt_string;

  foropt
    if (opt->short_opt)
      getopt_string << opt->short_opt <<
        (opt->type == ARG_NO || opt->type == ARG_FLAG ? "" : ":");

  return getopt_string.str ();
}

void
CmdlineParserCreator::generate_handle_help(ostream &stream, 
                                           unsigned int indent)
{
 if (no_handle_help)
   {
     generic_option_gen_class help_gen;
     help_gen.set_long_option (HELP_LONG_OPT);
     help_gen.set_short_option (HELP_SHORT_OPT_STR);
     help_gen.set_option_comment (HELP_OPT_DESCR);
     help_gen.set_option_var_name (HELP_LONG_OPT);
     help_gen.set_gen_exit ("return");

     help_gen.generate_generic_option (stream, indent);
     
     string indent_str (indent, ' ');
     stream << "return 0;";
   }
 else
   {
     handle_help_gen_class help_gen;
     help_gen.set_parser_name (parser_function_name);
     help_gen.generate_handle_help (stream, indent);
   }
}

void
CmdlineParserCreator::generate_handle_version(ostream &stream, 
                                              unsigned int indent)
{
 if (no_handle_version)
   {
     generic_option_gen_class version_gen;
     version_gen.set_long_option (VERSION_LONG_OPT);
     version_gen.set_short_option (VERSION_SHORT_OPT_STR);
     version_gen.set_option_comment (VERSION_OPT_DESCR);
     version_gen.set_option_var_name (VERSION_LONG_OPT);
     version_gen.set_gen_exit ("return");

     version_gen.generate_generic_option (stream, indent);
     
     string indent_str (indent, ' ');
     stream << "return 0;";
   }
 else
   {
     handle_version_gen_class version_gen;
     version_gen.set_parser_name (parser_function_name);
     version_gen.generate_handle_version (stream, indent);
   }
}

void
CmdlineParserCreator::generate_handle_no_short_option(ostream &stream, 
                                                      unsigned int indent)
{
  struct gengetopt_option * opt;
  no_short_option_gen_class opt_gen;
  string indent_str (indent, ' ');
  opt_gen.set_gen_exit (no_handle_error ? "return" : "exit");
  bool first = true;

  foropt
    if (! opt->short_opt)
      {
        stream << indent_str;
        ostringstream str_stream;

        opt_gen.set_option_comment (opt->desc);
        opt_gen.set_long_option (opt->long_opt);
        opt_gen.set_option_var_name (opt->var_arg);

        do_update_arg (opt, str_stream, indent + 2);
        opt_gen.set_update_arg (str_stream.str ());

        generic_option_group_gen_class group_gen;
        if (opt->group_value)
          {
            str_stream.str ("");
            str_stream << " ";
            group_gen.set_group_var_name (opt->group_value);
            group_gen.generate_generic_option_group (str_stream, indent);
            opt_gen.set_update_group_count (str_stream.str ());
          }

        opt_gen.generate_no_short_option (stream, indent);

        stream << endl;

        if (first)
          {
            first = false;
            opt_gen.set_gen_else ("else ");
          }
      }

  stream << endl;
}

void
CmdlineParserCreator::generate_handle_option(ostream &stream, 
                                             unsigned int indent)
{
  struct gengetopt_option * opt;
  generic_option_gen_class option_gen;
  multiple_option_gen_class multi_gen;
  string indent_str (indent, ' ');

  if (no_handle_error)
    option_gen.set_gen_exit ("return");
  else
    option_gen.set_gen_exit ("exit");

  foropt 
    {
      if (opt->short_opt)
        {
          if (opt->short_opt == 'h' || opt->short_opt == 'V')
            continue;

          stream << indent_str;

          string short_opt (1, opt->short_opt);
	  if (opt->multiple)
	    {
	      multi_gen.set_short_option (short_opt);
	      multi_gen.set_option_comment (opt->desc);
	      multi_gen.set_option_var_name (opt->var_arg);

	      multi_gen.generate_multiple_option (stream, indent);
	    }
	  else
	    {
	      option_gen.set_short_option (short_opt);
	      option_gen.set_option_comment (opt->desc);
	      option_gen.set_long_option (opt->long_opt);
	      option_gen.set_option_var_name (opt->var_arg);

	      option_gen.generate_generic_option (stream, indent);
	    }
          generic_option_group_gen_class group_gen;
          if (opt->group_value)
            {
              group_gen.set_group_var_name (opt->group_value);
              group_gen.generate_generic_option_group (stream, indent);
            }

          do_update_arg (opt, stream, indent + 2);

          stream << endl;
          stream << endl;
        }
    }
}

#define GROUP_REQUIRED_COMPARISON "!="
#define GROUP_NOT_REQUIRED_COMPARISON ">"
#define GROUP_REQUIRED_MESSAGE "One"
#define GROUP_NOT_REQUIRED_MESSAGE "At most one"

void
CmdlineParserCreator::generate_handle_group(ostream &stream,
                                            unsigned int indent)
{
  group_option_gen_class opt_gen;
  string indent_str (indent, ' ');

  groups_collection_t::const_iterator end = gengetopt_groups.end();
  for ( groups_collection_t::const_iterator idx = gengetopt_groups.begin();
        idx != end; ++idx)
    {
      stream << indent_str;
      opt_gen.set_group_var_name (idx->first);
      if (idx->second)
        {
          opt_gen.set_Comparison_rule(GROUP_REQUIRED_COMPARISON);
          opt_gen.set_number_required(GROUP_REQUIRED_MESSAGE);
        }
      else
        {
          opt_gen.set_Comparison_rule(GROUP_NOT_REQUIRED_COMPARISON);
          opt_gen.set_number_required(GROUP_NOT_REQUIRED_MESSAGE);
        }

      opt_gen.generate_group_option (stream, indent);
      stream << endl;
    }
}

void
CmdlineParserCreator::generate_handle_required(ostream &stream,
                                               unsigned int indent)
{
  struct gengetopt_option * opt;
  required_option_gen_class opt_gen;
  string indent_str (indent, ' ');

  /* write test for required options */
  foropt
    if ( opt->required )
      {
        stream << indent_str;

        ostringstream req_opt;
        req_opt << "'--" << opt->long_opt << "'";
        if (opt->short_opt)
          req_opt << " ('-" << opt->short_opt << "')";

        opt_gen.set_option_var_name (opt->var_arg);
        opt_gen.set_option_descr (req_opt.str ());

        opt_gen.generate_required_option (stream, indent);

        stream << endl;
      }
}

void
CmdlineParserCreator::generate_group_counters(ostream &stream,
                                              unsigned int indent)
{
  group_counter_gen_class counter_gen;
  string indent_str (indent, ' ');

  groups_collection_t::const_iterator end = gengetopt_groups.end();
  for ( groups_collection_t::const_iterator idx = gengetopt_groups.begin();
        idx != end; ++idx)
    {
      stream << indent_str;
      counter_gen.set_group_name (idx->first);
      counter_gen.generate_group_counter (stream, indent);
      stream << endl;
    }
}

int
CmdlineParserCreator::generate_source ()
{
  /* ****************************************************** */
  /* ********************************************** C FILE  */
  /* ****************************************************** */

  set_usage_string (generate_usage_string ());
  set_getopt_string (generate_getopt_string ());

  ofstream *output_file = open_fstream (c_filename);
  generate_c_source (*output_file);
  output_file->close ();
  delete output_file;

  return 0;
}

void
CmdlineParserCreator::generate_handle_unamed(ostream &stream,
                                             unsigned int indent)
{
  if (! unamed_options)
    return;

  stream << string (indent, ' ');
  handle_unamed_gen_class unamed_gen;
  unamed_gen.generate_handle_unamed (stream, indent);
  stream << endl;
}

void
CmdlineParserCreator::generate_conf_parser(ostream &stream,
                                           unsigned int indent)
{
  if (! conf_parser)
    return;

  stream << endl;
  ConfigParserGenerator conf_parser_gen;
  conf_parser_gen.set_parser_name (c_source_gen_class::parser_name);
  conf_parser_gen.set_gen_exit (gen_exit);
  conf_parser_gen.generate_config_parser_source (stream);
}

/*
  return a copy of the string passed after canonizing it (i.e. '-' and
  '.' are transformed in '_'.
*/
char *
canonize_names (const char *name)
{
  char *pvar;
  char *p;

  pvar = strdup (name);

  for (p = pvar; *p; ++p)
    if (*p == '.' || *p == '-') *p = '_';

  return pvar;
}

// remove the path from the file name
const string
strip_path(const string &s)
{
  string::size_type pos_of_sep;

  pos_of_sep = s.rfind("/");
  if (pos_of_sep == string::npos)
    pos_of_sep = s.rfind("\\"); // try also with DOS path sep

  if (pos_of_sep == string::npos)
    return s; // no path

  return s.substr (pos_of_sep + 1);
}

const string
to_upper(const string &old)
{
  string upper = old;

  for (string::iterator s = upper.begin ();
       s != upper.end (); ++s)
    *s = toupper (*s);

  return upper;
}

