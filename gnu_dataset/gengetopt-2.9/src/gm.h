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

#ifndef _GM_H
#define _GM_H

#include "my_string.h"
#include "my_map.h"

#include "header.h"
#include "c_source.h"

#define TAB_LEN 2

typedef map<string,bool> groups_collection_t;

struct gengetopt_option;

class CmdlineParserCreator : public header_gen_class, public c_source_gen_class
{
 protected:
  char *parser_function_name;
  char *filename;
  char *header_filename;
  char *c_filename;
  string comment;

  bool unamed_options;
  bool long_help;
  bool no_handle_help;
  bool no_handle_version;
  bool no_handle_error;
  bool conf_parser;

  unsigned int tab_indentation ; /* tab indentation level */

  void inc_indent()
    {
      tab_indentation += TAB_LEN ;
    }
 
  void dec_indent()
    {
      tab_indentation -= TAB_LEN ;
    }
 
  void indent()
    {
      unsigned int i ;
      
      for ( i = 1 ; i <= tab_indentation ; ++i )
        printf (" ");
    }

  void do_update_arg (struct gengetopt_option *opt, 
                      ostream &stream, unsigned int indent);

  int generate_header_file();
  int generate_source();

  string generate_usage_string();
  string generate_getopt_string();

  // to be implemented in header_gen_class
  virtual void generate_option_arg(ostream &stream, unsigned int indent);
  virtual void generate_option_given(ostream &stream, unsigned int indent);

  // to be implemented in c_source_gen_class
  virtual void generate_print_purpose(ostream &stream, unsigned int indent);
  virtual void generate_clear_arg(ostream &stream, unsigned int indent);
  virtual void generate_given_init(ostream &stream, unsigned int indent);
  virtual void generate_struct_def(ostream &stream, unsigned int indent);
  virtual void generate_multiple_fill_array(ostream &stream, unsigned int indent);
  virtual void generate_init_unamed(ostream &stream, unsigned int indent);
  virtual void generate_handle_help(ostream &stream, unsigned int indent);
  virtual void generate_handle_no_short_option(ostream &stream, 
                                               unsigned int indent);
  virtual void generate_handle_option(ostream &stream, unsigned int indent);
  virtual void generate_handle_required(ostream &stream, unsigned int indent);
  virtual void generate_handle_group(ostream &stream, unsigned int indent);
  virtual void generate_group_counters(ostream &stream, unsigned int indent);
  virtual void generate_handle_version(ostream &stream, unsigned int indent);
  virtual void generate_help_option_print(ostream &stream, 
                                          unsigned int indent);
  virtual void generate_long_option_struct(ostream &stream, 
                                           unsigned int indent);
  virtual void generate_strdup(ostream &stream, unsigned int indent);
  virtual void generate_handle_unamed(ostream &stream, unsigned int indent);
  virtual void generate_conf_parser(ostream &stream, unsigned int indent);

  void generateBreak(ostream &stream, unsigned int indent);

 public:

  CmdlineParserCreator (char *function_name, bool unamed_options,
                        char *filename, char *header_ext, char *c_ext,
                        bool long_help, bool no_handle_help,
                        bool no_handle_version,
                        bool no_handle_error, bool conf_parser,
                        const string &comment);

  int generate();
};

#endif /* _GM_H */
