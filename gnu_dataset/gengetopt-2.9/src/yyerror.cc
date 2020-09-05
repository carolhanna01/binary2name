/*
This file is licensed to you under the license specified in the included file
`LICENSE'. Look there for further details.
*/


/*
  Called by yyparse on error.
 */
#include <stdio.h>

extern int gengetopt_count_line;
extern char * gengetopt_input_filename;

extern int tokenpos;
extern char linebuf[];
extern char *yytext;

void
yyerror (char *s)
{
  const char *source =
    (gengetopt_input_filename ? gengetopt_input_filename : "gengetopt");

  fprintf (stderr, "%s:%d: %s %s\n", source, gengetopt_count_line, s, yytext);
  fprintf (stderr, "%s:%d: %s\n", source, gengetopt_count_line, linebuf);
  fprintf (stderr, "%s:%d: %*s\n", source, gengetopt_count_line,
           tokenpos + 1, "^");
}

// this second version should be used when the line has already been parsed,
// but there's a "semantic" error.  Indeed the line number has to be
// decremented.
void
yyerror2 (char *s)
{
  const char *source =
    (gengetopt_input_filename ? gengetopt_input_filename : "gengetopt");

  fprintf (stderr, "%s:%d: %s\n", source, gengetopt_count_line - 1, s);
}
