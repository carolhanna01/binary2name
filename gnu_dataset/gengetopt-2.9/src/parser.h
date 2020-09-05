#ifndef BISON_PARSER_H
# define BISON_PARSER_H

#ifndef YYSTYPE
typedef union {
char * str;
char chr;
int argtype;
int boolean;
} yystype;
# define YYSTYPE yystype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	TOK_PACKAGE	257
# define	TOK_VERSION	258
# define	TOK_OPTION	259
# define	TOK_DEFGROUP	260
# define	TOK_GROUPOPTION	261
# define	TOK_YES	262
# define	TOK_NO	263
# define	TOK_FLAG	264
# define	TOK_PURPOSE	265
# define	TOK_ONOFF	266
# define	TOK_STRING	267
# define	TOK_DEFAULT	268
# define	TOK_GROUP	269
# define	TOK_MLSTRING	270
# define	TOK_CHAR	271
# define	TOK_ARGTYPE	272
# define	TOK_MULTIPLE	273


extern YYSTYPE yylval;

#endif /* not BISON_PARSER_H */
