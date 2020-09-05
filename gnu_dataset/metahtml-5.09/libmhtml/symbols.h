/* symbols.h: Structures used to store symbols and values. */

/* Author: Brian J. Fox (bfox@ai.mit.edu) Mon Aug 21 11:31:19 1995.

   This file is part of <Meta-HTML>(tm), a system for the rapid deployment
   of Internet and Intranet applications via the use of the Meta-HTML
   language.

   Copyright (c) 1995, 1996, Brian J. Fox (bfox@ai.mit.edu).
   Copyright (c) 1996, Universal Access Inc. (http://www.ua.com).

   Meta-HTML is free software; you can redistribute it and/or modify
   it under the terms of the UAI Free Software License as published
   by Universal Access Inc.; either version 1, or (at your option) any
   later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   UAI Free Software License for more details.

   You should have received a copy of the UAI Free Software License
   along with this program; if you have not, you may obtain one by
   writing to:

   Universal Access Inc.
   129 El Paseo Court
   Santa Barbara, CA
   93101  */

#if !defined (_SYMBOLS_H_)
#define _SYMBOLS_H_ 1

#define DEFAULT_PACKAGE_NAME "DEFAULT"

#if defined (__cplusplus)
extern "C"
{
#endif

/* What a symbol looks like.
   Note that PRESERVED_NAME is almost always NULL, and can only be used
   when it has been explicity set. */
typedef struct
{
  char *name;			/* The characters of the symbol name. */
  char *preserved_name;		/* The symbol name, with case preserved. */
  int name_len;			/* The length of the symbol name. */
  char **values;		/* The values associated with this symbol. */
  int values_index;		/* Number of values stored within. */
  int values_slots;		/* Number of slots allocated to VALUES. */
  int type;			/* Data typing allows binary data. */
  int *notifier;		/* Address of var to modify when this
				   symbol's value changes. */
  int flags;			/* ReadOnly, etc. */
  void *package;		/* The package that owns this symbol. */
} Symbol;

#define sym_READONLY	  0x01
#define sym_INVISIBLE	  0x02
#define sym_NOEXPAND	  0x04	/* <get-var foo> --> <get-var-once foo> */

/* Here are some data types. */
#define symtype_STRING    0x00	/* Value[x] points directly to data. */
#define symtype_FUNCTION  0x01	/* Value points to a function definition. */
#define symtype_BINARY    0x02	/* Value points to a datablock. */
#define symtype_USERFUN	  0x03	/* Value is a user defined function. */
#define symtype_ALIAS	  0x04	/* This symbol shares it's value. */
#define symtype_PACKAGE	  0x05	/* This symbol contains a package. (not imp) */

/* Random data can be stored within. */
typedef struct
{
  char *data;
  int length;
} Datablock;

/* Create a Datablock object from DATA and LENGTH. */
extern Datablock *datablock_create (char *data, int length);

/* Return a duplicate of BLOCK. */
extern Datablock *datablock_copy (Datablock *block);

/* Free BLOCK and contents. */
extern void datablock_free (Datablock *block);


/* A linked list of symbols. */
typedef struct _symbol_list_
{
  struct _symbol_list_ *next;
  Symbol *symbol;
} SymbolList;

/* What a table of symbols looks like. */
typedef struct
{
  SymbolList **symbol_list;	/* An array of linked symbols. */
  int rows;			/* Number of rows in this array. */
  int entries;			/* Number of entries in this table. */
} SymbolTable;
  
/* A Package holds on to many symbols. */
typedef struct
{
  char *name;			/* The name of this package. */
  int name_len;			/* The length of NAME. */
  SymbolTable *table;		/* The symbols stored within. */
} Package;

#define SYMBOL_PACKAGE_NAME(sym) (((Package *)sym->package)->name)
#define SYMBOL_PACKAGE_NAME_LEN(sym) (((Package *)sym->package)->name_len)

/* The current package. */
extern Package *CurrentPackage;

/* The list of every package. */
extern Package **AllPackages;

/* An array of recently current packages.  The packages get on this list
   with symbol_push_package (new_package), and are removed with
   symbol_pop_package ().  Deleting a package from the global list side
   effects this PDL -- possibly destroying the usefulness of the carat
   referencing semantics. */
extern Package **PackagePDL;

/* Set the default package.  This is the package to use when
   the name of the package is "". */
extern void symbol_set_default_package (Package *package);

/* Locate and return the existing package NAME. */
extern Package *symbol_lookup_package (char *name);

/* Create a package named NAME suitable for storing symbols in.
   If the package already exists, return that package.
   You can create an anonymous package by passing NULL as the name. */
extern Package *symbol_get_package (char *name);

/* Create a package named NAME suitable for storing symbols in.
   If the package already exists, return that package.
   A non-zero value for SMALL_PRIME creates the table with that many
   hash buckets, instead of using the default value.
   You can create an anonymous package by passing NULL as the name. */
extern Package *symbol_get_package_hash (char *name, int small_prime);

/* Push CurrentPackage onto the Package PDL, and make NEW_PACKAGE 
   the current package. */
extern void symbol_push_package (Package *new_package);

/* Make the top of the Package PDL be the current package, and shrink
   the PDL. */
extern void symbol_pop_package (void);

/* Default number of slots for a symbol table.  Some callers
   may want to change this if they create a large number of
   symbol tables designed to hold only a few entries.
   The default value for this is 107. */
extern int symbol_small_prime;

/* Destroy the package PACKAGE, freeing all of the space that it
   was using.  If the package was not anonymous, this deletes it
   from the list of AllPackages. */
extern void symbol_destroy_package (Package *package);

/* Copy all of the symbols from FROM into TO. */
extern void symbol_copy_package (Package *from, Package *to);

/* Return the package of IDENTIFIER, a full identifier for a symbol.
   Identifiers have two portions: the package part and the symbol name
   part.  The two parts are separated by a pair of colons, as in
   PACKAGE::SYMBOL. */
extern Package *symbol_package (char *identifier);

/* Look in PACKAGE for NAME, and return the associated symbol. */
extern Symbol *symbol_lookup_in_package (Package *package, char *name);

/* Intern in PACKAGE, the symbol named NAME, and return the interned symbol.
   If the symbol is already present, then return that. */
extern Symbol *symbol_intern_in_package (Package *package, char *name);

/* Remove from PACKAGE the symbol specified by NAME.
   The removed symbol is returned. */
extern Symbol *symbol_remove_in_package (Package *package, char *name);

/* Intern the symbol named NAME, and return that symbol.
   This interns in the package specified in the symbol name given,
   so that `foo::bar' interns BAR in the FOO package, and `bar'
   interns BAR in the current package. */
extern Symbol *symbol_intern (char *name);

/* Find in PACKAGE for NAME, and return the associated symbol. */
extern Symbol *symbol_lookup (char *name);

/* Remove the symbol specified by NAME.  The removed symbol is returned. */
extern Symbol *symbol_remove (char *name);

/* Copy SYMBOL into PACKAGE.  Both SYMBOL and PACKAGE are real data items,
   not names of those items.  Returns the copied symbol. */
extern Symbol *symbol_copy (Symbol *symbol, Package *package);

/* Move SYMBOL from its current package into PACKAGE.
   Both SYMBOL and PACKAGE are real data items, not names of those items.
   Returns the moved symbol. */
extern Symbol *symbol_move (Symbol *symbol, Package *package);

/* Change the name of SYMBOL to NEWNAME. */
extern Symbol *symbol_rename (Symbol *symbol, char *newname);

/* Free all of the data associated with SYMBOL. */
extern void symbol_free (Symbol *symbol);

/* Add to SYMBOL, the value VALUE.  The modified symbol is returned. */
extern Symbol *symbol_add_value (Symbol *symbol, char *value);

/* Get the values of NAME. */
extern char **symbol_get_values (char *name);

/* Return the full print name of SYMBOL. */
extern char *symbol_full_name (Symbol *symbol);

/* Copy the contents of ARRAY, returning a new copy. */
extern char **symbol_copy_array (char **array);

/* Return the number of elements in ARRAY. */
extern int symbol_array_length (char **array);

/* Free the individual pointers in ARRAY, and the ARRAY itself. */
extern void symbol_free_array (char **array);

/* Return an array of pointers to every symbol in the package named by NAME.
   The actual symbols are returned, so modifications can be made to them
   directly.  Beware!  Changing the name of a symbol in the returned array
   will probably make the symbol impossible to find by normal lookup
   methods, so don't do it. */
extern Symbol **symbol_package_symbols (char *name);

/* Return an array of pointers to every symbol in the package PACKAGE.
   The actual symbols are returned, so modifications can be made to them
   directly.  Beware!  Changing the name of a symbol in the returned array
   will probably make the symbol impossible to find by normal lookup
   methods, so don't do it. */
extern Symbol **symbols_of_package (Package *package);

/* Associate SYMBOL with NOTIFIER, the address of an integer. */
extern void symbol_notify_value (Symbol *symbol, int *address);

/* Store into the symbol referenced by NAME, the array of character strings
   pointed to by ARRAY. */
extern void symbol_store_array (char *name, char **array);

/* Return the canonicalized version of NAME.  This just strips out
   array brackets.  Always returns a new string, unless the input
   string is NULL. */
extern char *symbol_canonical_name (char *name);

extern int symbol_get_flags (Symbol *symbol);
extern int symbol_get_flag (Symbol *symbol, int flag);
extern void symbol_set_flag (Symbol *symbol, int flag);
extern void symbol_clear_flag (Symbol *symbol, int flag);
extern void symbol_reset (Symbol *symbol);

#if defined (__cplusplus)
}
#endif

#endif /* _SYMBOLS_H_ */
