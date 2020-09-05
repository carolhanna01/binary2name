// A Bison parser, made by GNU Bison 3.0.2.

// Skeleton implementation for Bison LALR(1) parsers in C++

// Copyright (C) 2002-2013 Free Software Foundation, Inc.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

// As a special exception, you may create a larger work that contains
// part or all of the Bison parser skeleton and distribute that work
// under terms of your choice, so long as that work isn't itself a
// parser generator using the skeleton or a modified version thereof
// as a parser skeleton.  Alternatively, if you modify or redistribute
// the parser skeleton itself, you may (at your option) remove this
// special exception, which will cause the skeleton and the resulting
// Bison output files to be licensed under the GNU General Public
// License without this special exception.

// This special exception was added by the Free Software Foundation in
// version 2.2 of Bison.

// Take the name prefix into account.
#define yylex   Sliplex

// First part of user declarations.


   # include "lex.Slip.h"
   
   using namespace std;
 



# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

#include "Slip.Bison.hpp"

// User implementation prologue.




#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> // FIXME: INFRINGES ON USER NAME SPACE.
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K].location)
/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

# ifndef YYLLOC_DEFAULT
#  define YYLLOC_DEFAULT(Current, Rhs, N)                               \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).begin  = YYRHSLOC (Rhs, 1).begin;                   \
          (Current).end    = YYRHSLOC (Rhs, N).end;                     \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).begin = (Current).end = YYRHSLOC (Rhs, 0).end;      \
        }                                                               \
    while (/*CONSTCOND*/ false)
# endif


// Suppress unused-variable warnings by "using" E.
#define YYUSE(E) ((void) (E))

// Enable debugging if requested.
#if YYDEBUG

// A pseudo ostream that takes yydebug_ into account.
# define YYCDEBUG if (yydebug_) (*yycdebug_)

# define YY_SYMBOL_PRINT(Title, Symbol)         \
  do {                                          \
    if (yydebug_)                               \
    {                                           \
      *yycdebug_ << Title << ' ';               \
      yy_print_ (*yycdebug_, Symbol);           \
      *yycdebug_ << std::endl;                  \
    }                                           \
  } while (false)

# define YY_REDUCE_PRINT(Rule)          \
  do {                                  \
    if (yydebug_)                       \
      yy_reduce_print_ (Rule);          \
  } while (false)

# define YY_STACK_PRINT()               \
  do {                                  \
    if (yydebug_)                       \
      yystack_print_ ();                \
  } while (false)

#else // !YYDEBUG

# define YYCDEBUG if (false) std::cerr
# define YY_SYMBOL_PRINT(Title, Symbol)  YYUSE(Symbol)
# define YY_REDUCE_PRINT(Rule)           static_cast<void>(0)
# define YY_STACK_PRINT()                static_cast<void>(0)

#endif // !YYDEBUG

#define yyerrok         (yyerrstatus_ = 0)
#define yyclearin       (yyempty = true)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYRECOVERING()  (!!yyerrstatus_)


namespace slip {


  /// Build a parser object.
  parser::parser (SlipHeader** head_yyarg, SlipInputMethods* methods_yyarg)
    :
#if YYDEBUG
      yydebug_ (false),
      yycdebug_ (&std::cerr),
#endif
      head (head_yyarg),
      methods (methods_yyarg)
  {}

  parser::~parser ()
  {}


  /*---------------.
  | Symbol types.  |
  `---------------*/

  inline
  parser::syntax_error::syntax_error (const location_type& l, const std::string& m)
    : std::runtime_error (m)
    , location (l)
  {}

  // basic_symbol.
  template <typename Base>
  inline
  parser::basic_symbol<Base>::basic_symbol ()
    : value ()
  {}

  template <typename Base>
  inline
  parser::basic_symbol<Base>::basic_symbol (const basic_symbol& other)
    : Base (other)
    , value ()
    , location (other.location)
  {
    value = other.value;
  }


  template <typename Base>
  inline
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const semantic_type& v, const location_type& l)
    : Base (t)
    , value (v)
    , location (l)
  {}


  /// Constructor for valueless symbols.
  template <typename Base>
  inline
  parser::basic_symbol<Base>::basic_symbol (typename Base::kind_type t, const location_type& l)
    : Base (t)
    , value ()
    , location (l)
  {}

  template <typename Base>
  inline
  parser::basic_symbol<Base>::~basic_symbol ()
  {
  }

  template <typename Base>
  inline
  void
  parser::basic_symbol<Base>::move (basic_symbol& s)
  {
    super_type::move(s);
    value = s.value;
    location = s.location;
  }

  // by_type.
  inline
  parser::by_type::by_type ()
     : type (empty)
  {}

  inline
  parser::by_type::by_type (const by_type& other)
    : type (other.type)
  {}

  inline
  parser::by_type::by_type (token_type t)
    : type (yytranslate_ (t))
  {}

  inline
  void
  parser::by_type::move (by_type& that)
  {
    type = that.type;
    that.type = empty;
  }

  inline
  int
  parser::by_type::type_get () const
  {
    return type;
  }


  // by_state.
  inline
  parser::by_state::by_state ()
    : state (empty)
  {}

  inline
  parser::by_state::by_state (const by_state& other)
    : state (other.state)
  {}

  inline
  void
  parser::by_state::move (by_state& that)
  {
    state = that.state;
    that.state = empty;
  }

  inline
  parser::by_state::by_state (state_type s)
    : state (s)
  {}

  inline
  parser::symbol_number_type
  parser::by_state::type_get () const
  {
    return state == empty ? 0 : yystos_[state];
  }

  inline
  parser::stack_symbol_type::stack_symbol_type ()
  {}


  inline
  parser::stack_symbol_type::stack_symbol_type (state_type s, symbol_type& that)
    : super_type (s, that.location)
  {
    value = that.value;
    // that is emptied.
    that.type = empty;
  }

  inline
  parser::stack_symbol_type&
  parser::stack_symbol_type::operator= (const stack_symbol_type& that)
  {
    state = that.state;
    value = that.value;
    location = that.location;
    return *this;
  }


  template <typename Base>
  inline
  void
  parser::yy_destroy_ (const char* yymsg, basic_symbol<Base>& yysym) const
  {
    if (yymsg)
      YY_SYMBOL_PRINT (yymsg, yysym);

    // User destructor.
    switch (yysym.type_get ())
    {
            case 30: // listDefinition


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 31: // description


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 32: // descriptionList


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 33: // descriptionItemlist


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 34: // descriptionElement


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 37: // list


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 38: // listItemList


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 39: // listItem


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 41: // namedList


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 42: // userData


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 43: // datum


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;

      case 44: // number


        {  methods->incErrorCount();
               (yysym.value.DESCRIPTION)->deleteData(); 
               (yysym.value.DESCRIPTION) = NULL;
            }

        break;


      default:
        break;
    }
  }

#if YYDEBUG
  template <typename Base>
  void
  parser::yy_print_ (std::ostream& yyo,
                                     const basic_symbol<Base>& yysym) const
  {
    std::ostream& yyoutput = yyo;
    YYUSE (yyoutput);
    symbol_number_type yytype = yysym.type_get ();
    yyo << (yytype < yyntokens_ ? "token" : "nterm")
        << ' ' << yytname_[yytype] << " ("
        << yysym.location << ": ";
    YYUSE (yytype);
    yyo << ')';
  }
#endif

  inline
  void
  parser::yypush_ (const char* m, state_type s, symbol_type& sym)
  {
    stack_symbol_type t (s, sym);
    yypush_ (m, t);
  }

  inline
  void
  parser::yypush_ (const char* m, stack_symbol_type& s)
  {
    if (m)
      YY_SYMBOL_PRINT (m, s);
    yystack_.push (s);
  }

  inline
  void
  parser::yypop_ (unsigned int n)
  {
    yystack_.pop (n);
  }

#if YYDEBUG
  std::ostream&
  parser::debug_stream () const
  {
    return *yycdebug_;
  }

  void
  parser::set_debug_stream (std::ostream& o)
  {
    yycdebug_ = &o;
  }


  parser::debug_level_type
  parser::debug_level () const
  {
    return yydebug_;
  }

  void
  parser::set_debug_level (debug_level_type l)
  {
    yydebug_ = l;
  }
#endif // YYDEBUG

  inline parser::state_type
  parser::yy_lr_goto_state_ (state_type yystate, int yysym)
  {
    int yyr = yypgoto_[yysym - yyntokens_] + yystate;
    if (0 <= yyr && yyr <= yylast_ && yycheck_[yyr] == yystate)
      return yytable_[yyr];
    else
      return yydefgoto_[yysym - yyntokens_];
  }

  inline bool
  parser::yy_pact_value_is_default_ (int yyvalue)
  {
    return yyvalue == yypact_ninf_;
  }

  inline bool
  parser::yy_table_value_is_error_ (int yyvalue)
  {
    return yyvalue == yytable_ninf_;
  }

  int
  parser::parse ()
  {
    /// Whether yyla contains a lookahead.
    bool yyempty = true;

    // State.
    int yyn;
    /// Length of the RHS of the rule being reduced.
    int yylen = 0;

    // Error handling.
    int yynerrs_ = 0;
    int yyerrstatus_ = 0;

    /// The lookahead symbol.
    symbol_type yyla;

    /// The locations where the error started and ended.
    stack_symbol_type yyerror_range[3];

    /// The return value of parse ().
    int yyresult;

    // FIXME: This shoud be completely indented.  It is not yet to
    // avoid gratuitous conflicts when merging into the master branch.
    try
      {
    YYCDEBUG << "Starting parse" << std::endl;


    /* Initialize the stack.  The initial state will be set in
       yynewstate, since the latter expects the semantical and the
       location values to have been already stored, initialize these
       stacks with a primary value.  */
    yystack_.clear ();
    yypush_ (YY_NULLPTR, 0, yyla);

    // A new symbol was pushed on the stack.
  yynewstate:
    YYCDEBUG << "Entering state " << yystack_[0].state << std::endl;

    // Accept?
    if (yystack_[0].state == yyfinal_)
      goto yyacceptlab;

    goto yybackup;

    // Backup.
  yybackup:

    // Try to take a decision without lookahead.
    yyn = yypact_[yystack_[0].state];
    if (yy_pact_value_is_default_ (yyn))
      goto yydefault;

    // Read a lookahead token.
    if (yyempty)
      {
        YYCDEBUG << "Reading a token: ";
        try
          {
            yyla.type = yytranslate_ (yylex (&yyla.value, &yyla.location));
          }
        catch (const syntax_error& yyexc)
          {
            error (yyexc);
            goto yyerrlab1;
          }
        yyempty = false;
      }
    YY_SYMBOL_PRINT ("Next token is", yyla);

    /* If the proper action on seeing token YYLA.TYPE is to reduce or
       to detect an error, take that action.  */
    yyn += yyla.type_get ();
    if (yyn < 0 || yylast_ < yyn || yycheck_[yyn] != yyla.type_get ())
      goto yydefault;

    // Reduce or error.
    yyn = yytable_[yyn];
    if (yyn <= 0)
      {
        if (yy_table_value_is_error_ (yyn))
          goto yyerrlab;
        yyn = -yyn;
        goto yyreduce;
      }

    // Discard the token being shifted.
    yyempty = true;

    // Count tokens shifted since error; after three, turn off error status.
    if (yyerrstatus_)
      --yyerrstatus_;

    // Shift the lookahead token.
    yypush_ ("Shifting", yyn, yyla);
    goto yynewstate;

  /*-----------------------------------------------------------.
  | yydefault -- do the default action for the current state.  |
  `-----------------------------------------------------------*/
  yydefault:
    yyn = yydefact_[yystack_[0].state];
    if (yyn == 0)
      goto yyerrlab;
    goto yyreduce;

  /*-----------------------------.
  | yyreduce -- Do a reduction.  |
  `-----------------------------*/
  yyreduce:
    yylen = yyr2_[yyn];
    {
      stack_symbol_type yylhs;
      yylhs.state = yy_lr_goto_state_(yystack_[yylen].state, yyr1_[yyn]);
      /* If YYLEN is nonzero, implement the default value of the
         action: '$$ = $1'.  Otherwise, use the top of the stack.

         Otherwise, the following line sets YYLHS.VALUE to garbage.
         This behavior is undocumented and Bison users should not rely
         upon it.  */
      if (yylen)
        yylhs.value = yystack_[yylen - 1].value;
      else
        yylhs.value = yystack_[0].value;

      // Compute the default @$.
      {
        slice<stack_symbol_type, stack_type> slice (yystack_, yylen);
        YYLLOC_DEFAULT (yylhs.location, slice, yylen);
      }

      // Perform the reduction.
      YY_REDUCE_PRINT (yyn);
      try
        {
          switch (yyn)
            {
  case 2:

    {  *head = (yylhs.value.SLIPHEADER) = (SlipHeader*)((yystack_[1].value.DESCRIPTION)->getPtr()); 
                        DEBUG(methods->getInputFlag(), methods->debugOut("declarations listDefinition END", (yystack_[1].value.DESCRIPTION));)
                        delete (yystack_[1].value.DESCRIPTION);
                        YYACCEPT; 
                     }

    break;

  case 3:

    {  *head = (yylhs.value.SLIPHEADER) = (SlipHeader*)((yystack_[1].value.DESCRIPTION)->getPtr()); 
                        DEBUG(methods->getInputFlag(), methods->debugOut("declarations namedList END", (yystack_[1].value.DESCRIPTION));)
                        delete (yystack_[1].value.DESCRIPTION);
                        YYACCEPT; 
                     }

    break;

  case 4:

    {  (yylhs.value.SLIPHEADER) = NULL; 
                        DEBUG(methods->getInputFlag(), methods->debugOut("declarations END", NULL);)
                     }

    break;

  case 5:

    {  *head = (yylhs.value.SLIPHEADER) = (SlipHeader*)((yystack_[1].value.DESCRIPTION)->getPtr()); 
                        DEBUG(methods->getInputFlag(), methods->debugOut("listDefinition END", (yystack_[1].value.DESCRIPTION));)
                        delete (yystack_[1].value.DESCRIPTION);
                        YYACCEPT; 
                     }

    break;

  case 6:

    {  *head = (yylhs.value.SLIPHEADER) = (SlipHeader*)((yystack_[1].value.DESCRIPTION)->getPtr());
                        DEBUG(methods->getInputFlag(), methods->debugOut("namedList END", (yystack_[1].value.DESCRIPTION));)
                        delete (yystack_[1].value.DESCRIPTION);
                        YYACCEPT; 
                     }

    break;

  case 7:

    { DEBUG(methods->getInputFlag(), methods->debugOut("declarations declarationItem", NULL);)
                     }

    break;

  case 8:

    { DEBUG(methods->getInputFlag(), methods->debugOut("declarationItem", NULL);)
                     }

    break;

  case 9:

    {  (yylhs.value.INTEGER) = 0;
                        methods->declarationItem("name listDefinition", (yystack_[2].value.STRING), (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 12:

    {  if (!isTop())  restoreLexState(); 
                        DEBUG(methods->getInputFlag(), methods->debugOut("END", NULL);)
                     }

    break;

  case 13:

    {  (yylhs.value.DESCRIPTION) = methods->markDescriptionList("(mark description list)", (yystack_[3].value.UCHAR), (yystack_[2].value.DESCRIPTION), (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 14:

    {  (yylhs.value.DESCRIPTION) = methods->markDescription("(mark description)", (yystack_[2].value.UCHAR), (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 15:

    {  (yylhs.value.DESCRIPTION) = methods->descriptionList("(description list)", (yystack_[2].value.DESCRIPTION), (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 16:

    {  (yylhs.value.DESCRIPTION) = methods->descriptionNULLList("(description)", (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 17:

    {  (yylhs.value.DESCRIPTION) = methods->markList("(mark list)", (yystack_[2].value.UCHAR), (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 18:

    {  (yylhs.value.DESCRIPTION) = methods->mark("(mark)", (yystack_[1].value.UCHAR));
                     }

    break;

  case 19:

    {  (yylhs.value.DESCRIPTION) = methods->listDefinition("(mark list)", (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 20:

    {  (yylhs.value.DESCRIPTION) = methods->nullList("(list)");
                     }

    break;

  case 21:

    {  (yylhs.value.DESCRIPTION) = methods->descriptionDescriptionList("<description descriptionList>", (yystack_[2].value.DESCRIPTION), (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 22:

    {  (yylhs.value.DESCRIPTION) = methods->simpleReturn("< descriptionList >", (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 23:

    {  (yylhs.value.DESCRIPTION) = methods->description("< description >", (yystack_[1].value.DESCRIPTION));
                     }

    break;

  case 24:

    {  (yylhs.value.DESCRIPTION) = methods->markDescription("< mark > ", (yystack_[1].value.UCHAR));
                     }

    break;

  case 25:

    {  (yylhs.value.DESCRIPTION) = methods->nullDescription("< mark > ");
                     }

    break;

  case 26:

    {  (yylhs.value.DESCRIPTION) = methods->simpleReturn("descriptionItemlist", (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 27:

    {  (yylhs.value.DESCRIPTION) = methods->markDescriptionItemList("mark descriptionItemlist", (yystack_[1].value.UCHAR), (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 28:

    {  (yylhs.value.DESCRIPTION) = methods->simpleReturn("namedList", (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 29:

    {  (yylhs.value.DESCRIPTION) = methods->descriptionItemlist("descriptionItemlist", (yystack_[2].value.DESCRIPTION), (yystack_[1].value.DESCRIPTION), (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 30:

    {  (yylhs.value.DESCRIPTION) = methods->descriptionItem("descriptionElement", (yystack_[1].value.DESCRIPTION), (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 31:

    {  (yylhs.value.DESCRIPTION) = (yystack_[0].value.DESCRIPTION); }

    break;

  case 32:

    {  (yylhs.value.DESCRIPTION) = (yystack_[0].value.DESCRIPTION); }

    break;

  case 33:

    {  (yylhs.value.DESCRIPTION) = (yystack_[1].value.DESCRIPTION); }

    break;

  case 34:

    {  (yylhs.value.INTEGER) = 0;
                        methods->includeFile("INCLUDE datum", (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 35:

    {  methods->userDataList("", (yystack_[0].value.STRING));
                     }

    break;

  case 36:

    {  methods->userDataList("", (yystack_[0].value.STRING));
                     }

    break;

  case 37:

    {  (yylhs.value.DESCRIPTION) = methods->simpleReturn("list", (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 38:

    {  (yylhs.value.DESCRIPTION) = methods->listItemList("listItemList listItem", (yystack_[1].value.DESCRIPTION), (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 39:

    {  (yylhs.value.DESCRIPTION) = methods->listItem("listItem", (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 40:

    {  (yylhs.value.DESCRIPTION) = (yystack_[0].value.DESCRIPTION); }

    break;

  case 41:

    {  (yylhs.value.DESCRIPTION) = (yystack_[0].value.DESCRIPTION); }

    break;

  case 42:

    {  (yylhs.value.DESCRIPTION) = (yystack_[0].value.DESCRIPTION); }

    break;

  case 43:

    {  (yylhs.value.DESCRIPTION) = methods->listItemListDefinition("listDefinition", (yystack_[0].value.DESCRIPTION));
                     }

    break;

  case 44:

    {  (yylhs.value.UCHAR) = (UCHAR)(*(SlipDatum*)((yystack_[1].value.DESCRIPTION)->getPtr()));
                        delete (yystack_[1].value.DESCRIPTION);
                     }

    break;

  case 45:

    {  (yylhs.value.DESCRIPTION) = methods->namedList("{ name }", (yystack_[1].value.STRING));
                     }

    break;

  case 46:

    { (yylhs.value.DESCRIPTION) = methods->userData("name listDefinition", (yystack_[1].value.STRING), (yystack_[0].value.DESCRIPTION)); }

    break;

  case 47:

    { (yylhs.value.DESCRIPTION) = methods->userData("name {namedList}", (yystack_[1].value.STRING), (yystack_[0].value.DESCRIPTION)); }

    break;

  case 48:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.BOOL)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 49:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.CHAR)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 50:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.UCHAR)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 51:

    { (yylhs.value.DESCRIPTION) = (yystack_[0].value.DESCRIPTION); }

    break;

  case 52:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.FLOAT)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 53:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.STRING)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 54:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.INTEGER)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 55:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.UINTEGER)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 56:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.CHAR)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 57:

    { (yylhs.value.DESCRIPTION) = new SlipDescription(new SlipDatum((yystack_[0].value.UCHAR)), SlipDescription::DATA, methods->getDebugFlag()); }

    break;

  case 58:

    { (yylhs.value.STRING) = (yystack_[0].value.STRING); }

    break;



            default:
              break;
            }
        }
      catch (const syntax_error& yyexc)
        {
          error (yyexc);
          YYERROR;
        }
      YY_SYMBOL_PRINT ("-> $$ =", yylhs);
      yypop_ (yylen);
      yylen = 0;
      YY_STACK_PRINT ();

      // Shift the result of the reduction.
      yypush_ (YY_NULLPTR, yylhs);
    }
    goto yynewstate;

  /*--------------------------------------.
  | yyerrlab -- here on detecting error.  |
  `--------------------------------------*/
  yyerrlab:
    // If not already recovering from an error, report this error.
    if (!yyerrstatus_)
      {
        ++yynerrs_;
        error (yyla.location, yysyntax_error_ (yystack_[0].state,
                                           yyempty ? yyempty_ : yyla.type_get ()));
      }


    yyerror_range[1].location = yyla.location;
    if (yyerrstatus_ == 3)
      {
        /* If just tried and failed to reuse lookahead token after an
           error, discard it.  */

        // Return failure if at end of input.
        if (yyla.type_get () == yyeof_)
          YYABORT;
        else if (!yyempty)
          {
            yy_destroy_ ("Error: discarding", yyla);
            yyempty = true;
          }
      }

    // Else will try to reuse lookahead token after shifting the error token.
    goto yyerrlab1;


  /*---------------------------------------------------.
  | yyerrorlab -- error raised explicitly by YYERROR.  |
  `---------------------------------------------------*/
  yyerrorlab:

    /* Pacify compilers like GCC when the user code never invokes
       YYERROR and the label yyerrorlab therefore never appears in user
       code.  */
    if (false)
      goto yyerrorlab;
    yyerror_range[1].location = yystack_[yylen - 1].location;
    /* Do not reclaim the symbols of the rule whose action triggered
       this YYERROR.  */
    yypop_ (yylen);
    yylen = 0;
    goto yyerrlab1;

  /*-------------------------------------------------------------.
  | yyerrlab1 -- common code for both syntax error and YYERROR.  |
  `-------------------------------------------------------------*/
  yyerrlab1:
    yyerrstatus_ = 3;   // Each real token shifted decrements this.
    {
      stack_symbol_type error_token;
      for (;;)
        {
          yyn = yypact_[yystack_[0].state];
          if (!yy_pact_value_is_default_ (yyn))
            {
              yyn += yyterror_;
              if (0 <= yyn && yyn <= yylast_ && yycheck_[yyn] == yyterror_)
                {
                  yyn = yytable_[yyn];
                  if (0 < yyn)
                    break;
                }
            }

          // Pop the current state because it cannot handle the error token.
          if (yystack_.size () == 1)
            YYABORT;

          yyerror_range[1].location = yystack_[0].location;
          yy_destroy_ ("Error: popping", yystack_[0]);
          yypop_ ();
          YY_STACK_PRINT ();
        }

      yyerror_range[2].location = yyla.location;
      YYLLOC_DEFAULT (error_token.location, yyerror_range, 2);

      // Shift the error token.
      error_token.state = yyn;
      yypush_ ("Shifting", error_token);
    }
    goto yynewstate;

    // Accept.
  yyacceptlab:
    yyresult = 0;
    goto yyreturn;

    // Abort.
  yyabortlab:
    yyresult = 1;
    goto yyreturn;

  yyreturn:
    if (!yyempty)
      yy_destroy_ ("Cleanup: discarding lookahead", yyla);

    /* Do not reclaim the symbols of the rule whose action triggered
       this YYABORT or YYACCEPT.  */
    yypop_ (yylen);
    while (1 < yystack_.size ())
      {
        yy_destroy_ ("Cleanup: popping", yystack_[0]);
        yypop_ ();
      }

    return yyresult;
  }
    catch (...)
      {
        YYCDEBUG << "Exception caught: cleaning lookahead and stack"
                 << std::endl;
        // Do not try to display the values of the reclaimed symbols,
        // as their printer might throw an exception.
        if (!yyempty)
          yy_destroy_ (YY_NULLPTR, yyla);

        while (1 < yystack_.size ())
          {
            yy_destroy_ (YY_NULLPTR, yystack_[0]);
            yypop_ ();
          }
        throw;
      }
  }

  void
  parser::error (const syntax_error& yyexc)
  {
    error (yyexc.location, yyexc.what());
  }

  // Generate an error message.
  std::string
  parser::yysyntax_error_ (state_type, symbol_number_type) const
  {
    return YY_("syntax error");
  }


  const signed char parser::yypact_ninf_ = -46;

  const signed char parser::yytable_ninf_ = -29;

  const short int
  parser::yypact_[] =
  {
      35,   -46,   -46,    11,    80,     3,    11,    24,    35,   -46,
      28,   -46,    30,    31,    18,   -46,   -46,   -46,   -46,   -46,
     -46,   -46,   -46,   -46,   -46,   -46,   120,    10,   -46,   140,
      17,   200,   -46,   100,   -46,   -46,   -46,   -46,    40,    62,
      36,   -46,   -46,    56,    58,   -46,   -46,    49,   -46,    11,
     200,   -46,   220,    50,   260,   260,   240,    54,   -46,    57,
     -46,    60,   -46,   -46,   -46,   160,    70,   -46,   -46,   -46,
     -46,   -46,   -46,   -46,   -46,   180,   -46,    72,   260,   -46,
     260,   -46,   -46,   -46,   260,   -46,   -46,   -46,    77,   -46,
     -46,   -46,   -46,   -46
  };

  const unsigned char
  parser::yydefact_[] =
  {
       0,    12,    58,     0,     0,     0,     0,     0,     4,     8,
       0,    11,     0,     0,     0,    36,    48,    49,    56,    57,
      52,    54,    53,    50,    55,    20,     0,     0,    43,     0,
       0,    37,    39,     0,    41,    42,    40,    51,     0,     0,
       0,     1,     7,     0,     0,     5,     6,     0,    10,     0,
       0,    25,     0,     0,    26,     0,     0,    31,    32,     0,
      16,     0,    19,    38,    18,     0,     0,    46,    47,    34,
      45,     2,     3,     9,    35,     0,    23,     0,     0,    22,
       0,    31,    30,    24,    27,    44,    15,    14,     0,    17,
      33,    21,    29,    13
  };

  const signed char
  parser::yypgoto_[] =
  {
     -46,   -46,   -46,    73,     5,    -3,    45,   -45,   -40,   -46,
     -46,   -23,    51,   -28,    -7,     0,   -46,    -2,    75,     1
  };

  const signed char
  parser::yydefgoto_[] =
  {
      -1,     7,     8,     9,    28,    29,    53,    54,    55,    11,
      14,    30,    31,    32,    33,    34,    35,    58,    37,    38
  };

  const signed char
  parser::yytable_[] =
  {
      12,    13,    36,    63,    15,    10,    61,    40,    44,    13,
      66,    84,    39,    43,    80,    82,    18,    19,    47,    56,
      21,     2,     2,    52,    41,    24,    57,    36,    40,    36,
      65,    36,    45,    84,    46,    48,    62,    69,    68,     1,
      92,    49,    88,    67,    80,    78,     2,    63,    36,     4,
      74,     3,    57,     4,    81,    81,    81,     5,     4,     6,
      71,    70,    72,    36,     6,    16,    73,    17,    18,    19,
      20,    79,    21,    36,    22,   -28,    23,    24,    81,    86,
      81,    42,    85,    16,    81,    17,    18,    19,    20,    89,
      21,     2,    22,    91,    23,    24,    93,    77,     4,    25,
      26,    75,    59,    16,    27,    17,    18,    19,    20,     0,
      21,     2,    22,     0,    23,    24,     0,     0,     4,    64,
      26,     0,     0,    16,     6,    17,    18,    19,    20,     0,
      21,     0,    22,     0,    23,    24,     0,     0,    50,     0,
      26,    51,     0,    16,    27,    17,    18,    19,    20,     0,
      21,     2,    22,     0,    23,    24,     0,     0,     4,    60,
       0,     0,     0,    16,     6,    17,    18,    19,    20,     0,
      21,     2,    22,     0,    23,    24,     0,     0,     4,    87,
       0,     0,     0,    16,     6,    17,    18,    19,    20,     0,
      21,     2,    22,     0,    23,    24,     0,     0,     4,    90,
       0,     0,     0,    16,     6,    17,    18,    19,    20,     0,
      21,     2,    22,     0,    23,    24,     0,     0,     4,     0,
       0,     0,     0,    16,     6,    17,    18,    19,    20,     0,
      21,     0,    22,     0,    23,    24,     0,     0,    50,     0,
       0,    76,     0,    16,    27,    17,    18,    19,    20,     0,
      21,     0,    22,     0,    23,    24,     0,     0,    50,     0,
       0,    83,     0,    16,     6,    17,    18,    19,    20,     0,
      21,     0,    22,     0,    23,    24,     0,     0,    50,     0,
       0,     0,     0,     0,     6
  };

  const signed char
  parser::yycheck_[] =
  {
       0,     0,     4,    31,     3,     0,    29,     6,     8,     8,
      33,    56,     9,     8,    54,    55,     6,     7,    13,    26,
      10,    11,    11,    26,     0,    15,    26,    29,    27,    31,
      33,    33,     4,    78,     4,    17,    19,    39,    38,     4,
      80,    23,    65,    38,    84,    52,    11,    75,    50,    18,
      49,    16,    52,    18,    54,    55,    56,    22,    18,    24,
       4,    25,     4,    65,    24,     3,    17,     5,     6,     7,
       8,    21,    10,    75,    12,    21,    14,    15,    78,    19,
      80,     8,    25,     3,    84,     5,     6,     7,     8,    19,
      10,    11,    12,    21,    14,    15,    19,    52,    18,    19,
      20,    50,    27,     3,    24,     5,     6,     7,     8,    -1,
      10,    11,    12,    -1,    14,    15,    -1,    -1,    18,    19,
      20,    -1,    -1,     3,    24,     5,     6,     7,     8,    -1,
      10,    -1,    12,    -1,    14,    15,    -1,    -1,    18,    -1,
      20,    21,    -1,     3,    24,     5,     6,     7,     8,    -1,
      10,    11,    12,    -1,    14,    15,    -1,    -1,    18,    19,
      -1,    -1,    -1,     3,    24,     5,     6,     7,     8,    -1,
      10,    11,    12,    -1,    14,    15,    -1,    -1,    18,    19,
      -1,    -1,    -1,     3,    24,     5,     6,     7,     8,    -1,
      10,    11,    12,    -1,    14,    15,    -1,    -1,    18,    19,
      -1,    -1,    -1,     3,    24,     5,     6,     7,     8,    -1,
      10,    11,    12,    -1,    14,    15,    -1,    -1,    18,    -1,
      -1,    -1,    -1,     3,    24,     5,     6,     7,     8,    -1,
      10,    -1,    12,    -1,    14,    15,    -1,    -1,    18,    -1,
      -1,    21,    -1,     3,    24,     5,     6,     7,     8,    -1,
      10,    -1,    12,    -1,    14,    15,    -1,    -1,    18,    -1,
      -1,    21,    -1,     3,    24,     5,     6,     7,     8,    -1,
      10,    -1,    12,    -1,    14,    15,    -1,    -1,    18,    -1,
      -1,    -1,    -1,    -1,    24
  };

  const unsigned char
  parser::yystos_[] =
  {
       0,     4,    11,    16,    18,    22,    24,    27,    28,    29,
      30,    35,    41,    45,    36,    45,     3,     5,     6,     7,
       8,    10,    12,    14,    15,    19,    20,    24,    30,    31,
      37,    38,    39,    40,    41,    42,    43,    44,    45,     9,
      45,     0,    29,    30,    41,     4,     4,    30,    17,    23,
      18,    21,    31,    32,    33,    34,    40,    41,    43,    44,
      19,    37,    19,    39,    19,    31,    37,    30,    41,    43,
      25,     4,     4,    17,    45,    38,    21,    32,    40,    21,
      34,    41,    34,    21,    33,    25,    19,    19,    37,    19,
      19,    21,    34,    19
  };

  const unsigned char
  parser::yyr1_[] =
  {
       0,    26,    27,    27,    27,    27,    27,    28,    28,    29,
      29,    29,    29,    30,    30,    30,    30,    30,    30,    30,
      30,    31,    31,    31,    31,    31,    32,    32,    32,    33,
      33,    34,    34,    34,    35,    36,    36,    37,    38,    38,
      39,    39,    39,    39,    40,    41,    42,    42,    43,    43,
      43,    43,    43,    43,    44,    44,    44,    44,    45
  };

  const unsigned char
  parser::yyr2_[] =
  {
       0,     2,     3,     3,     1,     2,     2,     2,     1,     3,
       3,     1,     1,     5,     4,     4,     3,     4,     3,     3,
       2,     4,     3,     3,     3,     2,     1,     2,     1,     3,
       2,     1,     1,     3,     3,     3,     1,     1,     2,     1,
       1,     1,     1,     1,     3,     3,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1
  };


#if YYDEBUG
  // YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
  // First, the terminals, then, starting at \a yyntokens_, nonterminals.
  const char*
  const parser::yytname_[] =
  {
  "$end", "error", "$undefined", "BOOL", "END", "CHAR", "CHARS", "CHARU",
  "FLOAT", "INCLUDE", "INTEGER", "NAME", "STRING", "LIST", "UCHAR",
  "UINTEGER", "USER", "';'", "'('", "')'", "'<'", "'>'", "'#'", "','",
  "'{'", "'}'", "$accept", "sublistDeclarations_Definition",
  "declarations", "declarationItem", "listDefinition", "description",
  "descriptionList", "descriptionItemlist", "descriptionElement",
  "include", "userDataList", "list", "listItemList", "listItem", "mark",
  "namedList", "userData", "datum", "number", "name", YY_NULLPTR
  };


  const unsigned short int
  parser::yyrline_[] =
  {
       0,   236,   236,   242,   248,   252,   258,   267,   270,   276,
     280,   281,   282,   289,   292,   295,   298,   301,   304,   308,
     311,   317,   320,   323,   326,   329,   335,   338,   341,   347,
     351,   357,   359,   361,   365,   372,   375,   381,   387,   390,
     396,   398,   400,   402,   408,   414,   419,   421,   425,   427,
     429,   431,   433,   435,   439,   441,   443,   445,   448
  };

  // Print the state stack on the debug stream.
  void
  parser::yystack_print_ ()
  {
    *yycdebug_ << "Stack now";
    for (stack_type::const_iterator
           i = yystack_.begin (),
           i_end = yystack_.end ();
         i != i_end; ++i)
      *yycdebug_ << ' ' << i->state;
    *yycdebug_ << std::endl;
  }

  // Report on the debug stream that the rule \a yyrule is going to be reduced.
  void
  parser::yy_reduce_print_ (int yyrule)
  {
    unsigned int yylno = yyrline_[yyrule];
    int yynrhs = yyr2_[yyrule];
    // Print the symbols being reduced, and their result.
    *yycdebug_ << "Reducing stack by rule " << yyrule - 1
               << " (line " << yylno << "):" << std::endl;
    // The symbols being reduced.
    for (int yyi = 0; yyi < yynrhs; yyi++)
      YY_SYMBOL_PRINT ("   $" << yyi + 1 << " =",
                       yystack_[(yynrhs) - (yyi + 1)]);
  }
#endif // YYDEBUG

  // Symbol number corresponding to token number t.
  inline
  parser::token_number_type
  parser::yytranslate_ (int t)
  {
    static
    const token_number_type
    translate_table[] =
    {
     0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,    22,     2,     2,     2,     2,
      18,    19,     2,     2,    23,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,    17,
      20,     2,    21,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    24,     2,    25,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16
    };
    const unsigned int user_token_number_max_ = 271;
    const token_number_type undef_token_ = 2;

    if (static_cast<int>(t) <= yyeof_)
      return yyeof_;
    else if (static_cast<unsigned int> (t) <= user_token_number_max_)
      return translate_table[t];
    else
      return undef_token_;
  }


} // Slip





void parser::error(const location_type& loc, const std::string& msg) {
//   fprintf(stderr, "%s %4d:%4d %s\n"
//                 , getFileName().c_str()
   fprintf(stderr, "%4d:%4d %s\n"
                 , loc.begin.line
                 , loc.begin.column
                 , msg.c_str());
}; // parser::error(const location_type& loc, const std::string& msg)
