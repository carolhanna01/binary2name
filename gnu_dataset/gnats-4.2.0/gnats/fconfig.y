%{
#include "gnats.h"
#include "field.h"
#ifdef DS_FILE
#include "ds-file/index.h"
#endif
  extern DatabaseInfo databaseBeingDefined;
  static FieldDef currField;
  static ChangeActions currChange;
  static FieldEdit *currEdit;
  static QueryFormat *qformat;
  static FieldList requiredFlds;
  static InputTemplate *inputTemplate;
  static MailMessageFormat mailFormat;
  static int badFile;
  struct qstring;
#ifdef DS_FILE
  IndexDesc indexEntry;
#endif

  extern char *takeQString (struct qstring *str);
  extern char *qStrVal (struct qstring *str);
%}

%union {
  int intval;
  char *sval;
  struct qstring *qstr;
  AdmFieldDesc *adm_field_des;
  FieldList flist;
  StringList *stringlist;
  InputTemplate *inputlist;
  MailAddress *mailaddr;
  MailAddressList *mailaddrlist;
}

%token FIELD STRINGTYPE QDEFAULT MATCHING ENUM MULTIENUMTOK VALUES DEFAULT
%token EXACT_REGEXP INEXACT_REGEXP ALL FORMAT ENUMSEPARATORSTOK
%token MULTITEXTTYPE DATETYPE ENUM_IN_FILE MULTI_ENUM_IN_FILE
%token PATHTOK FIELDSTOK KEYTOK
%token QSTRING INTVAL TEXTSEARCH QUERYTOK FORMATTOK INDEXTOK
%token SEPARATORTOK RESTRICTEDTOK NOSPACESTOK INTEGERTOK INPUTDEFAULTTOK
%token BUILTINTOK ALLOWANYVALUETOK REQUIRETOK APPENDFIELDTOK SETFIELDTOK
%token CHANGETOK DESCRIPTIONTOK INPUTTOK DATABASEINFOTOK
%token DEBUGMODETOK KEEPRECTOK NOTIFYEXPTOK LIBEXECDIRTOK SUBMITTERACKTOK
%token BUSINESSDAYTOK BUSINESSWEEKTOK CREATECATEGORYDIRSTOK FALSETOK TRUETOK
%token MAILFORMATTOK TOADDRESSESTOK FROMADDRESSTOK REPLYTOTOK FIXEDTOK
%token BODYTOK HEADERTOK AUDITTRAILFMTTOK ADDAUDITTRAILTOK 
%token REQUIRECHANGEREASONTOK READONLYTOK BINARYINDEXTOK RAWTOK
%token BADTOK AUXFLAGSTOK PRLISTTOK MAXPRSTOK EDITONLYTOK VIRTUALFORMATTOK
%token CATPERMSTOK
%type <sval> optChangeExpr
%type <qstr> QSTRING
%type <intval> INTVAL
%type <adm_field_des> enumFieldList enumFieldMember
%type <flist> queryFieldsList FieldListMember fieldEditFieldList
%type <stringlist> enumValueList regexpList auxFlagsList
%type <inputlist> inputFields inputFieldsList
%type <intval> booleanVal
%type <flist> mailAddressTries MailAddressMember
%type <mailaddr> mailAddress
%type <mailaddrlist> mailAddressList
%type <flist> requiredFieldsList
%%

config		: configEnts
		| parseError
		;

configEnts	: databaseInfo fieldDecStmt optQueryList auditTrailFmt mailFormatList globalChangeEnts dsDescription inputDescription
		;

databaseInfo	: DATABASEINFOTOK '{' databaseInfoList '}'
		| DATABASEINFOTOK '{' parseError '}'
		| /* empty */ {
		    fconferror ("Missing/bad database info section");
		}
		;

parseError	: error { badFile = 1; } ;

databaseInfoList: databaseInfoEnt
		| databaseInfoList databaseInfoEnt
		;

databaseInfoEnt	: DEBUGMODETOK booleanVal { 
		    setDebugMode (databaseBeingDefined, $2);
		}
		| KEEPRECTOK booleanVal {
		    setKeepReceivedHeaders (databaseBeingDefined, $2);
		}
		| NOTIFYEXPTOK booleanVal {
		    setNotifyExpire (databaseBeingDefined, $2);
		}
		| LIBEXECDIRTOK QSTRING {
		    setBinDir (databaseBeingDefined, qStrVal ($2)); 
		}
		| SUBMITTERACKTOK booleanVal {
		    setSubmitterAck (databaseBeingDefined, $2); 
		}
		| BUSINESSDAYTOK INTVAL '-' INTVAL {
		  setBusinessDay (databaseBeingDefined, $2, $4);
		}
		| BUSINESSWEEKTOK INTVAL '-' INTVAL {
		    setBusinessWeek(databaseBeingDefined,$2, $4);
		}
		| CREATECATEGORYDIRSTOK booleanVal {
		    setCreateCategoryDirs (databaseBeingDefined, $2);
		}
		| CATPERMSTOK QSTRING {
		    setCategoryDirPerms (databaseBeingDefined, qStrVal ($2));
		}
		;

booleanVal	: FALSETOK { $$ = 0; }
		| TRUETOK  { $$ = 1; }
		;

fieldDecStmt	: fieldDecList
		| /* empty */ {
		    fconferror ("Missing/bad field declarations");
		}
		;

fieldDecList	: fieldDec
		| fieldDecList fieldDec
		;

fieldDec	: startFieldDec '{' fieldDataList '}' {
		    currField = NULL;
		}
		| startFieldDec '{' parseError '}' {
		    currField = NULL;
		}
		;

startFieldDec	: FIELD QSTRING {
		    char *fname = takeQString ($2);
		    currField = newFieldDef (databaseBeingDefined, fname);
		    if (currField == NULL)
		      {
			char *msg;
			asprintf (&msg, "Duplicate field definition for %s\n",
				  fname);
			fconferror (msg);
			free (msg);
		      }
		    currField->default_value = NULL;
		}
		;

fieldDataList	: fieldData
		| fieldDataList fieldData
		;

fieldData	: fieldDataType
		| miscOptions
		| queryDefault
		| virtualFieldFormat
		;

virtualFieldFormat: VIRTUALFORMATTOK plainFormat {
		    currField->virtualFormat = qformat;
		    qformat = NULL;
		}
		;

fieldDataType	: stringType
		| enumType
		| MULTITEXTTYPE optSimple {
		    currField->datatype = MultiText;
		    currField->defaultSearchType = NilSearch;
		}
		| DATETYPE {
		  currField->datatype = Date;
		  currField->defaultSearchType = LessThan;
		}
		| INTEGERTOK optSimple {
		    currField->datatype = Integer;
		    currField->defaultSearchType = NilSearch;
		}
		| PRLISTTOK prListOpts {
		    currField->datatype = PRListType;
		    currField->defaultSearchType = RegCmp;
		}
		;

stringType	: STRINGTYPE {
		    currField->datatype = Text;
		}
		| STRINGTYPE MATCHING '{' regexpList '}' {
		    currField->datatype = TextWithRegex;
		}
		| STRINGTYPE MATCHING '{' parseError '}'
		;

regexpList	: QSTRING {
		    $$ = new_string_list_ent (takeQString ($1), NULL);
		    currField->regex = $$;
		}
		| regexpList QSTRING {
		    $1->next = new_string_list_ent (takeQString ($2), NULL);
		    $$ = $1->next;
		}
		;

enumType	: ENUM '{' enumcontents '}' {
		    currField->datatype = Enum;
		    currField->defaultSearchType = RegCmp;
		}
		| ENUM_IN_FILE '{' enumFileContents '}' {
		    currField->datatype = Enum;
		    currField->defaultSearchType = RegCmp;
		    initAdmField (currField);
		}
		| MULTI_ENUM_IN_FILE '{' multiEnumFileContents '}' {
		    currField->datatype = MultiEnum;
		    currField->defaultSearchType = RegCmp;
		    initAdmField (currField);
		}
		| MULTIENUMTOK '{' multienumcontents '}' {
		   currField->datatype = MultiEnum;
		   currField->defaultSearchType = RegCmp;
		   if (currField->multiEnumSeparator == NULL)
		     {
		       currField->multiEnumSeparator
			 = xstrdup (DEFAULT_MULTIENUM_SEPARATOR);
		     }
		}
		| ENUM '{' parseError '}'
		| ENUM_IN_FILE '{' parseError '}'
		;

globalChangeEnts: /* empty */
		| globalChangeEnts globalChangeEnt
		;

globalChangeEnt	: changeHeader changeOpts '}' {
		    addGlobalChangeActions (databaseBeingDefined, currChange);
		    currChange = NULL;
		}
		;

changeClause	: changeHeader changeOpts '}' {
		    ChangeActions *p = &(currField->changeActions);
		    while (*p != NULL)
		      {
			p = &((*p)->next);
		      }
		    *p = currChange;
		    currChange = NULL;
		}
		;

changeHeader	: CHANGETOK optChangeExpr '{' {
		    currChange = newChangeAction (databaseBeingDefined, $2);
		    if ($2 != NULL) 
		      {
			free ($2);
		      }
		}
		;

optChangeExpr	: QSTRING {
		    $$ = takeQString ($1);
		}
		| /* Empty */ {
		    $$ = NULL;
		}
		;

changeOpts	: changeOpt
		| changeOpts changeOpt
		;

changeOpt	: REQUIRETOK '{' reqFieldNameList '}'
		| REQUIRETOK '{' parseError '}'
		| SETFIELDTOK fieldEditOpts {
		    currChange->edits = currEdit;
		    currEdit = NULL;
		}
		| APPENDFIELDTOK fieldEditOpts {
		    currEdit->append = 1;
		    currChange->edits = currEdit;
		    currEdit = NULL;
		}
		| ADDAUDITTRAILTOK {
		    currChange->addAuditTrail = 1;
		}
		| AUDITTRAILFMTTOK plainFormat {
		    currChange->auditTrailFormat = qformat;
		    qformat = NULL;
		}
		| REQUIRECHANGEREASONTOK {
		    currChange->requireChangeReason = 1;
		}
		;

reqFieldNameList: reqFieldNameEnt
		| reqFieldNameList reqFieldNameEnt
		;

reqFieldNameEnt: QSTRING {
		    FieldList foo 
		      = newFieldListEnt (databaseBeingDefined, qStrVal ($1),
					 currChange->requiredFields);
		    currChange->requiredFields = foo;
		}
		;

fieldEditOpts	: fieldEditName '{' fieldEditFormat optFieldEditFieldList '}'
		| fieldEditName '{' parseError '}'
		;

fieldEditName	: QSTRING {
		    currEdit = (FieldEdit *) xmalloc (sizeof (FieldEdit));
		    currEdit->expr = NULL;
		    currEdit->fieldToEditName = takeQString ($1);
		    currEdit->append = 0;
		    currEdit->textFormat = NULL;
		    currEdit->fieldsForFormat = NULL;
		    currEdit->next = NULL;
		}
		;

fieldEditFormat	: QSTRING {
		    currEdit->textFormat = takeQString ($1);
		}
		;

optFieldEditFieldList: /* empty */ {
		    currEdit->fieldsForFormat = NULL;
		}
		| fieldEditFieldList {
		}
		;

fieldEditFieldList: QSTRING {
		    $$ = newFieldListEnt (databaseBeingDefined, qStrVal ($1),
					  NULL);
		    currEdit->fieldsForFormat = $$;
		}
		| fieldEditFieldList QSTRING {
		    $$ = newFieldListEnt (databaseBeingDefined, qStrVal ($2),
					  NULL);
		    $1->next = $$;
		}
		;

optSimple	: /* Empty */
		| '{' defaultFieldVal '}'
		| '{' parseError '}'
		;

prListOpts	: /* Empty */
		| '{' prListOptList '}'
		;

prListOptList	: MAXPRSTOK INTVAL {
		    currField->maxPrsPerLine = $2;
		}
		;

enumcontents	: enumItem
		| enumcontents enumItem
		;

multienumcontents: multiEnumItem
		| multienumcontents multiEnumItem
		;

multiEnumItem	: enumItem
		| ENUMSEPARATORSTOK QSTRING {
		   currField->multiEnumSeparator = takeQString ($2);
		}
		;

enumItem	: VALUES '{' enumValueList '}'
		| VALUES '{' parseError '}'
		| defaultFieldVal
		;

enumValueList	: QSTRING {
		    $$ = new_string_list_ent (takeQString ($1), NULL);
		    currField->enumValues = $$;
		}
		| enumValueList QSTRING {
		    $1->next = new_string_list_ent (takeQString ($2), NULL);
		    $$ = $1->next;
		}
		;

enumFileContents: enumFileItem
		| enumFileContents enumFileItem
		;

enumFileItem	: PATHTOK QSTRING {
		    currField->adm_db_name = takeQString ($2);
		}
		| FIELDSTOK '{' enumFieldList '}' KEYTOK QSTRING {
		    AdmFieldDesc *p;
		    int which = 0;

		    for (p = currField->adm_field_des; p != NULL; p = p->next)
		      {
			if (strcmp (p->name, qStrVal ($6)) == 0)
			  {
			    break;
			  }
			which++;
		      }

		    if (p != NULL)
		      {
			currField->key_field = which;
		      }
		    else
		      {
			char *msg;

			asprintf (&msg, "Invalid adm subfield %s\n",
				  qStrVal ($6));
			fconferror (msg);
			free (msg);
		      }
		}
		| FIELDSTOK '{' parseError '}'
		| defaultFieldVal
		| ALLOWANYVALUETOK {
		    currField->allow_any_value = 1;
		}
		;

multiEnumFileContents: multiEnumFileItem
		| enumFileContents multiEnumFileItem
		;

multiEnumFileItem : enumFileItem
		| ENUMSEPARATORSTOK QSTRING {
		   currField->multiEnumSeparator = takeQString ($2);
		}
		;

defaultFieldVal : DEFAULT QSTRING {
		    currField->default_value = takeQString ($2);
		}
		| INPUTDEFAULTTOK QSTRING {
		    currField->input_default_value = takeQString ($2);
		}
		;

enumFieldList	: enumFieldMember {
		    currField->adm_db_fields = 1;
		    currField->adm_field_des = $1;
		    $$ = $1;
		}
		| enumFieldList enumFieldMember {
		    $1->next = $2;
		    $$ = $2;
		    currField->adm_db_fields++;
		}
		;

enumFieldMember	: QSTRING {
		    $$ = (AdmFieldDesc *) 
		      xmalloc (sizeof (AdmFieldDesc));
		    $$->name = takeQString ($1);
		    $$->next = NULL;
		}
		;

queryDefault	: QDEFAULT EXACT_REGEXP {
		    currField->defaultSearchType = RegCmp;
		}
		| QDEFAULT INEXACT_REGEXP {
		    currField->defaultSearchType = RegFind;
		}
		;

miscOptions	: TEXTSEARCH {
		    currField->textsearch = 1;
		}
		| RESTRICTEDTOK {
		    currField->restricted = 1;
		}
		| NOSPACESTOK {
		     currField->nospaces = 1;
		}
		| BUILTINTOK QSTRING {
		    if (setBuiltinField (currField, qStrVal ($2)) != 0)
		      {
			char *msg;
			asprintf (&msg, "Invalid builtin fieldname %s",
				  qStrVal ($2));
			fconferror (msg);
			free (msg);
		      }
		}
		| changeClause
		| DESCRIPTIONTOK QSTRING {
		    currField->description = takeQString ($2);
		}
		| READONLYTOK {
		    currField->readonly = 1;
		}
		| AUXFLAGSTOK '{' auxFlagsList '}' {
		    currField->auxFlags = $3;
		}
		| EDITONLYTOK {
		    currField->editonly = 1;
		}
		;

auxFlagsList	: QSTRING {
		    $$ = new_string_list_ent (takeQString ($1), NULL);
		    currField->auxFlags = $$;
		}
		| auxFlagsList QSTRING {
		    $1->next = new_string_list_ent (takeQString ($2), NULL);
		    $$ = $1->next;
		}
		;

optQueryList	: /* empty */
		| queryList
		;

queryList	: query
		| queryList query
		;

query		: queryBegin '{' queryFmt '}' {
		    addQueryFormat (databaseBeingDefined, qformat);
		    qformat = NULL;
		}
		| queryBegin '{' parseError '}' {
		    freeQueryFormat (qformat);
		    qformat = NULL;
		}
		;

queryBegin	: QUERYTOK QSTRING {
		    qformat = (QueryFormat *) xmalloc (sizeof (QueryFormat));
		    qformat->name = takeQString ($2);
		    qformat->printf = NULL;
		    qformat->separator = NULL;
		    qformat->fields = NULL;
		    qformat->next = NULL;
		}
		;

queryFmt	: queryFieldDesc optQueryOpts
		| queryOpts queryFieldDesc
		;

queryFieldDesc	: FIELDSTOK ALL
		| queryfields
		| queryPrintf queryfields
		| queryfields queryPrintf
		;

optQueryOpts	:/* empty */
		| queryOpts
		;

queryOpts	: RAWTOK {
		    qformat->rawQuery = 1;
		}
		;

queryPrintf	: FORMATTOK QSTRING {
		    qformat->printf = takeQString ($2);
		}
		;

queryfields	: FIELDSTOK '{' queryFieldsList '}'
		| FIELDSTOK '{' parseError '}'
		;

queryFieldsList	: FieldListMember {
		    qformat->fields = $1;
		}
		| queryFieldsList FieldListMember {
		    $1->next = $2;
		    $$ = $2;
		}
		;

FieldListMember: QSTRING {
		    $$ = newFieldListEnt (databaseBeingDefined, qStrVal ($1),
					  NULL);
		    if (parseComplexFieldIndex ($$->ent) != 0)
		      {
			char *msg;

			asprintf (&msg, "Field %s is invalid\n", qStrVal ($1));
			fconferror (msg);
			free (msg);
		      }
		}
		;

inputDescription: INPUTTOK '{' inputEnt '}'
		| INPUTTOK '{' parseError '}'
		;

inputEnt	: inputFields requiredFields {
		    setInputTemplate (databaseBeingDefined, $1);
		}
                ;

requiredFields  :/* empty */
                | REQUIRETOK '{' requiredFieldsList '}' {
		    setRequiredInputFields (databaseBeingDefined, requiredFlds);
                }
                | REQUIRETOK '{' parseError '}' {
                    freeFieldList (requiredFlds);
                    requiredFlds = NULL;
                }
		;

requiredFieldsList: FieldListMember {
		    requiredFlds = $1;
                }
                | requiredFieldsList FieldListMember {
                    $1->next = $2;
                    $$ = $2;
                }
		;

inputFields	: FIELDSTOK '{' inputFieldsList '}' {
		    $$ = inputTemplate;
		    inputTemplate = NULL;
		}
		| FIELDSTOK '{' parseError '}' {
		    $$ = NULL;
		}
		;

inputFieldsList	: QSTRING {
		     $$ = (InputTemplate *) 
		       xmalloc (sizeof (InputTemplate));
		     $$->index = find_field_index (databaseBeingDefined,
						   qStrVal ($1));
		     if ($$->index == InvalidFieldIndex)
		       {
			 char *msg;

			 asprintf (&msg, "Field %s is invalid\n",
				   qStrVal ($1));
			 fconferror (msg);
			 free ($$);
			 free (msg);
			 inputTemplate = NULL;
		       }
		     else
		       {
			 inputTemplate = $$;
			 $$->next = NULL;
		       }
		}
		| inputFieldsList QSTRING {
		     $$ = (InputTemplate *)
		       xmalloc (sizeof (InputTemplate));
		     $$->index = find_field_index (databaseBeingDefined,
						   qStrVal ($2));
		     if ($$->index == InvalidFieldIndex)
		       {
			 char *msg;

			 asprintf (&msg, "Field %s is invalid\n",
				   qStrVal ($2));
			 fconferror (msg);
			 free (msg);
			 free ($$);
		       }
		     else
		       {
			 $$->next = NULL;
			 $1->next = $$;
			 if (inputTemplate == NULL)
			   {
			     inputTemplate = $$;
			   }
		       }
		}
		;

mailFormatList	: mailFormat
		| mailFormatList mailFormat
		;

mailFormat	: mailFormatHeader '{' mailFormatBody '}' {
		    addMessageFormat (databaseBeingDefined, mailFormat);
		    mailFormat = NULL;
		}
		| mailFormatHeader '{' parseError '}' {
		    freeMessageFormat (mailFormat);
		    mailFormat = NULL;
		}
		;

mailFormatHeader: MAILFORMATTOK QSTRING {
  		    mailFormat 
		      = (MailMessageFormat)
		      xmalloc (sizeof (struct mail_message_format));
		    mailFormat->name = takeQString ($2);
		    mailFormat->toAddresses = NULL;
		    mailFormat->fromAddress = NULL;
		    mailFormat->replyTo = NULL;
		    mailFormat->body = NULL;
		    mailFormat->header = NULL;
		    mailFormat->next = NULL;
		}
		;

mailFormatBody	: mailFormatEnt
		| mailFormatBody mailFormatEnt
		;

mailFormatEnt	: bodyFormat {
		    mailFormat->body = qformat;
		    qformat = NULL;
		}
		| headerFormat {
		    mailFormat->header = qformat;
		    qformat = NULL;
		}
		| TOADDRESSESTOK '{' mailAddressList '}' {
		    mailFormat->toAddresses = $3;
		}
		| FROMADDRESSTOK '{' mailAddress '}'  {
		    mailFormat->fromAddress = $3;
		}
		| REPLYTOTOK '{' mailAddressList '}' {
		  mailFormat->replyTo = $3;
		}
		;

bodyFormat	: BODYTOK plainFormat
		;

headerFormat	: HEADERTOK plainFormat
		;

plainFormat	: plainFormatHeader queryFmt '}'
		| plainFormatHeader parseError '}' {
		    freeQueryFormat (qformat);
		    qformat = NULL;
		}
		;

plainFormatHeader: '{' {
		    qformat = (QueryFormat *) xmalloc (sizeof (QueryFormat));
		    qformat->name = NULL;
		    qformat->printf = NULL;
		    qformat->separator = NULL;
		    qformat->fields = NULL;
		    qformat->next = NULL;
		}
		;

mailAddressList	: mailAddress {
		   $$ = (MailAddressList *) xmalloc (sizeof (MailAddressList));
		   $$->address = $1;
		   $$->next = NULL;
		}
		| mailAddressList mailAddress {
		   MailAddressList *lp = $$;
		   while (lp->next != NULL) { lp = lp->next; }
		   lp->next =
		     (MailAddressList *) xmalloc (sizeof (MailAddressList));
		   lp->next->address = $2;
		   lp->next->next = NULL;
		}
		;

mailAddress	: FIXEDTOK QSTRING {
		    $$ = (MailAddress *) xmalloc (sizeof (MailAddress));
		    $$->fixedAddress = takeQString ($2);
		    $$->addresses = NULL;
		}
		| mailAddressTries {
		    $$ = (MailAddress *) xmalloc (sizeof (MailAddress));
		    $$->fixedAddress = NULL;
		    $$->addresses = $1;
		}
		;

mailAddressTries: MailAddressMember {
		    $$ = $1;
		}
		| mailAddressTries '|' MailAddressMember {
		    FieldList p = $$;
		    while (p->next != NULL) 
		      {
			p = p->next;
		      }
		    p->next = $3;
		}
		;

MailAddressMember: QSTRING {
		    $$ = newFieldListEnt (databaseBeingDefined, qStrVal ($1),
					  NULL);
		}
		;

auditTrailFmt	: AUDITTRAILFMTTOK plainFormat {
		    setAuditTrailFormat (databaseBeingDefined, qformat);
		}
		;

/* The flat-file datastore configuration options... */

dsDescription   : indexDescription
		;

indexDescription: indexheader '{' indexlist '}' {
		    setIndexDesc (databaseBeingDefined, indexEntry);
		    indexEntry = NULL;
		}
		| indexheader '{' parseError '}' {
		    freeIndexDesc (indexEntry);
		    indexEntry = NULL;
		}
		| parseError
		;

indexheader	: INDEXTOK {
		    indexEntry = newIndexDesc (databaseBeingDefined);
		}
		;

indexlist	: indexEnt
		| indexlist indexEnt
		;

indexEnt	: PATHTOK QSTRING {
		    setIndexDescPath (indexEntry, qStrVal ($2));
		}
		| FIELDSTOK '{' indexFieldList '}'
		| FIELDSTOK '{' parseError '}'
		| indexSep
		;

indexFieldList	: FieldListMember {
		    addFieldToIndex (indexEntry, $1);
		}
		| indexFieldList FieldListMember {
		    addFieldToIndex (indexEntry, $2);
		}
		;

indexSep	: SEPARATORTOK QSTRING {
		    setIndexDescSeparator (indexEntry, takeQString ($2));
		}
		| BINARYINDEXTOK booleanVal {
		    setIndexDescBinary (indexEntry, $2);
		}
		;

