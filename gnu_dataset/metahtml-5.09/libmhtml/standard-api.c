DOC_SECTION (MISCELLANEOUS-TAGS)
DOC_SECTION (ARRAYS)
DEFUN (pf_array_delete_index,  index arrayvar,
" Delete the element of <var arrayvar> indicated by <var index>.
 The remainder of the array after <var index> is shifted back by one,
 so that the array ends up with one less element than it had before.
")
DOC_SECTION (ARITHMETIC-OPERATORS)
DEFUN (pf_avg,  &rest nums[],
" Returns the average of all of the arguments passed.
 Examples:
 <example>
 <avg 3 4 5>    --> 4
 <avg 4 9 3.2>  --> 5.40
 </example>
")
DOC_SECTION (AUTHORIZATION-TAGS)
DEFUN (pf_basic_auth::authorization_database,  filename type,
" When passed FILENAME, sets the default authorization database
 file to be that fully qualified pathname.  When passed an empty
 FILENAME, returns the current pathname of the authorization
 database.

 The default authorization database used is the standard Meta-HTML
 PowerStrip database.  Note that authorizing based on this method
 does not log the user into the PowerStrip system, it simply
 authenticates them against that database.

 If <var type> is set to \"Meta-HTML\" (the default), then the
 database is assumed to be a standard Meta-HTML/GDBM database,
 with a key value of the username, and a field called PASSWORD.
 The password field will be decoded as a unix based password.

 If <var type> is set to \"Unix\", then the authorization database
 is treated as if it were a standard Unix password file (/etc/passwd).

 If <var type> is set to anything else, then it is assumed to be one
 of the valid SQL database types for your Meta-HTML installation, and
 <var filename> is treated as the <b>DSN</b> to use to talk to that
 database.  The table which is searched is called <b>AUTH</b>, and it
 must contain <b>username</b> and <b>password</b> fields.
")
DOC_SECTION (AUTHORIZATION-TAGS)
DEFUN (pf_basic_auth::force_login_popup,  realm page-var,
" Force the appearance of a popup dialog on the user's screen which asks
 for a username and password.

 <var realm> is a string which is displayed at the top of the popup
 dialog -- it is the variable text in \"Enter username for <var realm>\".

 <var page-var> is the name of a binary variable which contains the
 contents of the page to display in case the user clicks the \"Cancel\"
 button in the dialog box.
")
DOC_SECTION (AUTHORIZATION-TAGS)
DEFUN (pf_basic_auth::gdbm_authorize,  username password,
" Authorize <var username> and <var password> against the GDBM style
 database specified in <var basic-auth::authorization-database>.

 The database must contain fields of <code>username</code>, and
 <code>password</code>.  The password field should be encrypted
 in the fashion normal for that system (i.e., unix::crypt).

 Returns \"true\" if the authentication succeeds, otherwise the empty
 string.
")
DOC_SECTION (AUTHORIZATION-TAGS)
DEFUN (pf_basic_auth::get_auth_info, ,
" Retrieves the authorization info passed to the server from the browser.
 Places the base 64 decoded username in <var basic-auth::username> and
 the base 64 decoded password in <var basic-auth::password>.

 You probably don't want to use this function directly.  See
 <funref AUTHORIZATION-TAGS basic-auth::require-authorization> instead.
")
DOC_SECTION (POWERSTRIP-SYSTEM-TAGS)
DEFUN (pf_basic_auth::powerstrip_login, ,
"")
DOC_SECTION (AUTHORIZATION-TAGS)
DEFUN (pf_basic_auth::require_authorization,  for-realm bad-login-page,
" Force the connecting browser to be authenticated against the local
 authentication database or display a popup window asking the user
 for a username and password.

 <var for-realm> is a string to print at the top of the popup.

 <var  bad-login-page> is the name of a page to redirect to when
 authorization has completely failed.

 The entire authorization step is bypassed if the variable
 <b><code>basic-auth::skip-auth</code></b> is set to a non-empty value.

 Please read the description of <funref authorization-tags
 basic-auth::authorization-database> for more details.
")
DOC_SECTION (AUTHORIZATION-TAGS)
DEFUN (pf_basic_auth::sql_authorize,  username password,
" Authorize <var username> and <var password> against the SQL style
 database whose DSN is in <var basic-auth::authorization-database>.

 The database must contain a table called <code>AUTH</code>, and that
 table must have fields of <code>username</code>, and
 <code>password</code>.

 Returns \"true\" if the authentication succeeds, otherwise the empty
 string.
")
DOC_SECTION (AUTHORIZATION-TAGS)
DEFUN (pf_basic_auth::unix_authorize,  username password,
" Authorize <var username> and <var password> against the Unix style
 password file in <var basic-auth::authorization-database> (usually,
 /etc/passwd).

 Returns \"true\" if the authentication succeeds, otherwise the empty
 string.
")
DOC_SECTION (LANGUAGE-OPERATORS)
DEFUN (pf_bootstrapper::system_initialize, ,
" Defined in libmhtml/standard.mhtml, this function is called immediately
 <i>after</i> the tagsets in libmhtml/tagsets have been initialized by the
 system on bootstrap.  You probably don't know what this means, so just
 forget about it.

 Suffice to say, here is the first user-defined function run by Meta-HTML
 for every invocation of the server, engine, debugger, or standalone
 processor (excepting when that program is called with \"-z\").

 I cannot think of a reason why or how you would modify this function,
 unless you are modifying the behaviour of Meta-HTML in general.  If you
 think that you need to modify this, perhaps you really want something
 like <funref primitive-operators %%after-page-return>.
")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::add_color,  color name &optional nickname,
" Associate <var color> with <var name> and <var nickname> in the system
 color tables and arrays.  <var color> is the hexadecimal representation
 of the color, in standard RRGGBB format.  <var name> is a pretty
 name of the color, as in \"Antique White\".
")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::color_name,  color,
" Given a string of hex digits (perhaps with a leading \"#\"), return
 the human readable name of that color as defined in our array of
 color names.  The color names are definitely suitable for Netscape,
 since I got the names off of their pages.

 <complete-example>
 <color::color-name #F5FFFA>
 </complete-example>
")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::decimal_to_hex,  byte-value,
" Return the two hex characters which represent <var byte-value>,
 a decimal number between 0 and 255 inclusive.

 <complete-example>
 <color::decimal-to-hex 132>
 </complete-example>
")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::hex_value,  color,
" Given the human readable name of a color in <var color>, return the
 color value as a string of 6 hex digits: RRGGBB.

 <complete-example>
 <color::hex-value \"White Smoke\">
 </complete-example>
")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::opposite,  color,
" Return the \"opposite\" of COLOR in HTML format.  COLOR is supplied
 in HTML format as well.  For example:
 <example code><color::opposite #000000> --> #FFFFFF</example>

")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::parse_hex_pair,  hex-pair,
" Return the decimal value of <var hex-pair>.
 <complete-example>
 <color::parse-hex-pair 84>
 </complete-example>
")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::parse_rgb,  rgb-string,
" Return an array of decimal values, one for each color in <var
 rgb-string>.

 <complete-example>
 <color::parse-rgb #ff78e4>
 </complete-example>
")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::select_any_color,  varname,
" Create an HTML SELECTion pull-down menu containing the list of colors
 taken from the list of known colors in the system array
 <var color::color-names>.  Assign the choice to <var varname> when the
 containing form is POSTed.
")
DOC_SECTION (COLOR-MANIPULATION)
DEFUN (pf_color::select_color,  varname,
" Create an HTML SELECTion pull-down menu containing a short list of
 colors including the primaries and hues.  Assign the choice to
 <var varname> when the containing form is POSTed.
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_comma_separated,  arrayvar,
" Produce a human readable string of the elements in the array
 variable <var arrayvar> separated by commas where appropriate,
 and with the word \"and\" after the penultimate item.

")
DOC_SECTION (ARITHMETIC-OPERATORS)
DEFUN (pf_comma_separated_digits,  num,
" Produce NUM in standard human readable format, inserting commas
 where appropriate.

 Example:
 <complete-example>
 <comma-separated-digits 98342367.09>
 </complete-example>
")
DOC_SECTION (VARIABLES)
DEFUN (pf_content_length, ,
" Return the length of the contents of <var var>.
 If <var var> is an array or string, returns the number of
 elements in the array.
 If <var var> is a binary variable, returns the amount of data
 stored within.
 If <var var> is a function, returns the empty string.
 <example>
 <dir::read-file /etc/passwd myvar> --> true
 <content-length myvar>             --> 864
 </example>
")
DOC_SECTION (PACKAGES)
DEFUN (pf_copy_package,  src dest,
" Copy the contents of the package <var src> to the package <var dest>.
")
DOC_SECTION (PAGECOUNT-APPENDIX)
DEFUN (pf_count_this_page,  &optional key,
" Add the current hit on this page to the pagecounter database, and
 return the current number of hits.  The pagecounter database is
 only accessed if this is the first time that this function is
 called on this page.

 If <var key> is not supplied, it defaults to the web-relative
 path to the current document.

 <example>
   <count-this-page> --> 2349
 </example>
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::canonicalize,  date-string,
" Return the canonical version of DATE-STRING, in the format
 \"MM/DD/YY HH:MM:SS\".

 Example:
 <complete-example>
 <date::canonicalize <date>>
 </complete-example>
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::days_in_month,  month,
" Return the number of days in <var month>.
 <var month> is the name of a month, such as \"Apr\" or \"april\".
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::format_time,  format-string &optional time,
" Return a string representation of the date from the time in <var time>.
 If <var time> is not supplied, it defaults to the current time.
 The format string consists of any characters you would like, with the
 following special sets being replaced by their corresponding elements
 of the date.  Please note that case is significant:

 <ul>
 <li> <b>YY</b>:   Last two digits of the year.
 <li> <b>YYYY</b>: Four digits of the year.
 <li> <b>MM</b>:   Two digits of month, with January as <code>01</code>.
 <li> <b>MON</b>:  3 characters of month, as in <code>Jan</code>.
 <li> <b>MONTH</b>: The long name of the month, as in <code>March</code>
 <li> <b>DD</b>:   Two digits of day, as in <code>23</code>.
 <li> <b>DDD</b>:  3 character day of week, as in <code>Thu</code>.
 <li> <b>DAY</b>:  Full name of the weekday, as in <code>Tuesday</code>.
 <li> <b>hh</b>:   Hours, in 24 hour format, as in <code>17</code>.
 <li> <b>mm</b>:   Minutes, as in <code>43</code>.
 <li> <b>ss</b>:   Seconds, as in <code>04</code>.
 </ul>

 Example:
 <complete-example>
 <date::format-time \"DAY, MONTH DD, YYYY (DD-MM-YY) at hh:mm:ss\">
 </complete-example>
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::is_leap_year?,  year,
" Returns \"true\" if <var year> is a leap year.
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::long_month_name,  month,
" Return the canonical long name of <var month>.
 <var month> is the name of a month, such as \"Apr\" or \"april\".
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::month_start_string,  time,
" Return a date string represent the first second of the month specified
 by <var time>.
 <complete-example>
 <set-var now  = <time>>
 <set-var today= <date <get-var now>>>
 <set-var mstart = <date::month-start-string <get-var now>>>
 <pre>
  Now: <get-var today>
 Then: <get-var mstart>
 </pre>
 </complete-example>
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::month_table,  &optional time &key highlight-days[] link-days[] href show-year formatter,
" Create a table representing the month found in <var time>.

 Keyword argument <var highlight-days> is an array of day numbers
 that you would like to have highlighted in the month display.

 Keyword argument <var link-days> is an array of day numbers
 that should be linked to <var href> in the month display.
 
 The arrays may have overlapping members.

 The keyword argument <var href> is the document to go to when a linked
 day is clicked on -- it is passed the day, month, and year, as two
 digit fields in the POSTED package under the variable names
 <var day>, <var month>, and <var year>.

 <html-complete-example>
 <date::month-table
    highlight-days=\"3\n8\" link-days=\"18\n8\" href=<thisdoc>>
 </html-complete-example>
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::month_table_centering_around,  &optional time count &key bgcolor formatter,
" Display a table of <var COUNT> months, centered around <var TIME>.

 <var TIME> defaults to the current time, while <var COUNT> defaults
 to <code>3</code>.
 <html-complete-example>
 <date::month-table-centering-around>
 </html-complete-example>
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::parse_date,  string &optional pack,
" Populates the package <var pack> with the result of parsing
 <var string>.

 If <var string> is passed, that string is used instead of <tag date>,
 and should be the date in human readable format, just as
 <funref language-operators date> returns.

 If no <var pack> is specified, returns an alist of the values instead.
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::seconds_in_month,  month,
" Return the number of seconds in <var month>.
 <var month> is the name of a month, such as \"Apr\" or \"april\".
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::seconds_since_epoch,  date-string,
" Return the number of seconds that have transpired between
 Jan 1, 1970 and <var date-string>.
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::short_month_name,  month,
" Return the canonical short name of <var month>.
 <var month> is the name of a month, such as \"Apr\" or \"april\".
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::skip_days_backward,  count &optional time,
" Return the seconds since the epoch representing the date which is
 exactly the first second of the day which is <var count> days before
 the day represented by <var time>.

 The <i>epoch</i> is defined as Jan 01, 1970, 00:00:00.

 <var time> defaults to the current time.
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::skip_days_forward,  count &optional time,
" Return the seconds since the epoch representing the date which is
 exactly the first second of the day which is <var count> days after
 the day represented by <var time>.

 The <i>epoch</i> is defined as Jan 01, 1970, 00:00:00.

 <var time> defaults to the current time.
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::skip_months_backward,  count &optional time,
" Return the seconds since the epoch representing the date which is
 exactly the first of month which is <var count> months before the
 month represented by <var time>.

 The <i>epoch</i> is defined as Jan 01, 1970, 00:00:00.

 <var time> defaults to the current time.
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_date::skip_months_forward,  count &optional time,
" Return the seconds since the epoch representing the date which is
 exactly the first of month which is <var count> months following the
 month represented by <var time>.

 The <i>epoch</i> is defined as Jan 01, 1970, 00:00:00.

 <var time> defaults to the current time.
")
DOC_SECTION (MACRO-COMMANDS)
DEFUN (pf_defined?,  name,
" Returns \"true\" if <var name> is defined as a <Meta-HTML> primitive or
 a user-defined function, or the empty string otherwise.
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::basename,  pathname,
" Return the \"basename\" of <var pathname>.  This simply returns the last
 component of <var pathname>.

 Example:
 <complete-example>
 <dir::basename /foo/bar/baz.dtd>
 </complete-example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::canonicalize_pathname,  path,
" Make <var path> be absolute in the file system.

 If <var path> starts with a \"/\", then do nothing, otherwise, prefix
 it with <varref mhtml::include-prefix> and
 <varref mhtml::relative-prefix>.

 Given an empty <var path>, return the full path to the directory that
 this page resides in.

 Example:
 <complete-example>
 <dir::canonicalize-pathname foo.mhtml>
 </complete-example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::create_emacs_backup,  pathname &key max force,
" Create an Emacs-style backup of <var pathname>, and return the name
 of the backup file.  No more than <var max> backups may exist --
 if you pass <var max> as a positive integer then multiple backups
 up to that many will be made.  The default value for <var max> is
 infinity.

 If <var pathname> exists, a backup of it is made by appending
 \"~<i>num</i>\" to the name.  If the most recent backup is within
 1 hour of the existing file, no backup is made (unless <var force>
 is \"true\". Thus, we maintain some compatibility with the Emacs
 style of making backup files.

 If <var pathname> is already a backup file, nothing is done.
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::dirname,  pathname,
" Return the directory part of <var pathname>.  This simply
 returns everything but the last component of <var pathname>.

 Example:
 <complete-example>
 <dir::dirname /foo/bar/baz.dtd>
 </complete-example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::filename_extension,  filename,
" Return the filename extension of <var filename>, without the leading
 period.  Returns the empty string if <var filename> doesn't have an
 extension.

 <example>
 <dir::filename-extension /images/foo.gif>       --> gif
 <dir::filename-extension /downloads/foo.tar.gz> --> gz
 <dir::filename-extension /data/file>            -->
 </example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::filename_sans_extension,  filename,
" Return <var filename> without its extension part.
 <complete-example>
 <dir::filename-sans-extension /www/bin/foo.lib>
 </complete-example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::handle_dots,  path,
" Return <var path>, resolving any \"./\" or \"../\" found within.
 Example:
 <complete-example>
 <dir::handle-dots /foo/bar/../baz/./../below-foo>
 </complete-example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::names_in_dir,  &optional dir type matching,
" Return an array of the fully qualified pathnames in <var dir> which
 are of type <var type> (either \"<code>FILE</code>\" or
 \"<code>DIRECTORY</code>\") and which satisfy the regular
 expression <var matching>.

 <var dir> defaults to the directory of the currently executing page.<br>
 <var type> defaults to \"<code>FILE</code>\".<br>
 <var matching> defaults to all possible filenames.
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::read_file,  filename &optional varname,
" Read in the contents of the file named by FILENAME and return them.
 If the optional variable <var varname> is given, it is the name of
 a binary variable to receive the contents of the file.  In that case,
 the word \"true\" is returned if the file was successfully read, or the
 empty string otherwise.

 <example>
 <dir::read-file /tmp/foo.gif gifdata> -->true
 <set-var hosts[]=<dir::read-file /etc/hosts>>
 </example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_dir::write_file,  filename varname,
" Write the contents of the variable named by VARNAME to the file
 named by FILENAME.  Returns \"true\" if the file was written, and
 the empty string otherwise.
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_directory_p,  var,
" Treat the argument <var var> as the name of a variable which contains
 an alist produced by <funref file-operators get-file-properties> or
 <funref file-operators directory-contents>, and return \"true\" if
 the referenced file object is a directory, as opposed to a file.
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_directory?,  pathname,
" Return \"true\" if <var pathname> is a directory, or \"\" if not.
")
DOC_SECTION (DEBUGGING-COMMANDS)
DEFUN (pf_dump_alist,  #alist &key recurse?,
" Dump out the contents of <var alist> in the same fashion as 
 <funref language-operators dump-package> would.
")
DOC_SECTION (DEBUGGING-COMMANDS)
DEFUN (pf_dump_all_packages,  &key recurse?,
" Call <funref language-operators dump-package> on every defined package.
 If the keyword argument <var recurse?> is supplied, it indicates
 that any association lists found as values in the package
 variables should be dumped out as well.
")
DOC_SECTION (DEBUGGING-COMMANDS)
DEFUN (pf_dump_package,  &key recurse? text-mode? &rest packages[],
" Dump the contents of the specified <var packages> in a format
 suitable for including in <code>HTML</code> output.  If the
 keyword argument <var RECURSE?> is non-empty, then recursively
 dump the contents of any alists found as values in <var pack>.
 If the keyword argument <var text-mode?> is non-empty, then
 dump the contents of the packages in a format suitable for
 placing within a &lt;PRE&gt; ... &lt;/PRE&gt; construct.  This
 happens automatically when running under <b>mdb</b>.
")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_e_mail::parse_message,  msgvar,
" Given a complete RFC-822 E-mail message stored in <var msgvar>, this
 function returns an alist describing the message in detail.

 The elements of the alist include the raw headers, and variables
 called <var hdr_TO>, <var hdr_FROM>, <var hdr_SUBJECT>, <var hdr_CC>,
 and <var hdr_DATE>, in additions to the variable <var body>.

")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_e_mail::sendmail,  from to[] cc[] bcc[] headers[] message,
" Send the mail message MESSAGE using a network stream from FROM to TO.

 The host used to connect to for SMTP defaults to the value of
 the variable E-MAIL::MAILHOST, or \"localhost\" if that variable is
 not defined.

 TO, CC, BCC and HEADERS are arrays containing elements of the indicated
 type.

 <example>
 <e-mail::sendmail webmaster@mysite.com bfox@ua.com \"\" \"\"
    \"Subject: Testing e-mail::sendmail
     Reply-to: nobody@nobody.com\"
    \"This is the body of the message.\">
 </example>

 If the variable <var e-mail::debug-sendmail-session> is non-empty,
 it says to populate the array e-mail::sendmail-session with the
 conversation that takes place between <Meta-HTML> and the SMTP
 server at <var e-mail::mailhost>.
")
DOC_SECTION (LANGUAGE-OPERATORS)
DEFUN (pf_eval_file,  filename &optional &unevalled alt,
" Read the contents of <var filename>, and evaluate its contents in the
 context of the caller.  If <var filename> cannot be read, then execute
 the value of <var alt> if it is passed.

 <var filename> is a fully qualified pathname to a file anywhere within
 the file system.
")
DOC_SECTIONS (ARITHMETIC-OPERATORS)
DEFUN (pf_factorial,  x,
" Computes the factorial of <var x>.
 Example:
 <complete-example>
 <factorial 12>
 </complete-example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_file_newer?,  file1 file2,
" Return \"true\" if <var file1> has been written more recently than
 <var file2>.

 Both <var file1> and <var file2> are fully qualified pathnames.
")
DOC_SECTION (ARITHMETIC-OPERATORS)
DEFUN (pf_float, ,
" Returns the floating point representation of <var x>.
 <var x> may be an integer, a floating point number, or a variable
 containing an integer or floating point number.
 <complete-example>
 <set-var foo=7>
 <float foo>
 <float 3>
 <float 3.0>
 </complete-example>
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFMACRO (pf_form_to_database,  database &key dbname key redirect-to action-name,
" Used almost exactly like <code>FORM</code>, this stores the
 values entered into the form into the specified database.  Takes
 keyword arguments of <var dbname>, <var key>, <var redirect-to>,
 and <var action-name>.

 <ul>
 <li> <var dbname>
 The name of the database to store the results in.  If it
 isn't supplied, \"anonymous.db\" is used.  The name of the
 database file is always relative to the current directory.

 <li> <var key>
 The name of the input field which contains the unique key
 for this record.  For example, this might be \"name\" for a
 rolodex application which asked for that information field.

 <li> <var redirect-to>
 The URL of the page to go to after the data is stored.
 If this is not specified, the user remains on the page
 containing this form.

 <li> <var action-name>
 The text to place in the button which submits the form.
 Defaults to \"Submit Form\".
</ul>
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_forms::multiple_instance_resolve,  session-pack dest-pack,
" For every instance of a form in SESSION-PACK, append the
 elements in order to arrays in DEST-PACK.  Thus, if your
 form contained \"name\" and \"age\" input fields, and the user
 created multiple instances of this form, the output of
 calling (forms::multiple-instance-resolve mypack results)
 would be:
 RESULTS::NAME[0] --> Brian
 RESULTS::NAME[1] --> John
 ...
 RESULTS::AGE[0] --> 38  (i.e., Brian's age)
 RESULTS::AGE[1] --> 37  (i.e., John's age).

 You have to call FORMS::MULTIPLE-INSTANCE-RESOLVE for every form
 in your page which used FORMS::MULTIPLE-INSTANCES.  Since this
 requires the use of the session database, you can call
 FORMS::MULTIPLE-INSTANCE-RESOLVE from any page at any time to read
 the contents of the session data, and return the information.
")
DOC_SECTION (HTML-HELPERS)
DEFMACRO (pf_forms::multiple_instances,  session-pack &key modify-hook create-hook remove-hook method action enctype &body &unevalled the-form-body,
" Creates a repeating form, allowing multiple entries of data sets.
 The body of this macro is exactly whatever you might put in a
 standard FORM body.  What is needed here is examples, and there
 really isn't space for any.  Please see the file called
 multiple-instances.mhtml in the distribution for examples.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_forms::standard_buttons,  &optional forms-index,
" Display the standard buttons for input when performing
 <funref HTML-HELPERS forms::multiple-instances>.
 If there are no current instances of the form, this produces
 a language sensitive submit button with the name of \"form-create\" --
 if the instance that this function is called in already has values,
 then two submit buttons are produced, with names \"form-modify\" and
 \"form-accept\".

 If you would like to control the placement of these buttons, then
 you should set the variable <var forms::no-submits>.  In this case,
 you are responsible for calling forms::standard-buttons yourself.
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_function_def,  fun,
" Return a human readable rendition of the function named by <var fun>.
")
DOC_SECTION (USING-GNUPLOT)
DEFUN (pf_gnuplot::make_gif_plot,  data &optional output-file &key style xlabel ylabel title xtics ytics,
" Create a GIF plot of the points in <var data>, using <var style>
 (default 'boxes') and perhaps labelling the X and Y axes.

 The output GIF will be stored in <var output-file>, or returned
 directly to <code>*standard-output*</code>, (in which case, the
 output stream is then closed).

 The default value for <var title> is \"Unspecified Data Graph\".
 
 Possibilities for <var style> are:
 <code>lines</code>, <code>points</code>, <code>linespoints</code>,
 <code>impulses</code>, <code>dots</code>, <code>steps</code>,
 <code>fsteps</code>, <code>histeps</code>, <code>errorbars</code>,
 <code>xerrorbars</code>, <code>yerrorbars</code>,
 <code>xyerrorbars</code>, <code>boxes</code>, <code>boxerrorbars</code>,
 <code>boxxyerrorbars</code>, <code>financebars</code>,
 <code>candlestick</code>s or <code>vector</code>. 

")
DOC_SECTION (USING-GNUPLOT)
DEFUN (pf_gnuplot::make_ps_plot,  data &key style xlabel ylabel title xtics ytics xtics-rotate,
" Create a PostScript plot of the points in <var data>, using <var style>
 (default 'boxes') and perhaps labelling the X and Y axes.

 The default value for <var title> is \"Unspecified Data Graph\".
 
 This function returns the PostScript text necessary to produce
 the graph itself -- the text could be passed to a PostScript
 printer, or, one could use
 <funreg using-gnuplot gnuplot::postscript-to-gif> to
 create a GIF image suitable for displaying on a Web page.

 Possibilities for <var style> are:
 <code>lines</code>, <code>points</code>, <code>linespoints</code>,
 <code>impulses</code>, <code>dots</code>, <code>steps</code>,
 <code>fsteps</code>, <code>histeps</code>, <code>errorbars</code>,
 <code>xerrorbars</code>, <code>yerrorbars</code>,
 <code>xyerrorbars</code>, <code>boxes</code>, <code>boxerrorbars</code>,
 <code>boxxyerrorbars</code>, <code>financebars</code>,
 <code>candlestick</code>s or <code>vector</code>. 

 <example>
 <set-var data = \"20 45 40 47 68 98\">
 <set-var ps = <gnuplot::make-ps-plot <get-var-once data[]> title=\"Foo\">>
 </example>

")
DOC_SECTION (USING-GNUPLOT)
DEFUN (pf_gnuplot::plot_alist_internal,  stream alist,
" You pass in <var stream> and an <var alist> containing:
 <ul>
    <li>DATA-FUNCTION: The name of the function which generates the points
<li>DATA-FUNCTION-ARGS: Arguments to pass to DATA-FUNCTION
		  <li>TITLE: A title for this data (defaults to DATA-FUNCTION).
             <li>STYLE: The style for plotting.  See (gnuplot::make-plot).
             <li>SCALE: A factor for scaling the data.
 </ul>
")
DOC_SECTION (USING-GNUPLOT)
DEFUN (pf_gnuplot::plot_alists,  &key output-type varname output-file rotation xlabel ylabel xtics ytics xtics-rotate boxwidth &rest alists[],
" For now, see <funref using-gnuplot gnuplot::plot-alist-internal>.

 Call `gnuplot' to actually generate the plot from the data.
 The default output type is GIF (works with gnuplot 3.6 and greater).
")
DOC_SECTION (USING-GNUPLOT)
DEFUN (pf_gnuplot::postscript_to_gif,  ps-data &key filename varname,
" Using the PostScript text in <var ps-data>, create a GIF image
 suitable for display from a Web page.

 If <var filename=foo.gif> is specified, then the GIF image will
 be written to \"foo.gif\", and the function returns \"true\" if successful.

 If <var varname=gif-var> is specified, then the GIF image will
 be placed into the binary variable \"GIF-VAR\", and the function returns
 \"true\" if successful.

 If neither <var filename> nor <var varname> is specified, the
 function writes the raw data of the GIF image as an HTML
 document to the standard output, and closes the standard output
 stream.  This is to facilitate the creation of Web pages which
 produce GIF images as output.
")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_http::get_document,  host port path &key strip-headers var &rest headers[],
" Get an HTTP document specified by <var host>, <var port> and <var path>.

 If a redirection is necessary in order to get the final
 document, the variable <code>http::redirected-url</code> is set
 to the fully qualified URI of the document.

 If the document could not be retrieved, the variable
 <code>http::error-status</code> contains the word \"NO-DATA\", and
 the variable <code>http::explanation</code> contains an human
 readable string explaining the problem.

 If <var strip-headers> is given, the HTTP protocol headers are
 stripped from the document before it is returned.

 If the keyword argument <var var> is supplied, it is the
 name of variable that should receive the contents of the HTTP
 document verbatim -- by necessity, this variable is binary.
 You can use the <var content-length> function to find out the
 length of the retrieved document.

 Finally, any extra headers that you would like to pass along may
 be written into the call, perhaps to set a cookie for example.

 <example>
 <http::get-document www.metahtml.com 80 \"/\" \"Cookie: SID=829389834\">
 </example>
")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_http::host_part,  url,
" Return the hostname and port portion of the fully qualified <var url>.
 <complete-example>
 <http::host-part http://www.metahtml.com/page.mhtml>
 </complete-example>
")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_http::include,  url &key varname &rest extra-headers[],
" Include the contents of the Web document referenced by <var url>
 in the page.  If <var varname> is supplied, it is the name of a
 binary variable which will receive the page contents.  Otherwise,
 the document is simply returned.

 <var extra-headers> can be supplied, perhaps to pass a cookie to
 the server:

 <example>
 <http::include http://www.foo.com/cgi-bin/foo.exe \"Cookie: SID=10\">
 </example>
")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_http::path_part,  url,
" Return the path portion of the fully qualified <var url>.
 This is the part of <var url> that is meaningful to the host.
 <complete-example>
 <http::path-part http://www.metahtml.com:8080/page.mhtml?foo=bar>
 </complete-example>
")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_http::port_part,  url,
" Return the port portion of the fully qualified <var url>.
 If the <var url> doesn't have a port specifier, the value \"80\" is
 returned.
 <complete-example>
 <http::port-part http://www.metahtml.com:8080/page.mhtml>
 </complete-example>
")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_http::post,  url pack &key user-agent &rest http-headers,
" POST the variables and values in <var pack> to the server and page
 specified by <var url>.  If the keyword argument <var user-agent>
 is supplied, it overides the default value, which is either the
 value of <var env::http_user_agent>, or the string
 <b><get-var-once http::default-user-agent></b>.
")
DOC_SECTION (NETWORK-APPENDIX)
DEFUN (pf_http::query_part,  url,
" Return the query portion of the fully qualified <var url>.
 If <var url> doesn't have a query string, the empty string
 is returned, otherwise, that portion of <var url> following the
 first question-mark (<code>?</code>) is returned.
 <complete-example>
 <http::query-part http://www.metahtml.com:8080/page.mhtml?foo=bar>
 </complete-example>
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_incdir, ,
" The Web based pathname to the directory containing the document which
 is currently being included with <tag include>.
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_lang,  &key en de fr it es,
" Provides an interface for selecting one of several phrases
 in different languages, based on the value of <var authdata::language>.

 Use in the following situations:
 <example code>
 <input type=submit name=action value=<lang en=Now de=Jetzt>>
 </example>

")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_lang_inform,  &key en de fr it es &rest info,
" This function uses the value of <var authdata::language> to select a
 language, and then calls <var inform> with <info>.
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_lang_message,  &key en de fr it es &rest message,
" This function uses the value of <var authdata::language> to select a
 language, and then calls <var message> with that choice.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::iso_country_name_to_abbrev2,  country-name,
" Returns the two-letter abbreviation of the ISO-3166 country name
 passsed in <var country-name>.  If <var country name> isn't an
 ISO-3166 country name, then it is simply returned as is.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::iso_country_name_to_abbrev3,  country-name,
" Returns the three-letter abbreviation of the ISO-3166 country name
 passsed in <var country-name>.  If there isn't a three-letter
 abbreviation, but there is a two-letter one, then that is returned.
 If <var country name> isn't an ISO-3166 country name, then it is
 simply returned as is.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_common_country,  postname &optional sitename,
" Produce a selection widget suitable for placing in a form which
 allows the user to select a country from a pull-down menu.
 <var postname> is the name of the variable which should be posted
 when the form is submitted.
 <var sitename> is the name of another variable which can contain the
 currently selected country.

 This differs from <funref html-helpers locale::select-country>
 only in the number of countries that are displayed.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_country,  postname &optional sitename,
" Produce a selection widget suitable for placing in a form which
 allows the user to select a country from a pull-down menu.
 <var postname> is the name of the variable which should be posted
 when the form is submitted.
 <var sitename> is the name of another variable which can contain the
 currently selected country -- this variable must be fully
 qualified with its package name (i.e., DEFAULT::FOO, or SITE::COUNTRY).
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_from_country,  postname sitename country-array,
" Produce a selection widget suitable for placing in a form which
 allows the user to select a country from a pull-down menu.
 <var postname> is the name of the variable which should be posted
 when the form is submitted.
 <var sitename> is the name of another variable which can contain the
 currently selected country -- this variable must be fully
 qualified with its package name (i.e., DEFAULT::FOO, or SITE::COUNTRY).
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_iso_country,  postname &optional sitename,
" Produce a selection widget suitable for placing in a form which
 allows the user to select a country from a pull-down menu which
 includes all of the ISO-3166 country names.
 <var postname> is the name of the variable which should be posted
 when the form is submitted.
 <var sitename> is the name of another variable which can contain the
 currently selected country -- this variable must be fully
 qualified with its package name (i.e., DEFAULT::FOO, or SITE::COUNTRY).
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_iso_country_abbrev2,  postname &optional sitename,
" Produce a selection widget suitable for placing in a form which
 allows the user to select an ISO-3166 country name from a pull-down menu.
 <var postname> is the name of the variable which should be posted
 when the form is submitted.  The long name of the country is displayed,
 but the short, two-letter version of the country is returned.
 <var sitename> is the name of another variable which can contain the
 currently selected country.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_iso_country_abbrev3,  postname &optional sitename,
" Produce a selection widget suitable for placing in a form which
 allows the user to select an ISO-3166 country name from a pull-down menu.
 <var postname> is the name of the variable which should be posted
 when the form is submitted.  The long name of the country is displayed,
 but the short, three-letter version of the country is returned.
 <var sitename> is the name of another variable which can contain the
 currently selected country.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_province,  postname sitename,
" Produce a selection widget suitable for placing in a form which
 allows the user to select a province name from a pull-down menu.
 <var postname> is the name of the variable which should be posted
 when the form is submitted.  The long name of the province is displayed,
 but the short, two- or three-letter version of the province is returned.
 <var sitename> is the name of another variable which can contain the
 currently selected province.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_state,  postname sitename,
" Produce a selection widget suitable for placing in a form which
 allows the user to select a US state name from a pull-down menu.
 <var postname> is the name of the variable which should be posted
 when the form is submitted.  The long name of the state is displayed,
 but the short, two-letter version of the state is returned.
 <var sitename> is the name of another variable which can contain the
 currently selected state.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_locale::select_state_or_province,  postname sitename,
" Produce a selection widget suitable for placing in a form which
 allows the user to select a US state or province name from a
 pull-down menu.  <var postname> is the name of the variable
 which should be posted when the form is submitted.  The long
 name of the state or province is displayed, but the short, two-
 or three-letter version of the state or province is returned.
 <var sitename> is the name of another variable which can contain the
 currently selected state or province.
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_mailable_address,  addr,
" Return \"true\" if ADDR appears to be a correctly formatted E-mail address.
")
DOC_SECTION (PACKAGES)
DEFUN (pf_make_alist, ,
" Return an alist from the name=value pairs passed.
 <complete-example>
 <make-alist foo=bar baz=this>
 </complete-example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_make_directories,  path &optional mode-string,
" Create all of the directories in <var path> (a fully qualified path to
 a directory) if they are not already present.
")
DOC_SECTION (PACKAGES)
DEFUN (pf_make_innocuous,  &rest packages[],
" Causes each variable in the specified <var packages> to have
 non-interpretable text.
 This is done by substituting HTML constructs for angle braces,
 among other things.

")
DOC_SECTION (HTML-HELPERS)
DEFMACRO (pf_menu,  title &key nobr,
" Create an HTML menu with title, text and links.
 
 Calling sequence:
 <example>
 <menu \"This Is The Title\">
   This is link text: target.mhtml
   External Site:  http://www.external.com/welcome.mhtml
 </menu>
 </example>
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_message, ,
" Add BODY to the contents of the session variable \"message\".
 Also see <funref html-helpers show-message>.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_meta_html, ,
" Produces the canonical form of the words \"Meta-HTML\".
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_mhtml::engine_per_page_function, ,
" Code that sets up the URL rewriter.  This has to be a per-page function
 because we haven't computed the URL of the document at the time
 the engine.conf file is read, so variables such as mhtml::http-prefix
 aren't set yet, and will be changed at the time the page location is
 computed.
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_number_to_english,  value,
" Take the numeric value of VALUE and produce an English representation
 of it.  For example:
 <complete-example>
 <number-to-english 232>
 </complete-example>
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_number_to_englith,  value,
" Take the numeric value of VALUE and produce an English representation
 of it as a counting value.  For example:
 232 --> Two Hundred and Thirty-Second.
")
DOC_SECTION (ARITHMETIC-OPERATORS)
DEFUN (pf_number?,  arg &key base,
" Returns \"true\" if <var arg> is the string representation of an integer
 in base <var base> (default <code>10</code>), or the string
 representation of a floating point number in base 10.

 For integer checks, the special value of zero (<code>0</code>) for
 <var base> allows the common radixes of decimal, octal, and hexadecimal
 to be understood.  That is to say:
 <example>
 <number? <get-var x> base=0>
 </example>
 is equivalent to:
 <example>
 <or <integer? <get-var x> base=8>
     <integer? <get-var x> base=10>
     <integer? <get-var x> base=16>
     <real?    <get-var x>>>
 </example>

 Some examples:
 <example>
 <number? 10>          --> true
 <number? .9>          --> true
 <number 0xEF base=16> --> true
 </example>
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_nummer_zum_deutsch,  nummer,
" Produziert die Wourter auf Deutsch, das den numerischen Wert des
 <var nummer> darstellt.  Zum Beispiel:
 <complete-example>
 <nummer-zum-deutsch 232>
 </complete-example>
")
DOC_SECTION (MISCELLANEOUS-TAGS)
DEFUN (pf_nummer_zum_deutschte,  nummer,
" Produziert die Wourter auf Deutsch, das den numerischen Wert des
 <var nummer> darstellt.  Zum Beispiel:
 <complete-example>
 <nummer-zum-deutschte 232>
 </complete-example>
")
DOC_SECTION (PAGECOUNT-APPENDIX)
DEFUN (pf_pagecount::available_fonts, ,
" Return an array of the names of the fonts available on this system
 for displaying digit counters.
")
DOC_SECTION (PAGECOUNT-APPENDIX)
DEFUN (pf_pagecount::english_counter, ,
" Produce an English text representation of the number of times
 this page has been accessed.  Implicitly calls the function
 <funref pagecount-appendix count-this-page>.
 
 <example>
 <pagecount::english-counter> --> One Thousand Two Hundred and Fifty
 </example>
")
DOC_SECTION (PAGECOUNT-APPENDIX)
DEFUN (pf_pagecount::englith_counter, ,
" Produce an English text representation of the number of times
 this page has been accessed.  Implicitly calls the function
 <funref pagecount-appendix count-this-page>.
 
 <example>
 <pagecount::english-counter> --> One Thousandth Two Hundred and Fiftieth
 </example>
")
DOC_SECTION (PAGECOUNT-APPENDIX)
DEFUN (pf_pagecount::gif_counter,  fontname,
" Produce a sequence of GIFS representing the number of times this
 page has been accessed.  Implicitly calls count-this-page.
 The gifs are rendered using the font specified in FONTNAME.
")
DOC_SECTION (DATES-AND-TIMES)
DEFUN (pf_parse_date_string,  string,
" Deprecated.  Please use <funref DATES-AND-TIMES date::parse-date> instead.
 <example code> <parse-date-string> </example> is equivalent to
 <example code> <date::parse-date <date> date> </example>
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_path::find_executable,  basename,
" Return the fully qualified path to the program which would be
 executed by CGI-EXEC for BASENAME.

 Example:
 <example>
 <path::find-executable ls> --> /bin/ls
 </example>
")
DOC_SECTION (ARITMETIC-OPERATORS)
DEFUN (pf_percent,  num total,
" Return the percentage of <var total> that <var num> is.
 <example>
 <percent 20 200> --> 10.00
 </example>
")
DOC_SECTION (ARITMETIC-OPERATORS)
DEFUN (pf_percentage,  percent total,
" Return the number which is <var percentage> of <var total>.
 <example>
 <percentage 10 200> --> 20.00
 </example>
")
DOC_SECTION (MACRO-COMMANDS)
DEFUN (pf_primitive?,  name,
" Returns \"true\" if <var name> is defined as a <Meta-HTML> primitive, or
 the empty string otherwise.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_select_option,  varname display &optional value,
" Use instead of the <example code><option></example> HTML tag within
 the select HTML tag.

 Creates an <i>option</i> item in the page in which <var varname>
 is expected to contain the current value of the choice, or nothing,
 if no choice has been made.  If you supply the optional <var value>
 argument, that value is returned instead of <var display>.

 If <var varname> contains <var value>, then this option has the
 HTML <b>SELECTED</b> attribute turned on.

 Example:
 <example>
 <select name=airport>
   <select-option airport Atlanta>
   <select-option airport Boston>
   <select-option airport Chicago>
   <select-option airport \"Los Angeles\">
   <select-option airport \"New York\">
   <select-option airport Miami>
   <select-option airport \"San Francisco\">
 </select>
</example>
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_select_option_value,  varname display &optional value,
" Use instead of the <example code><option></example> HTML tag within
 the select HTML tag.

 Creates an <i>option</i> item in the page in which <var varname>
 is expected to contain the current value of the choice, or nothing,
 if no choice has been made.  If you supply the optional <var value>
 argument, that value is returned instead of <var display>.

 If <var varname> contains <var value>, then this option has the
 HTML <b>SELECTED</b> attribute turned on.

 Example:
 <example>
 <select name=airport>
   <select-option airport Atlanta>
   <select-option airport Boston>
   <select-option airport Chicago>
   <select-option airport \"Los Angeles\">
   <select-option airport \"New York\">
   <select-option airport Miami>
   <select-option airport \"San Francisco\">
 </select>
</example>
")
DOC_SECTION (SESSION-OPERATORS)
DEFUN (pf_session_destroy_package,  &rest packages,
" Delete all of the variables in the current session which are prefixed
 by any of the packages specified in <var packages>.

 Also see <funref packages session-export>.
")
DOC_SECTION (SESSION-OPERATORS)
DEFUN (pf_session::initialize,  &optional timeout timeout-page,
" Create or resume a session for the currently connected browser.
 <var timeout> is the length of time this session should stick around
 in the session database expressed in minutes; it defaults to 200.

 If the variable <code>SESSION::INHIBIT-SESSION</code> is
 non-empty, then no session processing is done.

 If the session has timed out, (detected by the browser passing in a SID
 which doesn't exist in the session database), then a new session is
 created using that SID, and the variable <var session::restarted> is
 set to \"true\".

 If a new session had to be created, either because the browser had
 a SID which was timed out, or because the browser didn't have a SID,
 then the variable <var session::new-arrival> is set to \"true\".

 If you supply <var timeout-page>, and the session was timed out,
 then the browser is redirected to <var timeout-page>, and the
 session is not instantiated in the session database (although the
 aforementioned variables are still set).
")
DOC_SECTION (SESSION-OPERATORS)
DEFUN (pf_session::reset_cookie, ,
" Place this at the end of a page which is supposed to timeout the
 user's session.  It sends the browser a Set-Cookie header which
 makes the cookie go away.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_show_message, ,
" Display the contents of the session variable \"message\",
 and then remove that variable from the session.
 Also see <funref html-helpers message>.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_source_link,  text,
" You write `Click <source-link here> for source.'
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_source_of,  web-path highlight-keywords-p,
" Display the contents of <var web-path> in <code>HTML</code>.
 Use sparingly.
 If <var highlight-keywords-p> is non-empty, then make each
 <Meta-HTML> function in the displayed text be a link which will
 display the contents of the the manual page for that keyword.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::admin_prog, ,
" Returns the complete path of the executable program which is used to
 perform administration functions on the undrlying SQL database.
 For example, <code>/usr/local/Hughes/bin/msqladmin</code>, or
 <code>/usr/local/bin/mysqladmin</code>.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::affected_rows,  cursor,
" Return the number of rows affected by the last SQL query execution,
 associated with the database referenced indirectly by <var cursor>.
 This may be completely unrelated to the query which created the
 cursor object, depending on the underlying database which is in
 use.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::allow_system_tables,  &optional allow?,
" For those underlying database engines which keep information
 about the database structure within the database itself, a
 request to <example code><sql::host-databases ...></example>
 can return a great deal of information, most of which is
 uninteresting to an application.  Call this function with an
 argument of \"true\" to allow such system tables to be returned,
 or with no argument to disallow them.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::cursor_get_column,  cursor column-number,
" Called with a <var cursor> (the result from a call to
 <funref generic-sql-interface sql::database-exec-query>),
 which has been filled in by at least one call to
 <funref generic-sql-interface sql::database-next-record>,
 this function returns the data from the current result set,
 in the column numbered <var column-number> (starting at column 0).

 If there is no result set, or if the result set is empty, the
 empty string is returned.

 <example>
 <sql::with-open-database db dsn=\"host=localhost;database=admin\">
   <set-var query=\"select * from users\">
    <sql::database-exec-query db <get-var query> cursor=row>
    <set-var any? = <sql::database-next-record row>>
    <when <get-var any?>>
      Result: <sql::cursor-get-column row 0>
    </when>
 </sql::with-open-database>
 </example>
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_column_info,  dbvar table fieldname,
" Returns an association list of properties on the specified
 <var column> in the table <var table>.

 Example:
 <pre>
 ((NAME . realname)
  (LENGTH . 30)
  (TYPE . GSQL_CHAR)
  (IS_UNIQUE . \"\")
  (IS_NULLABLE . true)
  (QUALIFIER . \"\")
  (OWNER . \"\")
  (TYPENAME . \"\")
  (PRECISION . 0)
  (SCALE . 0)
  (RADIX . 0)
  (TABLE . accounts))
 </pre>

 This is actually implemented by fetching information on <i>all</i>
 columns and selecting the specified one.  This means that you should
 use the command
 <funref generic-sql-interface sql::database-columns-info> if you
 are looking for information on several columns, as opposed to
 the specific column mentioned by <var column>.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_columns,  dbvar tablename &key result=varname,
" Returns a newline separated list of the columns present in <var
 tablename> in the open SQL database referenced by <var dbvar>.

 The keyword <var result=varname> argument specifies the name of
 a variable to put the result array into; if not present, the
 array is returned to the caller.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_columns_info,  dbvar table &key result=varname,
" Analogous to <funref generic-sql-interface sql::database-column-info>,
 this function returns an <i>array</i> of association lists where
 each entry in the array is the information about a single column.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_delete_record,  dbvar key &key table=tablename keyname=fieldname,
" Deletes the specified record in the database referenced by
 <var dbvar>. The record to delete is specified by <var key> and
 <var keyname>. An SQL <code>DELETE</code> query is issued which
 will delete all records in the table named <var table> whose column
 named <var fieldname> matches the value of <var key>.

 You must supply values for <var table> and <var keyname>.

")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_exec_query,  dbvar query-string &key cursor=varname,
" Executes the SQL query in <var query-string> by sending it to
 the open database connection referenced by <var dbvar>.

 <var query-string> can be any <Meta-HTML> expression which
 evaluates to a valid SQL query or command string.

 Returns a cursor object, which can be passed to
 <funref generic-sql-interface sql::database-next-record>,
 <funref generic-sql-interface sql::number-of-rows>, or
 <funref generic-sql-interface sql::set-row-position>.

 If the keyword argument <var cursor=varname> is given, the
 returned cursor value is stored in <var varname>.

")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_exec_sql,  dbvar query-string,
" Executes the SQL query in <var query-string>, on the open
 database connection specified by <var dbvar>.

 Returns TRUE if no errors are encountered.

 This function differs from
 <funref generic-sql-interface sql::database-exec-query> in that
 it does not return any cursor; i.e., there is no way to get
 results back from the execution of your SQL command. Thus, it is
 most useful for sending commands which expect no return values,
 such as a SQL DELETE statement:

 <example>
 <sql::database-exec-sql 
    db \"DELETE FROM maillists WHERE list = '<get-var listname>'\">
 </example>
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_load_record,  dbvar key &key table=tablename keyname=fieldname package=packagename,
" Load variable values from the database referenced by <var dbvar>
 and <var key> into the package specified by <var packagename>
 (defaulting to the current package if not supplied).

 <var tablename> and <var fieldname> must be supplied -- they
 specify the table and primary key field to be operated on.

")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_next_record,  cursor &key colnames=namelist prefixtablenames=true package=packagename,
" Fill the variables in <var packagename> (defaulting to the
 current package if not supplied) with the values of the next
 record which last satisfied the search referenced by <var cursorvar>.

 Each subsequent call gets a subsequent record from the list of
 results, until all of the results are exhausted.

 Returns <code>true</code> if there are any records left in the
 search results, or the empty string if not.

 If <var colnames> is supplied, then the column values of this
 result are bound sequentially to the comma-separated list of
 names in <var namelist> instead of the field names in the result
 set.

 <example>
 <sql::database-next-record db cursor package=tmp
             colnames=\"a.name, a.partnum, b.name\">
 <get-var tmp::b.name>
 <get-var tmp::a.name>
 </example>

 If <var colnames> is non-null, or the database connection option
 <code>SQL-PREFIX-TABLENAMES</code> is non-null, then for each
 column in the result set, use the column's table name, if it
 exists, as a prefix to the column name as the variable name.

")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_query,  dbvar query &key colnames=namelist prefixtablenames=true format=fexpr keys=varname keyname=fieldname,
" Select and optionally format records in the database referenced
 by <var dbvar> according to the criterion in <var fexpr>.

 <var query> is an SQL query, which is expected to return a list of rows.

 For each result row, <var expr> is then evaluated, with the
 column values of each row bound to their corresponding column
 name. If the result of that evaluation is not an empty string,
 then that record is selected for further processing by either
 <var format>, <var keys>, or to return in plain text the list of
 keys. If <var format=fexpr> is present, <var fexpr> is an
 expression to evaluate in the context of the database fields,
 (as with <var expr>).

 The value of \"true\" for <var expr> is specially optimized to
 for those queries where no additional processing is necessary.

 The example below shows an SQL query which is formatted as rows
 of an HTML table. In this case, the <var expr> is simply the
 constant <code>true</code>, and all of the selection of records
 is done via the SQL query itself. (Note that <var expr> could be
 used to impose additional conditions to decide whether to invoke
 <var fexpr> on a row.

 <example>
 <table border=1>
   <tr> <th>Name</th> <th>Age</th> </tr>
   <sql::with-open-database db dsn=\"DATABASE=ROLO;HOST=localhost\">
     <sql::database-query db true
         \"SELECT * FROM people
          WHERE name like 'Washington' ORDER BY lastname\"
         format=<group <tr>
                       <td> <get-var lastname>, <get-var firstname> </td>
                       <td> <get-var age> </td>
                       </tr>>>
   </sql::with-open-database>
 </table>
 </example>

 If <var keys=varname> is given, then <var varname> is the name
 of a variable to receive the array of keys which satisfied
 the query.  If you specify an argument for <var keys>, you must
 also specify which column to collect the values from, using the
 <var keyname=fieldname> keyword argument.

 If the optional argument <var colnames=namelist> is supplied,
 then for each row, column values are bound sequentially to these
 comma separated names instead of the column names in the result set.

 <example>
 <sql::database-query db true <get-var query>
    colnames=\"foo.name, bar.name\"
    format=<group FOO.NAME IS <get-var foo.name> 
                  BAR.NAME IS <get-var bar.name>>>
 </example>

 If <var prefixtablenames> is non-null, or the database
 connection option <code>SQL-PREFIX-TABLENAMES</code> is
 non-null, then for each column in the result set, use that
 column's table name as a prefix to the column name as the
 variable name.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_query_info,  cursor &key result=varname,
" Returns an array of association lists giving information about
 each column in the result set specified by <var cursor>.
 
 If the keyword argument <var result=varname> is given, the
 resultant array is placed into that variable -- otherwise it is
 simply returned.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_save_package,  dbvar key package &key table=tablename keyname=fieldname,
" Save the variables in <var package> associated with the value
 <var key> in column <var keyname> in the table <var tablename>
 of the database referenced by <var dbvar>.

 This only saves variables which have names matching existing
 table column names. Package prefixes are stripped from the
 variables, and the symbol name is used as the column name to
 store the data. Symbol names and column names are treated in a
 case-insensitive manner.

 The implementation is to first attempt to do a SQL
 <code>INSERT</code> into the table, and if that fails to try a
 <code>UPDATE</code> query, with <var keyname> = <var key>.

 Example: Say we want to save some information about employee
 Kate Mulgrew, in a table of employees, which has a primary key
 field named \"id\", and we want to save this record with id=103:

 <example>
 <set-var record::name=\"Kate Mulgrew\" record::age=45
          record::salary=34000 record::dept=travel>
.blank
 <set-var saved? =
    <sql::database-save-package db 103 record table=employees keyname=id>>
 </example>

 If a variable in the package you are saving corresponds to a
 column with a numeric field type, and the value of the variable
 is the empty string, the system will attempt to store a NULL
 value into this field of the record. If the table does not
 support NULL values on that column, the operation will fail.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_save_record,  dbvar key &optional var... &key table=tablename key=fieldname,
" Write a record to the open SQL database connection referenced by
 <var dbvar>, using <var key> as the value of the primary key, on
 the table <var table>.  The keyword argument <var KEY> is the
 SQL column name of the column that you are specifying as the
 primary key.

 For each <var var>, treat the name of <var var> as a column
 name, and set that column's value to the value stored within
 <var var>.

 An <code>UPDATE</code> is initially attempted to store the data.
 If the <code>UPDATE</code> fails, then an <code>INSERT</code> is
 attempted.

 NOTE: If the key field for table you are using is not configured
 as the primary key, then it is possible to create duplicate entries
 in the database.

 If you pass in a variable <var var> which does not match a column
 name in <var table>, that variable is silently ignored.

 If you try to save character data into a numeric field, the
 <code>INSERT</code> or <code>UPDATE</code> will fail.

 As always, the most recent query executed is available by
 calling the function <funref generic-sql-interface sql::recent-query>,
 and error messages may be retrieved by calling the function
 <funref generic-sql-interface sql::sql-error-message>.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_set_options,  dbvar &key option-name=value,
" Sets various database-specific options.

 Possible values for <var option-name> are:
 <ul>
 <li> <b>SQL-ESCAPE-CHARACTER-CODE</b>:
 The character which is used to escape single quotes in strings.
 <li> <b>SQL-TRUNCATE-COLUMNS</b>:
 When set to a non-empty string, queries composed by
 <funref generic-sql-interface sql::database-save-record> and
 <funref generic-sql-interface sql::database-save-package> will have
 character string field data automatically truncated to the maximum
 column width of the field being stored into, before the query is
 ever sent to the SQL server.
 <li> <b>SQL-PREFIX-TABLENAMES</b>:
 For <funref generic-sql-interface sql::database-next-record> and
 <funref generic-sql-interface sql::database-query>, prefix the names
 of symbols for column of the result set with the name table to which
 the column belongs.
 </ul>
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_table_info,  dbvar &optional tablename &key result=varname tabletype=regexp tablequalifier=regexp tableowner=regexp,
" Returns an array of association lists, one per table in the
 open SQL database referenced by <var dbvar>.

 Each alist contains database-specific information about the
 table, including at least the <i>name</i> of the table.
 <example>
 ((name . \"table_name\"))
 </example>

 The keyword arguments starting with
 <code>\"table...\"</code> are ANSI SQL regular expression
 patterns.  They are used if supplied, ignored if not.

 The keyword <var result=varname> argument specifies the name of
 a variable to put the result array into; if not present, the
 array is returned to the caller.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_tables,  dbvar &key result=varname,
" Return the names of the tables in the database referenced by
 <var dbvar> as a newline separated list. If <var varname> is
 supplied, it is the name of the variable to receive the table
 names.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::database_tables_info,  dbvar &optional tablename &key result=varname tabletype=regexp tablequalifier=regexp tableowner=regexp,
" Returns an array of association lists, one per table in the
 open SQL database referenced by <var dbvar>.

 Each alist contains database-specific information about the
 table, including at least the <i>name</i> of the table.
 <example>
 ((name . \"table_name\"))
 </example>

 The keyword arguments starting with
 <code>\"table...\"</code> are ANSI SQL regular expression
 patterns.  They are used if supplied, ignored if not.

 The keyword <var result=varname> argument specifies the name of
 a variable to put the result array into; if not present, the
 array is returned to the caller.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::get_database_type, ,
" Return the type of the underlying SQL database which will be accessed
 by the various SQL::... functions.  The possibilities are one of
 
 <ul>
 <li> <b>msql</b>: The Minerva mSQL database from Hughes,
 <li> <b>mysql</b>: The mySQL database, or
 <li> <b>odbc</b>: A generic ODBC database, such as Oracle or Sybase.
 </ul>
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::host_databases,  &optional host,
" Return an array of the database names present on <var host>
 (defaults to <code>localhost</code>.

 This command is only meaningful to the <code>mSQL</code> and
 <code>mySQL</code> database engines -- calling it on
 <code>odbc</code> databases always returns an empty array.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::number_of_rows,  cursor,
" Return the number of rows SELECTed by the last SQL query execution,
 associated with the database referenced indirectly by <var cursor>.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::query_get_column, ,
" Undocumented.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::recent_query, ,
" Returns the last SQL query statement that was sent to the underlying
 SQL database.  Can be useful in debugging.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::search_form_params,  alist &key dsn table key editurl defcol title,
"
 Get or set the standard search form parameters.
 Always returns an association list of the current search form
 parameters, and can optionally set them to new values.  In
 typical usage, this function might be called as follows:

 <example>
 <set-var orig-params =
    <sql::search-form-params DSN=<mydsn> table=CLIENTS>>
 <sql::standard-search-form posted>
 <set-var ignore = <sql::search-form-params <get-var-once orig-params>>>
 </example>


 We have provided a simple but useful generic search form for use in
 your Meta-HTML pages.  Calling
 <funref generic-sql-interface sql::standard-search-form> both produces
 the search form, and the results.

 You should set the following variables before calling
 <var sql::standard-search-form>:

 <ul>
 <li> sql::search-dsn -- The DSN string for the database.
 <li> sql::search-dbtable -- The name of the table in the database to search.
 <li> sql::search-keyname -- The name of the key field in the database.
 <li> sql::search-editurl -- The url of a single record editor.
 <li> sql::search-defcol -- The default column to display in the list.
 <li> sql::search-title -- The title to display in the input form.
 </ul>

 Optionally, you can specify columns that you would like not to be
 offered as columns that may be searched for, in the array
 sql::search-ignore-cols[].
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::set_database_type,  type,
" Create a command set called SQL::... which contains all of the functions
 normally used for accessing ODBC, mySQL, or mSQL databases.  This allows
 you to write a complete application using the mySQL database,
 <funref generic-sql-interface sql::with-open-database> and
 friends, and then to painlessly \"port\" it to ODBC, simply by calling this
 function with an argument of <code>odbc</code>.
 
 The possible values for <var type> are
 
 <ul>
 <li> <b>msql</b>: The Minerva mSQL database from Hughes,
 <li> <b>mysql</b>: The mySQL database,
 <li> <b>pgsql</b>: The PostGres database, or
 <li> <b>odbc</b>: A generic ODBC database, such as Oracle or Sybase.
 </ul>
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::sql_error_message, ,
" Return the current error message from recent SQL statements.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::sql_transact, ,
" Currently unimplemented.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::standard_search_form,  &optional package &key fsize href title,
" Display a complete form suitable for searching for a record from
 the database and table specified in <code>sql::search-dsn</code> and
 <code>sql::search-dbtable</code>.

 Optional keyword argument HREF specifies the destination page --
 it defaults to the current page if not supplied.
 Optional keyword FSIZE is the size of the font to use in the form --
 this has the effect of growing or shrinking the form.

 When this form is posted to, it gathers all of the
 columns which match the specified search criteria, and then
 displays the results in a table, linking each to the
 sql::search-editurl.  When only one result is found, redirects
 to the page specified in <code>search::editurl</code>, providing a
 message in the session, displayable with
 <funref html-helpers show-message>, which explains why.

 For more information on what happens when this form is posted to,
 please see <funref generic-sql-interface
 sql::standard-search-form-handler>.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::standard_search_handler,  package &key fsize href,
" Perform a database search on the search table using the criteria
 in PACKAGE.  Variables in PACKAGE must include:

 <ol>
 <li> csf-search   (simply must be set)
 <li> csf-column   (the name of the column to search on)
 <li> csf-operator (either \"LIKE\" \"NOT LIKE\" or \"=\")
 <li> csf-value    (the text to search for)
 </ol>

 Produces a table with the contents of the specified column linked
 with an href to sql::search-defurl?<cgi-encode sql::search-keyname>.

 You can overide the information which is displayed for each record
 that matches the search criteria by creating a function called
 <b>sql::search-result-name</b>.  If this function exists, it should
 take two arguments: the column name that was being searched for, and
 an association list representing the data of the current record.

 An example might be to always display the last and first name of
 any search where the column name to be searched contained the
 string \"name\":
 <example>
 <defun sql::search-result-name col alist>
   <if <match <get-var-once col> \"name\" caseless=true>
       <concat <alist-get-var alist last_name>
               \", \"
               <alist-get-var alist first_name>>
     <alist-get-var alist <get-var-once col>>>
 </defun>
 </example>
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFUN (pf_sql::translate_column,  col,
" Return a pretty version of the column name specified in <var col>,
 by translating it through the array <var sql::column-translations>.
 Each member of that array is a column name followed by a colon,
 followed by a pretty version of that column name.
 If <var col> doesn't appear in the list of possible names, then
 it is simply capitalized.
")
DOC_SECTION (GENERIC-SQL-INTERFACE)
DEFMACRO (pf_sql::with_open_database,  dbvar &key dsn=dsn-string nolock=true,
" Opens the database specified by DSN (database service name)
 string, and stores a referent to it in <var dbvar>.  If the
 connection fails, the value of <var dbvar> is the empty string.

 <var DSN> should contain a minimum of
 \"HOST=hostname;DATABASE=dbname\" in order to connect successfully
 to an mSQL database.  Although the older <var HOST=host> <var
 DATABASE=dbname> keyword arguments are still supported, they
 have been deprecated, and the <var DSN=dsn-string> form is
 preferred.

 If <var nolock=true> is supplied, then no lock file on the server is
 created.  This is useful when you don't want to block other processes
 from accessing the database for the duration of the
 <code>sql::with-open-database</code> form.
")
DOC_SECTION (STRING-OPERATORS)
DEFUN (pf_string_to_array,  string arrayvar,
" Create an array in <var arrayvar> which is made of of the individual
 characters of <var string>.  Given the following:
 <example>
 <set-var s=\"This is a string.\">
 <string-to-array <get-var-once s> chars>
 </example>
 Then, <example code><get-var chars[3]></example> returns <code>s</code>.

")
DOC_SECTION (STRING-OPERATORS)
DEFUN (pf_strings::both_trim,  varname,
" Obsoleted by <funref string-operators strings::trim>.
")
DOC_SECTION (STRING-OPERATORS)
DEFUN (pf_strings::left_trim,  varname,
" Trims whitespace from the \"left-hand\" side of the string stored
 in <var varname>, replacing the contents of that variable with
 the trimmed string.
 <complete-example>
 <set-var foo=\"    string with whitespace on the left\">
 String: [<get-var-once foo>]
 <strings::left-trim foo>
 String: [<get-var-once foo>]
 </complete-example>
")
DOC_SECTION (STRING-OPERATORS)
DEFUN (pf_strings::right_trim,  varname,
" Trims whitespace from the \"right-hand\" side of the string stored
 in <var varname>, replacing the contents of that variable with
 the trimmed string.
 <complete-example>
 <set-var foo=\"string with whitespace on the right     \">
 String: [<get-var-once foo>]
 <strings::right-trim foo>
 String: [<get-var-once foo>]
 </complete-example>
")
DOC_SECTION (STRING-OPERATORS)
DEFUN (pf_strings::trim,  varname,
" Trims whitespace from both the \"left\" and \"right\" -hand sides
 of the string stored in <var varname>, replacing the contents of
 that variable with the trimmed string.
 <complete-example>
 <set-var foo=\"    string with whitespace on left and right     \">
 String: [<get-var-once foo>]
 <strings::trim foo>
 String: [<get-var-once foo>]
 </complete-example>
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_thisdir, ,
" The fully qualified pathname to the directory containing the currently
 executing document.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_thisdoc, ,
" The complete URL to this document, without the query string or
 path information.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_thisfile, ,
" The fully qualified pathname to the document which is currently
 executing.
")
DOC_SECTION (HTML-HELPERS)
DEFUN (pf_thispage, ,
" The local web-based URL to this document, without the query string
 or path information.
")
DOC_SECTION (MACRO-COMMANDS)
DEFUN (pf_user_function?,  name,
" Returns \"true\" if <var name> is defined as a user function, or
 the empty string otherwise.
")
DOC_SECTION (FILE-OPERATORS)
DEFUN (pf_webpath_exists,  document-path,
" Returns \"true\" if the specified <var document-path> exists with
 \"/\" being equal to the document root directory.
")
DOC_SECTION (LANGUAGE-OPERATORS)
DEFMACRO (pf_with_safe_environment, ,
" Execute <var body> in an environment where commands that could
 affect the file system are disabled.  In addition, commands
 which can define or undefine functions are disabled.

 This command is especially useful when filtering input from an
 external source, such as a third party Web page.
")
