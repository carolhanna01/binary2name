proc helpMsg {field} {
    global TkGnats help_text env
    
set help_text(TkGnats_Version) "\n$TkGnats(tkgnats_version)\n\nYou are running [info nameofexecutable] patchlevel [info patchlevel]."

if {$field == "Changes"} {
    show_help $field [file_get_text $TkGnats(TKGNATSLIB)/CHANGES]
    return
}

if {$field == "TkGnats_Version"} {
    if {[info exists TkGnats(Servers)]} {
        append help_text($field) "\n\nServers: $TkGnats(Servers)"
    }
    show_help $field $help_text($field)
    return
}

set help_text(TkGnats_Variables) $help_text(TkGnats_Version)\n
foreach t [lsort [array names TkGnats]] {
    if {[string first "Password" $t] < 0} {
        append help_text(TkGnats_Variables) "[format "\n%-24s:" $t] $TkGnats($t)"
    }
}

set help_text(TkGnats_About) $help_text(TkGnats_Version)
if {$TkGnats(GNATS_ACCESS) == "network"} {
    if {[info exists TkGnats(GNATS_SERVER)] && $TkGnats(GNATS_SERVER) != ""} {
        set host " (host: $TkGnats(GNATS_SERVER) port: $TkGnats(GNATS_PORT))"
    } {
        set host ""
    }
    append help_text(TkGnats_About) "\n\nTkGnats is accessing a network GNATS daemon.$host\n"
} {
    append help_text(TkGnats_About) "\n\nTkGnats is accessing a local GNATS database.\n"
}

append help_text(TkGnats_About) "\nGNATS_Version:       $TkGnats(GNATS_Version)"
append help_text(TkGnats_About) "\nGNATS_ACCESS:        $TkGnats(GNATS_ACCESS)"
if {$TkGnats(GNATS_ACCESS) == "local"} {
    append help_text(TkGnats_About) "\nGNATS_USER:          $TkGnats(GNATS_USER)"
} {
    append help_text(TkGnats_About) "\nGNATS_SERVER:        $TkGnats(GNATS_SERVER)"
    append help_text(TkGnats_About) "\nGNATS_PORT  :        $TkGnats(GNATS_PORT)"
    if {[info exists TkGnats(ServerInfo)]} {
        append help_text(TkGnats_About) "\nServerInfo:          $TkGnats(ServerInfo)"
    }
}

append help_text(TkGnats_About) "\nGNATS_ACCESS_METHOD: $TkGnats(GNATS_ACCESS_METHOD)"

if {$TkGnats(GNATS_ACCESS_METHOD) == "batch"} {
    append help_text(TkGnats_About) "\nGNATS_ROOT:          $TkGnats(GNATS_ROOT)"
    append help_text(TkGnats_About) "\nLIBEXECDIR:          $TkGnats(GNATS_LIBEXECDIR)"
}

append help_text(TkGnats_About) "\nGNATS_ADDR:          $TkGnats(GNATS_ADDR)"
append help_text(TkGnats_About) "\nSUBMITTER:           $TkGnats(SUBMITTER)"
append help_text(TkGnats_About) "\nEmailAddr:           $TkGnats(EmailAddr)"
append help_text(TkGnats_About) "\nLogName:             $TkGnats(LogName)"
append help_text(TkGnats_About) "\nFullName:            $TkGnats(FullName)"
append help_text(TkGnats_About) "\nGroupName:           $TkGnats(GroupName)"
append help_text(TkGnats_About) "\nORGANIZATION:        $TkGnats(ORGANIZATION)"
append help_text(TkGnats_About) "\nHOSTNAME:            $TkGnats(HOSTNAME)"

append help_text(TkGnats_About) "\nMailMethod:          $TkGnats(MailMethod)"
if {$TkGnats(MailMethod) == "mailer"} {
    append help_text(TkGnats_About) "\nMailer:              $TkGnats(Mailer)"
} {
    append help_text(TkGnats_About) "\nSMTP_SERVER:         $TkGnats(SMTP_SERVER)"
    append help_text(TkGnats_About) "\nSMTP_PORT  :         $TkGnats(SMTP_PORT)"
}
append help_text(TkGnats_About) "\nQuerySortMethod:     $TkGnats(QuerySortMethod)"
append help_text(TkGnats_About) "\nQueryMode:           $TkGnats(QueryMode)"

if {[info exists TkGnats(ReleaseBased)]} {
    append help_text(TkGnats_About) "\nReleasedBased:       $TkGnats(ReleaseBased)"
}
if {[info exists TkGnats(TKGNATSINI)]} {
    set ini $TkGnats(TKGNATSINI)
} {
    set ini ""
}
append help_text(TkGnats_About) "\nTKGNATSINI:          $ini"
append help_text(TkGnats_About) "\nWISHPATH:            $TkGnats(WISHPATH)"
append help_text(TkGnats_About) "\nTKGNATSLIB:          $TkGnats(lib)"
append help_text(TkGnats_About) "\nSiteServerDir:       $TkGnats(SiteServerDir)"
append help_text(TkGnats_About) "\nUserAccess:          $TkGnats(UserAccess)"
append help_text(TkGnats_About) "\nUserDir:             $TkGnats(UserDir)"
append help_text(TkGnats_About) "\nUserServerDir:       $TkGnats(UserServerDir)"
append help_text(TkGnats_About) "\nedit_authorized:     $TkGnats(edit_authorized)"
append help_text(TkGnats_About) "\ndelete_authorized:   $TkGnats(delete_authorized)"
append help_text(TkGnats_About) "\nDialogFont:          $TkGnats(DialogFont)"
append help_text(TkGnats_About) "\nTestFont:            $TkGnats(TextFont)"
append help_text(TkGnats_About) "\nHelpFont:            $TkGnats(HelpFont)"

set help_text(Cut_Copy_Paste) {
    Selections and Cut, Copy and Paste operations

    Text is selected by clicking and dragging the mouse. Control-/
    selects the entire contents of the widget, and Control-\ clears
    any selection in the widget. There are many other selection
    bindings. See the Tk manual page for the Text and Entry widgets.
    
    On UNIX, selected text is exported to the Xselection buffer and
    can be pasted directly into Tk widgets with the middle mouse
    button or the Insert key. This is true even for text selected in
    other UNIX applications such as xterms, Emacs, etc. Text selected
    in Tk widgets can also be pasted into other UNIX applications with
    the middle mouse button. One problem with pasting text into Tk
    widgets is that you have to hold the mouse very still when you
    press and release the mouse button. If it moves even one pixel,
    the mouse motion is detected and it interprets your movement to be
    a scrolling action.

    Besides the Xselection buffer, UNIX also has a "Clipboard" buffer,
    which Windows also has.
    
    On both UNIX and Windows, you can Cut or Copy the selection into
    the Clipboard with the ^x, ^c keys respectively. ^v pastes the
    clipboard contents into the widget. On Sun workstations, the
    Keyboard keys labelled Cut, Copy and Paste also do these
    functions.

    Cut/Copy/Paste/SelectAll operations can be found on the right
    mouse button pop-up menu.
}
    

set help_text(Edit_Overview) {
Overview of Editing Problem Reports

     Make your changes, and click "Update edits"!

     If you change the Responsible or the State, you will be prompted
     for a reason for the change. This gets stored in the Audit-Trail
     for the problem report.
}
 
set help_text(Edit_Radio_Buttons) {
Radio Buttons: Class, State, Priority, Severity and Confidential

     Clicking the buttons selects the value. Please use reasonable
     priorities when reporting problems.

     Click the heading title (eg Class:) for a definition of the
     field itself.
}

set help_text(Edit_Listbox_Selectors) {
Listbox Selectors: Category, Submitter-Id and Responsible

     Click on a listbox item and it gets entered into the entry box for
     you.

     The entry box uses a quick-fill feature for easy entry if you
     choose not to scroll the listbox looking for an item. As you type
     in each character, it automatically completes the value according
     to the available items in the listbox. It will not allow you to
     enter anything that isn't in the listbox.

     Click the heading title (eg Category:) for a definition of the
     field itself.

     Besides the scroll-bar, you can also scroll the listbox with the
     middle mouse button. Just click and hold the middle button anywhere
     in the listbox data area, and drag it vertically.
}
   
set help_text(Edit_Entry_Fields) {
Entry Fields: Originator, Notify-List, Release and Synopsis

     These are one-line, free-format text entry fields.

     For Notify-List, TkGnats expects one or more valid
     email-addresses for any additional people interested in the
     progress of this bug report. They will be copied on all
     email. The list must be comma-separated. If domain names
     are not specified (@somewhere.something) they will be added
     if you have set TkGnats(DefaultDomainName).
}

set help_text(Edit_Text_Fields) {
Text Fields: Description, How-To-Repeat, Environment, Audit-Trail,
             Unformatted, Fix and Release-Note

     These are the multi-line, free-format text entry fields of a
     problem report.

     The MultiText fields are shown one at a time.  Click on the field
     buttons to change fields. When the button is depressed, click again
     to get help for that field.

     The "Insert File..." button gives a file selector.  Select a file
     to insert at the cursor position.
}

set help_text(View_Overview) {
Overview of Viewing Problem Reports

     You can look, but don't touch!
}
  
set help_text(View_Text_Fields) {
Text Fields: Description, How-To-Repeat, Environment, Audit-Trail,
             Unformatted, Fix and Release-Note

     These are the multi-line, free-format text entry fields of a
     problem report.

     The MultiText fields are shown one at a time.  Click on the field
     buttons to change fields. When the button is depressed, click again
     to get help for that field.
}

#TTD: Yikes! How can this (change-request) be dynamic?
set help_text(Create_Overview) {
Overview of Creating Problem Reports

     Fill in the form, and click "Send"!

     All fields must be filled in, except Release, Environment and Fix
     are optional. If Class is set to "change-request", then
     How-To-Repeat becomes optional as well.
}
 
set help_text(Create_Radio_Buttons) {
Radio Buttons: Class, Priority, Severity and Confidential

     Clicking the buttons selects the value. Please use reasonable
     priorities when reporting problems.

     Click the heading title (eg Class:) for a definition of the
     field itself.
}

set help_text(Create_Listbox_Selectors) {
Listbox Selectors: Category and Submitter-Id

     Click on a listbox item and it gets entered into the entry box for
     you.

     The entry box uses a quick-fill feature for easy entry if you
     choose not to scroll the listbox looking for an item. As you type
     in each character, it automatically completes the value according
     to the available items in the listbox. It will not allow you to
     enter anything that isn't in the listbox.

     Click the heading title (eg Category:) for a definition of the
     field itself.

     Besides the scroll-bar, you can also scroll the listbox with the
     middle mouse button. Just click and hold the middle button anywhere
     in the listbox data area, and drag it vertically.
}
   
set help_text(Create_Entry_Fields) {
Entry Fields: Originator, Notify-List, Release and Synopsis

     These are one-line, free-format text entry fields.

     For Notify-List, TkGnats expects one or more valid
     email-addresses for any additional people interested in the
     progress of this bug report. They will be copied on all
     email. The list must be comma-separated. If domain names
     are not specified (@somewhere.something) they will be added
     if you have set TkGnats(DefaultDomainName).
}

#TTD: Yikes! How can this (change-request) be dynamic?
set help_text(Create_Text_Fields) {
Text Fields: Description, How-To-Repeat, Environment and Fix

     These are the multi-line, free-format text entry fields of a
     problem report.

     Environment and Fix are optional. If Class is set to
     "change-request", then How-To-Repeat becomes optional as well.

     TkGnats tries to fill the Environment section with meaningfull
     information about the system that you're running TkGnats on. If
     this is not the system where the problem occured, be sure to change
     the entry.

     If you have a work-around for the problem, enter the details into
     the Fix field. Otherwise, leave Fix blank.

     The MultiText fields are shown one at a time.  Click on the field
     buttons to change fields. When the button is depressed, click again
     to get help for that field.

     The "Insert File..." button gives a file selector.  Select a file
     to insert at the cursor position.
}
 
set help_text(Query_Overview) {
Overview of the Query System

     Select the search criteria, and click "Do Query"!

     All of the fields are logically and'd together. Only problem
     reports that match everything that you specify are selected.
     Leaving any field blank, or not selecting any check buttons (Class,
     State, etc), means that field is unrestricted in the search.

     Be warned that searching the "Text-Fields" causes the entire bug
     report to be read (by the server) and will take much longer to
     run.  Be sure to restrict the search by specifying other search
     criteria, and thereby minimizing the number of reports that have
     to actually be read.

     Pressing the "Return" key in the entry fields is a shortcut
     for the "Do Query" button.
}
 
set help_text(Field_Definitions) {
GNATS Problem Report Fields

     All of the field headings that are terminated with a colon ":",
     such as "Class:", "State:", etc, are also Help Buttons. Notice that
     they get highlighted as the mouse pointer passes over them. Click
     them to get the definition of each field.

     In Create, View and Edit modes, the MultiText fields such as
     "Description:", "How-To-Repeat:", etc are shown one at a time.
     Click on the MultiText field buttons to change fields. When the
     button is depressed, click again to get help for that field.
}

set help_text(Query_Regular_Expressions) {
Regular Expressions

     The following applies to the "RexExp:" fields and the text entry
     widgets that accept regular expressions.

     o  the match is _not_ case sensitive.
     o  use ".*", not just "*", to mean zero or more of any character.
     o  use "|" to specify values to be or'd, such as "seg|bus".
     o  the "RexExp:" fields use "matching", so you must specify
        a leading ".*" to match the middle of a string. For example,
        ".*alg" matches the Submitter-Id "calgary", but "alg" alone
        does not.
        The other input fields use "searching", and don't require this.
     o  a trailing ".*" is never required.

     Click on "RegExp:" in one of the listbox selectors for a more
     detailed discussion of regular expressions.
}
  
set help_text(Query_Check_Buttons) {
Check Buttons: Class, State, Priority, Severity and Confidential

     Clicking the buttons turns them on and off. Leaving all buttons
     unchecked is the same as selecting them all.  Select the values
     that you want to include in the search.

     Click the heading title (eg Class:) for a definition of the
     field itself.
}

set help_text(Query_Listbox_Selectors) {
Listbox Selectors: Category, Submitter-Id and Responsible

     Click on the items to move them back and forth between "Available"
     and "Selected".

     Within the listboxes, the selected items and the "regular
     expression" input field ("RegExp:") are logically or'd together. If
     at least one of the two are matched, this is and'd with all of the
     other search criteria specified.

     See the Regular Expression help for some highlights about using
     regular expressions in the Query dialog.

     Click "RegExp:" for a detailed explanation of regular expressions.

     Click the heading title (eg Category:) for a definition of the
     field itself.

     Besides the scroll-bar, you can also scroll the Query Listbox with
     the middle mouse button. Just click and hold the middle button
     anywhere in the listbox data area, and drag it vertically.
}

set help_text(Query_Entry_Fields) {
Entry Widgets: Synopsis, Release, Originator and Text-Fields

     These are all regular expression input fields.

     The Text-Fields are the multi-line, free-format text entry fields
     of a problem report such as Description, How-To-Repeat,
     Environment, Audit-Trail, Unformatted, Fix and Release-Note. They
     are all searched with the one regular expression that you enter.

     Be warned that searching the "Text-Fields" causes the entire bug
     report to be read (by the server) and will take much longer to
     run.  Be sure to restrict the search by specifying other search
     criteria, and thereby minimizing the number of reports that have
     to actually be read.

     Note that some fields, such as Originator, don't show their entire
     contents in the Query Listbox for space reasons. Thus, you may get
     matches that appear to be incorrect.  For example, searching for
     Originator "don" may find "donw" and "rickm".  "rickm" is matched
     because the full value for Originator is: "rickm (Rick Macdonald)".

     Click the heading title (eg Synopsis:) for a definition of the
     field itself.

     See the Regular Expression help for some highlights about using
     regular expressions in the Query dialog.

     Click on "RegExp:" in one of the listbox selectors for a more
     detailed discussion of regular expressions.

Entry Widgets: Number

     Enter one or more problem report numbers separated by blanks or
     commas to get to specific problem reports directly. There is no
     warning if some of the problem reports are not found, or even if
     you enter non-numeric values.

     All of the other selections are cleared when the Number entry is
     used.

     Click the heading title (eg Number:) for a definition of the
     field itself.

Entry Widgets: Days-Idle

     This checks the "Last-Modifed-Date" against todays date and
     compares the difference (in days) to the value that you enter.

     Click the heading title (eg Days-Idle:) for a definition of the
     field itself.
}

set help_text(Query_Menubar) {
Menu Bar

     The menu bar contains buttons and menus. Note that the menus have
     "tear-off" bars. Click on the "perforated" line ("--------") to
     make the menu appear in a little window of its own. This is very
     handy when playing with the "Fields" selection, for example.
     

     Do Query (button)

          Just Do It.

     Clear (button)

          All the selection widgets are cleared.

     Query

          Do Query        - same as the button above.
          Clear Widgets   - same as the button above.
          Query Selection - extract problem Ids from active selection
          Save Current >  - cascaded menu
                            - To Saved Queries Menu...
                              - save the current state of the selection
                                widgets, sort order and selected fields
                                and add a command to the Query Menu.
          Manage Saved... - delete or rename saved queries.
          Query For >     - cascaded menu
                            - these are queries that your site
                              administrator has created for global use.
          Saved Queries:  - these are queries that you have saved above. They
                            are permanent until you delete them.

     Sort

          New...          - specify a new sort order
          Save Current >  - cascaded menu
                            - To Saved Sorts Menu...
                              - save the current sort order and add a
                                command to the Sort Menu.
                            - As Startup Default
                              - save the current sort order as the default
                                for when TkGnats is started.
          Manage Saved... - delete or rename saved sorts.
          Sort By >       - cascaded menu
                            - these are sorts that your site
                              administrator has saved for global use.
          Saved Sorts:    - these are sorts that you have saved. They
                            are permanent until you delete them.
     Fields

          Save Current >  - cascaded menu
                            - As Startup Default
                              - save the currently selected fields as the default
                                for when TkGnats is started.

          Click on the check buttons to specify the fields that you want
          see in the Query Listbox. The fields selected in no way
          effects or restricts the search itself.

     Print

          This menu contains various reports that can be previewed and
          printed.

          The numbered entries below the separator line of the menu are
          reports that your site administrator has added to the system.
          (Well, we supplied the first few to get them started.)

          Each entry brings up a configuration menu that allows
          specification of the device (printer, previewer or file) and
          the format (postscript, ascii text, etc).  Also, you can print
          all the problem reports currently displayed in the Query
          Listbox, or just the one that is currently selected
          (highlighted).

     Actions

          These actions apply _only_ to the problem report currently
          selected (highlighted) in the Query Listbox. You can get
          this same menu to pop-up by clicking the right mouse button
          on any report in the listbox.

          Edit...          - invoke the problem report editor.
          View...          - invoke the problem report viewer.
          View Raw Data... - have a look at how the data is stored.
          Remove From List - this temporarily removes the problem
                             report from the Query Listbox. This 
                             allows more control when printing
                             more than one problem report.
          Send Email...    - send email to GNATS and optionally
                             various people involved with this
                             problem. GNATS adds the emaiil to the
                             Audit-Trail field of the problem report.

     Exit

          Click here when you've fixed all the bugs that have been
          reported.
}

set help_text(Query_Results_Listbox) {
Query Listbox

     Problem reports that match your search criteria are listed here.
     The fields displayed are not related to the search, they are
     set according to the fields that you select in the "Fields"
     menu on the menubar.

     Click the left  mouse button on any problem report to "select" it.

     Click the right mouse button on any problem report to pop-up the
     Actions menu.

     Actions, such as editing or sending follow-up email, can be found
     under the "Actions" menubar item. The actions there are performed
     on the problem report in the Query Listbox that is currently
     highlighted.

     Double-clicking on a listbox entry is a shortcut to "View" that
     problem report.

     Besides the scroll-bars, you can also scroll the Query Listbox with
     the middle mouse button. Just click and hold the middle button
     anywhere in the listbox data area, and drag it vertically and
     horizontally.
}
   
###append help_text(Query_Dialog) \
###        $help_text(Field_Definitions) \
###        $help_text(Query_Widget_Help)
   
set help_text(Text-Fields) {
Text-Fields

     These are the multi-line, free-format text entry fields of a
     problem report such as Description, How-To-Repeat, Environment,
     Audit-Trail, Unformatted, Fix and Release-Note.
    
     Be warned that searching the "Text-Fields" causes the entire bug
     report to be read (by the server) and will take much longer to
     run.  Be sure to restrict the search by specifying other search
     criteria, and thereby minimizing the number of reports that have
     to actually be read.

     In Create, View and Edit modes, the MultiText fields are shown
     one at a time.  Click on the field buttons to change fields. When
     the button is depressed, click again to get help for that field.

     In Create and Edit modes, the "Insert File..." button gives a file
     selector.  Select a file to insert at the cursor position.
}

set help_text(State) "\n`>State:'\n     (ENUMERATED) The current state of the PR.  Accepted values are:\n"
foreach state $TkGnats(StatesFile) {
    append help_text(State) "\n    `[lindex [split $state ":"] 0]'\n          [lindex [split $state ":"] 2]\n"
    if {[lindex [split $state ":"] 1] != ""} {
        append help_text(State) "\n          This state has the state type \"[lindex [split $state ":"] 1]\".\n"
    }
}

set help_text(Submitter-Id) {
`>Submitter-Id:'
     (TEXT) A unique identification code assigned by the Support Site.
     It is used to identify all Problem Reports coming from a particular
     site.  (Submitters without a value for this field can invoke
     `send-pr' with the `--request-id' option to apply for one from the
     support organization.  Problem Reports from those not affiliated
     with the support organization should use the default value of `net'
     for this field.)
}

set help_text(Originator) {
`>Originator:'
     (TEXT) Originator's real name.  The default is the value of the
     originator's environment variable `NAME'.
}

set help_text(Notify-List) {
`Notify-List:'
     (TEXT) Email addresses for any additional people interested in
     the progress of this bug report. They will be copied on all
     email. The list must be comma-separated. If domain names
     are not specified (@somewhere.something) they will be added
     if you have set TkGnats(DefaultDomainName).
}

set help_text(Organization) {
`>Organization:'
     (MULTITEXT) The originator's organization.  The default value is
     set with the variable `TkGnats(ORGANIZATION)' in the `config' file.
}

set help_text(Confidential) {
`>Confidential:'
     (ENUMERATED) Use of this field depends on the originator's
     relationship with the support organization; contractual agreements
     often have provisions for preserving confidentiality.  Conversely,
     a lack of a contract often means that any data provided will not
     be considered confidential.  Submitters should be advised to
     contact the support organization directly if this is an issue.

     If the originator's relationship to the support organization
     provides for confidentiality, then if the value of this field is
     `yes' the support organization treats the PR as confidential; any
     code samples provided are not made publicly available (e.g., in
     regression test suites).  The default value is `yes'.
}

set help_text(Synopsis) {
`>Synopsis:'
     (TEXT) One-line summary of the problem.  `send-pr' copies this
     information to the `Subject:' line when you submit a Problem
     Report.
}

set help_text(Severity) {
`>Severity:'
     (ENUMERATED) The severity of the problem.  Accepted values include:

    `critical'
          The product, component or concept is completely
          non-operational or some essential functionality is missing.
          No workaround is known.

    `serious'
          The product, component or concept is not working properly or
          significant functionality is missing.  Problems that would
          otherwise be considered `critical' are rated `serious' when a
          workaround is known.

    `non-critical'
          The product, component or concept is working in general, but
          lacks features, has irritating behavior, does something
          wrong, or doesn't match its documentation.

     The default value is `serious'.
}

set help_text(Priority) {
`>Priority:'
     (ENUMERATED) How soon the originator requires a solution.  Accepted
     values include:

    `high'
          A solution is needed as soon as possible.

    `medium'
          The problem should be solved in the next release.

    `low'
          The problem should be solved in a future release.

     The default value is `medium'.
}

set help_text(Category) {
`>Category:'
     (TEXT) The name of the product, component or concept where the
     problem lies.  The values for this field are defined by the Support
     Site.
}

set help_text(Class) "\n`>Class:'\n     (ENUMERATED) The current class of the PR.  Accepted values are:\n"
foreach class $TkGnats(ClassesFile) {
    append help_text(Class) "\n    `[lindex [split $class ":"] 0]'\n          [lindex [split $class ":"] 2]\n"
    if {[lindex [split $class ":"] 1] != ""} {
        append help_text(Class) "\n          This class has the class type \"[lindex [split $class ":"] 1]\".\n"
    }
}

set help_text(Release) {
`>Release:'
     (TEXT) Release or version number of the product, component or
     concept.
}

set help_text(Keywords) {
`>Keywords:'
     (TEXT) This field is available if your GNATS system has been
     configured with the flag "--with-released-based". Text entered
     here can be queried directly.
}

set help_text($TkGnats(Quarter)) "
`>$TkGnats(Quarter):'
     (TEXT) This field is available if your GNATS system has been
     configured with the flag \"--with-released-based\". Text entered
     here can be queried directly.
"

set help_text(Date-Required) {
`>Date-Required:'
     (TEXT) This field is available if your GNATS system has been
     configured with the flag "--with-released-based". Dates entered
     here can be queried directly.
}

set help_text(Environment) {
`>Environment:'
     (MULTITEXT) Description of the environment where the problem
     occured: machine architecture, operating system, host and target
     types, libraries, pathnames, etc.
}

set help_text(Description) {
`>Description:'
     (MULTITEXT) Precise description of the problem.
}

set help_text(How-To-Repeat) {
`>How-To-Repeat:'
     (MULTITEXT) Example code, input, or activities to reproduce the
     problem.  The support organization uses example code both to
     reproduce the problem and to test whether the problem is fixed.
     Include all preconditions, inputs, outputs, conditions after the
     problem, and symptoms.  Any additional important information
     should be included.  Include all the details that would be
     necessary for someone else to recreate the problem reported,
     however obvious.  Sometimes seemingly arbitrary or obvious
     information can point the way toward a solution.
}

set help_text(Release-Note) {
`>Release-Note:'
     (MULTITEXT) Human readable (user friendly) comments about
     problems or fixes suitable for incorporation into release notes
     for a given software distribution.
}

set help_text(Fix) {
`>Fix:'
     (MULTITEXT) A description of a solution to the problem, or a patch
     which solves the problem.  (This field is most often filled in at
     the Support Site; we provide it to the submitter in case she has
     solved the problem.)
}

set help_text(Number) {
`>Number:'
     (ENUMERATED) The incremental identification number for this PR.
     This is included in the automated reply to the submitter (if that
     feature of GNATS is activated.  It is also included in the
     copy of the PR that is sent to the maintainer.

     The `>Number:' field is often paired with the `>Category:' field as

          CATEGORY/NUMBER

     in subsequent email messages.  This is for historical reasons, as
     well as because Problem Reports are stored in subdirectories which
     are named by category.
}

set help_text(Responsible) {
`>Responsible:'
     (TEXT) The person responsible for this category.  GNATS retrieves
     this information from the `categories' file.
}

set help_text(Arrival-Date) {
`>Arrival-Date:'
     (TEXT) The time that this PR was received by GNATS.  The date is
     provided automatically by GNATS.
}

set help_text(Last-Modified) {
`>Last-Modified:'
     (TEXT) The time that this PR was last edited or changed.
}

set help_text(Closed-Date) {
`>Closed-Date:'
     (TEXT) The time that this PR was closed.
}

set help_text(Audit-Trail) {
`>Audit-Trail:'
     (MULTITEXT) Tracks related electronic mail as well as changes in
     the `>State:' and `>Responsible:' fields with the sub-fields:

    `State-Changed-<From>-<To>: OLDSTATE>-<NEWSTATE'
          The old and new `>State:' field values.

    `Responsible-Changed-<From>-<To>: OLDRESP>-<NEWRESP'
          The old and new `>Responsible:' field values.

    `State-Changed-By: NAME'
    `Responsible-Changed-By: NAME'
          The name of the maintainer who effected the change.

    `State-Changed-When: TIMESTAMP'
    `Responsible-Changed-When: TIMESTAMP'
          The time the change was made.

    `State-Changed-Why: REASON...'
    `Responsible-Changed-Why: REASON...'
          The reason for the change.

     The `>Audit-Trail:' field also contains any mail messages received
     by GNATS related to this PR, in the order received.  Use a Subject
     of the following format and copy the email to the gnats address.

          Subject: Re: category/prnum
}

set help_text(Unformatted) {
`>Unformatted:'
     (MULTITEXT) Any random text found outside the fields
     in the original Problem Report.
}

set help_text(Days-Idle) {
`Days-Idle'
     This is for selecting problem reports that have not been updated
     for this number of days or more.
}

set help_text(RegExp) {
Querying using regular expressions
**********************************

   GNATS uses GNU regular expression syntax with these settings:

     RE_SYNTAX_POSIX_EXTENDED | RE_BK_PLUS_QM & RE_DOT_NEWLINE

This means that parentheses (`(' and `)') and pipe symbols (`|') do not
need to be used with the escape symbol `\'.  The tokens `+' and `?' do
need the escape symbol, however.

   Unfortunately, we do not have room in this manual for an adequate
tutorial on regular expressions.  The following is a basic summary of
some regular expressions you might wish to use.

   *Note Regular Expression Syntax: (regex)Regular Expression Syntax,
for details on regular expression syntax.  Also see *Note Syntax of
Regular Expressions: (emacs)Regexps, but beware that the syntax for
regular expressions in Emacs is slightly different.

   All search criteria options to `query-pr' rely on regular expression
syntax to construct their search patterns.  For example,

     query-pr --state=open

matches all PRs whose `>State:' values match with the regular
expression `open'.

   We can substitute the expression `o' for `open', according to GNU
regular expression syntax.  This matches all values of `>State:' which
begin with the letter `o'.

     query-pr --state=o

   is equivalent to

     query-pr --state=open

in this case, since the only value for `>State:' which matches the
expression `o' is `open'.  (Double quotes (`"') are used to protect the
asterix (`*') from the shell.)  `--state=o' also matches `o', `oswald',
and even `oooooo', but none of those values are valid states for a
Problem Report.

   Regular expression syntax considers a regexp token surrounded with
parentheses, as in `(REGEXP)', to be a "group".  This means that
`(ab)*' matches any number of contiguous instances of `ab', including
zero.  Matches include `', `ab', and `ababab'.

   Regular expression syntax considers a regexp token surrounded with
square brackets, as in `[REGEXP]', to be a "list".  This means that
`Char[(ley)(lene)(broiled)' matches any of the words `Charley',
`Charlene', or `Charbroiled' (case is significant; `charbroiled' is not
matched).

   Using groups and lists, we see that

     query-pr --category="gcc|gdb|gas"

is equivalent to

     query-pr --category="g(cc|db|as)"

and is also very similar to

     query-pr --category="g[cda]"

with the exception that this last search matches any values which begin
with `gc', `gd', or `ga'.

   The `.' character is known as a "wildcard".  `.' matches on any
single character.  `*' matches the previous character (except
newlines), list, or group any number of times, including zero.
Therefore, we can understand `.*' to mean "match zero or more instances
of any character."  For this reason, we never specify it at the end of
a regular expression, as that would be redundant.  The expression `o'
matches any instance of the letter `o' (followed by anything) at the
beginning of a line, while the expression `o.*' matches any instance of
the letter `o' at the beginning of a line followed by any number
(including zero) of any characters.

   We can also use the expression operator `|' to signify a logical
`OR', such that

     query-pr --state="o|a"

matches all `open' or `analyzed' Problem Reports.  (Double quotes (`"')
are used to protect the pipe symbol (`|') from the shell.)

   By the same token,(1) using

     query-pr --state=".*a"

matches all values for `>State:' which contain an `a'.  (These include
`analyzed' and `feedback'.)

   Another way to understand what wildcards do is to follow them on
their search for matching text.  By our syntax, `.*' matches any
character any number of times, including zero.  Therefore, `.*a'
searches for any group of characters which end with `a', ignoring the
rest of the field.  `.*a' matches `analyzed' (stopping at the first
`a') as well as `feedback'.

   *Note:* When using `--text' or `--multitext', you do not have to
specify the token `.*' at the beginning of TEXT to match the entire
field.  For the technically minded, this is because `--text' and
`--multitext' use `re_search' rather than `re_match'.  `re_match'
"anchors" the search at the beginning of the field, while `re_search'
does not anchor the search.

   For example, to search in the `>Description:' field for the text

     The defrobulator component returns a nil value.

we can use

     query-pr --multitext="defrobulator.*nil"

   To also match newlines, we have to include the expression `(.|^M)'
instead of just a dot (`.').  `(.|^M)' matches "any single character
except a newline (`.') *or* (`|') any newline (`^M')."  This means that
to search for the text

     The defrobulator component enters the bifrabulator routine
     and returns a nil value.

we must use

     query-pr --multitext="defrobulator(.|^M)*nil"

   To generate the newline character `^M', type the following depending
on your shell:

`csh'
     `*control*-V *control*-M'

`tcsh'
     `*control*-V *control*-J'

`sh (*or* bash)'
     Use the RETURN key, as in

          (.|
          )

   Again, see *Note Regular Expression Syntax: (regex)Regular
Expression Syntax, for a much more complete discussion on regular
expression syntax.

   ---------- Footnotes ----------

   (1)  No pun intended.
}

    set alias [get_field_alias $field]
    if {![info exists help_text($alias)]} {
        set help_text($alias) "\n     No help available for $alias"
    }
    
    show_help $alias $help_text($alias)
} 
