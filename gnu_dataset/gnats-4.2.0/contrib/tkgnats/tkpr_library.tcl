proc dputs {text} {
    global env tcl_platform
    if {$tcl_platform(platform) == "unix"} {
        if {[info exists env(TKGNATSDEBUG)]} {
            if {$env(TKGNATSDEBUG) == "debug"} {
                puts $text
            }
        }
    }
}

proc TkGnats_config_platform {} {
    global TkGnats tcl_platform
    if {$tcl_platform(platform) != "unix"} {
        set TkGnats(GNATS_ACCESS) network
        set TkGnats(GNATS_ACCESS_METHOD) socket
        set TkGnats(MailMethod) smtp
        set TkGnats(UserSubdir) tkgnats
    } {
        set TkGnats(UserSubdir) .tkgnats
        # This resets control-v to be Paste instead of scrolling.
        bind Text <Control-v> {}
    }
}

proc TkGnats_config {} {
    global TkGnats
    
    TkGnats_config_platform

    # This file is optional
    if {[info exists TkGnats(ServerInfo)]} {
        if {[llength $TkGnats(ServerInfo)] != 6} {
            wm withdraw .
            Msg "Invalid ServerInfo: $TkGnats(ServerInfo)\n\nPlease completely exit TkGnats and start again. If it still fails, contact your TkGnats administrator."
            exit 1
        }
        if {[lindex $TkGnats(ServerInfo) 1] != {}} {
            set  TkGnats(GNATS_SERVER)  [lindex $TkGnats(ServerInfo) 1]
        }
        if {[lindex $TkGnats(ServerInfo) 2] != {}} {
            set  TkGnats(GNATS_PORT)    [lindex $TkGnats(ServerInfo) 2]
        }
        if {[lindex $TkGnats(ServerInfo) 3] != {}} {
            set  TkGnats(GNATS_ADDR)    [lindex $TkGnats(ServerInfo) 3]
        }
        if {[lindex $TkGnats(ServerInfo) 4] != {}} {
            set  TkGnats(GNATS_DB)      [lindex $TkGnats(ServerInfo) 4]
        }
        if {[lindex $TkGnats(ServerInfo) 5] != {}} {
            set  TkGnats(GNATS_DBALIAS) [lindex $TkGnats(ServerInfo) 5]
        }
    }

    foreach l [array names TkGnats] {
        set tmpvars($l) $TkGnats($l)
    }

    TkGnats_config_rc tmpvars

    foreach l [array names tmpvars] {
        if {![info exists TkGnats($l)]} {
            set TkGnats($l) $tmpvars($l)
        }
    }
    set TkGnats(EmailAddr) [add_email_domainname $TkGnats(EmailAddr)]

    if {[info tclversion] < 8.0} {
        foreach F {DialogFont TextFont HelpFont} {
            set f [string tolower $F]
            set TkGnats($f) $TkGnats($F)
        }
    } {
        foreach F {DialogFont TextFont HelpFont} {
            set f [string tolower $F]
            set TkGnats($f) $f
            eval font create $f [font actual $TkGnats($F)]
        }
    }
    option add *font $TkGnats(dialogfont) 100

    set TkGnats(WISHPATH) [info nameofexecutable]

    # Automatic check for TkGnats updates
    TkGnats_UpdateCheck

    # Clipboard popup memu
    clipboard_create
}

proc TkGnats_exec {args} {
    global TkGnats
    if {[TkGnats_UpdateCheck]} {
	return
    }
    eval exec $args
}

proc TkGnats_UpdateCheck {} {
    global TkGnats
    if {$TkGnats(AutoUpdateCheck)} {
	if {![info exists TkGnats(UpdateCheckTime)]} {
	    set TkGnats(UpdateCheckTime) [file mtime $TkGnats(lib)/VERSION]
	} {
	    if {$TkGnats(UpdateCheckTime) != [file mtime $TkGnats(lib)/VERSION]} {
		TkGnats_UpdateRequired
		return 1
	    }
	}
    }
    return 0
}

proc TkGnats_UpdateRequired {} {
    global TkGnats
    Msg "TkGnats has detected that some of its components have been recently updated.\n\nYou must quit all TkGnats windows and restart TkGnats."
}

proc TkGnats_config_rc {tmparr} {
    global env tcl_platform help_text
    upvar $tmparr TkGnats

    # We need to protect a previously set GNATS_ADDR from the value in tkgnats.config
    catch {set tempaddr $TkGnats(GNATS_ADDR)}
    catch {source $TkGnats(lib)/tkgnats.config}
    catch {set TkGnats(GNATS_ADDR) $tempaddr}

    ### This section tries to determine the users login, home directory, ###
    ### group and email address. If it fails for you, please let me know ###
    ### what you had to do to fix it.                                    ###

    # TkGnats(LogName)
    # The login name for the user.
    #   - first check the first output field of the id command
    #   - then check USER env var
    #   - then try LOGNAME
    #   - then try running whoami
    #
    # TkGnats(GroupName)
    # The group name for the user.
    #   - first check the output of the groups command for a list of groups
    #   - then check the second output field of the id command for a primary group
    #   - then check GROUP env var

    set user   ""
    set group  ""
    set x      ""
    set y      ""

    if {$tcl_platform(platform) == "unix"} {
	if {![catch {exec groups} result]} {
	    set group $result
	}
	
	if {![catch {exec id} result]} {
	    regexp \\(\[a-zA-Z0-9\]+\\) [lindex [split $result " "] 0] x
	    regexp \\(\[a-zA-Z0-9\]+\\) [lindex [split $result " "] 1] y
	    set user  [string trim $x "()"]
	    if {$group == ""} {
		set group [string trim $y "()"]
	    }
	}
    }

    if {![info exists TkGnats(LogName)]} {
        if {"$user" != ""} {
            set TkGnats(LogName) $user
        } {
            if {[info exists env(USER)] && $env(USER) != "nouser"} {
                set TkGnats(LogName) $env(USER)
            } {
                if {[info exists env(LOGNAME)]} {
                    set TkGnats(LogName) $env(LOGNAME)
                } {
                    if {$tcl_platform(platform) == "unix" && ![catch {exec whoami} x]} {
                        set TkGnats(LogName) $x
                    }
                }
            }
        }
    }

    if {![info exists TkGnats(LogName)]} {
        wm withdraw .
        Msg "No user name found!\nPlease set USER or LOGNAME in environment."
        exit 1
    }

    set TkGnats(FullName) [fullname_from_logname $TkGnats(LogName)]
        
    if {![info exists TkGnats(GroupName)]} {
        if {$group != ""} {
            set TkGnats(GroupName) $group
        } {
            if {[info exists env(GROUP)]} {
                set TkGnats(GroupName) $env(GROUP)
            } {
                set TkGnats(GroupName) ""
            }
        }
    }
         
    if {![info exists TkGnats(EmailAddr)]} {
        if {[info exists env(REPLYTO)]} {
            set TkGnats(EmailAddr) $env(REPLYTO)
        } {
            set TkGnats(EmailAddr) $TkGnats(LogName)
        }
    }

    if {![info exists TkGnats(HOME)]} {
        if {[info exists env(HOME)]} {
            set TkGnats(HOME) $env(HOME)
        }
    }
    
    if {![info exists TkGnats(CategoryList)]} {
        set TkGnats(CategoryList)    ""
    }
    if {![info exists TkGnats(SubmitterList)]} {
        set TkGnats(SubmitterList)   ""
    }
    if {![info exists TkGnats(ResponsibleList)]} {
        set TkGnats(ResponsibleList) ""
    }
    if {![info exists TkGnats(ResponsibleFile)]} {
        set TkGnats(ResponsibleFile) ""
    }
    if {![info exists TkGnats(ClassesList)]} {
        set TkGnats(ClassesList) ""
    }
    if {![info exists TkGnats(ClassesFile)]} {
        set TkGnats(ClassesFile) ""
    }
    if {![info exists TkGnats(StatesList)]} {
        set TkGnats(StatesList) ""
    }
    if {![info exists TkGnats(StatesFile)]} {
        set TkGnats(StatesFile) ""
    }
    
    #
    # Read in any and all tkgnatsrc files
    #
    
    set system $tcl_platform(os)

    if {[file readable $TkGnats(lib)/tkgnatsrc]} {
        source $TkGnats(lib)/tkgnatsrc
    }

    if {[file readable $TkGnats(lib)/tkgnatsrc.$system]} {
        source $TkGnats(lib)/tkgnatsrc.$system
    }

    set TkGnats(tkgnats_version) [file_get_text $TkGnats(lib)/VERSION]
    
    if {![info exists TkGnats(UserDir)]} {
        if {![info exists TkGnats(HOME)]} {
            wm withdraw .
            Msg "No home directory found!\n\nPlease set HOME in environment or TkGnats(UserDir) in the ini file.."
            exit 1
        }
        set TkGnats(UserDir) $TkGnats(HOME)/$TkGnats(UserSubdir)
    }
    set TkGnats(UserDir) [string trimright $TkGnats(UserDir) /]

    check_tkgnats_userdir $TkGnats(UserDir)
    
    if {[file readable $TkGnats(UserDir)/tkgnatsrc]} {
        source $TkGnats(UserDir)/tkgnatsrc
    }
    if {[file readable $TkGnats(UserDir)/tkgnatsrc.$system]} {
        source $TkGnats(UserDir)/tkgnatsrc.$system
    }

    #
    # Get environment variable overrides
    #
    
    if {[info exists env(GNATS_ROOT)]} {
        set config [file_get_text $env(GNATS_ROOT)/gnats-adm/config]
        if {"$config" != ""} {
            set TkGnats(GNATS_ADDR) \
                    [string trim [lindex [split [lindex $config [lsearch -regexp $config GNATS_ADDR]] =] 1] \"]
            
            set TkGnats(GNATS_USER) \
                    [string trim [lindex [split [lindex $config [lsearch -regexp $config GNATS_USER]] =] 1] \"]
            
            set TkGnats(SUBMITTER) \
                    [string trim [lindex [split [lindex $config [lsearch -regexp $config SUBMITTER]] =] 1] \"]
            
            set TkGnats(GNATS_ROOT) $env(GNATS_ROOT)
        }
    }

    if {![info exists TkGnats(ORGANIZATION)]} {
        set TkGnats(ORGANIZATION) $TkGnats(SUBMITTER)
        if {[info exists env(ORGANIZATION)]} {
            if {[file readable $env(ORGANIZATION)]} {
                set TkGnats(ORGANIZATION) [file_get_text $env(ORGANIZATION)]
            } {
                set TkGnats(ORGANIZATION) $env(ORGANIZATION)
            }
        } {
            if {[info exists TkGnats(HOME)]} {
                if {[file readable $TkGnats(HOME)/.signature]} {
                    set TkGnats(ORGANIZATION) [file_get_text $TkGnats(HOME)/.signature]
                }
            }
        }
    }

    set TkGnats(HOSTNAME) [info hostname]
    if {[info exists env(HOSTNAME)]} {
        set TkGnats(HOSTNAME) $env(HOSTNAME)
    }
    if {$tcl_platform(platform) == "unix" && $TkGnats(HOSTNAME) == ""} {
        if {[catch {exec /bin/hostname} TkGnats(HOSTNAME)]} {
            if {[catch {exec /usr/bin/hostname} TkGnats(HOSTNAME)]} {
                if {[catch {exec /usr/ucb/hostname} TkGnats(HOSTNAME)]} {
                    if {[catch {exec /usr/bsd/hostname} TkGnats(HOSTNAME)]} {
                        set TkGnats(HOSTNAME) ""
                    }
                }
            }
        }
    }
    
    #
    # Make sure some required variables are set
    #
    
    if {![info exists TkGnats(GNATS_SERVER)]} {
        set TkGnats(GNATS_SERVER) ""
    }
    if {$TkGnats(GNATS_SERVER) == ""} {
        set TkGnats(GNATS_ACCESS)        local
        set TkGnats(GNATS_ACCESS_METHOD) batch
    } {
        set TkGnats(GNATS_ACCESS) network
        if {![info exists TkGnats(GNATS_ACCESS_METHOD)]} {
            set TkGnats(GNATS_ACCESS_METHOD) socket
        }
        if {$TkGnats(GNATS_ACCESS_METHOD) != "batch"} {
            set TkGnats(GNATS_ACCESS_METHOD) socket
        }
    }
    if {![info exists TkGnats(GNATS_PORT)]} {
        set TkGnats(GNATS_PORT) 1529
    }

    if {![info exists TkGnats(QuerySortMethod)]} {
        set TkGnats(QuerySortMethod) internal
    }

    catch {source $TkGnats(UserDir)/fonts}
    
    label   .l
    set TkGnats(TkDialogFont) [.l cget -font]
    destroy .l
    
    if {![info exists TkGnats(DialogFont)]} {
        if {$tcl_platform(platform) == "unix"} {
            set TkGnats(DialogFont) $TkGnats(TkDialogFont)
        } {
            if {[info tclversion] < 8.0} {
                set TkGnats(DialogFont) $TkGnats(TkDialogFont)
            } {
                set TkGnats(DialogFont) {"ms sans serif" 8}
            }
        }
    }
    
    if {![info exists TkGnats(TextFont)]} {
        if {[info tclversion] < 8.0} {
            set font fixed
        } {
            if {[font metrics [list [lindex $TkGnats(DialogFont) 0]] -fixed]} {
                set font $TkGnats(DialogFont)
            } {
                set font fixed
            }
        }
        if {$tcl_platform(platform) == "unix"} {
            set TkGnats(TextFont) $font
        } {
            if {[info tclversion] < 8.0} {
                set TkGnats(TextFont) $font
            } {
                set TkGnats(TextFont) {"courier new" 8}
            }
        }
    }
    
    if {![info exists TkGnats(HelpFont)]} {
        if {$tcl_platform(platform) == "unix"} {
            set TkGnats(HelpFont) $TkGnats(TextFont)
        } {
            if {[info tclversion] < 8.0} {
                set TkGnats(HelpFont) $TkGnats(TextFont)
            } {
                set TkGnats(HelpFont) {"courier new" 10}
            }
        }
    }
    
    #
    # Set up for local or network access
    #

    if {![info exists TkGnats(GNATS_BINDIR)]} {
        set TkGnats(GNATS_BINDIR) ""
    } {
        set TkGnats(GNATS_BINDIR) [string trim $TkGnats(GNATS_BINDIR)]
    }
    if {$TkGnats(GNATS_BINDIR) != ""} {
        set bindir $TkGnats(GNATS_BINDIR)/
    } {
        set bindir ""
    }

    if {[info exists TkGnats(GNATS_DBALIAS)] && $TkGnats(GNATS_DBALIAS) != ""} {
        set parms " -d $TkGnats(GNATS_DBALIAS)"
    } {
        set parms ""
    }
    if {$TkGnats(GNATS_ACCESS) == "local"} {
        set TkGnats(pr-edit)    $TkGnats(GNATS_LIBEXECDIR)/gnats/pr-edit$parms
        set TkGnats(query-pr)   ${bindir}query-pr$parms
    } {
        if {$TkGnats(GNATS_SERVER) != ""} {
            append parms " --host $TkGnats(GNATS_SERVER)"
        }
        if {$TkGnats(GNATS_PORT) != ""} {
            append parms " --port $TkGnats(GNATS_PORT)"
        }
        set TkGnats(pr-edit)    $TkGnats(GNATS_LIBEXECDIR)/gnats/npr-edit$parms
        set TkGnats(query-pr)   ${bindir}nquery-pr$parms
    }
    
    #
    # Now get any SERVER based tkgnatsrc
    #
    
    set TkGnats(SiteServerDir) $TkGnats(lib)
    set TkGnats(UserServerDir) $TkGnats(UserDir)

    set db ""
    if {[info exists TkGnats(ServerInfo)]} {
        set db [lindex $TkGnats(ServerInfo) 4]
    }
    if {$db != ""} {
        set TkGnats(SiteServerDir) $TkGnats(SiteServerDir)/$db
        if {[file readable $TkGnats(SiteServerDir)/tkgnatsrc]} {
            source $TkGnats(SiteServerDir)/tkgnatsrc
        }
        if {[file  isdirectory $TkGnats(UserDir)/$TkGnats(GNATS_SERVER)]} {
            catch {file rename $TkGnats(UserDir)/$TkGnats(GNATS_SERVER) $TkGnats(UserDir)/$db}
        }
        set TkGnats(UserServerDir) $TkGnats(UserServerDir)/$db
        check_tkgnats_userdir $TkGnats(UserServerDir)
        if {[file readable $TkGnats(UserServerDir)/tkgnatsrc]} {
            source $TkGnats(UserServerDir)/tkgnatsrc
        }
    }
    
    # This checks for the query and sort subdirectories
    check_tkgnats_usersubdir $TkGnats(UserServerDir)

    #
    # Set up user id and password
    #
    
    if {$TkGnats(GNATS_ACCESS) == "local"} {
        set TkGnats(UseridPassword) ""
    } {
        set userid ""
        if {[info exists TkGnats(ServerPasswords)]} {
            foreach pwd [split $TkGnats(ServerPasswords) \n] {
                if {[lindex $pwd 0] == $TkGnats(GNATS_ADDR)} {
                    set userid [lindex $pwd 1]
                    set passwd [lindex $pwd 2]
                    break
                }
            }
        }
        if {$userid == ""} {
            # Might as well see if there's a default access that's higher than the host access
                set userid "anonymous"
                set passwd "guest"
        }
        if {$TkGnats(GNATS_ACCESS_METHOD) == "socket"} {
            set TkGnats(UseridPassword) "$userid $passwd"
        } {
            set TkGnats(UseridPassword) "--user $userid --passwd $passwd"
        }
    }

    #
    # Set up mail method
    #
    
    if {![info exists TkGnats(SMTP_SERVER)]} {
        set TkGnats(SMTP_SERVER) ""
    }
    if {$TkGnats(SMTP_SERVER) == ""} {
        set TkGnats(MailMethod) mailer
    } {
        set TkGnats(MailMethod) smtp
    }
    if {![info exists TkGnats(SMTP_PORT)]} {
        set TkGnats(SMTP_PORT) 25
    }
        
    #
    # Determine if this user is authorized to edit problem reports
    # This only applies to local disk access to the GNATS database.
    # Network access is controlled (later) by gnatsd.
    #
    
    if {$TkGnats(GNATS_ACCESS) == "local"} {
        set glist {}
        set ulist {}
        if {[info exists TkGnats(edit_authorized_groups)]} {
            set  glist  $TkGnats(edit_authorized_groups)
        }
        if {[info exists TkGnats(edit_authorized_users)]} {
            set  ulist  $TkGnats(edit_authorized_users)
        }
        set TkGnats(edit_authorized) 0
        if {$ulist == {} && $glist == {}} {
            set TkGnats(edit_authorized) 1
        }
        if {[lsearch -exact $ulist $TkGnats(LogName)] > -1} {
            set TkGnats(edit_authorized) 1
        }
        foreach group $glist {
            if {[lsearch -exact $TkGnats(GroupName) $group] > -1} {
                set TkGnats(edit_authorized) 1
            }
        }
    }
    
    #
    # Determine if this user is authorized to delete problem reports
    #
    
    set glist {}
    set ulist {}
    if {[info exists TkGnats(delete_authorized_groups)]} {
        set  glist  $TkGnats(delete_authorized_groups)
    }
    if {[info exists TkGnats(delete_authorized_users)]} {
        set  ulist  $TkGnats(delete_authorized_users)
    }
    set TkGnats(delete_authorized) 0
    if {[lsearch -exact $ulist $TkGnats(LogName)] > -1} {
        set TkGnats(delete_authorized) 1
    }
    foreach group $glist {
        if {[lsearch -exact $TkGnats(GroupName) $group] > -1} {
            set TkGnats(delete_authorized) 1
        }
    }
    # The delete function only works on local GNATS systems.
    if {$TkGnats(GNATS_ACCESS) != "local"} {
        set TkGnats(delete_authorized) 0
    }
}

proc get_gnats_config {} {
    global TkGnats
    #
    # Determine if GNATS is Release Based, and get GNATS lists
    #

    # If TkGnats(GNATS_ACCESS_METHOD) != "socket" then this does nothing
    if {[info exists TkGnats(socket,$TkGnats(GNATS_SERVER),$TkGnats(GNATS_PORT),keepopen)]} {
        set keepopen ""
    } {
        set keepopen 1
    }
    if {[open_socket_gnatsd $keepopen] == "-1"} {
        return -1
    }

    get_gnats_version

    set mingnats 4.0
    
    if {$TkGnats(GNATS_Version) < $mingnats || $TkGnats(GNATS_Version) == 3.2} {
        wm withdraw .
        Msg "Sorry, you're trying to talk to a GNATS $TkGnats(GNATS_Version) database but this version of TkGnats requires GNATS $mingnats or newer.\n\nTry ftp://sourceware.cygnus.com/pub/gnats/snapshots"
        return -1
    }
            
    get_gnats_access
    check_release_based
    get_gnats_server_config

    if {[info exists TkGnats(SERVER-TKGNATS_MIN_VERSION)]} {
        set ver (unknown)
        regexp "tkgnats-(\[^ \]*)" $TkGnats(tkgnats_version) match ver
        if {[string compare $ver $TkGnats(SERVER-TKGNATS_MIN_VERSION)] < 0} {
            wm withdraw .
            Msg "Sorry, you're trying to talk to a GNATS $TkGnats(GNATS_Version) database that requires TkGnats version $TkGnats(SERVER-TKGNATS_MIN_VERSION) or newer, but you're only running TkGnats version $ver.\n\nTry http://www.cuug.ab.ca/~macdonal/tkgnats"
            return -1
        }
    }
    
    if {![info exists TkGnats(QueryMode)]} {
        set TkGnats(QueryMode) sql2
    }

    get_category_list
    get_submitter_list
    get_responsible_list
    get_classes_list
    get_states_list

    # If TkGnats(GNATS_ACCESS_METHOD) != "socket" then this does nothing
    close_socket_gnatsd $keepopen
    return ""
}

proc set_edit_authorized {} {
    global TkGnats
    if {$TkGnats(UserAccess) == "edit" || $TkGnats(UserAccess) == "admin"} {
        set TkGnats(edit_authorized) 1
    } {
        set TkGnats(edit_authorized) 0
    }
}

proc get_gnats_access {} {
    global TkGnats
    get_gnats_access_$TkGnats(GNATS_ACCESS_METHOD)
}

proc get_gnats_access_batch {} {
    global TkGnats
    if {![info exists TkGnats(UserAccess)]} {
        set TkGnats(UserAccess) unknown
        if {$TkGnats(GNATS_ACCESS) == "network"} {
            catch {eval exec $TkGnats(query-pr) $TkGnats(UseridPassword) --closed-before=1970-01-01 -D} rep
            foreach l [split $rep \n] {
                if {[string first "received 520" $l] > 0} {
                    Msg "GNATSD access error:\n\n$l'"
                    Exit 1
                }
                if {[string first "access level set" $l] > 0} {
                    set TkGnats(UserAccess) [lindex $l end]
                    break
                }
            }
            set_edit_authorized
        }
    }
}

proc get_gnats_access_socket {} {
    global TkGnats
    # The access level is saved when the socket is opened
    if {![info exists TkGnats(UserAccess)]} {
        set TkGnats(UserAccess) unknown
    }
}

proc get_gnats_version {} {
    global TkGnats
    get_gnats_version_$TkGnats(GNATS_ACCESS_METHOD)
}

proc get_gnats_version_batch {} {
    global TkGnats
    if {![info exists TkGnats(GNATS_Version)]} {
        if {[catch {eval exec $TkGnats(query-pr) --version} rep]} {
            set TkGnats(GNATS_Version) 0.0
        } {
            set TkGnats(GNATS_Version) [lindex $rep end]
        }
    }
}

proc get_gnats_version_socket {} {
    global TkGnats
    # The version number is saved when the socket is opened
    if {![info exists TkGnats(GNATS_Version)]} {
        set TkGnats(GNATS_Version) 0.0
    }
}

proc check_tkgnats_userdir {userdir} {
    global TkGnats
    if {$TkGnats(CurrentProgram) == "tkgnats"} {
        return
    }
    if {![file isdirectory $userdir]} {
        file mkdir $userdir
    }
}

proc check_tkgnats_usersubdir {userdir} {
    global TkGnats
    if {$TkGnats(CurrentProgram) == "tkgnats"} {
        return
    }
    if {![file isdirectory $userdir/query]} {
        file mkdir $userdir/query
        catch {eval file copy [glob [file dirname $userdir]/query/*] $userdir/query}
    }
    if {![file isdirectory $userdir/sort]} {
        file mkdir $userdir/sort
        catch {eval file copy [glob [file dirname $userdir]/sort/*]  $userdir/sort}
    }
    if {![file exists $userdir/default-sort]} {
        catch {eval file copy [glob [file dirname $userdir]/default-sort] $userdir}
    }
    if {![file exists $userdir/default-view]} {
        catch {eval file copy [glob [file dirname $userdir]/default-view] $userdir}
    }
}

#
# Procedures
#

proc Msg {args} {
    set msg ""
    set nargs [expr [llength $args] - 1]
    for {set i 0} {$i < $nargs} {incr i} {
        append msg "[lindex $args $i]\n"
    }
    append msg "[lindex $args $nargs]"
    bell
    tk_dialog .tkerr "TkGnats Error" $msg "error" 0 "OK"
}

proc Notice {args} {
    set msg ""
    set nargs [expr [llength $args] - 1]
    for {set i 0} {$i < $nargs} {incr i} {
        append msg "[lindex $args $i]\n"
    }
    append msg "[lindex $args $nargs]"
    tk_dialog .tknotice "TkGnats Notice" $msg "info" 0 "OK"
}

# reap any zombied exec's
set TkGnats(reap_scheduled) 0
proc do_reap {} {
    global TkGnats
    catch {exec true}
    set TkGnats(reap_scheduled) 0
}

proc schedule_reap {} {
    global TkGnats
    if {!$TkGnats(reap_scheduled)} {
        set TkGnats(reap_scheduled) 1
        after 5000 do_reap
    }
}

proc Exit {x} {
    #destroy .; # use if before tk3.3
    exit $x
}

proc get_responsible_addr {name} {
    global TkGnats
    # strip off whitespace and then take out the first word.
    # we assume the rest is a (Full Name) type comment
    set name [string  trim  $name "\t\n "]
    set name [lindex [split $name "\t\n "] 0]
    set addr $name
    foreach n $TkGnats(ResponsibleFile) {
        set res [split $n :]
        if {$name == [lindex $res 0]} {
            if {[lindex $res 2] != ""} {
                set addr [lindex $res 2]
                break
            }
        }
    }
    return $addr
}

#
# trim pr field data whitespace
#
proc ftrim {s} {
    return [string trim $s "\t\n "]
}

proc gnatsd_send {s msg} {
    global TkGnats

    #puts stderr "gnatsd_send $s $msg"

    if { [catch {puts $s $msg} errstr] } {
      Msg "Failure writing to socket, assuming it was closed. Error: $errstr"
      unset -nocomplain TkGnats(socket,$TkGnats(GNATS_SERVER),$TkGnats(GNATS_PORT))
      set s  [open_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT) $keepopen]
      gnatsd_chdb $s
      puts $s $msg
    } 
}

proc gnatsd_chdb {s} {
    global TkGnats

    # intentionally, this does not use gnatsd_send to avoid a loop
    puts $s "CHDB $TkGnats(GNATS_DBALIAS)"
    set rep [get_socket_reply $s]
    if {![string match 2* [lindex $rep 0]]} {
        Msg "GNATSD error setting GNATS database:\n" "[join $rep \n]"
        return -1
    }
    return $s
}

proc open_socket_gnatsd {{keepopen {}}} {
    global TkGnats
    if {$TkGnats(GNATS_ACCESS_METHOD) == "socket"} {
        if {[info exists TkGnats(socket,$TkGnats(GNATS_SERVER),$TkGnats(GNATS_PORT))]} {
            return $TkGnats(socket,$TkGnats(GNATS_SERVER),$TkGnats(GNATS_PORT))
        }
        set s  [open_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT) $keepopen]
        if {$s == -1} {
            return -1
        }
        if {[gnatsd_chdb $s] != $s} {
            return -1
        }
        set cmd [subst "USER $TkGnats(UseridPassword)"]
        gnatsd_send $s $cmd
        set rep [get_socket_reply $s]    
        if {[string match 2* [lindex $rep 0]]} {
	    set accesslevelstr [lindex [lindex $rep 1] end]
	    set accesslevel [string range $accesslevelstr 1 end-1]
	    dputs "access level: $accesslevel"
            set TkGnats(UserAccess) $accesslevel
            set_edit_authorized
            return $s
        } {
            # gnatsd closes the connection when this happens
            unset TkGnats(socket,$TkGnats(GNATS_SERVER),$TkGnats(GNATS_PORT))
            catch {unset TkGnats(socket,$TkGnats(GNATS_SERVER),$TkGnats(GNATS_PORT),keepopen)}
            Msg "GNATSD error sending USER command:\n" "[join $rep \n]"
            return -1
        }
    }
    return ""
}

proc close_socket_gnatsd {{force {}}} {
    global TkGnats
    if {$TkGnats(GNATS_ACCESS_METHOD) == "socket"} {
        return [close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT) $force]
    }
    return ""
}

proc open_socket {server port {keepopen {}}} {
    global TkGnats
    if {[info exists TkGnats(socket,$server,$port)]} {
        return $TkGnats(socket,$server,$port)
    }
    if {[catch {set s [socket $server $port]} rep]} {
        Msg "Error-1 opening socket/port $server $port:\n\n$rep"
        return "-1"
    }
    fconfigure $s -buffering line -translation crlf -buffersize 102400

    set rep [get_socket_reply $s]
    #puts "rep=$rep" 
    
    if {![string match 2* [lindex $rep 0]]} {
        Msg "Error-2 opening socket/port $server $port:\n" "[join $rep \n]"
        return "-1"
    }

    set TkGnats(socket,$server,$port) $s
    set rep [lindex $rep 0]
    if {[lsearch $rep GNATS] >= 0} {
        set TkGnats(GNATS_Version) [lindex $rep [expr [llength $rep] - 2]]
    }
    
    if {$keepopen != ""} {
        set TkGnats(socket,$server,$port,keepopen) $keepopen
    }
dputs "opened socket $server $port $keepopen"
    return $s
}

proc close_socket {server port {force {}}} {
    global TkGnats
    if {![info exists TkGnats(socket,$server,$port)]} {
dputs "non-exist return"
        return ""
    }
    #puts "force=$force exists=[info exists TkGnats(socket,$server,$port,keepopen)]"
    if {$force != "" || ![info exists TkGnats(socket,$server,$port,keepopen)]} {
        set    s $TkGnats(socket,$server,$port)
        puts  $s "QUIT"
        set   rep [get_socket_reply $s]
        #puts "rep=$rep"
        close $s
        unset TkGnats(socket,$server,$port)
        catch {unset TkGnats(socket,$server,$port,keepopen)}
dputs "closed socket $server $port $force"
    } {
dputs "non-close return"
    }
    return ""
}

proc get_gnats_list {type} {
    global TkGnats
    return [get_gnats_list_$TkGnats(GNATS_ACCESS_METHOD) $type]
}

proc get_gnats_list_batch {type} {
    global TkGnats
    return [eval exec $TkGnats(query-pr) $TkGnats(UseridPassword) --list-$type]
}

proc get_gnats_list_socket {type} {
    global TkGnats
    if {[set s [open_socket_gnatsd]] == "-1"} {
        return -1
    }
    case $type categories {
        set cmd {LIST Categories}
    } submitters {
        set cmd {LIST Submitters}
    } responsible {
        set cmd {LIST Responsible}
    } 

    gnatsd_send $s $cmd
    set rep [get_socket_reply $s]
    if {![string match 301* [lindex $rep 0]]} {
        Msg "GNATSD error getting GNATS $type list:\n" "[join $rep \n]"
        return -1
    }

    set glist [join [get_gnatsd_reply_dot_ended $s] \n]
#    dputs $glist    
    close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)
    
    return $glist
}

#
# Get the valid values for a non-built-in field.
# 
proc get_gnats_fieldvalue {type} {
    global TkGnats
    return [get_gnats_fieldvalue_$TkGnats(GNATS_ACCESS_METHOD) $type]
}

proc get_gnats_fieldvalue_batch {type} {
    global TkGnats
    return [eval exec $TkGnats(query-pr) $TkGnats(UseridPassword) --list-$type]
}

proc get_gnats_fieldvalue_socket {type} {
    global TkGnats
    if {[set s [open_socket_gnatsd]] == "-1"} {
        return -1
    }

    gnatsd_send $s "FVLD $type"
    set rep [get_socket_reply $s]
    if {![string match 301* [lindex $rep 0]]} {
        Msg "GNATSD error getting GNATS $type list:\n" "[join $rep \n]"
        return -1
    }

    set glist [join [get_gnatsd_reply_dot_ended $s] \n]
    #puts $glist    
    close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)
    
    return $glist
}

#
# get a list of valid gnats categories
#
proc get_categories {{pat "*"}} {
    global TkGnats
    set catlist {}
    set clist [get_gnats_list categories]
    foreach c [split $clist "\n"] {
        # ignore lines with leading hash or underscore
        case $c "#*" {
        } "_*" {
        } $pat {
            lappend catlist [lindex [split $c ":"] 0]
        }
    }
    if {$catlist == ""} {
        Msg "Cannot get GNATS categories"
        return ""
    }
    return [lsort $catlist]
}

#
# get a list of valid gnats submitters
#
proc get_submitters {{pat "*"}} {
    global TkGnats
    set sublist {}
    set slist [get_gnats_list submitters]
    foreach s [split $slist "\n"] {
        # ignore lines with leading hash or underscore
        case $s "#*" {
        } "_*" {
        } $pat {
            lappend sublist [lindex [split $s ":"] 0]
        }
    }
    if {$sublist == ""} {
        Msg "Cannot get GNATS submitters"
        return ""
    }
    return [lsort $sublist]
}

#
# get a list of valid gnats responsibles
#
proc get_responsibles {{pat "*"}} {
    global TkGnats
    set reslist {}
    set TkGnats(ResponsibleFile) {}
    set rlist [get_gnats_list responsible]
    foreach r [split $rlist "\n"] {
        # ignore lines with leading hash or underscore
        case $r "#*" {
        } "_*" {
        } $pat {
            lappend reslist [lindex [split $r ":"] 0]
            lappend TkGnats(ResponsibleFile) $r
        }
    }
    if {$reslist == ""} {
        Msg "Cannot get GNATS responsibles"
        return ""
    }
    return [lsort $reslist]
}

#
# get a list of valid gnats classes
#
proc get_classes {} {
    global TkGnats
    set reslist {}
    set TkGnats(ClassesFile) {}
    set classeslist {}
    set rlist [get_gnats_fieldvalue class]

    dputs "Classlist: $rlist"

    foreach r [split $rlist "\r\n"] {
        lappend classeslist $r 
        lappend TkGnats(ClassesFile) $r
    }
    if {$classeslist == ""} {
        Msg "Cannot get GNATS classes"
        return ""
    }
    return $classeslist
}

#
# get a list of valid gnats states
#
proc get_states {} {
    global TkGnats
    set reslist {}
    set TkGnats(StatesFile) {}
    set rlist [get_gnats_fieldvalue state]
    foreach r [split $rlist "\r\n"] {
        lappend stateslist $r 
        lappend TkGnats(StatesFile) $r
    }
    if {$stateslist == ""} {
        Msg "Cannot get GNATS states"
        return ""
    }
    return $stateslist
}

proc get_classes_list {} {
    global TkGnats
    if {$TkGnats(ClassesList) == ""} {
        if {$TkGnats(ClassesFile) == ""} {
            set  TkGnats(ClassesList) [get_classes] 
            if {$TkGnats(ClassesList) == ""} {
                Msg "The classes list is empty!"
                Exit 1
            }
        } {
            set clist ""
            foreach c $TkGnats(ClassesFile) {
                lappend clist [lindex [split $c ":"] 0]
            }
            set TkGnats(ClassesList) $clist
        }
    }
}

proc get_states_list {} {
    global TkGnats
    if {$TkGnats(StatesList) == ""} {
        if {$TkGnats(StatesFile) == ""} {
            set  TkGnats(StatesList) [get_states] 
            if {$TkGnats(StatesList) == ""} {
                Msg "The states list is empty!"
                Exit 1
            }
        } {
            set slist ""
            foreach s $TkGnats(StatesFile) {
                lappend slist [lindex [split $s ":"] 0]
            }
            set TkGnats(StatesList) $slist
        }
    }
}

proc get_category_list {} {
    global TkGnats
    if {$TkGnats(CategoryList) == ""} {
        set  TkGnats(CategoryList) [get_categories] 
        if {$TkGnats(CategoryList) == ""} {
            Msg "The categories list is empty!"
            Exit 1
        }
    }
}

proc get_submitter_list {} {
    global TkGnats
    if {$TkGnats(SubmitterList) == ""} {
        set  TkGnats(SubmitterList) [get_submitters] 
        if {$TkGnats(SubmitterList) == ""} {
            Msg "The submitters list is empty!"
            Exit 1
        }
    }
}

proc get_responsible_list {} {
    global TkGnats
    if {$TkGnats(ResponsibleList) == ""} {
        if {$TkGnats(ResponsibleFile) == ""} {
            set  TkGnats(ResponsibleList) [get_responsibles] 
            if {$TkGnats(ResponsibleList) == ""} {
                Msg "The responsibles list is empty!"
                Exit 1
            }
        } {
            set reslist ""
            foreach r $TkGnats(ResponsibleFile) {
                lappend reslist [lindex [split $r ":"] 0]
            }
            set TkGnats(ResponsibleList) [lsort $reslist]
        }
    }
}

proc get_audittrail_state {field} {
    global TkGnats

    if {[set s [open_socket_gnatsd]] == "-1"} {
        return -1
    }

    gnatsd_send $s "FIELDFLAGS $field"
    
    set rep [get_socket_reply $s]

    dputs "gnatsd FIELDFLAGS $field says $rep"

    if {![string match 350* [lindex $rep 0]]} {
	Msg "GNATSD error determining FIELDFLAGS $field: \n" "[join $rep \n]"
	return -1
    }

    if {[lsearch [lindex $rep 0] "requireChangeReason"] >= 0} {
	set TkGnats(Require-Reason-$field) 1
    } {
	set TkGnats(Require-Reason-$field) 0
    }

    dputs "gnatsd FIELDFLAGS $field results in $TkGnats(Require-Reason-$field)"
    return $TkGnats(Require-Reason-$field)
}
	
proc fullname_from_logname {{lname ""}} {
    global TkGnats
    if {"$lname" == ""} {
        set lname $TkGnats(LogName)
    }
    set fullname ""
    # Get the users name from the passwd file given their logname.
    # First try NIS, then try the regular passwd file...
    if {[catch {set fullname [exec niscat passwd.org_dir | grep ^${lname}: | cut -f5 -d:]}]} {
        if {[catch {set fullname [exec ypcat passwd | grep ^${lname}: | cut -f5 -d:]}]} {
            set fullname [lindex [split [file_search_string /etc/passwd ^${lname}:] :] 4]
        }
    }
    return [lindex [split $fullname ,] 0]
}

proc radiobar_frame {parent frname} {
    frame $frname
    frame $frname.labels
    frame $frname.values
    frame $frname.bars
    grid  columnconfigure $frname 0 -weight 0
    grid  columnconfigure $frname 1 -weight 0
    grid  columnconfigure $frname 2 -weight 1
}

# text field related procs

proc textset {l t {p ""} {e text}} {
    # a label
    if {[winfo exists ${p}._${l}.textlabel]} {
        ${p}._${l}.textlabel configure -text $t
        return
    }
    # text widget
    if {[winfo exists ${p}._${l}_fr.text]} {
        ${p}._${l}_fr.text delete 1.0 end
        ${p}._${l}_fr.text insert 1.0 $t
        return
    }
    # entry widget (text and date)
    if {[winfo exists ${p}._${l}.$e]} {
        set state [${p}._${l}.$e cget -state]
        ${p}._${l}.$e config -state normal
        ${p}._${l}.$e delete 0 end
        ${p}._${l}.$e insert 0 $t
        ${p}._${l}.$e config -state $state
        return
    }
    #error "no such window for $l"
}

proc textget {l {p ""} {e text}} {
    if {[catch  {set x [lindex [${p}._${l}.textlabel configure -text] 4]}  ]} {
        if {[catch {set x [${p}._${l}_fr.text get 1.0 end]}]} {
            #return [string trim [${p}._${l}.$e get] "\n"]
            if {[catch {string trim [${p}._${l}.$e get] "\n"} x]} {
                return ""
            } {
                return $x
            }
        }
    }
    return "\n$x"
}

proc readonly_singletext {l {t ""} {labwid 12} {valwid 0}} {
    global TkGnats
    if {$valwid == 0} {
        set valwid [string length "$t"]
    }
    set alias [get_field_alias $l]
    set f  [frame ._${l}]
    set lw [button $f.label    -anchor w -width $labwid -text "${alias}: " -command "helpMsg $alias" \
            -relief flat -padx 0 -pady 0 -borderwidth 0 -highlightthickness 0]
    set ew [label $f.textlabel -anchor w -width $valwid -text "$t" -relief groove \
            -highlightthickness 0 -borderwidth 2 -background $TkGnats(ReadOnlyBackground) -padx 2 -pady 0]
    pack $lw -side left -anchor w -pady 0 -padx 2 -ipady 0
    pack $ew -side left -anchor w -pady 0 -ipady 0
    pack $f  -side top  -anchor w -pady 0 -fill x -ipady 0
    return $ew
}

bind Entry <KeyPress-Return> " "

proc check_date_invalid {date {msg {Invalid date string:}}} {
    if {[regexp {^[0-9][0-9]?[0-9]?[0-9]?-[0-9][0-9]?-[0-9][0-9]?([ \t]?[0-9][0-9]?:[0-9][0-9]?)?$} $date]} {
        # convert yy-mm-dd format coming in
        set t [convert_date_format $date]
    } {
        set t $date
    }
    if {[catch {clock scan $t} err]} {
        Msg "$msg $date"
        return 1
    }
    return 0
}

proc normalize_date {date} {
    # Return date in YY-MM-DD format
    if {[regexp {^[0-9][0-9]?[0-9]?[0-9]?-[0-9][0-9]?-[0-9][0-9]?([ \t]?[0-9][0-9]?:[0-9][0-9]?)?$} $date]} {
        # convert yy-mm-dd format coming in
        set t [convert_date_format $date]
    } {
        set t $date
    }
    return [clock format [clock scan $t] -format "%Y-%m-%d"]
}

proc convert_date_format {date} {
    # convert from query-pr "yy-mm-dd hh:mm" format to "mm/dd/yy hh:mm" for Tcl clock command
    set dt  [split $date]
    set d   [split [lindex $dt 0] -]
    return "[lindex $d 1]/[lindex $d 2]/[lindex $d 0] [lindex $dt 1]"
}

proc get_datesel {w title} {
    global TkGnats
    set date [datesel [$w get] $title $TkGnats(textfont)]
    if {$date != ""} {
        $w delete 0 end
        $w insert 0 $date
    }
}

proc daterange {parent l w {t1 ""} {t2 ""} {labwid 12}} {
    global TkGnats
    set f [frame $parent._${l}]
    set alias [get_field_alias $l]
    set lw [button $f.label -anchor w -width $labwid -text "${alias}: " \
            -command "helpMsg $alias" -relief flat -padx 0 -pady 0 -borderwidth 0]
    pack $lw -side left -anchor w
    set lw1 [button $f.label1 -anchor center -width 4 -text from \
            -command "get_datesel $f.after \"$alias - from\"" \
            -relief raised -padx 0 -pady 0 -borderwidth 1]
    set ew1 [entry $f.after  -width 11 \
        -insertwidth 1  -insertofftime 400 -highlightthickness 2 \
        -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground)]
    set_focus_style $ew1
    $ew1 insert end $t1
    pack $lw1 -side left -anchor w
    pack $ew1 -side left -anchor w -fill none -expand 0
    set lw2 [button $f.label2 -anchor c -width 2 -text to \
            -command "get_datesel $f.before \"$alias - to\"" \
            -relief raised -padx 0 -pady 0 -borderwidth 1]
    set ew2 [entry $f.before -width 11 \
        -insertwidth 1  -insertofftime 400 -highlightthickness 2 \
        -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground)]
    set_focus_style $ew2
    $ew2 insert end $t2
    bind $ew1 <3> "clipboard_post $ew1 %X %Y"
    bind $ew2 <3> "clipboard_post $ew2 %X %Y"
    pack $lw2 -side left -anchor w
    pack $ew2 -side left -anchor w -fill none -expand 0
    pack $f   -side top  -anchor w -fill x -pady 2
    return "$ew1 $ew2"
}

proc singletext {parent l w {t ""} {labwid 12}} {
    global TkGnats
    set alias [get_field_alias $l]
    set f [frame $parent._${l}]
    # trim off any leading >'s for the label text
    set lw [button $f.label -anchor w -width $labwid -text "${alias}: " \
            -command "helpMsg $alias" -relief flat -padx 0 -pady 0 -borderwidth 0]
    set ew [entry $f.text -width $w \
        -insertwidth 1 -insertofftime 400 -highlightthickness 2 \
        -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground)]
    set_focus_style $ew
    $ew insert end  $t

    bind $ew <3> "clipboard_post $ew %X %Y"
    if {[check_suppressed_field [string trimleft $l >]] == 2} {
        $ew configure -state disabled -background $TkGnats(ReadOnlyBackground) -highlightcolor grey85
    }
    pack $lw -side left -anchor w
    pack $ew -side left -anchor w -fill x -expand true
    if {[string first "Date-Required" $l] >= 0} {
        set cw [button $f.cal -anchor c -width 13 -text "Calendar..." \
            -command "get_datesel $f.text $l" \
            -relief raised -padx 0 -pady 0 -borderwidth 2]
        pack $cw -side left -anchor w -padx 1
    }
    pack $f  -side top  -anchor w -fill x -pady 2
    return $ew
}

proc bagged_singletext {l w bagname {prefix ""} {t ""}} {
    global TkGnats
    upvar #0 $bagname bag
    set f [frame ._${l}]
    set lw [label $f.label -anchor w -text "$l: "]
    set ew [entry $f.text -width 80 \
        -insertwidth 1  -insertofftime 400  \
        -relief sunken -borderwidth 2 -background $TkGnats(EditFieldBackground)]
    set_focus_style $ew
    $ew insert end $t
    pack $lw -side left  -anchor w -fill x -expand true
    pack $ew -side right -anchor e
    pack $f  -side top   -anchor w -fill x -pady 2
    set bag($prefix$l) [format {[string trim [%s get]]} $lw]
    return $ew
}

proc multitext {lbl h} {
    global TkGnats
    set l _$lbl
    set f [frame .${l}_fr]
    button $f.label -anchor w -text "[string trimleft $lbl >]: " \
            -command "helpMsg [string trimleft $lbl >]" \
            -relief flat -padx 0 -pady 0 -borderwidth 0
    text $f.text \
        -wrap $TkGnats(TextWrap) \
        -yscrollcommand "$f.sb set" \
        -height $h -width 80 -relief sunken -padx 4 -insertwidth 1 \
        -insertofftime 400 -borderwidth 2 -background $TkGnats(EditFieldBackground)
    set_focus_style $f.text
    scrollbar $f.sb -command "$f.text yview" -relief sunken
    pack $f.label -side top   -anchor w
    pack $f.sb    -side left  -fill y
    pack $f.text  -side right -fill both -expand true
    pack $f       -side top   -fill both -expand true -padx 16 -pady 2

    return $f.text
}
proc set_focus_style {w} {
    global TkGnats
    if {$TkGnats(FocusStyle) == "mouse"} {
        bind $w <Enter> "+focus $w"
    }
}

proc set_text_traversal {tlist} {
    set ll [llength $tlist]
    if {$ll < 2} {
        return
    }
    for {set x 1} {$x<$ll} {incr x} {
        set w [lindex $tlist $x]
        set prevw [lindex $tlist [expr $x-1]]
        bind $prevw <Tab>       "focus $w
        break"
        bind $prevw <Control-n> "focus $w"
    }
    bind [lindex $tlist [expr $ll-1]] <Tab>       "focus [lindex $tlist 0]
    break"
    bind [lindex $tlist [expr $ll-1]] <Control-n> "focus [lindex $tlist 0]"
    
    bind [lindex $tlist 0] <Shift-Tab> "focus [lindex $tlist [expr $ll-1]]
        break"
    bind [lindex $tlist 0] <Control-p> "focus [lindex $tlist [expr $ll-1]]"
    for {set x 0} {$x < [expr $ll-1]} {incr x} {
        set w [lindex $tlist $x]
        set nextw [lindex $tlist [expr $x+1]]
        bind $nextw <Shift-Tab> "focus $w
        break"
        bind $nextw <Control-p> "focus $w"
    }
}

proc bagged_radiobar {fr n labeltext blist offLabel dstbag {valwid 0} {rownum 0}} {
    radiobar $fr $n $labeltext $blist $offLabel > $dstbag $valwid $rownum
}

# make one in a list a radiobutton bar
proc radiobar {fr n labeltext blist offLabel {varprefix ""} {aname ""} {valwid 0} {rownum 0}} {
    global TkGnats flds tcl_platform
    if {$tcl_platform(platform) == "unix"} {
        set buttonbd 2
    } {
        set buttonbd 0
    }
    if {"$aname" != ""} {
        set vname [set aname]($varprefix$labeltext)
    } {
        set vname $varprefix$labeltext
    }
    global $vname

    # alternate the colours so one can see the seperations
    if {($rownum & 1) == 1} {
	set panelbg "lightblue"
    } { 
	set panelbg "grey"
    }

    set alias [get_field_alias $labeltext]
    set $vname ""
    button $fr.labels_$n -text "${alias}: " -command "helpMsg $alias" \
            -relief flat -padx 0 -pady 0 -borderwidth 0 -width 14 -anchor w \
            -highlightthickness 0 -borderwidth 0

    grid   $fr.labels_$n -in $fr -column 0 -row $rownum -sticky w -padx 0 -pady 0 -ipady 0
    
    if {$valwid != 0} {
        label $fr.values_$n -text "[string trim $flds($varprefix$labeltext) " \n\t"]" \
                -relief groove -anchor w -width $valwid -background $TkGnats(ReadOnlyBackground) \
                -padx 2 -pady 0 -highlightthickness 0 -borderwidth 2
        grid  $fr.values_$n -in $fr -column 1 -row $rownum -sticky {w n s} -padx 0 -pady 0 -ipady 0
    }

    if {[check_suppressed_field $labeltext] == 2} {
        set state disabled
    } {
        set state normal
    }
    frame $fr.bars_$n -bg $panelbg
    set bcount 0
    set bframe 0
    frame $fr.bars_$n.$bframe -bg $panelbg
    pack $fr.bars_$n.$bframe -side top -anchor nw -fill x

    foreach b $blist {
        radiobutton $fr.bars_$n._$b -bg $panelbg \
                -text $b -relief flat -variable $vname -pady 0 \
                -highlightthickness 0 -borderwidth $buttonbd -state $state
        # Buttons that say None should set variable to the empty
        # string...
        if {"$b" == "$offLabel"} {
            $fr.bars_$n._$b configure -value ""
        } {
            $fr.bars_$n._$b configure -value $b
        }
        pack $fr.bars_$n._$b -in $fr.bars_$n.$bframe -side left -anchor w -padx 8 -pady 0 -fill none -expand 0 -ipady 0
	incr bcount
	if {$bcount > 7} {
	    set bcount 0
	    incr bframe
	    frame $fr.bars_$n.$bframe -bg $panelbg
	    pack $fr.bars_$n.$bframe -side top -anchor nw -fill x
	}
    }
    grid $fr.bars_$n -in $fr -column 3 -row $rownum -sticky {w e} -padx 0 -pady 0
}

proc radiobar_set {fr n b} {
    $fr.bars_$n._$b invoke
}

# make one in a list a radiobutton bar
proc checkbar {fr n labeltext blist offLabel {rownum 0}} {
    global tcl_platform
    if {$tcl_platform(platform) == "unix"} {
        set buttonbd 2
    } {
        set buttonbd 0
    }
    upvar #0 gbag ${labeltext}

    # alternate the colours so one can see the seperations
    if {($rownum & 1) == 1} {
	set panelbg "lightblue"
    } { 
	set panelbg "grey"
    }

    set alias [get_field_alias $labeltext]
    button $fr.labels_$n -text "${alias}: " -command "helpMsg $alias" \
            -relief flat -width 14 -padx 0 -pady 0 -borderwidth 0 -anchor w -highlightthickness 0

    grid   $fr.labels_$n -in $fr -column 0 -row $rownum -sticky w -padx 0 -pady 0 -ipady 0

    frame $fr.bars_$n -bg $panelbg
    set bcount 0
    set bframe 0
    frame $fr.bars_$n.$bframe -bg $panelbg
    pack $fr.bars_$n.$bframe -side top -anchor nw -fill x

    foreach b $blist {
        checkbutton $fr.bars_$n._$b -bg $panelbg \
                -offvalue "" \
                -text $b -relief flat -highlightthickness 0 -borderwidth $buttonbd \
                -variable [format "%s(%s)" ${labeltext} ${b}] -pady 0

        # Buttons that say None should set variable to the empty string...
        if {"$b" == "$offLabel"} {
            $fr.bars_$n._$b configure -onvalue "_ALL_" -offvalue ""
        } {
            $fr.bars_$n._$b configure -onvalue $b -offvalue ""
        }
        set gbag($b) ""
        pack $fr.bars_$n._$b -in $fr.bars_$n.$bframe -side left -anchor w -padx 8 -pady 0 -fill none -expand 0 -ipady 0

	incr bcount
	if {$bcount > 7} {
	    set bcount 0
	    incr bframe
	    frame $fr.bars_$n.$bframe -bg $panelbg
	    pack $fr.bars_$n.$bframe -side top -anchor nw -fill x
	}
    }
    # set active [lindex $blist 0]
    # $fr.bars_$n.$active select

    grid $fr.bars_$n -in $fr -column 3 -row $rownum -sticky {w e} -padx 0 -pady 0
}

#
# convert some numeric fields in a 'query-pr --sql2'
# record named 'f' in the caller to mnemonic strings
#
proc convertsqlflds {f} {
    upvar 1 $f flds
    foreach a [array names flds] {
        set n $flds($a)
        case $a Severity {
            case $n 1 { } 2 { } 3 { }
        } Priority {
            case $n 1 { } 2 { } 3 { }
        }
    }
}

#
# split a pr stream into a tcl array named v
#
# A special array index called _prefix_ contains  all the text prior to
# to the first gnats field
#
proc parsepr_txt {txt varname} {
    upvar 1 $varname fields
    set gnats_tag_exp {^(>[^:]+):(.*)}
    set mail_tag_exp {^([A-Z][^:]+):[ 	]*(.*)}
    set no_gnats_tags_yet 1
    set fields(_prefix_) ""
    set fldtags {_prefix_}

    set leftoverln ""
    set prtxt [split $txt "\n"]
    set prlen [llength $prtxt]
    set pridx 0
    while {1} {

        if {"$leftoverln" == ""} {
            if {$pridx >= $prlen} {
                break
            }
            set ln [lindex $prtxt $pridx]
            incr pridx
        } {
            set ln $leftoverln
            set leftoverln ""
        }

        set tag ""
        set val ""

        regexp $gnats_tag_exp $ln matched tag val
        if {"$tag" != ""} {
            set no_gnats_tags_yet 0
            # a gnats tag
            # gnats tags can be multiline so now get all the lines 'till the next gnats tag
            lappend fldtags $tag
            set fields($tag) "$val\n"
            while {$pridx < $prlen} {
                set ln [lindex $prtxt $pridx]
                incr pridx
                set tag2 ""
                regexp $gnats_tag_exp $ln matched tag2 val
                if {"$tag2" != ""} {
                    #  a new gnats tag so we have hit the end of the 
                    # current one.. leave the line we just read in
                    # leftoverln and continue on in the loop
                    set leftoverln $ln
                    break;
                }
                append fields($tag) "$ln\n"
            }
            continue
        }

        # If we get here the current line is not part of a gnats tag value pair
        if {$no_gnats_tags_yet} {
            append fields(_prefix_) "$ln\n"
        }
        
        set tag ""
        set val ""
        # Here is where we split out regular mail headers if needed.
        regexp $mail_tag_exp $ln matched tag val
        if {"$tag" != ""} {
            # mail header tags can be multiline so now
            # get all the lines 'till the next gnats or mail tag
            lappend fldtags $tag
            set fields($tag) "[string trim $val]\n"
            # _prefix_len_ is used in writepr to know how many lines to replace (ie Reply-To).
            set fields(_prefix_len_$tag) 1
            while {$pridx < $prlen} {
                set ln [lindex $prtxt $pridx]
                incr pridx
                set tag2 ""
                set tag3 ""
                regexp $gnats_tag_exp $ln matched tag2 val
                regexp $mail_tag_exp  $ln matched tag3 val
                if {"$tag2$tag3" != ""} {
                    # a new gnats or mail tag so we have hit the end of the 
                    # current one.. leave the line we just read in
                    # leftoverln and continue on in the loop
                    set leftoverln $ln
                    break;
                }
                append fields(_prefix_) "$ln\n"
                set    fields($tag) "[string trim "[string trim $fields($tag)] [string trim $ln]"]\n"
                incr   fields(_prefix_len_$tag)
            }
        } {
            # This is a header line that doesn't match the mail_tag_exp, such as the first "From ".
        }
    }

    return $fldtags
}

proc write_listbox {lbname fname} {
    set fout [open $fname w]
    set sz [$lbname size]
    for {set x 0} {$x < $sz} {incr x 1} {
        puts $fout [$lbname get $x]
    }
    close $fout
}

proc write_listbox_selection {lbname fname} {
    set    fout [open $fname w]
    puts  $fout [$lbname get [$lbname curselection]]
    close $fout
}

proc foreach_listbox {lbname procname} {
    set sz [$lbname size]
    for {set x 0} {$x < $sz} {incr x 1} {
        if {[$procname [$lbname get $x]] != 0} {
            return
        }
    }
}

proc get_max_strlen { l } {
    set maxlen 0
    foreach e $l {
        if {[string length $e] > $maxlen} {
            set maxlen [string length $e]
        }
    }
    return $maxlen
}

proc build_sort_cmd {fieldnames fieldflgs sortfields} {
    global TkGnats
    return [build_sort_cmd_$TkGnats(QuerySortMethod) $fieldnames $fieldflgs $sortfields]
}

proc build_sort_cmd_external {fieldnames fieldflgs sortfields} {
    set rval "-fb -t|"
    foreach fname $sortfields {
        set idx  [lsearch $fieldnames $fname]
        set flgs [lindex  $fieldflgs  $idx]
        append rval [format " +%d%s -%d" $idx $flgs [expr $idx+1]]
    }
    return "$rval"
}

proc build_sort_cmd_internal {fieldnames fieldflgs sortfields} {
    set rval ""
    foreach fname $sortfields {
        append rval " [lsearch $fieldnames $fname]"
    }
    return "$rval"
}

proc sort_listbox {lb} {
    set vals [lsort [$lb get 0 end]]
    $lb delete 0 end
    eval $lb insert end $vals
}

proc clipboard_create {} {
    event add <<Cut>> <Control-Key-X>
    event add <<Copy>> <Control-Key-C>
    event add <<Paste>> <Control-Key-V>
    set   m .clipboardmenu
    menu $m -tearoff 0
    $m add command -label "Cut"        -accel "Ctrl+X"
    $m add command -label "Copy"       -accel "Ctrl+C"
    $m add command -label "Paste"      -accel "Ctrl+V"
    $m add command -label "Delete"     -accel "Delete"
    $m add separator
    $m add command -label "Select All" -accel "Ctrl+/"
}

proc clipboard_post {w x y} {
    set m .clipboardmenu
    if {[winfo class $w] == "Text"} {
        set sel [$w tag ranges sel]
    } {
        if {[$w selection present]} {
            set sel "1"
        } {
            set sel ""
        }
    }
    if {$sel == ""} {
        set selstate disabled
    } {
        set selstate normal
    }
    set editstate [$w cget -state]
    if {$editstate == "disabled"} {
        set cutstate $editstate
    } {
        set cutstate $selstate
    }
    if {[catch {selection get -displayof $w -selection CLIPBOARD} sel] || $sel == ""} {
        set pastestate disabled
    } {
        set pastestate $editstate
    }
    $m  entryconfig "Cut"        -command "clipboard_doit $w <Cut>"   -state $cutstate
    $m  entryconfig "Copy"       -command "clipboard_doit $w <Copy>"  -state $selstate 
    $m  entryconfig "Paste"      -command "clipboard_doit $w <Paste>" -state $pastestate
    $m  entryconfig "Delete"     -command "clipboard_doit $w  Delete" -state $cutstate
    $m  entryconfig "Select All" -command "clipboard_doit $w  Control-slash"
    tk_popup $m $x $y
}

proc clipboard_doit {w opt} {
    event generate $w <$opt>
}

proc make_txt_mb {multitextflds} {
    global TkGnats
    global flds
    set   f [frame .multiline -relief groove  -borderwidth 2]
    
    frame  .mb
    button .mb.lab -text "Text Fields =>" -width 14 -anchor w -command "helpMsg Text-Fields" \
            -relief flat -padx 0 -pady 0 -borderwidth 0
    pack   .mb.lab -side left -anchor w
    set count 0
    set num   0
    foreach field $multitextflds {
        if {[check_suppressed_field $field] != 1} {

	    if {[info exists flds($field)]} {
		set tlength [string length $flds($field)]
		#puts stderr "field $field ($flds($field) has length: $tlength"
	    } { 
		set tlength 0
	    }
	    if {$tlength < 2} {
		set buttonbackground "grey"
	    } {
		set buttonbackground "lightblue"
	    }
            set alias [get_field_alias $field]
            button .mb.b$num -text "${alias}:" -command "switch_txt $field {$multitextflds}" -bg $buttonbackground
            pack   .mb.b$num -side left -anchor w
            incr   count
            if {$count == 1} {
                set TkGnats(first_multitext) $field
            }
        }
        incr num
    }

    if {$count == 0} {
        wm withdraw .
        Msg "GNATS config error: at least one Multitext field must be included but all have been suppressed in the gnats-adm/config file."
        exit 1
    }
    
    button .mb.insert -text "Insert File..." -command "insert_file .multiline.text"
    pack   .mb.insert -side right -anchor e

    pack   .mb -side top -anchor w -fill x -in $f

    text $f.text -height 12 -width 80 -relief sunken -padx 4 -insertwidth 1 \
            -wrap $TkGnats(TextWrap) -yscrollcommand "$f.sby set" -xscrollcommand "$f.sbx set" \
            -highlightthickness 2 -insertofftime 400 -borderwidth 2 -font $TkGnats(TextFont)
    #set TkGnats(mtextbackground) [$f.text cget -background]
    set TkGnats(mtextbackground) [.mb.insert cget -background]
    $f.text configure -background $TkGnats(EditFieldBackground)
    
    set_focus_style $f.text
    bind $f.text <Tab>   ""
    bind $f.text <3> "clipboard_post $f.text %X %Y"
    scrollbar $f.sby -command "$f.text yview" -relief sunken

    # Create padding based on the y scrollbar width and border
    frame $f.bottom
    scrollbar $f.sbx -command "$f.text xview" -borderwidth 2 -orient horizontal
    set pad [expr [$f.sby cget -width] + 2 * \
            ([$f.sby cget -bd] + [$f.sby cget -highlightthickness])]
    frame $f.pad -width $pad -height $pad

    pack  $f.pad    -in $f.bottom -side left        
    pack  $f.sbx    -in $f.bottom -side bottom -fill x
    pack  $f.bottom -side bottom -fill x 

    pack  $f.sby    -side left  -fill y
    pack  $f.text   -side right -fill both -expand true
    pack  $f        -side top   -fill both -expand true -pady 4
    
    return "$f.text"
}

proc flush_multitext {} {
    global current_multi_text frm
    set f .multiline
    if {"$current_multi_text" != ""} {
        set frm($current_multi_text) "[string trim [$f.text get 1.0 end] "\t\n "]\n"
    }
}

proc flush_singletext {lst {p ""}} {
    global frm
    #set frm($tag) [string trim [textget $tag] "\t\n "]
    foreach tag $lst {
        if {[catch {string trim [textget $tag $p] "\t\n "} frm($tag)]} {
            set frm($tag) ""
        }
    }
    if {[info exists frm(X-GNATS-Notify)]} {
        set frm(X-GNATS-Notify) [fix_email_addresses $frm(X-GNATS-Notify)]
    }
}

proc switch_txt {name multitextflds} {
    global TkGnats current_multi_text frm
    set f .multiline

    # write the current text out back into the frm bag
    flush_multitext

    # reset the currently selected button relief and command
    set num [lsearch $multitextflds $current_multi_text]
    if {$num >= 0} {
        .mb.b$num configure -relief raised \
                -command "switch_txt $current_multi_text {$multitextflds}"
    }

    # load the text widget with the new text
    if {[$f.text cget -state] == "disabled"} {
        $f.text configure -state normal   -background $TkGnats(EditFieldBackground) \
                -highlightcolor black
	catch {.mb.insert configure -state normal}
    }
    $f.text delete 1.0 end
    $f.text insert 1.0 $frm($name)
    if {[check_suppressed_field [string trimleft $name >]] == 2} {
        $f.text configure -state disabled -background $TkGnats(mtextbackground) \
                -highlightcolor grey85
	catch {.mb.insert configure -state disabled}
    }

    # set the newly selected button relief and command
    set current_multi_text $name
    set num [lsearch $multitextflds $current_multi_text]
    .mb.b$num configure -relief sunken -command "helpMsg [get_field_alias $current_multi_text]"
}

proc save_sort_fields {sortfile sortfields} {
    if {$sortfile != ""} {
        set    fout [open $sortfile "w"]
        puts  $fout "set Query(user_sort_flds) \{$sortfields\}"
        close $fout
    }
}

proc insert_file {w} {
    global TkGnats tk_strictMotif
    set initialdir ""
    if {[info exists TkGnats(InsertFileDir)]} {
        if {[file isdirectory $TkGnats(InsertFileDir)]} {
            set   initialdir  $TkGnats(InsertFileDir)
        }
    }
    if {$initialdir == ""} {
        if {[info exists TkGnats(HOME)]} {
            set initialdir $TkGnats(HOME)
        } {
            set initialdir [pwd]
        }
    }
    set tk_strictMotif 1
    set file [tk_getOpenFile -initialdir $initialdir -title "Enter filename to insert"]
    set tk_strictMotif 0
    if {$file != ""} {
        set fin [open $file r]
        $w insert insert [read $fin]
        close $fin
    }
}

proc busy_cursor {opt} {
    global BusyCursor
    switch $opt {
        set {
            set BusyCursor [list {. left_ptr}]
            set list [winfo children .]
            while {$list != ""} {
                set next {}
                foreach  w $list {
                    set  cursor [lindex [$w config -cursor] 4]
                    set  class  [winfo class $w]
                    if {$class == "Toplevel" || $cursor != ""} {
                        lappend BusyCursor [list $w $cursor]
                    }
                    set next [concat $next [winfo children $w]]
                }
                set list $next
            }
            foreach w $BusyCursor {
                catch {[lindex $w 0] config -cursor watch}
            }
            update idletasks
        }
        clear {
            foreach w $BusyCursor {
                catch {[lindex $w 0] config -cursor [lindex $w 1]}
            }
        }
    }
}

proc file_search_string {file string} {
    if {[catch {open $file r} fin]} {
        return ""
    }
    set    text [split [read $fin] "\n"]
    close  $fin
    return [lindex $text [lsearch -regexp $text $string]]
}

proc file_get_text {file} {
    if {[catch {open $file r} fin]} {
        return ""
    }
    set    text [read $fin]
    close  $fin
    return [string trimright $text "\n"]
}

proc file_put_text {file text} {
    if {[catch {open $file w} fout]} {
        return ""
    }
    puts   $fout $text
    close  $fout
    return $file
}

proc get_socket_reply {s} {
    set rep [gets $s]
    while {[regexp {^[0-9]+-} $rep]} {
        lappend reply $rep
        set rep [gets $s]
    }
    lappend reply $rep
    return $reply
}

proc check_release_based_batch {} {
    global TkGnats
    catch {eval exec $TkGnats(query-pr) --keywords} rep
    if {[string first "unrecognized option" $rep] < 0} {
        set TkGnats(ReleaseBased) 1
    } {
        set TkGnats(ReleaseBased) 0
    }
}

proc check_release_based_socket {} {
    global TkGnats
    set TkGnats(ReleaseBased) 0
    if {[set s [open_socket_gnatsd]] == "-1"} {
        return -1
    }

    gnatsd_send $s "KYWD"
    set rep [get_socket_reply $s]
    #puts "rep=$rep"   
    if {[string first "Unrecognized command" [lindex $rep 0]] < 0} {
        set TkGnats(ReleaseBased) 1
    } {
        set TkGnats(ReleaseBased) 0
    }
    
    close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)
}

proc check_release_based {} {
    global TkGnats
    check_release_based_$TkGnats(GNATS_ACCESS_METHOD)
}

#proc get_gnats_server_config_batch {} {
#    global TkGnats
#    if {[catch {eval exec $TkGnats(query-pr) --list-config} config] != 0 || \
#            [string first "unrecognized option" $config] >= 0} {
#       Msg "Error getting server CONFIG list:\n" "[lindex [split $config \n] 0]"
#	return -1
#    }
#    return $config
#}
#
#proc get_gnats_server_config_socket {} {
#    global TkGnats
#    if {[set s [open_socket_gnatsd]] == "-1"} {
#        return -1
#    }
#
#    gnatsd_send $s "LCFG"
#    set rep [get_socket_reply $s]
#    #puts "rep=$rep"  
#    if {![string match 2* [lindex $rep 0]]} {
#        Msg "GNATSD error getting server CONFIG list:\n" "[join $rep \n]"
#        return -1
#    }
#
#    set config [join [get_gnatsd_reply_dot_ended $s] \n]
#    
#    close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)
#    return $config
#}

proc get_gnats_server_config {} {
    global TkGnats
#    set config [get_gnats_server_config_$TkGnats(GNATS_ACCESS_METHOD)]
#    if {$config == "-1"} {
#	return
#    }
#    foreach option [split $config \n] {
#	set val [split [string trim $option "{} \t\n\r"] "="]
#        set TkGnats(SERVER-[lindex $val 0]) [string trim [lindex $val 1] {\\"}]
#    }

    

    if {![info exists TkGnats(SERVER-REQUIRE_AUDIT_TRAIL_ENTRY)]} {
        set TkGnats(SERVER-REQUIRE_AUDIT_TRAIL_ENTRY)  "Responsible,State,Priority,Severity,Date-Required"
    }
    if {![info exists TkGnats(SERVER-REQUIRE_AUDIT_TRAIL_REASON)]} {
        set TkGnats(SERVER-REQUIRE_AUDIT_TRAIL_REASON) "Responsible,State,Priority,Severity,Date-Required"
    }
    if {![info exists TkGnats(SERVER-REQUIRE_AUDIT_TRAIL_EMAIL)]} {
        set TkGnats(SERVER-REQUIRE_AUDIT_TRAIL_EMAIL)  "Responsible,State,Priority,Severity,Date-Required"
    }
    if !{[info exists TkGnats(SERVER-SUPPRESSED_FIELDS)]} {
        set TkGnats(SERVER-SUPPRESSED_FIELDS)      {}
    }
    if {![info exists TkGnats(SERVER-SUPPRESSED_SEND_FIELDS)]} {
        set TkGnats(SERVER-SUPPRESSED_SEND_FIELDS) {State,Responsible,Release-Note,Unformatted,Audit-Trail}
    }
    if {![info exists TkGnats(SERVER-SUPPRESSED_EDIT_FIELDS)]} {
        set TkGnats(SERVER-SUPPRESSED_EDIT_FIELDS) {}
    }
    if {![info exists TkGnats(SERVER-MANDATORY_FIELDS)]} {
        # Responsible, even if TkGnats(ResponsibleInSendPr)=1, still doesn't need to be mandatory by default.
        set TkGnats(SERVER-MANDATORY_FIELDS) "Class,State,Priority,Severity,Confidential,Category,Submitter-Id,Originator,Synopsis,Description"
    }
    if {![info exists TkGnats(SERVER-FIELD_ALIASES)]} {
        set TkGnats(SERVER-FIELD_ALIASES) {}
    }
    
    set TkGnats(SuppressedFields) ""
    foreach suppressed [split $TkGnats(SERVER-SUPPRESSED_FIELDS) ,] {
        if {[lsearch -exact "Text-Fields Number" $suppressed] < 0} {
            lappend TkGnats(SuppressedFields) [string trim $suppressed]
        }
    }

    set TkGnats(MandatoryFields) ""
    set mandatory [split $TkGnats(SERVER-MANDATORY_FIELDS) ,]
    foreach field $mandatory {
        set field [string trim $field]
        if {[lsearch -exact $TkGnats(SuppressedFields) $field] < 0} {
            lappend TkGnats(MandatoryFields) $field
        }
    }
    
    foreach {var parm} {RequireAudit REQUIRE_AUDIT_TRAIL_ENTRY RequireReason REQUIRE_AUDIT_TRAIL_REASON RequireEmail REQUIRE_AUDIT_TRAIL_EMAIL} {
        set TkGnats($var) ""
        foreach val [split $TkGnats(SERVER-$parm) ,] {
            lappend TkGnats($var) [string trim $val]
        }
    }
    
    foreach {var1 var2 parm} {SuppressedSendFields SuppressedSendFieldsUnless SUPPRESSED_SEND_FIELDS SuppressedEditFields SuppressedEditFieldsUnless SUPPRESSED_EDIT_FIELDS} {
        set TkGnats($var1) ""
        set TkGnats($var2) ""
        foreach val [split $TkGnats(SERVER-$parm) ,] {
            foreach {field unless junk} [split $val ()] {break}
            lappend TkGnats($var1) [string trim $field]
            lappend TkGnats($var2) [string trim $unless]
        }
    }
    
    set TkGnats(FieldAliases) ""
    set TkGnats(FieldAliasesReverse) ""
    foreach val [split $TkGnats(SERVER-FIELD_ALIASES) ,] {
        foreach {field alias junk} [split $val ()] {break}
        if {$alias == ""} {
            set alias $field
        }
        lappend TkGnats(FieldAliases)        [string trim $field] [string trim $alias]
        lappend TkGnats(FieldAliasesReverse) [string trim $alias] [string trim $field]
    }
}

proc check_audit_trail_opts {requirement field} {
    global TkGnats
    set field [string trimleft $field >]

    # if we have to do this for every field, then it is a no brainer.
    #if {[lsearch -exact $TkGnats(Require$requirement) "all"] >= 0} {
    #	return 1
    #}

    # if we found out that we needed to do it for this field, do it 
    if {[info exists TkGnats(Require-$requirement-$field)]} {
	dputs "Reason trail for $requirement $field $TkGnats(Require-$requirement-$field)"
	return $TkGnats(Require-$requirement-$field)
    }

    if {$requirement == "Reason"} {
	dputs "Reason audit trail for $requirement $field unknown, asking."
	return [get_audittrail_state $field]
    }
    
    # no way to check the other kinds yet.
    return 0
}

proc get_field_alias {f} {
    global TkGnats
    
    set field [string trimleft $f >]

    array set aliases $TkGnats(FieldAliases)
    if {[info exists aliases($field)]} {
        set field $aliases($field)
    }
    
    if {$field == "X-GNATS-Notify"} {
	set alias Notify-List
    } {
	set alias $field
    }

    return $alias
}

proc get_field_alias_reverse {field} {
    global TkGnats
    
    array set aliases $TkGnats(FieldAliasesReverse)
    if {[info exists aliases($field)]} {
        return $aliases($field)
    }

    return $field
}

proc check_mandatory_field {f} {
    global TkGnats
    
    # Return flags:
    # 0 not mandatory
    # 1 mandatory
    
    set field [string trimleft $f >]
    
    if {([lsearch -exact $TkGnats(MandatoryFields) "all"] >= 0  && \
            [lsearch -exact "Audit-Trail Unformatted Release-Note" $field] < 0) || \
	    [lsearch -exact $TkGnats(MandatoryFields) $field] >= 0} {
        # This field is mandatory.
        return 1
    }
    
    return 0
}

proc check_suppressed_field {f} {
    global TkGnats

    # Return flags:
    # 0 don't suppress
    # 1 suppress totally
    # 2 read-only in tkeditpr or tkviewpr
    
    set field [string trimleft $f >]
    
    if {[lsearch -exact $TkGnats(SuppressedFields) $field] >= 0} {
        # This field is totally suppressed.
        return 1
    }
    
    if {$TkGnats(CurrentProgram) == "tkviewpr"} {
        # View is all read-only.
        return 2
    }
    
    # The remainder (the "unless" options) only applies to Send and Edit
    set apps(tksendpr) Send
    set apps(tkeditpr) Edit

    if {![info exists apps($TkGnats(CurrentProgram))]} {
        # Don't suppress.
        return 0
    }
    set  app $apps($TkGnats(CurrentProgram))

    set  idx [lsearch -exact $TkGnats(Suppressed${app}Fields) $field]
    if {$idx < 0} {
        # No mention of this field for this app; don't suppress.
        return 0
    }

    # If suppressing, return 1 (suppress) for Send and 2 (read-only) for Edit.
    set suppressflags(Send)  1
    set suppressflags(Edit)  2
    
    if {[info exists TkGnats(${field}In${app}Pr)] && $TkGnats(${field}In${app}Pr) != "0"} {
        # User asked for it, so don't suppress.
        set useropt 0
    } {
        # Application suppress default from above.
        set useropt $suppressflags($app)
    }

    set serveropt [lindex $TkGnats(Suppressed${app}FieldsUnless) $idx]

    set flag $suppressflags($app)
    switch $serveropt {
        user {
            # Whatever the user asked for.
            set flag $useropt
        }
        edit {
            if {$TkGnats(edit_authorized)} {
                # User's edit authorized, so don't suppress.
                set flag 0
            }
        }
        useredit -
        edituser {
            if {$TkGnats(edit_authorized)} {
                # User's edit authorized, so whatever the user asked for is OK.
                set flag $useropt
            }
        }
        admin {
            if {$TkGnats(UserAccess) == "admin"} {
                # User's admin authorized, so don't suppress.
                set flag 0
            }
        }
        useradmin -
        adminuser {
            if {$TkGnats(UserAccess) == "admin"} {
                # User's admin authorized, so whatever the user asked for is OK.
                set flag $useropt
            }
        }
    }

    return $flag
}

proc get_gnatsd_reply_dot_ended {s} {
    set reply ""
    set rep [gets $s]
    while {$rep != "."} {
        if {[string first "410 Invalid PR " $rep] == 0} {
            # TTD: There's a bug in gnatsd. If you send "SQL2 23 547" and 547 doesn't exist,
            #      it gives 410 error and closes the socket. We need to do one PR at a time.
            #      This just affects selection query and Number field query.
            #      See query_cmd_socket.
            break
        }
        lappend reply $rep
        set rep [gets $s]
    }
    # don't need the dot at the end
    # lappend reply $rep
    return $reply
}
 
proc escape_dots {txt} {
    set txttmp [split $txt \n]
    set len    [llength $txttmp]
    for {set l 0} {$l < $len} {incr l} {
        set line [lindex $txttmp $l]
        if {[string match .* $line]} {
            #puts "escaping this line: $line"
            set txttmp [lreplace $txttmp $l $l .$line]
        }
    }
    set newlen [llength $txttmp]
    if {$len != $newlen} {
        Msg "Internal programming error escaping .'s in message body.\n" \
                "Lines in=$len; lines out=$newlen"
        return $txt
    }
    return [join $txttmp \n]
}

proc unescape_dots {txt} {
    set txttmp [split $txt \n]
    set len    [llength $txttmp]
    for {set l 0} {$l < $len} {incr l} {
        set line [lindex $txttmp $l]
        if {[string match .* $line]} {
            #puts "unescaping this line: $line"
            set txttmp [lreplace $txttmp $l $l [string range $line 1 end]]
        }
    }
    set newlen [llength $txttmp]
    if {$len != $newlen} {
        Msg "Internal programming error unescaping .'s in message body.\n" \
                "Lines in=$len; lines out=$newlen"
        return $txt
    }
    return [join $txttmp \n]
}

proc TkGnats_sendmail {addrs mailtxt} {
    global TkGnats
    return [TkGnats_sendmail_$TkGnats(MailMethod) [fix_email_addresses $addrs] $mailtxt]
}

proc TkGnats_sendmail_mailer {addrs mailtxt} {
    global TkGnats
    # sendmail errors are mailed back to sender by sendmail
    if {[catch {open "|$TkGnats(Mailer)" w} fout]} {
        Msg "Error executing mailer \"$TkGnats(Mailer)\":\n" $fout
        return -1
    }
    puts  $fout $mailtxt
    close $fout
    return 0
}

proc TkGnats_sendmail_smtp {addrs mailtxt} {
    global TkGnats
    if {[set s [open_socket $TkGnats(SMTP_SERVER) $TkGnats(SMTP_PORT)]] == "-1"} {
        return -1
    }

    set my_addr $TkGnats(HOSTNAME)
    
    gnatsd_send $s "HELO $my_addr"
    set rep [get_socket_reply $s]
    #puts "rep=$rep"
    if {![string match 2* [lindex $rep 0]]} {
        Msg "SMTP error sending HELO $my_addr:\n" "[join $rep \n]"
        close_socket $TkGnats(SMTP_SERVER) $TkGnats(SMTP_PORT)
        return -1
    }
    
    set add [lindex [extract_email_address $TkGnats(EmailAddr)] 0]
    gnatsd_send $s "MAIL FROM: <$add>"
    set rep [get_socket_reply $s]
    #puts "rep=$rep"
    if {![string match 2* [lindex $rep 0]]} {
        Msg "SMTP error sending MAIL FROM: <$add>\n" "[join $rep \n]"
        close_socket $TkGnats(SMTP_SERVER) $TkGnats(SMTP_PORT)
        return -1
    }

    foreach addr [split $addrs ,] {
        set add [lindex [extract_email_address $addr] 0]
        if {$add == {}} {
            continue
        }
        gnatsd_send $s "RCPT TO: <$add>"
        set rep [get_socket_reply $s]
        #puts "rep=$rep"
        if {![string match 2* [lindex $rep 0]]} {
            Msg "SMTP error sending RCPT TO: <$add>\n" "[join $rep \n]"
            close_socket $TkGnats(SMTP_SERVER) $TkGnats(SMTP_PORT)
            return -1
        }
    }

    set mailtxt [escape_dots $mailtxt]
    
    gnatsd_send $s "DATA"
    set rep [get_socket_reply $s]
    #puts "rep=$rep"
    if {![string match 3* [lindex $rep 0]]} {
        Msg "SMTP error sending DATA:\n" "[join $rep \n]"
        close_socket $TkGnats(SMTP_SERVER) $TkGnats(SMTP_PORT)
        return -1
    }
    
    gnatsd_send $s $mailtxt
    gnatsd_send $s "."
    set rep [get_socket_reply $s]
    #puts "rep=$rep"
    if {![string match 2* [lindex $rep 0]]} {
        Msg "SMTP error sending complete message:\n" "[join $rep \n]"
        close_socket $TkGnats(SMTP_SERVER) $TkGnats(SMTP_PORT)
        return -1
    }
    #puts "message=$mailtxt"
    
    close_socket $TkGnats(SMTP_SERVER) $TkGnats(SMTP_PORT)

    return 0
}

proc fix_email_addresses {addrs} {
    set fixed ""
    set str $addrs
    while {$str != ""} {
        set f [string first \" $str]
        if {$f < 0} {
            append fixed $str
            break
        }
        append fixed [string range $str 0 $f]
        set    str   [string range $str [incr f] end]
        set l [string first \" $str]
        if {$l < 0} {
            append fixed $str
            break
        }
        set seg [string range $str 0 [expr $l - 1]]
        #puts seg=$seg
        set str [string range $str [incr l] end]
        if {[string first , $seg] >= 0} {
            set lseg [split $seg ,]
            set seg "[string trim [lindex $lseg 1]] [string trim [lindex $lseg 0]]"
        }
        append fixed $seg\"
    }
    return $fixed
}
        
proc email_originator_send {prid textw top} {
    global TkGnats bugsval respval origval mail_cc replvals mail_sj
    
    if {[string trim [$textw get 1.0 end]] == ""} {
        Msg "The body of the message is blank."
        return
    }

    set addrs ""
    foreach a "bugsval respval origval mail_cc" {
        if {"[string trim [subst $$a]]" != ""} {
            if {"$addrs" != ""} {
                set addrs "$addrs, "
            }
            set addrs "$addrs[string trim [subst $$a]]"
        }
    }
    for {set i 1} {$i <= $replvals(nreps)} {incr i} {
        set a [string trim $replvals($i)]
        if {"$a" != ""} {
            if {"$addrs" != ""} {
                set addrs "$addrs, "
            }
            set addrs "$addrs$a"
        }
    }

    set mailtxt ""
    append mailtxt "From: $TkGnats(EmailAddr)\n"
    append mailtxt "Reply-To: $TkGnats(EmailAddr)\n"
    append mailtxt "To: $addrs\n"
    append mailtxt "Subject: Re: $prid: $mail_sj\n"
    append mailtxt "\n"
    append mailtxt "[$textw get 1.0 end]"

    if {[TkGnats_sendmail $addrs $mailtxt] == 0} {
        destroy $top
    }

    return
}

proc email_originator {to_rep to_res to_org prid synopsis} {
    global TkGnats mail_cc mail_sj bugsval respval origval replvals

    set to_rep [fix_email_addresses $to_rep]
    set to_res [fix_email_addresses $to_res]
    set to_org [fix_email_addresses $to_org]
    
#puts "to_rep=$to_rep"
#puts "to_res=$to_res"
#puts "to_org=$to_org"
    # expand the Responsible address, which could be an alias in the responsibles file

    set to_org  [add_email_domainname $to_org]
#puts "to_org=$to_org"

    set tmp_org [lindex [extract_email_address $to_org] 0]
    set tmp_usr [lindex [extract_email_address $TkGnats(EmailAddr)] 0]

    set to_res_addr [add_email_domainname [get_responsible_addr $to_res]]
    if {"$to_res" != "$to_res_addr"} {
        set restmp "$to_res: $to_res_addr"
    } {
        set restmp "$to_res"
    }
    set to_res "$to_res_addr"
    set tmp_res [lindex [extract_email_address $to_res] 0]
    
    set tlist {}
    
    set t [toplevel .tkgnats_email]
    wm minsize    $t 100 100
    wm title      $t "TkGnats - Send Email to Problem Report: $prid"
    wm iconbitmap $t @$TkGnats(lib)/tkeditpr.xbm
    wm iconname   $t "$TkGnats(LogName)'s tkemailpr [file tail $prid]"

    set   f [frame $t.mframe -borderwidth 1 -relief raised]
    pack $f -side top -fill x

    menubutton $f.file -text "File" -menu $f.file.m -underline 0
    menu       $f.file.m
    $f.file.m add command -label "Send"  \
            -command "email_originator_send $prid $f.text $t"
    $f.file.m add separator
    $f.file.m add command -label "Cancel" -command "destroy $t"
    
    menubutton $f.edit -text "Edit" -menu $f.edit.m -underline 0
    menu       $f.edit.m
    $f.edit.m     configure -disabledforeground [$f.edit.m cget -foreground]
    $f.edit.m add command -label "Use right mouse button for Cut/Copy/Paste" -state disabled
    $f.edit.m add separator
    $f.edit.m add command -label "Fonts..." -command "edit_fonts"
    
    pack $f.file $f.edit -side left
    
    menubutton $f.help -text "Help" -menu $f.help.m -underline 0
    menu       $f.help.m
    pack       $f.help -side right
    $f.help.m add command -label "Cut, Copy, Paste Operations" \
            -command "helpMsg Cut_Copy_Paste"
    $f.help.m add separator
    $f.help.m add command -label "View Configuration Variables" \
        -command "helpMsg TkGnats_Variables"
    $f.help.m add separator
    $f.help.m add command -label "Changes" \
            -command "helpMsg Changes"
    $f.help.m add command -label "About" \
            -command "helpMsg TkGnats_About"

    set    f [frame $t.f]
    pack  $f           -side top  -fill both -expand true

    set     b [frame $f.b -borderwidth 1 -relief raised]
    pack   $b -side top -fill x -anchor w
    #button $b.cancel -borderwidth 1 -text Cancel -command "destroy $t"
    button $b.send   -borderwidth 1 -text Send   \
            -command "email_originator_send $prid $f.text $t"
    pack   $b.send   -side left -padx 0 -pady 0
    #pack   $b.cancel -side left -padx 0 -pady 0

    frame $f.ad
    pack  $f.ad        -side top  -anchor n -fill x
    frame $f.ad.lab
    pack  $f.ad.lab    -side left -anchor w -fill y
    frame $f.ad.ent
    pack  $f.ad.ent    -side left -anchor w -fill x -expand true
    
    label $f.ad.lab.to   -relief flat -anchor e -text "To:" -width 8
    pack  $f.ad.lab.to   -side top    -anchor n

    label $f.ad.lab.sj   -relief flat -anchor e -text "Subject:"
    pack  $f.ad.lab.sj   -side bottom -anchor s -pady 2
    
    label $f.ad.lab.cc   -relief flat -anchor e -text "Cc:" -width 8
    pack  $f.ad.lab.cc   -side bottom -anchor s -pady 2
    
    checkbutton $f.ad.ent.to -relief flat -pady 0 -highlightthickness 0 -anchor w \
            -variable bugsval -text "$TkGnats(GNATS_ADDR)" \
            -offvalue "" -onvalue "$TkGnats(GNATS_ADDR)"
    pack        $f.ad.ent.to -side top -anchor w
    
    checkbutton $f.ad.ent.resp -relief flat -pady 0 -highlightthickness 0 -anchor w \
            -variable respval  -text "Responsible   ($restmp)" -offvalue "" -onvalue "$to_res"
    pack        $f.ad.ent.resp -side top -anchor w
    checkbutton $f.ad.ent.orig -relief flat -pady 0 -highlightthickness 0 -anchor w \
            -variable origval  -text "Originator    ($to_org)" -offvalue "" -onvalue "$to_org"
    pack        $f.ad.ent.orig -side top -anchor w

    set reps  [split $to_rep ,]
    set nreps [llength $reps]
    set nrep  0
    catch {unset replvals}
    for {set i 0} {$i < $nreps} {incr i} {
        set rep     [string trim [lindex $reps $i] "{} \t\n"]
#puts "rep($i)=$rep"
        set repaddr [lindex [extract_email_address $rep] 0]
#puts repaddr=$repaddr
        set tmp_rep [add_email_domainname [get_responsible_addr $repaddr]]
#puts tmp_rep=$tmp_rep
        if {"$tmp_rep" != "$repaddr"} {
            set reptmp "$rep: $tmp_rep"
        } {
            set reptmp "$rep"
        }
#        if {"$rep" == "$tmp_org"} {
#puts "skipping notify: $rep: $tmp_rep"
#            continue
#        }
        incr nrep
        set replvals($nrep) $tmp_rep
        checkbutton $f.ad.ent.repl_$nrep -relief flat -pady 0 -highlightthickness 0 -anchor w \
                -variable replvals($nrep) -text "Notify-List   ($reptmp)" \
                -offvalue "" -onvalue "$tmp_rep"
        pack        $f.ad.ent.repl_$nrep -side top -anchor w
    }
    set replvals(nreps) $nrep

    lappend tlist [entry $f.ad.ent.cc -relief sunken -borderwidth 2 \
            -background $TkGnats(EditFieldBackground) \
            -highlightthickness 2 -textvariable mail_cc]
    pack  $f.ad.ent.cc -side top -anchor w -fill x -expand true
    set_focus_style $f.ad.ent.cc
    bind $f.ad.ent.cc <3> "clipboard_post $f.ad.ent.cc %X %Y"
    
    frame $f.ad.ent.sj
    pack  $f.ad.ent.sj -anchor w -fill x -expand true
    label $f.ad.ent.sj.def -relief sunken -anchor w -text "Re: $prid:"
    pack  $f.ad.ent.sj.def -side left
    lappend tlist [entry $f.ad.ent.sj.ent -relief sunken -borderwidth 2 \
            -background $TkGnats(EditFieldBackground) \
            -highlightthickness 2 -textvariable mail_sj]
    pack  $f.ad.ent.sj.ent -side left -anchor w -fill x -expand true
    set_focus_style $f.ad.ent.sj.ent
    bind $f.ad.ent.sj.ent <3> "clipboard_post $f.ad.ent.sj.ent %X %Y"

    scrollbar $f.sb -command "$f.text yview" -relief sunken
    # The text entry is hardwired to no wrap because the wrap is only visual anyway
    # (no LF's are actually inserted)
    lappend tlist [text $f.text \
        -wrap none \
        -yscrollcommand "$f.sb set" \
        -height 30 -width 80 -relief sunken -padx 4 -insertwidth 1 \
        -insertofftime 400 -borderwidth 2 -highlightthickness 2 -background $TkGnats(EditFieldBackground)]
    set_focus_style $f.text
    bind $f.text <3> "clipboard_post $f.text %X %Y"

    set_text_traversal $tlist
    
    pack $f.sb   -side left  -fill y
    pack $f.text -side right -expand true -fill both

    focus $f.text

    set bugsval "$TkGnats(GNATS_ADDR)"

    if {$tmp_res != $tmp_usr} {
        set respval $to_res
    } {
        set respval ""
        $f.ad.ent.resp configure -state disabled
    }
    if {$tmp_org != $tmp_usr && $tmp_org != $tmp_res} {
        set origval $to_org
    } {
        set origval ""
        $f.ad.ent.orig configure -state disabled
    }

#dputs "tmp_usr=$tmp_usr tmp_org=$tmp_org tmp_res=$tmp_res tmp_org=$tmp_org to_res=$to_res to_org=$to_org"
#puts "replvals=[array get replvals]"
    # Disable any duplicate recipients
#dputs "nrep=$nrep"
    for {set i 1} {$i <= $nrep} {incr i} {
        set tmp_rep [get_responsible_addr [lindex [extract_email_address $replvals($i)] 0]]
        set tmp_rep [add_email_domainname $tmp_rep]
#dputs "replvals($i)=$replvals($i) tmp_rep=$tmp_rep"
        if {$tmp_rep == $tmp_usr || "$tmp_rep" == "$tmp_res"} {
            set replvals($i) ""
            $f.ad.ent.repl_$i configure -state disabled
        }
    }
    
    set mail_cc ""
    set mail_sj "$synopsis"
}

proc entryDialog {msg {cancel Cancel} {initial_value ""} {blankok 1} {master .}} {
    global  entryDialog_Done TkGnats
    set     entryDialog_Done 0
    catch  {destroy  .entryDialog}
    set t  [toplevel .entryDialog -borderwidth 2 -relief raised]
    message $t.msg -text $msg -aspect 99999
    entry   $t.e   -width 50 -borderwidth 2 -relief sunken \
            -highlightthickness 2 -background $TkGnats(EditFieldBackground)
    frame   $t.bar 
    button  $t.bar.ok         -text "OK"    -command "set entryDialog_Done  1"
    pack    $t.bar.ok -side left -padx 8 -pady 8
    if {$cancel != ""} {
        button  $t.bar.cancel -text $cancel -command "set entryDialog_Done -1"
        pack    $t.bar.cancel -side left -padx 8 -pady 8
    }
    pack    $t.msg -side top -fill x
    pack    $t.e   -side top -padx 8 -pady 8
    pack    $t.bar -side bottom -anchor center
    bind    $t.e <3> "clipboard_post $t.e %X %Y"
    bind    $t.e <KeyPress-Return> "set entryDialog_Done 1"
    # Center the dialog over the master
    wm withdraw  $t
    update
    wm geometry  $t +[expr [winfo rootx $master] + ([winfo width $master] - [winfo reqwidth $t]) / 2]+[expr [winfo rooty $master] + ([winfo height $master] - [winfo reqheight $t]) / 2]
    wm deiconify $t
    wm transient $t $master
    set_focus_style $t.e
    focus   $t.e
    $t.e    insert  0 $initial_value
    set     done    0
    while {$done == 0} {
        grab    $t
        # This raise causes a few seconds delay second time through the loop (when blank entered).
        # Lowering it first gets around this wierdness.
        lower   $t
        raise   $t
        tkwait  variable entryDialog_Done
        grab    release $t
        set     text [string trim [$t.e get]]
        if {$entryDialog_Done != -1 && !$blankok && $text == ""} {
            $t.e delete 0 end
            bell
            Msg "Blank entered!" "You must enter a value."
            set done 0
        } {
            set done 1
        }
    }
    destroy $t
    update idletasks
    if {$entryDialog_Done == -1} {
        error "$text"
    }
    return $text
}

proc textEntryDialog {msg {cancel Cancel} {initial_value ""} {blankok 1} {master .}} {
    global  textEntryDialog_Done TkGnats
    set     textEntryDialog_Done 0
    catch  {destroy  .textEntryDialog}
    set t  [toplevel .textEntryDialog  -borderwidth 2 -relief raised]
    message $t.msg -text $msg -aspect 99999
    text    $t.e   -width 50 -height 8 -borderwidth 2 -relief sunken \
            -highlightthickness 2 -background $TkGnats(EditFieldBackground)
    frame   $t.bar 
    button  $t.bar.ok         -text "OK"    -command "set textEntryDialog_Done  1"
    pack    $t.bar.ok -side left -padx 8 -pady 8
    if {$cancel != ""} {
        button  $t.bar.cancel -text $cancel -command "set textEntryDialog_Done -1"
        pack    $t.bar.cancel -side left -padx 8 -pady 8
    }
    pack    $t.msg -side top -fill x
    pack    $t.e   -side top -padx 8 -pady 8
    pack    $t.bar -side bottom -anchor center
    bind $t.e <3> "clipboard_post $t.e %X %Y"
    # Center the dialog over the master
    wm withdraw  $t
    update
    wm geometry  $t +[expr [winfo rootx $master] + ([winfo width $master] - [winfo reqwidth $t]) / 2]+[expr [winfo rooty $master] + ([winfo height $master] - [winfo reqheight $t]) / 2]
    wm deiconify $t
    wm transient $t $master
    set_focus_style $t.e
    focus $t.e
    $t.e insert 1.0 $initial_value
    set     done    0
    while {$done == 0} {
        grab    $t
        # This raise causes a few seconds delay second time through the loop (when blank entered).
        # Lowering it first gets around this wierdness.
        lower   $t
        raise   $t
        tkwait  variable textEntryDialog_Done
        grab    release $t
        set     text [string trim [$t.e get 1.0 end]]
        if {$textEntryDialog_Done != -1 && !$blankok && $text == ""} {
            $t.e delete 1.0 end
            bell
            Msg "Blank entered!" "You must enter a value."
            set done 0
        } {
            set done 1
        }
    }
    destroy $t
    update  idletasks
    if {$textEntryDialog_Done == -1} {
        error "$text"
    }
    return $text
}

proc quickfill_entry_from_listbox {ab ew lw vlist} {
    upvar 1 $ab tvar
    set cur [$lw curselection]
    if {$tvar == "" && $cur == ""} {
        return
    }
    set eidx   [$ew index insert]
    set tmpval [string range [$ew get] 0 [expr $eidx - 1]]
    if {$tmpval != ""} {
        set tvar $tmpval
    }
    set lidx [lsearch -glob $vlist $tvar*]
    if {$lidx < 0 || $tvar == ""} {
        if {$cur != ""} {
            set tvar [$lw get $cur]
        } {
            set tvar [string range [$ew get] 0 [expr $eidx - 2]]
        }
        set lidx [lsearch -glob $vlist $tvar*]
        incr eidx -1
        bell
    }
    $lw selection clear 0 end
    if {$tvar != ""} {
        if {$lidx >= 0} {
            $lw selection set $lidx
            $lw see $lidx
            set tvar [$lw get [$lw curselection]]
        }
    }
    set tmpval   $tvar
    $ew delete 0 end
    $ew insert 0 $tmpval
    $ew icursor  $eidx
    if {$eidx > 0} {
        $ew selection range $eidx [string length $tmpval]
    }
}

proc save_help {w} {
    set file [tk_getSaveFile]
    if {$file != ""} {
        if {[file_put_text $file [$w get 1.0 end]] == ""} {
            Msg "Unable to save help text to file \"$file\"."
        } {
            Msg "Saved help text to file \"$file\"."
        }
    } {
        Msg "Save help text aborted."
    }
}
    
proc show_help {title help} {
    global TkGnats
    set w .help_$title
    catch {destroy $w}
    
    regsub -all "_" $title " " Title

    toplevel  $w
    wm title  $w "$Title Help"

    frame     $w.opts
    pack      $w.opts -side top
    button    $w.opts.quit -text "OK" -command "destroy $w"
    button    $w.opts.save -text "Save to file..." -command "save_help $w.text"
    pack      $w.opts.quit $w.opts.save -side left -pady 2 -padx 20

    # Create a scrollbar and a text box in the main window.
    scrollbar $w.scrollx -orient horiz -command "$w.text xview"
    pack      $w.scrollx -side bottom -fill x
    scrollbar $w.scrolly -command "$w.text yview"
    pack      $w.scrolly -side right  -fill y
    text      $w.text -relief sunken -bd 2 -yscrollcommand "$w.scrolly set" \
            -xscrollcommand "$w.scrollx set" -setgrid 1 -height 24 -width 82 \
            -font $TkGnats(helpfont) -background [$w.opts.quit cget -background]
    pack      $w.text -side left -fill both -expand yes
    $w.text   insert end $help
    set nlines [lindex [split [$w.text index end] "."] 0]
    set height 24
    if {$height > $nlines} {
        set height $nlines
    }
    $w.text   configure -state disabled -height $height    
    set_focus_style $w.text
    bind $w.text <3> "clipboard_post $w.text %X %Y"
}

proc add_email_domainname {addrs} {
    global TkGnats
#puts "addrs=$addrs"
    if {![info exists TkGnats(DefaultDomainName)] || $TkGnats(DefaultDomainName) == ""} {
        return $addrs
    }
    set new_addrs ""
    foreach add  [split $addrs ,] {
#puts "add=$add"
        set addr [lindex [extract_email_address $add] 0]
        set name [lindex [extract_email_address $add] 1]
#puts "addr=$addr"
#puts "name=$name"
        if {[string first "@" $addr] < 0} {
            set addr ${addr}@$TkGnats(DefaultDomainName)
        }
        if {$name != ""} {
            lappend new_addrs "$name <$addr>"
        } {
            lappend new_addrs $addr
        }
    }
    return [join $new_addrs ", "]
}

proc extract_email_address {addr} {
    # Supported formats:
    # 01: "Rick Macdonald" <rickm@vsl.com>
    #     Rick Macdonald <rickm@vsl.com>
    # 02: rickm@vsl.com (Rick Macdonald)
    # 03: rickm@vsl.com
    #
    #     01 and 02 return: {rickm@vsl.com} {Rick Macdonald}
    #     03 returns: {rickm@vsl.com} {}
    set fmt01_exp {^(.*[^<]+)(<.*>)}
    set fmt02_exp {^(.*[^\(]+)(\(.*\))}
    set fmt03_exp {(.*)}
    set name    ""
    set address ""
    if {![regexp $fmt01_exp $addr matched name address]} {
        if {![regexp $fmt02_exp $addr matched address name]} {
            set address $addr
        }
    }
    set name [string trim $name "{}()<>\"\'        \n"]
    if {[string first , $name] >= 0} {
        regexp {(.*),(.*)} $name match last first
        set name "[string trim $first] [string trim $last]"
    }
    return [list [string trim $address "{}()<>\"\'        \n"] \
            [string trim $name "{}()<>\"\'        \n"]]
}

proc extract_full_name_from_address {addr} {
    set name [extract_email_address $addr]
    if {[lindex $name 1] != {}} {
        return [lindex $name 1]
    } {
        return $addr
    }
}

proc lock_pr_batch {me prid txt} {
    global TkGnats
    upvar 1 $txt text
    return [catch {eval exec $TkGnats(pr-edit) $TkGnats(UseridPassword) --lock $me $prid} text]
}

proc lock_pr_socket {me prid txt} {
    global TkGnats
    upvar 1 $txt text
    if {[set s [open_socket_gnatsd]] == "-1"} {
        set text "Can't open socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)"
        return 1
    }
    gnatsd_send $s "LOCK $prid $me"
    set rep [get_socket_reply $s]
    #puts "rep=$rep"
    if {![string match 300* [lindex $rep 0]]} {
        set text [lindex $rep 0]
        return 1
    }
    set text [join [get_gnatsd_reply_dot_ended $s] \n]

    close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)
    set text [unescape_dots $text]
    return 0
}

proc lock_pr {prid} {
    global TkGnats

    ### find a username@hostname

    set me $TkGnats(LogName)

    if {"$TkGnats(HOSTNAME)" != ""} {
        set me "$me@$TkGnats(HOSTNAME)"
    }

    ### lock the PR and bail out if the lock fails

    set stat [lock_pr_$TkGnats(GNATS_ACCESS_METHOD) $me $prid text]
    #puts "stat=$stat text=$text"        
    if {$stat == 0} {
        set prtxt $text
    } {
        wm withdraw .
        if {[string first "locked by" $text] >= 0} {
            # gnatsd, npr-edit, pr-edit
            Msg "Problem report '$prid' is locked by '[lindex [split $text] end]'"
        } { 
            Msg "Can't lock problem report '$prid':\n\n$text"
        }
        exit 1
    }

    return $prtxt
}

proc unlock_pr_batch {prid} {
    global TkGnats
    catch {eval exec $TkGnats(pr-edit) $TkGnats(UseridPassword) --unlock $prid} rep
    return $rep
}

proc unlock_pr_socket {prid} {
    global TkGnats
    if {[set s [open_socket_gnatsd]] == "-1"} {
        return "Error-3 opening socket/port $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)"
    }
    gnatsd_send $s "UNLK $prid"
    set rep [get_socket_reply $s]
    if {![string match 2* [lindex $rep 0]]} {
        set rep [lindex $rep 0]
    } {
        set rep ""
    }
    close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)
    return $rep
}

proc unlock_pr {prid} {
    global TkGnats
    set rep [unlock_pr_$TkGnats(GNATS_ACCESS_METHOD) $prid]
    if {$rep != ""} {
        Msg "Error unlocking PRID $prid:\n\n$rep"
    }
}

proc get_pr_full_text {prid} {
    global TkGnats
    return [get_pr_full_text_$TkGnats(GNATS_ACCESS_METHOD) $prid]
}

proc get_pr_full_text_batch {prid} {
    global TkGnats
    if {[catch {eval exec $TkGnats(query-pr) $TkGnats(UseridPassword) --full [file tail $prid]} rep]} {
        Msg "GNATS error getting full text of PRID $prid:\n\n$rep"
        return -1
    }
    return $rep
}

proc get_pr_full_text_socket {prid} {
    global TkGnats
    if {[set s [open_socket_gnatsd]] == "-1"} {
        return "-1"
    }
    gnatsd_send $s "RSET"
    set rep [get_socket_reply $s]
    if {![string match 2* [lindex $rep 0]]} {
        Msg "GNATSD error sending RSET getting full text of PRID $prid:\n" "[join $rep \n]"
        return -1
    }

    gnatsd_send $s "QFMT full"
    set rep [get_socket_reply $s]
    if {![string match 2* [lindex $rep 0]]} {
        Msg "GNATSD error setting format to full text (for PRID $prid):\n" "[join $rep \n]"
        return -1
    }

    gnatsd_send $s "QUER $prid"
    set rep [get_socket_reply $s]
    if {![string match 300* [lindex $rep 0]]} {
        Msg "GNATSD error getting full text of PRID $prid:\n" "[join $rep \n]"
        return -1
    }
    set plist [join [get_gnatsd_reply_dot_ended $s] \n]
    close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)
    return [unescape_dots $plist]
}

proc get_pr_medium_text {prid} {
    global TkGnats
    return [get_pr_medium_text_$TkGnats(GNATS_ACCESS_METHOD) $prid]
}

proc get_pr_medium_text_batch {prid} {
    global TkGnats
    return [eval exec $TkGnats(query-pr) $TkGnats(UseridPassword) [file tail $prid]]
}

proc get_pr_medium_text_socket {prid} {
    global TkGnats
    if {[set s [open_socket_gnatsd]] == "-1"} {
        return "-1"
    }
    gnatsd_send $s "RSET"
    set rep [get_socket_reply $s]
    if {![string match 2* [lindex $rep 0]]} {
        Msg "GNATSD error sending RSET getting full text of PRID $prid:\n" "[join $rep \n]"
        return -1
    }
    gnatsd_send $s "QURY $prid"
    set rep [get_socket_reply $s]
    if {![string match 2* [lindex $rep 0]]} {
        Msg "GNATSD error getting medium text of PRID $prid:\n" "[join $rep \n]"
        return -1
    }
    set plist [join [get_gnatsd_reply_dot_ended $s] \n]
    close_socket $TkGnats(GNATS_SERVER) $TkGnats(GNATS_PORT)
    
    return $plist
}

proc delete_pr {prid} {
    global TkGnats
    return [delete_pr_$TkGnats(GNATS_ACCESS) $prid]
}

proc delete_pr_network {prid} {
    global TkGnats
    bell
    Msg "delete_pr is only available for GNATS_ACCESS=local"
    return "delete_pr is only available for GNATS_ACCESS=local"
}

proc delete_pr_local {prid} {
    global TkGnats
# TTD: This is missing a global lock on the gnats database!
# TTD: This is missing a global lock on the gnats database!
# TTD: some things here need re-ordering (lock_pr, etc)
# TTD: some things here need re-ordering (lock_pr, etc)
    set INDEX $TkGnats(GNATS_ROOT)/gnats-adm/index

    set prtext  [get_pr_medium_text $prid]
    parsepr_txt $prtext flds
    set full_id [string trim $flds(>Category)]/$prid

    if {"$full_id" == ""} {
        #####wm withdraw .
        Msg "Error accessing problem report '$prid'!"
        return ""
    }

    set pr $TkGnats(GNATS_ROOT)/$full_id

    if {![file writable $INDEX] || ![file writable $pr]} {
        Msg "You don't have proper file permissions to delete $full_id."
        return ""
    }

    set   state  [string trim $flds(>State)]
    if {"$state" != "closed"} {
        Msg "Problem reports must be closed before deleting."
        unlock_pr $full_id
        return ""
    }
    
    bell
    if {[tk_dialog .tkquerypr_delete "Confirm_Delete" \
            "This will PERMANENTLY delete this bug report.\n\nAre you sure?" \
            "warning" -1 "Delete Report" "Cancel"] != 0} {
        return ""
    }
    
    lock_pr $full_id
    
    # take the relevant line out of the index - it should only be there once

    set stamp tkdeletepr.$TkGnats(LogName).[clock format [clock seconds] -format "%j:%T"]
    # catch {exec egrep -v ^$full_id: $INDEX > $INDEX.$stamp}
    set index [split [file_get_text $INDEX] \n]
    if {"$index" == ""} {
        Msg "Unable to read GNATS index."
        return ""
    }
    set idx [lsearch -regexp $index ^$full_id|]
    if {$idx < 0} {
        Msg "Problem report '$prid' not found in GNATS index!"
        return ""
    }
    set index [lreplace $index $idx $idx]
    set idx [lsearch -regexp $index ^$full_id|]
    if {$idx >= 0} {
        Msg "Problem report '$prid' seems to exist more than once in the GNATS index!"
        return ""
    }
    if {[file_put_text $INDEX.$stamp [join $index \n]] == ""} {
        Msg "Unable to write temporary GNATS index file."
        return ""
    }
    
    # here's where we actually delete the file.
    file rename -force $INDEX $INDEX.bak
    file delete $pr
    file rename -force $INDEX.$stamp $INDEX

    # It doesn't seem necessary to send mail about this
    
    # call PR_EDIT on the new file and clean up
    unlock_pr $full_id
    
    return "All references to $full_id now deleted."
}

proc chk_fld {fldname val {flag_if_missing 1}} {
    global mlist
    upvar 1 $fldname fld
    if {![info exists fld]} {
        if {$flag_if_missing} {
            set match ""
            regexp "\\(.*\\)" $fldname match
            set match [string trim $match ()]
            if {$match != ""} {
                lappend mlist $match
            }
        }
        set fld $val
    }
}

proc load_field_defaults {field_array} {
    global TkGnats mlist
    
    upvar 1 $field_array field

    set mlist {}
    
    chk_fld field(>State)         [lindex $TkGnats(StatesList) 0]
    chk_fld field(>Confidential)  no
    chk_fld field(>Severity)      serious
    chk_fld field(>Priority)      medium
    chk_fld field(>Class)         [lindex $TkGnats(ClassesList) 0]
    chk_fld field(>Arrival-Date)  [clock format [clock seconds]]
    chk_fld field(>Last-Modified) "\n"
    chk_fld field(>Closed-Date)   "\n" 
    chk_fld field(>Start-Date)    "\n" 
    chk_fld field(>End-Date)      "\n" 
    chk_fld field(>Cost)          ""   
    chk_fld field(>XrefPR)        ""   
    chk_fld field(>Originator)    Unknown
    chk_fld field(>Responsible)   gnats
    chk_fld field(>Category)      pending
    chk_fld field(>Synopsis)      None
    chk_fld field(>Release)       Unknown
    chk_fld field(>Description)   None
    chk_fld field(>Environment)   "\n"
    chk_fld field(>Audit-Trail)   "\n"
    chk_fld field(>How-To-Repeat) "\n"
    chk_fld field(>IPsec-barf-location) "http://" 0
    chk_fld field(>IPsec-look)    "\n" 
    chk_fld field(Reply-To)       ""   

    chk_fld field(X-GNATS-Notify) nobody 0

    # It's ok if these are missing
    chk_fld field(>Unformatted)   "\n" 0
    chk_fld field(>Fix)           "\n" 
    chk_fld field(>Release-Note)  "\n" 

    if {$TkGnats(ReleaseBased)} {
        chk_fld field(>Keywords)          "" 0
        chk_fld field(>$TkGnats(Quarter)) "" 0
        chk_fld field(>Date-Required)     "" 0
    }

    return $mlist
}

TkGnats_config
