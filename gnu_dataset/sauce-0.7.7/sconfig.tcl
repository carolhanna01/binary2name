########### sconfig.tcl
# Config variables for SAUCE and their defaults.
#
# This file is part of SAUCE, a very picky anti-spam receiver-SMTP.
# SAUCE is Copyright (C) 1997-2001 Ian Jackson
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. 
#
# $Id: sconfig.tcl,v 1.34 2001/03/21 23:53:15 ian Exp $


set @@@var_dir@@@ .
set @@@log_dir@@@ .
set @@@config_dir@@@ .


config_var ident_port                ident  port
config_var port                         25  port
config_var remote_port                smtp  port
				     
config_var always_blacklist_site     false  boolean
config_var blacklist_all_addresses   false  boolean

config_var conns_max                 120    number	     1  10000

config_var ident_timeout              60s   units interval   0ms    5m
config_var fail_send_timeout           1s   units interval   0ms    5m
config_var command_timeout             5m   units interval   0ms    1h
config_var verify_perconn_timeout      2m   units interval   0ms    5m
config_var verify_all_timeout          6m   units interval   0ms   15m
config_var dns_timeout                45s   units interval 500ms    5m
config_var verify_cache_timeout       60s   units interval   0ms    7d
config_var verify_reuse_timeout        4m   units interval   0ms    1h
config_var verify_rset_timeout         2m   units interval   0ms    5m
config_var verify_quit_timeout        12s   units interval   0ms    5m
config_var notifybl_timeout           15m   units interval   0ms    1h

config_var annoy_halflife              5h   units elapsed    1s     1wk
config_var annoy_grudge_max           15m   units interval   0ms    3h
config_var annoy_love_max             15m   units interval   0ms    3h
config_var annoy_actout_max          150s   units interval   0ms    5m
config_var annoy_actout_nopartresp    60s   units interval   0ms    5m
config_var annoy_grumpy              300s   units interval   0ms    3h
config_var annoy_partrespevery        30s   units interval   0ms   10m
config_var pleasure_command            1s   units interval   0ms   10m
config_var pleasure_delivery          60s   units interval   0ms   10m
config_var annoyance_minor             5s   units interval   0ms   10m
config_var annoyance_major            30s   units interval   0ms   10m

config_var busyfury_firewall	      true  boolean
config_var busyfury_firewall_time     30m   units interval   0ms   72h

config_var addr_whitelist_delay        5d   units elapsed    0s     2mth
config_var site_whitelist_delay        5d   units elapsed    0s     2mth
config_var addr_whitelist_timeout      2mth units elapsed    1m    30yr
config_var site_whitelist_timeout      2mth units elapsed    1m    30yr
config_var addr_verified_timeout      30d   units elapsed    0s    30yr
config_var site_verified_timeout      30d   units elapsed    0s    30yr
config_var addr_blacklist_timeout      2mth units elapsed    1m    30yr
config_var site_blacklist_timeout      2mth units elapsed    1m    30yr
				     
config_var new_addr_defer              1h   units elapsed    0s     3d
config_var new_site_defer              3h   units elapsed    0s     3d
config_var remember_addr_defer         2mth units elapsed    0s     1yr
config_var remember_site_defer         2mth units elapsed    0s     1yr

config_var admin_chal_timeout         30s   units elapsed    5s    15m
config_var admin_secret_refresh        4h   units interval   1m    7d
config_var admin_secret_length        32    number          16    64
config_var force_shutdown_delay       30s   units interval   1s    2h
				     
config_var max_header_size           100kb  units size      10kb  10mb
config_var max_smtpparms_size         10kb  units size       1kb 100kb
config_var max_verify_rcpts           10    number           1   100
config_var max_smtp_errors            30    number           1   300

config_var ipaddr_phase_proportion   256    number           0   256
config_var ipaddr_phase_offset         0    number           0   255

config_var mixedkinds_message  "Mixed recipient kinds, try this one later"      printable
config_var bland_message       "Recipient verified"                             printable
config_var blacklist_message   "You are blacklisted - contact postmaster."      printable
config_var blacknone_message   "Message blacklisted - contact postmaster."      printable
config_var blacksite_message   "Your site is blacklisted - contact postmaster." printable
config_var rbl_reject_message  "Your site is realtime-blacklisted (%d: %m)"     printable
config_var rbl_defer_message   "Site distrusted, try later (%d: %m)"            printable
config_var new_addr_message    "Sender not yet trusted, try later"              printable
config_var new_site_message    "Site not yet trusted, try later"                printable

config_nd  local_domain                            hlist domain
config_nd  local_interface                         hlist domain
config_var canonical_hostname   [info hostname]    domain
config_var notifybl_localpart   sauce-daemon       nicelocalpart
config_var admin_localpart      sauce-admin        nicelocalpart
config_var notifybl_bounces	sauce-bounces      nicelocalpart

config_var allow_saucestate                 false     boolean
config_var add_received                     true      boolean
config_var add_warnings                     true      boolean

config_var require_reverse_dns              true      boolean
config_var forbid_helo_ipliteral            true      boolean
config_var check_helo_name                  false     boolean
                              # ... violates a MUST NOT in RFC1123
config_var require_callingmaildomain_name   true      boolean
config_var require_callingmaildomain_dnsok  true      boolean
config_var require_messageid                false     boolean

config_var sauceadmin_connrefused_ok        false     boolean

config_var log_mode             644                  filemode
config_var log_mode_debug       640                  filemode
config_var log_stderr         false                  boolean
config_var debug_level            0                  number  0 9999

config_var var_dir            ${@@@var_dir@@@}          file
config_var log_dir            ${@@@log_dir@@@}          file
config_var config_dir         ${@@@config_dir@@@}       file
config_var config_file        config                    file
config_var initdb_file        db.manual                 file
config_var blmessage_file     blmessage.text            file
config_var policies_dir       policies                  file
config_var firewall_command   set-firewall              file

config_var irritated_tell_submissive	Submissive	printable
config_var irritated_tell_ecstatic	Ecstatic	printable
config_var irritated_tell_pleased	Pleased		printable
config_var irritated_tell_irritated	Irritated	printable
config_var irritated_tell_angry		Angry		printable
config_var irritated_tell_furious	Furious		printable

# CARE!  We completely trust policies/p$user, so whatever puts things
# there must translate the user's input into something suitable.
# This is what rcpt-policy.tcl is for.

config_raw anger_stallwith {
    global fill_msgs
    foreach l [split [config_normalise_printable $value] "\n"] {
	lappend fill_msgs $l
    }
}

config_raw rbl {
    global rbls rbl_reject_message rbl_defer_message
    if {![regexp -nocase \
 {^([-+.0-9a-z]+)[ \t]+(reject|\d+[a-z]+[ \t]+\d+[a-z]+)[ \t]*(.*)$} \
	    $value dummy dm what msg]} {
	error "rbl takes value `<domain> reject \[<message>\]' or `<domain> <new-addr-delay> <new-site-delay> \[<message>\]'"
    }
    set dm [config_normalise_domain $dm]
    set reject [expr {"[string tolower $what]" == "reject"}]
    if {$reject} {
	set ifnewaddr {}
	set ifnewsite {}
    } else {
	set ifnewaddr [config_normalise_units [lindex $what 0] elapsed 0s 3d]
	set ifnewsite [config_normalise_units [lindex $what 1] elapsed 0s 3d]
    }
    if {[string length $msg]} {
	set msg [config_normalise_printable $msg]
    } elseif {$reject} {
	set msg $rbl_reject_message
    } else {
	set msg $rbl_defer_message
    }
    lappend rbls [list $dm $ifnewaddr $ifnewsite $msg]
}

config_raw addr_regexps_nodefault {
    global addr_patterns
    if {[string length $value]} {
	error "addr_regexps_nodefault takes no value"
    }
    set addr_patterns {}
}
config_raw addr_regexp {
    global addr_patterns
    if {![regexp -- {^/(.*)/\s+([a-z]+[^/]*)$} $value dummy ap at] && \
	    ![regexp -- {^([^/ \t]\S*)\s+([a-z]+.*)$} $value dummy ap at]} {
	error "addr_regexp takes value `<regexp-no-ws> <type>' or \
		`/<regexp>/ <type-no-fwd-slashes>'"
    }
    switch -regexp $at {
	unchecked - lax - nodelay - normal - bait
	- {45[012] \S.*} - {55[023] \S.*}
	- policy - user - {user=.*} { }
	* {
	    error "addr_regexp type must be one of \
		    unchecked, lax, nodelay, normal, bait, \
		    policy, user, user=<username>, \
		    450|451|452|550|552|553 <message>
	    (or with addr_pattern, bypass or admin)"
	}
    }
    regexp -- $ap {}
    lappend addr_patterns [list $at $ap]
}

config_raw addr_patterns_nodefault {
    config_procsetvar addr_regexps_nodefault $value
}
config_raw addr_pattern {
    if {![regexp -- {^([a-z]+)[ \t]+(.+)$} $value dummy at re]} {
	error "addr_pattern takes value `<type> <regexp>' or \
		`<type> /<regexp>/"
    }
    switch -exact $at {
	bypass { set at lax }
	admin { set at unchecked }
    }
    config_procsetvar addr_regexp "$re $at"
}

proc reset_config {} {
    global addr_patterns fill_msgs rbls

    set addr_patterns {
	{policy .*}
	{unchecked sauce-admin@}
	{unchecked postmaster@}
	{bait bait@}
    }
    catch { unset fill_msgs }
    set rbls {}
}

reset_config

proc qualify_filename {fnvar dir} {
    upvar #0 $fnvar fn
    if {[regexp {^/} $fn]} { return }
    set fn "$dir/$fn"
}

proc readconfig {} {
    global argv current_bigerr blmessage config_dir var_dir
    global config_file policies_dir blmessage_file
    global site_whitelist_delay addr_whitelist_delay
    global ipaddr_phase_proportion local_interface

    reset_config    
    config_args $argv

    qualify_filename config_file $config_dir
    config_read $config_file

    if {![string length $current_bigerr]} {
	config_args $argv
    }

    qualify_filename initdb_file $config_dir
    qualify_filename blmessage_file $config_dir
    qualify_filename firewall_command $config_dir
    qualify_filename policies_dir $var_dir

    if {[catch {
	set blm [open $blmessage_file r]
	set blmsg [read $blm]
	close $blm
	regsub {\n\.} $blmsg {\n..} blmessage
    } emsg]} {
	config_err "$blmessage_file:$emsg"
	catch { close $blm }
    }

    config_checkmissing

    if {$ipaddr_phase_proportion < 256 && ![array size local_interface]} {
	set ipaddr_phase_proportion 256
	config_err "ipaddr_phase_proportion used, but local_interfaces not explicit"
    }

    readconfig_posthook
}

proc irrit_present {irritamt} {
    upvar #0 irritated_tell_[string tolower $irritamt] tell
    return $tell
}
