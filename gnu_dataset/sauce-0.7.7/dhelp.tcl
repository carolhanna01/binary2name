# Debugging helper code

proc printarray {an} {
    upvar #0 $an array
    foreach x [array names array] { puts [list "${an}($x)" $array($x)] }
}

proc thrinfo {type} {
    foreach v [uplevel #0 { info vars }] {
	if {[regexp {^([^/]+)/[0-9]+$} $v all t] && [string match $type $t]} {
	    printarray $v
	}
    }
}
