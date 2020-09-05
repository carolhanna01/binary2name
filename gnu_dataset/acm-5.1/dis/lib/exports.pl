opendir (dir, '.');
print "exports\n";
foreach $i (readdir(dir)) {
    next if ($i eq '.' || $i eq '..');
    if ($i =~ /\.C/ || $i =~ /\.c/) {
	print "; $i\n";
	open (file, $i) || die "unable to open $i: $!\n";
	$static = 0;
	while (<file>) {
	    if (/^static/) {
		$static = 1;
	    }
	    elsif (/^[a-z_A-Z].*\(/ ) {
		($name) = split ( /\s*\(/ );
		if ($static == 1) {
		    $static = 0;
		    next;
		}
		next if $name =~ /\s/;
		print "\t$name\n";
		$static = 0;
	    }
	    else {
		$static = 0;
	    }
	}
	close (file);
    }

