#!/usr/local/bin/perl
print "static dis_datum_info lookup[] =  {\n";
while (<STDIN>) {
    if (/^\s*Datum/) {
	chop;
	$front = substr($_, 0, index($_, '=') - 1);
	$back = substr($_, index($_, '=') + 1);
	$back =~ s/\/\*.*$//g;
	$back =~ s/,//g;
	$_ =~ s/^[ \t]+//;
	($datum) = split;
	$datum =~ s/^Datum//;
	$datum =~ s/([A-Z])/ $1/g;
	$datum =~ s/^ //;
	$datum =~ s/D I S/DIS/g;
	$datum =~ s/I D/ID/g;
	$datum =~ s/Alt /Alternate /g;
	$datum =~ s/=$//g;
	$datum =~ s/[0-9]mm([^ ])/mm $1/g;
	$datum =~ s/quantity/Quantity/g;
	$datum =~ s/H E A T/HEAT /g;
	$datum =~ s/S A B O T/Sabot /g;
	$_ = $back;
	$_ =~ s/^[ \t]+//;
	($value) = split;
	print "\t{ ",$value, ", \"", $datum, "\" },\n";
    }
}
print "};\n";
