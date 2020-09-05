#   gnatsweb-site-koi8-r.pl allows to set charset (koi8-r) in http headers 
#   without original gnatsweb.pl sources modification
#
#   Copyright (C) 2000 Konstantin Kivi <kkivi@mailru.com>
#
#   This program is free software; you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation; version 2 dated June, 1991.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program; if not, write to the Free Software
#   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
#
# On Debian GNU/Linux systems, the complete text of the GNU General
# Public License can be found in `/usr/share/common-licenses/GPL'
# and `/usr/share/common-licenses/LGPL'.
#
#
#   copy this file as /usr/lib/cgi-bin/gnatsweb-site.pl
#   or call it from existing gnatsweb-site.pl like
#   do  "$path_to_this_file/gnatsweb-site-koi8-r.pl"


{
# I don't know why should I call CGI::header once
# before redefintion
# I made test example with a simple module and it worked without this hack 
# if you know the reason please mail me at kkivi@mailru.com
       
	use CGI;
	eval{
		my($q)=new CGI;
		$q->header;
	}
}

package CGI;


sub myheader;
*CGI::oldheader=\&CGI::header;
*CGI::header=\&CGI::myheader;
#*CGI::header=*::header=\&CGI::myheader;
sub myheader{
      my($self)=shift;
      my($k,$v)=(undef,undef);
      my(@arg,$fl );
      for $l (@_){
		
		if(defined($k)){
			$v=$l;
		}else{
			$k=$l;
		}
		if(!defined($v)){
			next;
		}
		if($k eq "-type"){
			$fl=1;
			if($v =~ m#(text/html)(.*)#){
				if($2 !~ /charset/){
					$v="text/html; charset=koi8-r";
				}
			}
		}
		push(@arg,$k => $v);
		undef($k);
		undef($v);
			
      }

      if(!$fl){
	push(@arg, -type =>  "text/html; charset=koi8-r");
      }	
      return $self->CGI::oldheader( @arg);
}

