###############################################################################
#
# Common Substitutions for sed
#
s,@GNATSENTRY@,@GNATSUSER@:x:@GNATSID@:@GNATSGID@:Gnats Bug-Reporting System:@GNATSHOME@:/bin/sh,g
s,@GNATSID@,41,g
s,@GNATSGID@,41,g
s,@GNATSUSER@,gnats,g
s,@GNATSGROUP@,gnats,g
s,@MAILFILE@,/etc/aliases,g
s,@PASSWDFILE@,/etc/passwd,g
s,@GROUPFILE@,/etc/group,g
s,@GNATSDIR@,/usr/lib/gnats,g
s,@GNATSDBPAR@,/var/lib/gnats,g
s,@GNATSDBDIR@,/var/lib/gnats/gnats-db,g
s,@GNATSADMDIR@,/var/lib/gnats/gnats-db/gnats-adm,g
s,@GNATSHOME@,/var/lib/gnats,g
s,@GNATSOLDHOME@,/usr/lib/gnats/gnats-db,g
s,@CONFIGFILE@,/var/lib/gnats/gnats-db/gnats-adm/config,g
s,@CONFIGDIR@,/etc/gnats,g
s,@DEFAULTSDIR@,/etc/gnats/defaults,g
s,@DB_CONFIG_DIR@,/etc/gnats/db-config,g
s,@DB_CONFIG_DEFAULT_DIR@,/etc/gnats/db-config/default,g
s,@INFODIR@,/usr/share/info,g
s,@DOCDIR@,/usr/share/doc,g
s,@DEFSITE@,default,g
