# GNU Remote Control v2.0 Installation Instructions

To install the v2.0 release please follow the below instructions.

## Get the code!

Check out from Subversion:

 svn co \
  http://svn.savannah.gnu.org/svn/remotecontrol/trunk/ \
  /path/to/gnurc_v2

Then complete other configuration from the install directory:

 cd /path/to/gnurc_v2

E.g.:

 cd /var/www/gnurc_v2

## Do some house-keeping

You can remove some stuff that shouldn't be in the release:

 rm -rf src/old
 rm -rf test/

## Install/Configure the Database

### Load time zones

This step is optional. See here:

 http://dev.mysql.com/doc/refman/5.6/en/time-zone-support.html

for information on how to load timezones into your MySQL deployment.

On my system the command was:

 mysql_tzinfo_to_sql /usr/share/zoneinfo | mysql -u root -p mysql

Then for good measure:

 service mysql restart

### Automatic Database Installation

To install the database run the make-db.sh script, e.g.:

 etc/dbscripts/make-db.sh

The make-db.sh script will ask you for your settings. Alternatively you can
run the script non-interactively by providing command-line arguments.

Command-line arguments are sequential:

 1: database name
 2: database user
 3: database user password
 4: load test data? 'y' for yes, 'n' for no
 5: the password for the root account

For example:

 etc/dbscripts/make-db.sh gnurc_v2 gnurc_v2_user secret n more-secret

### Manual Database Installation

To install the database manually follow the below steps.

 1. Create the database, see etc/dbscripts/1-v2-database.sql for example.
 2. Create and authorize the user, see etc/dbscripts/2-v2-user.sql for example.
 3. Create the database schema, run etc/dbscripts/3-v2-schema.sql for details.
 4. Install basic languages, run /etc/dbscripts/4-v2-default-languages.sql

## Configure Apache

Enable Auth Digest:

 a2enmod auth_digest

Create a .htdigest file for user accounts and add at least one user. For
example, to add the first user:

 htdigest -c /etc/apache2/gnurc.htdigest GNUrc admin

You will be prompted to provdie a password for the new admin account.

Edit your Apache config file, e.g.:

 vim /etc/apache2/sites-available/default.conf

Insert the following stanza:

  Alias /gnurc_v2 /path/to/gnurc_v2/web
  <Location /gnurc_v2>
    AuthType Digest
    AuthName "GNUrc"
    AuthUserFile  /etc/apache2/gnurc.htdigest
    Require user admin
  </Location>

Replace the location ('/gnurc_v2') as appropriate for the location of your
GNU Remote Control files.

Restart Apache to apply configuration changes:

 apache2ctl graceful

## Configure GNU Remote Control

To create a new config.php configuration file it is convenient to create one
from a template, e.g.:

 cp config.example.php config.php

Then edit config.php:

 cp config.php

### Configure Debug Settings

To enable debugging (for developers only):

 define( 'DEBUG', true );

Appropriate production settings are:

 define( 'DEBUG', false );

### Configure Database Settings

Provide the host, user, password and database name settings to connect to the
database you created above. If you don't know these settings speak to your
system administrator. See the example config file for typical values.

If you have loaded time zone info into MySQL (see above) you can enable
named time zone support on the database by setting DB_TZ_NAME to true.
Default is false which means daylight savings arne't accommodated in reports.

### Configure Web Settings

The web root in the part of the URL that points to the 'web' directory of
GNU Remote Control. If you followed the above instructions for configuring
Apache with an alias to the GNU Remote Control 'web' directory then your
web root is '/gnurc_v2'; if not you need to provide the path to the 'web'
directory as you would have it in the URL.

### Configure HTML Settings

Choose a HTML version to support. Your options are HTML4, XHTML, HTML5. Note
that there is generally usually no reason to use anything other than HTML5.

Choose whether pretty printing is supported for HTML. If enabled HTML content
is formatted and indented with spaces, making it easier to read. Alternatively
pretty printing can be disabled, which improves bandwidth efficiency.

### Configure Temperature Settings

To support Fahrenheit:

 const DEFAULT_SCALE = FAHRENHEIT;

To support Celsius:

 const DEFAULT_SCALE = CELSIUS

Note, this setting should be set once in the beginning and never changed.
It is best practice to make sure your devices are using the same scale.

### Configure Date/Time Settings

You can nominate a default time zone for your installation. For a list of
available time zones, see:

 https://www.progclub.net/~jj5/test/php/timezones.php

### Configure Network Settings

Configure the number of times GNU Remote Control will retry network write
attempts and the number of times it will retry network read attempts. Set
the NET_TIMEOUT as the number of seconds after which network operations
time-out.

## User Administration

To add new users to GNU Remote Control, first register the new user's password:

 htdigest /etc/apache2/gnurc.htdigest GNUrc <username>

Replace <username> with the user's actual user name. You will be prompted
for the password. This creates their HTTP Digest Auth account. You then need
to authorize the new account. You do this by modifying your 'Require user'
Apache configuration. For example, before adding your new user your
'Require user' settings might be:

 Require user admin

And to authorize a new account for Joe:

 Require user admin joe

After adding new users restart Apache to apply changes:

 apache2ctl graceful
