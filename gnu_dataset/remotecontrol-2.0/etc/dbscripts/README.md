These scripts are for database administration.

Run 1-v2-database.sql to create a default database called 'gnurc_v2'.

Run 2-v2-user.sql to create a default user 'gnurc_v2_user' with password 'default'.

Run 3-v2-schema.sql to create the database schema.

Run 4-v2-default-languages.sql to include default languages.

Or you can run all the scripts with the interactive make-db.sh script, just run:

 # ./make-db.sh

and follow the prompts!

Once your database is created you need to configure your instance to use it... That means editing your config.php.
