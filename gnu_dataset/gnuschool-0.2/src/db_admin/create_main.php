<?php 
session_start();
session_register("s_password_main");
session_register("s_user_main");
session_register("s_access_n");
session_register("s_access_p");

/*
    gnuschool is a GNU program.
    gnuschool assists school administrators keep track of students.
    It assists educators in the assessment of students by letting 
    educators create and give online tests.
    It also gives students instant test feedback.

    Copyright (C) <2005>  <Peter E. Rios>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    The file COPYING is included with this program. It is a complete 
    copy of the General Public License. You should have received a
    copy of the GNU General Public License along with this program;
    if not, write to the
      Free Software Foundation, Inc.
      51 Franklin Street, Fifth Floor
      Boston, MA  02110-1301  USA


    Please contact the author at peter@gnuschool.org
*/




$create_school_db = $_POST['create_school_db'];

include('login.php');

if( $create_school_db != "Create School Database" )
  $yes_input = 0;

function create_db_form()
{
  echo <<<EOQ
<form method=post action="$PHP_SELF">
<input type=submit name=create_school_db
 value="Create School Database">
</form>
EOQ;
}


function create_db($errors = 0)
{
  $query = "CREATE DATABASE main";
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";
  return $errors;
}


function create_tables($errors = 0)
{
  $fields="(fname varchar(30) default '' not null,
            mname varchar(30) default '' not null,
            lname varchar(30) default '' not null,
            hr varchar(30) default '' not null,
            periods varchar(255) default '|' not null,
    id int(11) unsigned auto_increment, primary key(id))";
  $query = "create table main.spanish " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";

  $fields="(periods varchar(255) default '|' not null)";
  $query = "create table main.periods " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";

  if ($result !== FALSE)
  {
    $query = "insert into main.periods () values()";
    $result = mysql_query($query);
    $errors = query_outcome_echo ($query, $result, $errors);
    echo "<br>";
  }

  $fields="(fname varchar(30) default '' not null
           ,mname varchar(30) default '' not null
           ,lname varchar(30) default '' not null
           ,db_name varchar(30) default '' not null
           ,t_name varchar(30) default '' not null
           ,email varchar(60) default '' not null
           ,hr varchar(30)
           ,tid int(11) unsigned auto_increment
           ,primary key(tid))";
  $query = "create table main.teachers " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";

  $fields="(date date,search varchar(10))";
  $query = "create table main.todate " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  $query="insert into main.todate (date,search) 
          values ('0000-00-00','attendance')";
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  $query="insert into main.todate (date,search) 
          values ('0000-00-00','cut')";
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";


  $fields="(date date,id int(11) unsigned not null,
            reason varchar(255))";
  $query = "create table main.gone " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";

  $fields="(date date,time_out time,id int(11) unsigned not null,
            reason varchar(255))";
  $query = "create table main.dismiss " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";

  $fields = "(id int(11) unsigned, period varchar(12),
             date date)";
  $query = "create table main.cut " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query, $result, $errors);
  echo "<br>";

  $fields="(date date,id int(11) unsigned not null)";
  $query = "create table main.excused " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";

  $fields="(date date,time_in time,
            is_in tinyint(1) default 0 not null,
            id int(11) unsigned not null)";
  $query = "create table main.attendance " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";

  $fields = "(search_start date default '2005-03-15' not null,
              search_end date default '2005-03-16' not null,
              q1_start date default '2005-09-01' not null,
              q1_end date default '2005-11-13' not null,
              q2_start date default '2005-11-14' not null,
              q2_end date default '2006-02-01' not null,
              q3_start date default '2006-02-02' not null,
              q3_end date default '2006-04-15' not null,
              q4_start date default '2006-04-16' not null,
              q4_end date default '2006-06-22' not null,
              time_in time default '07:45:00' not null,
         file_name varchar(90) default 'students.txt' not null,
         file_dir varchar(90) default '/tmp/' not null,

              field_delim varchar(5) default ' ' not null,
              line_delim varchar(5) default ' ' not null,

              col_id varchar(30) default 'ID' not null,
              col_id_opt char(3) default 'no' not null,
              col_hr varchar(30) default 'HOMEROOM' not null,
              col_hr_opt char(3) default 'yes' not null,
              col_pe varchar(30) default 'PERIOD' not null,
              col_pe_opt char(3) default 'no' not null,
              col_all_name varchar(90) default 'NAME' not null,
              col_name_opt char(3) default 'no' not null,
              col_fname varchar(30) default 'FIRSTNAME' not null,
              col_mname varchar(30) default 'MIDDLENAME' not null,
              col_lname varchar(30) default 'LASTNAME' not null)";
  $query = "create table main.defaults " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ($query, $result, $errors);
  echo "<br>";

  if ($result !== FALSE)
  {
    $query = "insert into main.defaults () values()";
    $result = mysql_query($query);
    $errors = query_outcome_echo ($query, $result, $errors);
    echo "<br>";
  }
  return $errors;
}


function access_S_2_main($errors = 0)
{
  $query = "grant select on main.teachers to 'access_S'@'127.0.0.1'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result,$errors);
  return $errors;
}


$tag_title = "gnuschool.org Create Database";
$page_title = "Create a New Database";

start_html($tag_title);

stylesheet_link("admin_stylesheet");

start_body("admin_colors");
back_2_parentA("setup");


if (!$yes_input)
{
  $comment = "Click button to create the school database.";
  title_comment_div_noend($page_title, $comment);
  create_db_form();
  back_2_parentA("setup");
  echo "</DIV>";
}
else
{
  $comment = "Attempting to create School Database...";
  title_comment_div_noend($page_title, $comment);
  $errors = create_db();
  $errors = create_tables($errors);
  $errors = access_S_2_main($errors);

  if ($errors)
    print "<b>There were $errors error(s) while creating the database
           and/or the tables.</b><br>";
  else
    print "<b> The database and tables were successfully created. 
           </b><br><br>";

  back_2_parentA("setup");

  echo "</DIV>";

}
P?>

</body></html>
