<?php 
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


$user_super = $_POST['user_super'];
$password_super = $_POST['password_super'];
$user = $_POST['user'];
$password1 = $_POST['password1'];
$password2 = $_POST['password2'];
$create_user_db = $_POST['create_user_db'];
$tries = $_POST['tries'];

include('functions.php');

$yes_input = 1;
if ( empty($user) || empty($create_user_db))
  $yes_input = 0;

if (empty($password_super) || empty($user_super))
  $yes_input = 0;

if (empty($password1) || empty($password2))
  $yes_input = 0;

if ( $password1 != $password2)
  $yes_input = 0;

if (!$yes_input)
{
  get_user_password_login($tries);
  exit();
}

$password = $password1;

$mylink =
mysql_connect("127.0.0.1","$user_super","$password_super");


if($mylink === FALSE)
{
  $comment = "Failed to connect to MySQL server.";
  get_user_password_login($tries, $comment);
  exit();
}  


function get_user_password_login($tries="", $more_comments = "")
{
  if(empty($tries))
    $tries = "v1";
  else
  {
    $val = substr($tries, 1);
    $new_val = ((int) $val) + 1;
    $tries = "v" . "$new_val";
  }

  if($tries == "v1")
  {
    $page_title = "Welcome Administrator";
    $comment = "This page has 2 steps. Please complete the login at 
       the top, and the new administrator's name and passwords at the 
       bottom.";   
  }
  elseif(!empty($more_comments))
  {
    $page_title = "Administrator";
    $comment = "$more_comments";
  }
  else
  {
    $page_title = "Administrator";
    $comment = "Invalid user name or password. Please try again.";
  }

  start_html("Log In and Create User Database");
  stylesheet_link("admin_stylesheet");
  start_body("admin_colors");
  title_comment_div_noend($page_title, $comment);
  echo <<<EOQ
<form method=post action="$PHP_SELF">
<b>MySQL super user name:</b><br>
<input type=text name=user_super size=30 maxlength=30>
<br><br>
<input type=hidden name=tries value="$tries">
<b>MySQL super user password:</b><br>
<input type=password name=password_super size=30 maxlength=30>
<br><br><hr>
<h3>Further administration of the school database via the web will be done
by this new administrator. Please complete the form to create the user.</h3>
<b>New web administrator's login name:</b><br>
<input type=text name=user size=30 maxlength=30>
<br><br>
<b>New web administrator's password:</b><br>
<input type=password name=password1 size=30 maxlength=30>
<br><br>
<b>Retype the password:</b><br>
<input type=password name=password2 size=30 maxlength=30>
<br><br>
<input type=submit name=create_user_db value="Create User Database">
</form>
EOQ;
back_2_parentA("setup");
echo "</DIV></body></html>";
}


function create_db($errors = 0)
{
  $query = "CREATE DATABASE user";
  $result = mysql_query($query);
  $errors = query_outcome_echo ( $query, $result, $errors);
  return $errors;
}


function create_table($user="", $password="", $errors = 0)
{
  $fields="(password char(41) not null
            , user varchar(30) not null
            , access char(1) not null
            , changed char(1) default 'N' not null
            , tid int(11) unsigned
            , primary key(user))";
  $query = "create table user.users " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo ( $query, $result, $errors);

  if ($result !== FALSE)
  {
    $query = "insert into user.users (user, password, access)
              values('$user', password('$password'), 'A')";
    $result = mysql_query($query);
    $errors = query_outcome_echo ( $query, $result, $errors);

    $query = "grant all privileges on *.* to 'access_A'@'127.0.0.1'
              identified by 'nivelA_en_el_sistema'
              with grant option";
    $result = mysql_query($query);
    $errors = query_outcome_echo ( $query, $result, $errors);

    $query = "grant all privileges on *.* to 'access_P'@'127.0.0.1'
              identified by 'nivelP_en_el_sistema'";
    $result = mysql_query($query);
    $errors = query_outcome_echo ( $query, $result, $errors);

    $query = "grant all privileges on *.* to 'access_T'@'127.0.0.1'
              identified by 'nivelT_en_el_sistema'";
    $result = mysql_query($query);
    $errors = query_outcome_echo ( $query, $result, $errors);

    $query = "grant select on user.* to 'access_S'@'127.0.0.1'
              identified by 'nivelS_en_el_sistema'";
    $result = mysql_query($query);
    $errors = query_outcome_echo ( $query, $result, $errors);
  }

  return $errors;
}


$tag_title = "gnuschool.org Create User Database";
$page_title = "Create User Database";

start_html($tag_title);

stylesheet_link("admin_stylesheet");

start_body("admin_colors");
back_2_parentA("setup");


$comment = "Attempting to create user database... ";
title_comment_div_noend($page_title, $comment);
$errors = create_db();
$errors = create_table($user,$password,$errors);

if ($errors)
  print "<b>There were $errors error(s) while creating the database
         and/or the tables.</b><br>";
else
  print "<b> The database and tables were successfully created. 
         </b><br>";

echo "<br>";
back_2_parentA("setup");
echo "</DIV>";

?>

</body></html>

