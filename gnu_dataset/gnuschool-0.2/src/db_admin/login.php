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

$user = $_POST['user_main'];
$password = $_POST['password_main'];
$tries = $_POST['tries'];
$access_n = $_SESSION[s_access_n];
$access_p = $_SESSION[s_access_p];

if(empty($user)) $user = $_SESSION[s_user_main];
else $_SESSION[s_user_main] = $user;
if(empty($password)) $password = $_SESSION[s_password_main];
else $_SESSION[s_password_main] = $password;


include('functions.php');


$yes_user_password = 1;
$yes_input = 1;
if (empty($password) || empty($user))
  {$yes_user_password = 0; $yes_input = 0;}


if (!$yes_user_password)
{
  get_user_password_login($tries);
  exit(1);
}


if (empty($access_n) || empty($access_p))
{
  $mylink=
  @mysql_connect("127.0.0.1","access_S","nivelS_en_el_sistema");


  if($mylink === FALSE)
  {
    $comment = "Failure to connect to MySQL server.";
    get_user_password_login($tries, $comment);
    exit();
  }  


  $query = "select access from user.users where user='$user' and 
            password=password('$password')";
  $result = mysql_query($query);
  // $errors = 0;
  // $errors = query_outcome_echo($query,$result,$errors);


  if($result === FALSE)
  {
    $comment = "There was an ERROR in the SQL query.";
    get_user_password_login($tries, $comment);
    exit();
  }


  if (mysql_num_rows($result) == 0)
  {
    $comment = "Bad user name or password.";
    get_user_password_login($tries, $comment);
    exit();
  }

  $row = mysql_fetch_row($result);
  $access = $row[0];
  if ($access != "A") 
  {
    $comment = "Bad user name or password. You must have administrator 
                access to log in.";
    get_user_password_login($tries, $comment);
    exit();
  }

  $access_n = "access_" . "$access";
  $access_p = "nivel" . "$access" . "_en_el_sistema";

  $_SESSION[s_access_n] = $access_n;
  $_SESSION[s_access_p] = $access_p;
  mysql_close($mylink);
}

$mylink =
@mysql_connect("127.0.0.1","$access_n","$access_p");


if($mylink === FALSE)
{
  $comment = "Failure to connect to MySQL server with correct 
              user name and password. Please see your system 
              administrator.";
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
    $comments = "<h2>Welcome to database administration.</h2>
                 <h3>Please log in.</h3>";
  elseif(!empty($more_comments))
    $comments="<h2>Database administration.</h2>
           <h3>$more_comments</h3>";
  else
    $comments="<h2>Database administration.</h2>
           <h3>Invalid user name or password. Please try again.</h3>";

  start_html("Log In");
  stylesheet_link("admin_stylesheet");
  start_body("admin_colors");
  echo <<<EOQ
<DIV class=page_title>
$comments
<form method=post action="$PHP_SELF">
<b>Web administrator name:</b><br>
<input type=text name=user_main size=30 maxlength=30>
<br><br>
<input type=hidden name=tries value="$tries">
<b>Web administrator password:</b><br>
<input type=password name=password_main size=30 maxlength=30>
<br><br>
<input type=submit name=submit_login value="Log in">
</form>
</DIV></body></html>
EOQ;
}

?>

