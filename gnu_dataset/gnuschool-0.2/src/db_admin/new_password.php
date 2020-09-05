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

$proceed = $_POST['proceed'];
$fname = $_POST['fname'];
$mname = $_POST['mname'];
$lname = $_POST['lname'];
$u_name = $_POST['u_name'];
$u_only = $_POST['u_only'];
$u_too = $_POST['u_too'];

include('login.php');


if(empty($proceed))
  $yes_input = 0;

if( (empty($fname) || empty($mname) || empty($lname)) && empty($u_name))
  $yes_input = 0;

function new_password_forms()
{
  echo <<<EOQ
<BR>
<b>Use a full name to get a new user name and/or password.</b>
<form method=post action="$PHP_SELF">
<input type=checkbox name=u_only value="yes">
Get a new user name, but not a new password
<br><br>
First name:<input type=text size=30 name=fname maxlength=34>
<BR><BR>
Middle name:<input type=text size=30 name=mname maxlength=34>
<BR><BR>
Last name:<input type=text size=30 name=lname maxlength=34>
<BR>
<input type=submit name=proceed value="Proceed">
</form>
<BR>
<b>Give a user name to get a new user name and/or password.</b>
<form method=post action="$PHP_SELF">
<input type=checkbox name=u_too value="yes">
Get a new user name too
<br><br>
User name:<input type=text size=30 name=u_name maxlength=34>
<BR>
<input type=submit name=proceed value="Proceed">
</form>
EOQ;
}


function syllable_rand($start=0)
{
  $str = "";
  $vow = array('a','e','i','o','u');
  $vow_p = array('ai','au','ay','ea','ee','ia','ie'
                 ,'oa','oi','oy','oo','ou');
  $con = array('b','c','d','f','g','h','j','k','l','m','n','p'
               ,'r','s','t','v','w','z');
  $con_p = array('bl','br','ch','cl','cr','dr','fl','fr','gl'
                 ,'gr','ph','pl','pr','sc','sh','sl','sm','sn'
                 ,'sp','st','sw','th','tr','tw');

  $v = rand(0,sizeof($vow) - 1);
  $vp = rand(0,sizeof($vow_p) - 1);
  $c = rand(0,sizeof($con) - 1); 
  $cp = rand(0,sizeof($con_p) - 1); 

  $odds = rand(0,20);
  if($odds < 3 && $start)
  {
    $str = $vow[$v];
    return $str;
  }
  
  $odds = rand(0,20);
  if ($odds < 7)
    $str = $con_p[$cp];
  else
    $str = $con[$c];

  $odds = rand(0,20);
  if ($odds > 8)
    $str .= $vow[$v];
  elseif ($odds < 2)
    $str .= 'y';
  else
    $str .= $vow_p[$vp];
  
  return $str;
}


function user_rand()
{
  $temp = 1;

  while ($temp)
  {
    $user = syllable_rand(1);
    $user .= syllable_rand();
    $user .= syllable_rand();
    $query="select * from user.users where user='$user'";
    $result = mysql_query($query);
    query_outcome_echo($query,$result);
    if ($result === FALSE) return FALSE;
    if (mysql_num_rows($result) === 0) $temp = 0;
  }
  return $user;
}


function password_rand()
{
  $pwd = syllable_rand(1);
  $pwd .= syllable_rand();
  $pwd .= rand(1,99);

  if(rand(0,1))
    $pwd .= '-';
  else
    $pwd .= '_';

  $pwd .= syllable_rand();
  $pwd .= syllable_rand();
  $pwd .= rand(1,99);
  return $pwd;
}


function tid_select($fname,$mname,$lname)
{
  $query = "select tid from main.teachers where fname='$fname'
            and mname='$mname' and lname='$lname'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;

  $rows = mysql_num_rows($result);
  if ($rows)
  {
    $row = mysql_fetch_row($result);
    return $row[0];
  }
  elseif($rows === 0) return 0;
  else return FALSE;
}


function t_password_update( $tid, $user, $pwd)
{
  $pwd_part = "";
  if (!empty($pwd)) $pwd_part = ", password=password('$pwd')";
  $query = "update user.users set user='$user'
            $pwd_part where tid=$tid"; 
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  return $result;
}


function u_password_update( $u_name, $user, $pwd )
{
  $user_str = "";
  if (!empty($user)) $user_part = ", user='$user'";
  $query = "update user.users set password=password('$pwd')
            $user_part where user='$u_name'"; 
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  return $result;
}


function user_check( $u_name )
{
  $query = "select user from user.users where user='$u_name'"; 
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;

  $rows = mysql_num_rows($result);
  if ($rows) return 1;
  elseif($rows === 0) return 0;
  else return FALSE;
}


$tag_title = "gnuschool.org Renew Password and User Name";
$page_title = "Renew a password and/or a user name";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("index");


if (!$yes_input)
{
  $comment = "Get a new password and/or user name by supplying an 
              educator or principal's full name or user name.";
  title_comment_div_noend($page_title, $comment);

  new_password_forms();
  back_2_parentA("setup");
  end_html_exit();
}

$comment = "Attempting to create new login data...";
title_comment_div_noend($page_title, $comment);  

$new_pwd = "";
$new_user = "";
$show = FALSE;

if (empty($u_name))
{
  $tid = tid_select($fname,$mname,$lname);
  if ( $tid )
  {
    if (empty($u_only))
    {
      $new_user = user_rand();
      $new_pwd = password_rand();
      $show = t_password_update( $tid, $new_user, $new_pwd );
      if ($show)
      {
        echo "<b>New user name:</b> $new_user<br>
              <b>New password:</b> $new_pwd <br>
              <b>Please keep them in a safe place.</b>";
      }
    }
    else
    {
      $new_user = user_rand();
      $show = t_password_update( $tid, $new_user, "");
      if ($show)
      {
        echo "<b>New user name:</b> $new_user<br>
              <b>Please keep it in a safe place.</b>";
      }
    }
  }
}
else
{
  if (user_check( $u_name ))
  {
    if (empty($u_too))
    {
      $new_pwd = password_rand();
      $show = u_password_update( $u_name, "", $new_pwd);
      if ($show)
      {
        echo "<b>New password:</b> $new_pwd <br>
              <b>Please keep it in a safe place.</b>";
      }
    }
    else
    {
      $new_user = user_rand();
      $new_pwd = password_rand();
      $show = u_password_update( $u_name, $new_user, $new_pwd );
      if ($show)
      {
        echo "<b>New user name:</b> $new_user<br>
              <b>New password:</b> $new_pwd <br>
              <b>Please keep them in a safe place.</b>";
      }
    }
  }
}

if (!$show)
{
  if (empty($u_name))
  {
    echo "<b>The system was not able to find $fname $mname $lname.</b>";
  }
  else
  {
    echo "<b>The system was not able to find user $u_name.</b>";
  }
}

print "<br><br>";
back_2_parentA("index");
echo "</DIV>";

?>
</body></html>

