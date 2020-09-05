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


function get_comments()
{
  $directions=
"To access the test, please type in the user name and password.";
  return $directions;
}


function get_login()
{
  $login = <<<EOQ
<b>User name:</b><br>
<input type=text name="t_name" size=30 maxlength=30
onChange="set_name(this.value)">
<br><br>

<b>Password:</b><br>
<input type=password name="password" size=30 maxlength=30
onChange="set_pwd(this.value)">
<br><br>
<input type=submit class="gray_button" name=submit
value="Begin the test" onClick="open_win()">
EOQ;
  return $login;
}


function title_comment_login($page_title="", $comment="", $login="")
{
if (!empty($comment))
  $comment="<h3>$comment</h3>";
echo <<<EOQ
<DIV class=page_title>
<h2>$page_title</h2>
$comment
$login
</DIV>
EOQ;
}


function start_html($title)
{
echo <<<EOQ
<html>
<head>
<title> $title </title>
EOQ;
}


function start_body()
{
echo <<<EOQ
</head>
<body style="background: rgb(200,200,255)" onload="set_all()">
EOQ;
}

start_html("Assessment");

echo <<<EOQ

<SCRIPT LANGUAGE="JavaScript">

var url;
var s_width;
var s_height;  
var window_info;

var t_name;
var pwd;


function set_name(txt) {
  t_name = txt;
  url = '/students/test_by_password.php?t_name=' + t_name;
  url = url + '&password=' + pwd + '&submit=yes';
}


function set_pwd(txt) {
  pwd = txt;
  url = '/students/test_by_password.php?t_name=' + t_name;
  url = url + '&password=' + pwd + '&submit=yes';
}


function set_all() {
  s_width = screen.width - 17;
  s_height = screen.availHeight - 34;
   
  window_info = "top=0,left=0,width=" + s_width;
  window_info = window_info + ",height=" + s_height;
  window_info = window_info + ",scrollbars=yes,resizable=yes,status=no";
  window_info = window_info + ",directories=no,toolbar=no";
}

function open_win() {
  window.open(url,'stud_test',window_info);
}

</script>
EOQ;

start_body();

$page_title = "Assessment";

$login = get_login();
$comments = get_comments();
title_comment_login($page_title, $comments, $login);

?>
</body></html>

