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

function db_exists($db_name="")
{
  $db_exists=0;
  $query="show databases";
  $result = mysql_query("$query");
  while ($row=mysql_fetch_array($result, MYSQL_NUM))
  {
    if ($row[0]=="$db_name") $db_exists=1;
  }
  return $db_exists; 
}


function title_comment_div_noend($page_title="", $comment="")
{
if (!empty($comment))
  $comment="<h3>$comment</h3>";
echo "<DIV class=page_title><h2>$page_title</h2>";
echo $comment;
}


function get_title_comment_login($page_title="", $comment="", $login="")
{
if (!empty($comment))
  $comment="<h3>$comment</h3>";
$source = <<<EOQ
<DIV class=page_title>
<h2>$page_title</h2>
$comment
$login
</DIV>
EOQ;
  return $source;
}


function td_anchor($file, $text)
{
  $data = <<<EOQ
<td align="center">
  <a href="$file?=SID">$text</a>
</td>
EOQ;
  return $data;
}


function td_button($file, $text="")
{
  $data = <<<EOQ
<form method=post action="$file">
<td align="center">
<input class=green_button type=submit name=menu_submit value="$text">
</td>
</form>
EOQ;
  return $data;
}


function td_general_form($text="", $action="")
{
  if ($action=="") $action = $PHP_SELF;
  $data = <<<EOQ
<form method=post action="$action">
<td align="center">
$text
</td>
</form>
EOQ;
  return $data;
}


function td_no_form($text)
{
  $data = <<<EOQ
<td align="center">
$text
</td>
EOQ;
  return $data;
}


function get_main_row($period="", $defaults = "")
{
  $t_data = "<tr>";
  $t_data .=
  td_button("/principals/hr_attendance_search.php",
                                  "Attendance Search");
  $t_data .=
  td_button("/principals/cut_search.php", "Cut Search");
  $t_data .=
  td_button("/principals/students_edit.php","Student Info");
  $t_data .=
  td_button("/principals/dismiss.php","Dismiss");
  $t_data .= get_menu_defaults_form("Attendance Search");
  $t_data .= "</tr>";
  return $t_data;
}


function get_menu_defaults_form ($parent)
{
  $page = "/principals/defaults.php";
  $window_description =
"width=600,height=400,scrollbars=yes,resizable=yes,status=yes";

  $form_html = <<<EOQ
<form method=post target="maindefwindow" 
action="/principals/defaults.php"> 
<td align="center">
<input type=hidden name=menu_submit value="$parent">
<input type=submit class=green_button name=defaults value="Defaults"
onClick="window.open('$page','maindefwindow','$window_description')">
</td>
</form>
EOQ;
  return $form_html;  
}


function get_menu_array()
{
  $submit_arr["Main"] = "main";
  $submit_arr["Student"] = "main";
  $submit_arr["Student Info"] = "main";
  $submit_arr["Edit student information"] = "main";
  $submit_arr["Dismiss"] = "main";
  $submit_arr["Attendance Search"] = "main";
  $submit_arr["Attendance search"] = "main";
  $submit_arr["Cut Search"] = "main";
  $submit_arr["Cut search"] = "main";
  return $submit_arr;
}


function main_menu($var_arr)
{
  $bottom = "";
  $period = $var_arr['period'];
  $act = $var_arr['active_page'];
  $defaults = $var_arr['defaults'];
  $update_inputs = $var_arr['update_inputs'];

  $menu_r = get_menu_array();

  if($menu_r["$act"] == "main")
    $bottom = get_main_row($period, $defaults);

echo <<<EOQ
<DIV id=menu_div class=main_menu>
    <table border cellpadding="0" cellspacing="0" width="100%" 
bordercolor="#000000" bgcolor="#008000">
$bottom
    </table>
</div>
EOQ;
}


function get_start_html($title)
{
  $start_html =<<<EOQ
<html>
<head>
<title> $title </title>
EOQ;
  return $start_html;
}


function get_start_body($color="main_background")
{
  $start_body =<<<EOQ
</head>
<body class="$color">
EOQ;
  return $start_body;
}


function get_defaults_r($db_name)
{
  $defaults_r = array();
  $query = "select * from $db_name.defaults";
  $result = mysql_query($query);
  $row = mysql_fetch_array($result, MYSQL_ASSOC);

  $defaults_r['tables_across'] = $row['passwords_tables_across'];
  $defaults_r['row_space'] = $row['passwords_row_space'];
  $defaults_r['column_space'] = $row['passwords_column_space'];
  $defaults_r['order'] = $row['passwords_order'];

  $defaults_r['fname'] = $row['seating_fname'];
  $defaults_r['mname'] = $row['seating_mname'];
  $defaults_r['lname'] = $row['seating_lname'];
  $defaults_r['name_lines'] = $row['seating_name_lines'];

  $defaults_r['q1_start'] = $row['q1_start'];
  $defaults_r['q1_end'] = $row['q1_end'];
  $defaults_r['q2_start'] = $row['q2_start'];
  $defaults_r['q2_end'] = $row['q2_end'];
  $defaults_r['q3_start'] = $row['q3_start'];
  $defaults_r['q3_end'] = $row['q3_end'];
  $defaults_r['q4_start'] = $row['q4_start'];
  $defaults_r['q4_end'] = $row['q4_end'];

  $defaults_r['search_start'] = $row['search_start'];
  $defaults_r['search_end'] = $row['search_end'];

  return $defaults_r;
}


function end_html_exit()
{
  echo "</DIV></BODY></HTML>";
  exit(1);
}


function start_html($title)
{
echo <<<EOQ
<html>
<head>
<title> $title </title>
EOQ;
}


function stylesheet_link($file)
{
echo <<<EOQ
<LINK REL="STYLESHEET" TYPE="text/css" HREF="$file">
EOQ;
}


function start_body($color="main_background")
{
echo <<<EOQ
</head>
<body class="$color">
EOQ;
}


function query_outcome_echo($query,$result,$errors=0)
{
  if($result === FALSE)
  {
    echo "ERROR " . mysql_errno() . ": " . mysql_error() .
          "<br>$query<br>";
    $errors++;
  }
  else echo "SUCCESS:$query<br>";
  return $errors;
}


?>


