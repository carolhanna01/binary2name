<?php 
session_start();
session_register("s_password");
session_register("s_user");
session_register("s_db_name");
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

include ('../login.php');

$fname=$_POST['fname'];
$mname=$_POST['mname'];
$lname=$_POST['lname'];
$period=$_POST['period'];
$old_period=$_POST['old_period'];
$id=$_POST['id'];
$get_id=$_GET['id'];
$get_period=$_GET['period'];
$edit_delete=$_POST['edit_delete'];


if(empty($id)) $id = $get_id;
if(empty($period)) $period = $get_period;

if (empty($id) && empty($period))
  $yes_input = 0;


function get_edit_form($student_arr)
{
  $id = $student_arr['id'];
  $fname = $student_arr['fname'];
  $mname = $student_arr['mname'];
  $lname = $student_arr['lname'];
  $period = $student_arr['period'];

  $edit_form = <<<EOQ
<form method=post action="$PHP_SELF">
<b>Student's first name:</b><br>
<input type=text name=fname size=30 value="$fname">
<br><br>

<b>Student's middle name:</b>
<input type=text name=mname size=3 value="$mname">
<br><br>

<b>Student's last name:</b><br>
<input type=text name=lname size=30 value="$lname">
<br><br>

<b>Period:</b><br>
<input type=text name=period size=3 value="$period">
<br>

<input type=hidden name=id value="$id">
<input type=hidden name=old_period value="$period">

<input type=submit name=edit_delete value="Save changes">&nbsp &nbsp
<input type=submit name=edit_delete value="Delete student"
onClick="return doneConfirm1()">
</form>
EOQ;
  return $edit_form;
}


function tDataSource($data="", $align="", $width="", $colspan="")
{
  if ($align != "") $align = "align=" . $align;
  if ($colspan != "") $colspan = "colspan=" . $colspan;
  if ($width != "") $width = "width=" . $width;
$data_html = <<<EOQ
<TD $align $colspan $width>
$data
</TD>
EOQ;
return $data_html;
}


function rowPrint($t_data="", $align="")
{
  if ($align != "") $align = "align=" . $align;
return "<TR $align> $t_data </TR>";
}


function startTable($border="", $cellpadding=0, $cellspacing=0)
{
  $cellpadding = "cellpadding=" . "$cellpadding";
  $cellspacing = "cellspacing=" . "$cellspacing";
  return "<TABLE $border $cellspacing $cellpadding>";
}

function endTable()
{
  return "</TABLE>";
}


function show_table_column_title($period)
{
  echo startTable("border");
  $cell_data = tDataSource("Period $period student", "center","",1);
  echo rowPrint($cell_data);
}


function show_empty_table()
{
  echo startTable();
  $message = "All information pertaining to this student has been
              deleted successfully";
  $cell_data = tDataSource("$message", "left");
  echo rowPrint($cell_data);
  echo endTable();
}


function get_student_arr($db_name, $id, $period)
{
  $query = "select * from $db_name.spanish where id = $id and
            period='$period'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  return $row;
}


function show_student_info_table($student_arr)
{
  $period = $student_arr['period'];
  show_table_column_title($period);
  $edit_form = get_edit_form($student_arr);
  $cell_data = tDataSource($edit_form, "left");
  echo rowPrint($cell_data);
  echo endTable();
}


function edit_student_info ($db_name="",$submit="",$id=0,$fname="",
                          $mname="",$lname="",$period="",$old_period="")
{
  $query="";

  if ($submit == "Save changes")
  {
    $query = "update $db_name.spanish set fname='$fname',
              mname='$mname', lname='$lname', period='$period'
              where id=$id and period='$old_period'";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    $query = "update $db_name.passwords set period='$period' where id=$id";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    $query = "update $db_name.seating set period='$period' where id=$id
              and period='$old_period'";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    $query = "update $db_name.attendance set period='$period' where id=$id
              and period='$old_period'";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    $query = "update $db_name.scores set period='$period' where id=$id
              and period='$old_period'";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  }
  elseif ($submit == "Delete student")
  {
    $query = "delete from $db_name.spanish where id=$id";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    $query = "delete from $db_name.passwords where id=$id";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    $query = "delete from $db_name.seating where id=$id";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    $query = "delete from $db_name.attendance where id=$id";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    $query = "delete from $db_name.scores where id=$id";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  }
}


function start_main_div ()
{
  echo '<DIV class=main_table>';
}


function end_main_div ()
{
  echo '</DIV>';
}


$tag_title = "gnuschool.org Grades search";

start_html($tag_title);
stylesheet_link("../style_sheet");

if ($yes_input)
{
  echo <<<EOQ
<SCRIPT LANGUAGE="JavaScript">

function doneConfirm1() {
var str='You will delete all information associated with this student, including tests taken and attendance information. Do you want to continue?'; var ok=confirm(str);
if (ok) return true; else return false;
}

</script>
EOQ;
}

start_body("main_background");


$page_title = "Student information";

if (!$yes_input)
{
  $comment="Please access this page using the site menu.";
  title_comment_div_noend($page_title, $comment);
  end_html_exit();
}

title_comment_div_noend($page_title);  

if(!empty($edit_delete))
  edit_student_info($db_name,$edit_delete,$id,
                  $fname,$mname,$lname,$period,$old_period);


if ($edit_delete != "Delete student")
{
  $student_arr = get_student_arr($db_name, $id, $period);
  show_student_info_table ($student_arr);
}
else
{
  show_empty_table();
}

echo "</DIV>";

?>
</body></html>

