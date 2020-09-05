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

include ('./login.php');

$user=$_POST['user'];
$password=$_POST['password'];
$fname=$_POST['fname'];
$mname=$_POST['mname'];
$lname=$_POST['lname'];
$hr_num=$_POST['hr_num'];
$old_hr_num=$_POST['old_hr_num'];
$id=$_POST['id'];
$get_id=$_GET['id'];
$get_hr_num=$_GET['hr_num'];
$edit_delete=$_POST['edit_delete'];


if(empty($id)) $id = $get_id;
if(empty($hr_num)) $hr_num = $get_hr_num;

if (empty($id) && empty($hr_num))
  $yes_input = 0;


function get_edit_form($student_arr)
{
  $id = $student_arr['id'];
  $fname = $student_arr['fname'];
  $mname = $student_arr['mname'];
  $lname = $student_arr['lname'];
  $hr_num = $student_arr['hr'];

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

<b>Homeroom:</b><br>
<input type=text name=hr_num size=5 maxlength=10 value="$hr_num">
<br>

<input type=hidden name=id value="$id">
<input type=hidden name=old_hr_num value="$hr_num">

<input type=submit name=edit_delete
value="Edit student information">&nbsp &nbsp
<input type=submit name=edit_delete value="Delete student"
onClick="return doneConfirm()">
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


function show_table_column_title($hr_num)
{
  echo startTable("border");
  $cell_data = tDataSource("Homeroom $hr_num student", "center","",1);
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


function get_student_arr($db_name, $id, $hr_num)
{
  $query = "select * from $db_name.spanish where id = $id and
            hr='$hr_num'";
  $result = mysql_query($query);
  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  return $row;
}


function show_student_info_table($student_arr)
{
  $hr_num = $student_arr['hr'];
  show_table_column_title($hr_num);
  $edit_form = get_edit_form($student_arr);
  $cell_data = tDataSource($edit_form, "left");
  echo rowPrint($cell_data);
  echo endTable();
}


function edit_student_info ($db_name="",$submit="",$id=0,$fname="",
                          $mname="",$lname="",$hr_num="",$old_hr_num="")
{
  $comment = "There was an error, please see your administrator.";
  $query="";

  if ($submit == "Edit student information")
  {
    $query = "update $db_name.spanish set fname='$fname',
              mname='$mname', lname='$lname', hr='$hr_num'
              where id=$id and hr='$old_hr_num'";
    $result = mysql_query($query);
    if($result !== FALSE) $comment="The student data has been updated.";
  }
  elseif ($submit == "Delete student")
  {
    $query = "delete from $db_name.spanish where id=$id";
    mysql_query($query);
    $query = "delete from $db_name.attendance where id=$id";
    $result = mysql_query($query);
    if($result !== FALSE) $comment="The student has been deleted.";
  }
  return $comment;
}


$tag_title = "gnuschool.org Grades search";
start_html($tag_title);

stylesheet_link("./style_sheet");

echo <<<EOQ
<SCRIPT LANGUAGE="JavaScript">

function doneConfirm() {
var str='You will delete all information associated with this student, including attendance information. Do you want to continue?'; var ok=confirm(str);
if (ok) return true; else return false;
}

</script>
EOQ;

start_body("main_background");

$page_title = "Student information";

if (!$yes_input)
{
  $comments="Please access this page using the site menu.";
  title_comment_div_noend($page_title, $comments);
  end_html_exit();  
}

$comments="";
if(!empty($edit_delete))
{
  $comments = edit_student_info($db_name,$edit_delete,$id,
                  $fname,$mname,$lname,$hr_num,$old_hr_num);

}

title_comment_div_noend($page_title, $comments);  

if ($edit_delete != "Delete student")
{
  $student_arr = get_student_arr($db_name, $id, $hr_num);
  show_student_info_table ($student_arr);
}
else
{
  show_empty_table();
}
echo "</DIV>";
?>

</body></html>
