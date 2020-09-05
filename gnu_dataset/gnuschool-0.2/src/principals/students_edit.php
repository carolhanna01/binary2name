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

$fname=$_POST['fname'];
$mname=$_POST['mname'];
$lname=$_POST['lname'];
$hr_num=$_POST['hr_num'];
$all_periods=$_POST['all_periods'];
$user=$_POST['user'];
$password=$_POST['password'];
$insert=$_POST['insert'];
$menu_submit=$_POST['menu_submit'];


if (empty($fname) && empty($mname) && empty($lname) && empty($hr_num) &&
    empty($all_periods))
  $yes_input = 0;

if ($yes_input)
{
$spanish_query = "select * from $db_name.spanish";
if (empty($all_periods))
{
  $and_flag = 0;
  if (!empty($fname)) if($and_flag)
  { $spanish_query .= " and fname = '$fname'";}
  else { $spanish_query .= " where fname = '$fname'"; $and_flag=1;}

  if (!empty($lname)) if($and_flag)
  { $spanish_query .= " and lname = '$lname'";}
  else { $spanish_query .= " where lname = '$lname'"; $and_flag=1;}

  if (!empty($hr_num)) if($and_flag)
  { $spanish_query .= " and hr = '$hr_num'";}
  else { $spanish_query .= " where hr = '$hr_num'"; $and_flag=1;}
}
$spanish_query .= " order by hr,lname";

}//if $yes_input


function input_form()
{
  echo <<<EOQ
<form method=post action="$PHP_SELF">
<b>Homeroom:</b><br>
<input type=text name="hr_num" size=10 maxlength=10>&nbsp &nbsp
<input type=checkbox name=all_periods value="all_periods"><b>All periods
and all students:</b>
<br><br>

<b>Student's first name:</b><br>
<input type=text name=fname size=30>
<br><br>

<b>Student's middle name:</b><br>
<input type=text name=mname size=3>
<br><br>

<b>Student's last name:</b><br>
<input type=text name=lname size=30>
<br><br>

<input type=hidden name=menu_submit value="Student">
<input type=submit name=submit value="Search for student(s)">
<input type=submit name=insert value="Insert this new student">
</form>
EOQ;
}


function insert_new_student($db_name,$fname,$mname,$lname,$period)
{
  $comments = "";
  if(empty($fname)||empty($lname)||empty($period))
  {
    $comments = "Sorry, you need to complete the form to insert a new
                student.";
  }
  else
  {
    $query = "select * from $db_name.spanish where fname='$fname' and
             mname='$mname' and lname='$lname' and hr='$period'";
    $result = mysql_query($query);
    if( mysql_num_rows($result) == 0 )
    {
      $query = "insert into $db_name.spanish (fname,mname,lname,hr)
                values ('$fname','$mname','$lname','$period')";
      mysql_query($query);
      $comments ="You have inserted a new student.";
    }
    else
      $comments ="The student already exists.";    
  }
  return $comments;
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


function get_ids_arr ($spanish_query)
{
  $periods_idsNames_ass = array();
  $result = mysql_query($spanish_query);
  if (mysql_num_rows($result) == 0) return array(); 
  $period_idName_num=array();
  $id_name_arr=array();

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $hr_num = $row['hr'];
  $id_name_arr['fname'] = $row['fname'];
  $id_name_arr['mname'] = $row['mname'];
  $id_name_arr['lname'] = $row['lname'];
  $id_name_arr['id'] = $row['id'];
  $period_idName_num[]=$id_name_arr;

  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  { 
    $next_hr_num = $row['hr'];
    $id_name_arr['fname'] = $row['fname'];
    $id_name_arr['mname'] = $row['mname'];
    $id_name_arr['lname'] = $row['lname'];
    $id_name_arr['id'] = $row['id'];
    if ($hr_num == $next_hr_num)
      $period_idName_num[]=$id_name_arr;
    else
    {
      $periods_idsNames_ass["$hr_num"] = $period_idName_num;
      $period_idName_num=array();
      $period_idName_num[]=$id_name_arr;
      $hr_num = $next_hr_num;
    }
  }
  $periods_idsNames_ass["$hr_num"] = $period_idName_num;
// show_ids_arr($periods_idsNames_ass);
  return $periods_idsNames_ass;
}


function show_ids_arr($period_idsNames_ass=array())
{
  while (list($hr_num,$ids_names_num) = each($period_idsNames_ass))
  {
    print "Homeroom=$hr_num<br>";
    while (list(,$id_name_arr) = each($ids_names_num))
    {
      $fname = $id_name_arr['fname'];
      $mname = $id_name_arr['mname'];
      $lname = $id_name_arr['lname'];
      $id = $id_name_arr['id'];
      print "$fname $mname $lname, id=$id <br>";
    }
  }
  print "<br>";
}


function show_table_column_title($hr_num)
{
  echo "<br><br>";
  echo startTable("border");
  $cell_data = tDataSource("Homeroom $hr_num", "center", "",1);
  echo rowPrint($cell_data);
}


function show_periods_info_table($db_name="",$spanish_query="",
                                  $period_idsNames_ass)
{
  while (list($hr_num,$ids_names_num) = each($period_idsNames_ass))
  {
    show_table_column_title($hr_num);
    while (list(,$id_name_arr) = each($ids_names_num))
    {
      $form_data=get_info_form($id_name_arr, $hr_num);
      $cell_data = tDataSource($form_data, "center");          
      echo rowPrint($cell_data);
    }
    echo endTable();
  }
//  echo endTable();
}


function get_info_form ($id_name_arr=array(), $hr_num)
{
  $id = $id_name_arr['id'];
  $name = $id_name_arr['fname'];
  $name .= " " . $id_name_arr['mname'];
  $name .= " " . $id_name_arr['lname'];

  $page =
"/principals/students_e_d.php?id=$id&hr_num=$hr_num";
  $window_description =
"width=600,height=400,scrollbars=yes,resizable=yes,status=yes";

  $form_html = <<<EOQ
<input type=button name="button" value="$name" class=info_button
onClick="window.open('$page','newwindow','$window_description')">
EOQ;
  return $form_html;  
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

stylesheet_link("./style_sheet");

start_body("main_background");

if(empty($menu_submit)) $menu_submit = "Student";

$menu_r=array();
$menu_r['active_page']= $menu_submit;
main_menu($menu_r);

$page_title = "Student information";

if (!$yes_input)
{
  $comments = "Please complete the following information:";
  title_comment_div_noend($page_title, $comments);
  input_form();
  end_html_exit();
}

if(!empty($insert))
{
  $comments=insert_new_student($db_name,$fname,$mname,$lname,$hr_num);
  title_comment_div_noend($page_title, $comments);
  input_form();
  end_html_exit();
}

$period_idsNames_ass = get_ids_arr ($spanish_query);
if(sizeof($period_idsNames_ass) == 0)
{
  $comment = "Sorry, no student matches found.";
  title_comment_div_noend($page_title, $comment);
}   
else
{
  $page_title = "Edit student information";
  $comment = "Please click on the student you wish to edit.";
  title_comment_div_noend($page_title, $comment);
  show_periods_info_table ($db_name,$spanish_query,$period_idsNames_ass);
}
echo "</DIV>";
?>
</body></html>

