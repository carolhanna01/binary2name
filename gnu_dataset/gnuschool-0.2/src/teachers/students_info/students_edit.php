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
$all_periods=$_POST['all_periods'];
$user=$_POST['user'];
$password=$_POST['password'];
$insert=$_POST['insert'];
$menu_submit=$_POST['menu_submit'];


if (empty($fname) && empty($mname) && empty($lname) && empty($period) &&
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

    if (!empty($period)) if($and_flag)
    { $spanish_query .= " and period = '$period'";}
    else { $spanish_query .= " where period = '$period'"; $and_flag=1;}
  }
  $spanish_query .= " order by period,lname";
}


function input_form()
{
  $login = <<<EOQ
<form method=post action="$PHP_SELF">
<b>Period:</b><br>
<input type=text name=period size=4>&nbsp &nbsp
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
  return $login;
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

function get_new_password() {
  $digits_arr[]="2";
  $digits_arr[]="3";
  $digits_arr[]="4";
  $digits_arr[]="5";
  $digits_arr[]="6";
  $digits_arr[]="7";
  $digits_arr[]="8";
  $digits_arr[]="9";
  $digits_arr[]="A";
  $digits_arr[]="B";
  $digits_arr[]="C";
  $digits_arr[]="D";
  $digits_arr[]="E";
  $digits_arr[]="F";
  $digits_arr[]="G";
  $digits_arr[]="H";
  $digits_arr[]="J";
  $digits_arr[]="K";
  $digits_arr[]="L";
  $digits_arr[]="M";
  $digits_arr[]="N";
  $digits_arr[]="P";
  $digits_arr[]="R";
  $digits_arr[]="S";
  $digits_arr[]="T";
  $digits_arr[]="U";
  $digits_arr[]="V";
  $digits_arr[]="W";
  $digits_arr[]="X";
  $digits_arr[]="Y";
  $digits_arr[]="Z";

  $chars_num_arr[]=5;
  $chars_num_arr[]=6;
  $chars_num_arr[]=7;
  $chars_num_arr[]=8;

  srand ((double) microtime() * 1000000);
  $char_num = array_rand($chars_num_arr, 1);
  $digits = $chars_num_arr[$char_num];

  $arr_rand_keys = array_rand($digits_arr, $digits);
  shuffle($arr_rand_keys);
  
  $password_str = "";
  for ( $i=0; $i<$digits; $i++)
  {
    $password_str .= $digits_arr[$arr_rand_keys[$i]];
  }
  return $password_str;
}


function insert_password ($db_name="", $id=0)
{
  $password = "";
  $p_w_exists=1;
  while ($p_w_exists)
  {
    $password = get_new_password();
    $p_w_exists = password_exists ($db_name, $password);
  }
  $query = "insert into $db_name.passwords (id,password)
            values ($id,'$password')";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
}


function password_exists ($db_name, $password="")
{
  $queryString = "select * from $db_name.passwords where password =
                  '$password'";
  $result = mysql_query($queryString);
  // query_outcome_echo($query,$result);

  if (mysql_num_rows($result) == 0)
    return 0;
  else
    return 1;
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
             mname='$mname' and lname='$lname' and period='$period'";
    $result = mysql_query($query);
  // query_outcome_echo($query,$result);
    if( mysql_num_rows($result) == 0 )
    {
      $query = "insert into $db_name.spanish (fname,mname,lname,period)
                values ('$fname','$mname','$lname','$period')";
      $result = mysql_query($query);
      // query_outcome_echo($query,$result);
      $query = "select id from $db_name.spanish where fname='$fname' and
              mname='$mname' and lname='$lname' and period='$period'";
      $result = mysql_query($query);
      // query_outcome_echo($query,$result);
      $row = mysql_fetch_row($result);
      $id = $row[0];
      insert_password ($db_name, $id);
      $query = "insert into $db_name.seating (period,seat_x,seat_y,id)
                values ('$period',0,0,$id)";
      $result = mysql_query($query);
      // query_outcome_echo($query,$result);
      $comments ="You have inserted a new student.";
    }
    else
      $comments ="The student already exists.";    
  }
  return $comments;
}


function get_ids_arr ($spanish_query)
{
  $periods_idsNames_ass = array();
  $result = mysql_query($spanish_query);
  // query_outcome_echo($query,$result);
  if (mysql_num_rows($result) == 0) return array(); 
  $period_idName_num=array();
  $id_name_arr=array();

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $period = $row['period'];

  $id_name_arr['fname'] = $row['fname'];
  $id_name_arr['mname'] = $row['mname'];
  $id_name_arr['lname'] = $row['lname'];
  $id_name_arr['id'] = $row['id'];
  $period_idName_num[]=$id_name_arr;

  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  { 
    $next_period = $row['period'];
    $id_name_arr['fname'] = $row['fname'];
    $id_name_arr['mname'] = $row['mname'];
    $id_name_arr['lname'] = $row['lname'];
    $id_name_arr['id'] = $row['id'];
    if ($period == $next_period)
      $period_idName_num[]=$id_name_arr;
    else
    {
      $periods_idsNames_ass["$period"] = $period_idName_num;
      $period_idName_num=array();
      $period_idName_num[]=$id_name_arr;
      $period = $next_period;
    }
  }
  $periods_idsNames_ass["$period"] = $period_idName_num;
// show_ids_arr($periods_idsNames_ass);
  return $periods_idsNames_ass;
}


function show_ids_arr($period_idsNames_ass=array())
{
  while (list($period,$ids_names_num) = each($period_idsNames_ass))
  {
    print "period=$period<br>";
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


function show_table_column_title($period)
{
  echo "<br><br>";
  echo startTable("border");
  $cell_data = tDataSource("Period $period", "center", "",1);
  echo rowPrint($cell_data);
}


function show_periods_info_table($db_name="",$spanish_query="",
                                  $period_idsNames_ass)
{
  while (list($period,$ids_names_num) = each($period_idsNames_ass))
  {
    show_table_column_title($period);
    while (list(,$id_name_arr) = each($ids_names_num))
    {
      $form_data=get_info_form($id_name_arr, $period);
      $cell_data = tDataSource($form_data, "center");          
      echo rowPrint($cell_data);
    }
    echo endTable();
  }
//  echo endTable();
}


function get_info_form ($id_name_arr=array(), $period)
{
  $id = $id_name_arr['id'];
  $name = $id_name_arr['fname'];
  $name .= " " . $id_name_arr['mname'];
  $name .= " " . $id_name_arr['lname'];

  $page =
"/teachers/students_info/students_e_d.php?id=$id&period=$period";
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


$tag_title = "gnuschool.org Student Information";


start_html($tag_title);
stylesheet_link("../style_sheet");
start_body("main_background");

if(empty($menu_submit)) $menu_submit = "Student";

$menu_r=array();
$menu_r['active_page']= $menu_submit;
main_menu($menu_r);

$page_title = "Edit student information";

if (!$yes_input || !empty($insert))
{
  $comment = "Please complete the following information:";
  if(!empty($insert))
  {
    $comment = insert_new_student($db_name,$fname,$mname,$lname,$period);
  }

  title_comment_div_noend($page_title, $comment);
  echo input_form();
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
  $comment = "Please click on the student you wish to edit.";
  title_comment_div_noend($page_title, $comment);
  show_periods_info_table ($db_name,$spanish_query,$period_idsNames_ass);
}

echo "</DIV>";

?>
</body></html>

