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

$period=$_POST['period'];
$id=$_POST['id'];
$undo=$_POST['undo'];
$undo_all=$_POST['undo_all'];
$menu_submit=$_POST['menu_submit'];

if (empty($period)) $yes_input = 0;
else
 if ($period == "hr" || $period == "Hr" || $period == "hR") $period = "HR";


function input_form()
{
  $login = <<<EOQ
<form method=post action="$PHP_SELF">

<b>Period:</b><br>
<input type=text name=period size=4>
<br><br>

<input type=hidden name=menu_submit value="Attendance">
<input type=submit name=submit value="Take attendance">
</form>
EOQ;
  return $login;
}


function get_students_arr( $db_name="", $period="", $defaults_r)
{
  $def_fname = $defaults_r['fname'];
  $def_mname = $defaults_r['mname'];
  $def_lname = $defaults_r['lname'];
  $def_name_lines = $defaults_r['name_lines'];
  $students_arr = array();
  $query = "select * from $db_name.spanish where period = '$period'";
  $result = mysql_query($query);

  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    $fname = $row['fname'];
    $mname = $row['mname'];
    $lname = $row['lname'];
    $id = $row['id'];
    $continues = "";
    if ($def_fname == 'n') $fname = "";
    else
    {
      $length = strlen($fname);

      if ($length > (int)$def_fname)
        {$length = (int)$def_fname; $continues = "~";}
      $fname = substr($fname, 0, $length);
      $fname .= $continues;
      $continues = "";
      if($def_mname!='n' || $def_lname!='n')
      {
        if($def_name_lines=='f' || $def_name_lines=='a') $fname .= "\n";
        elseif($def_name_lines=='n'||$def_name_lines=='m') $fname .= " ";
      }
      elseif ($def_name_lines=='m' && $def_mname=='n') $fname .= "\n";
    }

    if ($def_mname == 'n') $mname = "";
    elseif($def_lname!='n')
    {
      if ($def_name_lines=='m' || $def_name_lines=='a') $mname .= "\n";
      elseif ($def_name_lines=='n' || $def_name_lines=='f') $mname .= " ";
    }

    if ($def_lname == 'n') $lname = "";
    else
    {
      $length = strlen($lname);
      if ($length > (int)$def_lname)
        { $length = (int)$def_lname; $continues = "~";}
      $lname = substr($lname, 0, $length);
      $lname .= $continues;
    }
    $students_arr["$id"] = "$fname" . "$mname" . "$lname";
  }
  return $students_arr;
}


function delete_today($db_name="", $period="")
{
 $dateTime = getdate();
 $date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";
 $query="";
 if ($period != "HR")
 {
   $query = "delete from $db_name.attendance where date='$date'
             and period='$period'";
 }
 else
 {
   $query = "delete from $db_name.hr_attendance where date='$date'";
 }
 mysql_query($query);
}


function set_all_hr_absent ($db_name="", $students_arr)
{
 $dateTime = getdate();
 $date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";
 while (list ($id,) = each($students_arr))
 {
   $query = "select id from $db_name.hr_attendance where date='$date'
             and id=$id";
   $result = mysql_query($query);
   if ($result !== FALSE && mysql_num_rows($result) == 0)
   {
     $query = "insert into $db_name.hr_attendance
               (date, time_in,id,is_in)
               values ('$date','00:00:00',$id,0)";
     mysql_query($query);
   }
 }
}


function set_all_absent ($db_name="", $students_arr, $period)
{
 $dateTime = getdate();
 $date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";
 while (list ($id,) = each($students_arr))
 {
   $query = "select id from $db_name.attendance where date='$date'
             and id=$id and period='$period'";
   $result = mysql_query($query);
   if ($result !== FALSE && mysql_num_rows($result) == 0)
   {
     $query="insert into $db_name.attendance 
             (period,date,time_in,id,is_in)
             values ('$period','$date','00:00:00',$id,0)";
     mysql_query($query);
   }
 }
}


function update_hr_attendance ($db_name="", $id=0, $undo="")
{
  $dateTime = getdate();
  $date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";
  $time_in = "$dateTime[hours]:$dateTime[minutes]:$dateTime[seconds]";

  if ($undo == "n")
  {
    $query="update $db_name.hr_attendance set time_in='$time_in',
        is_in=1 where date='$date' and id=$id and is_in=0";
    mysql_query($query);
  }
  else
  {
    $query="update $db_name.hr_attendance set time_in='00:00:00',
            is_in=0 where date='$date' and id=$id";
    mysql_query($query);
  }
}


function update_attendance ($db_name="", $id=0, $undo="", $period="")
{
  $dateTime = getdate();
  $date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";
  $time_in = "$dateTime[hours]:$dateTime[minutes]:$dateTime[seconds]";

  if ($undo == "n")
  {
    $query="update $db_name.attendance set time_in='$time_in',
        is_in=1 where date='$date' and id=$id and is_in=0
        and period='$period'";
    mysql_query($query);
  }
  else
  {
    $query="update $db_name.attendance set time_in='00:00:00',
         is_in=0 where date='$date' and id=$id and period='$period'";
    mysql_query($query);
  }
}


function get_seating_data( $db_name="", $id=0, $period="" )
{
  $id_arr = array();
  $query = "select * from $db_name.seating where id = $id and
            period='$period'";
  $result = mysql_query($query);
  $row = mysql_fetch_object($result);
  return $row;
}


function is_absent($db_name, $id=0, $period="")
{
  $dateTime = getdate();
  $date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";

  $query = "";

  if($period != "HR")
  {
    $query = "select * from $db_name.attendance where id=$id and
              date='$date' and is_in=0 and period='$period'";
    $result = mysql_query($query);
  }
  else
  {
    $query = "select * from $db_name.hr_attendance where id=$id and
              date='$date' and is_in=0";
    $result = mysql_query($query);
  }
  
  if ($result !== FALSE && mysql_num_rows($result) == 0)
    return 0;
  else
    return 1;
}


function array_to_html($db_name="", $students_arr=array(), $period)
{
  $source = "";
  while (list ($id , $name) = each($students_arr))
  {
    if (is_absent( $db_name, $id, $period))
      $class_str = "gray_button";
    else
      $class_str = "green2_button";
    $seating_arr = get_seating_data( $db_name, $id, $period);
    $seat_x = $seating_arr->seat_x;
    $seat_y = $seating_arr->seat_y;
    $undo = "u" . $id;
echo <<<EOQ
<DIV STYLE="position:absolute; top:$seat_y; left:$seat_x;">
<form method=post action="$PHP_SELF">
<input type="hidden" name="id" value=$id>
<input type="hidden" name="menu_submit" value="Attendance">
<input type="hidden" name="period" value="$period">
<input type="hidden" name="db_name" value="$db_name">
<input id="$undo" type="hidden" name="undo" value="n">
<input type="submit" class=$class_str name="submit" value="$name"
onclick="set_undo_value('$undo','check_undo');">
</form>
</DIV>
EOQ;
  }
}


$tag_title = "gnuschool.org Take attendance";

start_html($tag_title);
stylesheet_link("../style_sheet");

if ($yes_input && empty($undo_all))
{
  echo <<<EOQ
<SCRIPT LANGUAGE="JavaScript">

var brOK = false, mie = false; 
if (document.layers || document.all) brOK = true; 
if (document.all) mie = true; 


function set_undo_value(hidden_id,checkbox_id)
{
  if (!mie)
  {
    if ( document[checkbox_id].checked == true)
      document[hidden_id].value = 'y';
  }
  else
  {
    if ( document.getElementById(checkbox_id).checked == true)
      document.getElementById(hidden_id).value = 'y';
  }
  return true;
}

</SCRIPT>
EOQ;
}

start_body("main_background");

if(empty($menu_submit)) $menu_submit = "Attendance";

$def_r = get_defaults_r($db_name);
$students_arr = get_students_arr( $db_name, $period, $def_r);  

if (!empty($undo_all))
{
  delete_today($db_name, $period);
  $yes_input = 0;
  $menu_submit = "Main";
}

$menu_r=array();
$menu_r['active_page']= $menu_submit;
$menu_r['period'] = "$period";
main_menu($menu_r);

$page_title = "Take attendance";

if (!$yes_input)
{
  $comment = "Please complete the following information:";
  title_comment_div_noend($page_title, $comment);
  echo input_form();
  end_html_exit();
}

if (empty($id))
{
  if ($period != "HR")
    set_all_absent ($db_name, $students_arr, $period);
  else
    set_all_hr_absent ($db_name, $students_arr);
}
else
{
  if ($period != "HR")
    update_attendance($db_name, $id, $undo, $period);
  else
    update_hr_attendance($db_name, $id, $undo);
}

array_to_html($db_name, $students_arr, $period);
?>

</body></html>

