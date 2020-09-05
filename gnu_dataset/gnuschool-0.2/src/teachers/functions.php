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


function get_attendance_row($period="", $defaults = "")
{
  $t_data = "<tr>";
  $t_data .= 
td_button("/teachers/tests_admin/lock_unlock_db.php","Main");
  $t_data .=
  td_button("/teachers/attendance/attendance.php", "Take Attendance");
  $t_data .=
  td_button("/teachers/attendance/absent_search.php", "Absent Search");
  if(!empty($defaults))
  {
    $t_data .= get_menu_defaults_form("Absent Search");
  }
  if(!empty($period))
  {
    $temp_str = <<<EOQ
<input type=hidden name=period value="$period">
<input type=hidden name=menu_submit value="Monitor">
<input class=green_button type=submit name=""
 value='Monitor "PERIOD $period"'>
EOQ;
    $t_data .= td_general_form($temp_str,
                         "/teachers/tests_admin/test_monitor2.php");
  }
  $t_data .= "</tr>";
  if(!empty($period))
    $t_data .= get_attendance2_row($period);
  return $t_data;
}


function get_attendance2_row($period="")
{
  $t_data = "<tr>";
  $temp_str = <<<EOQ
<input id="check_undo" type=checkbox name="undo_attend"
value="yes"><FONT class="green_button">Absent</FONT>
EOQ;
  $t_data .= td_no_form($temp_str);
  
  $temp_str = <<<EOQ
<input type=hidden name=period value="$period">
<input class=green_button type=submit name="undo_all"
 value='Undo attendance for "PERIOD $period"'>
EOQ;
  $t_data .= td_general_form($temp_str);
  $temp_str = <<<EOQ
<input type=hidden name=period value="$period">
<input type=hidden name=menu_submit value="Seats">
<input class=green_button type=submit name=""
 value='Change seats for "PERIOD $period"'>
EOQ;
  $t_data .= td_general_form($temp_str, "/teachers/seats/seating_edit.php");
  $t_data .= get_menu_defaults_form("Attendance");
  $t_data .= "</tr>";

  return $t_data;
}


function get_menu_defaults_form ($parent)
{
  $page = "/teachers/defaults.php";
  $window_description =
"width=600,height=400,scrollbars=yes,resizable=yes,status=yes";

  $form_html = <<<EOQ
<form method=post target="defwindow" action="/teachers/defaults.php"> 
<td align="center">
<input type=hidden name=menu_submit value="$parent">
<input type=submit class=green_button name=defaults value="Defaults"
onClick="window.open('$page','defwindow','$window_description')">
</td>
</form>
EOQ;
  return $form_html;  
}


function get_grades_row($defaults="")
{
  $t_data = "<tr>";
  $t_data .= 
td_button("/teachers/tests_admin/lock_unlock_db.php","Main");
  $t_data .=
    td_button("/teachers/grades/grades_search.php", "Grades Search");
  $t_data .=
    td_button("/teachers/grades/grades_makeup.php", "Make Up Grades");
  $t_data .= 
    td_button("/teachers/grades/test_copy_search.php","Hardcopy Search");
  if(!empty($defaults))
  {
    $t_data .= get_menu_defaults_form("Grades Search");
  }
  $t_data .= "</tr>";
  return $t_data;
}


function get_seats_row($update_inputs, $period)
{
  $t_data = "<tr>";
  $t_data .= 
td_button("/teachers/tests_admin/lock_unlock_db.php","Main");
  $t_data .=
  td_button("/teachers/seats/seating_edit.php", "Edit Seats");
  if (!empty($update_inputs))
    $t_data .= td_no_form("&nbsp");
  $t_data .= "</tr>";
  if (!empty($update_inputs))
  {
    $t_data .= "<tr>";
    $temp_str = <<<EOQ
<input type=hidden name=period value="$period">
<input type=hidden name=menu_submit value="Attendance">
<input class=green_button type=submit name=""
 value='Take attendance for "PERIOD $period"'>
EOQ;
  $t_data .= td_general_form($temp_str, 
                            "/teachers/attendance/attendance.php");
    $t_data .= td_general_form($update_inputs);
    $t_data .= get_menu_defaults_form("Seats");
    $t_data .= "</tr>";
  }
  return $t_data;
}


function get_tests_row($period="")
{
  $t_data = "<tr>";
  $t_data .= 
td_button("/teachers/tests_admin/lock_unlock_db.php","Main");
  $t_data .=
td_button("/teachers/tests_admin/lock_unlock_db.php","Lock or Unlock Tests");
  $t_data .=
td_button("/teachers/tests_admin/new_passwords.php","View Test Passwords");
  $t_data .=
td_button("/teachers/tests_admin/test_find.php","Set Test for a Period");
  $t_data.=
td_button("/teachers/tests_admin/test_monitor1.php", "Monitor a Test");
  $t_data .= "</tr><tr>";
  $t_data .=  
td_button("/teachers/tests_admin/test_find.php", "Edit a Test");
  $t_data .=  
td_button("/teachers/tests_admin/test_find.php", "Find a Test");
  $t_data .=  
td_button("/teachers/tests_admin/test_review1.php", "Review a Test");
  $t_data .=  
td_button("/teachers/tests_admin/test_create.php","Create a Test");
  if(empty($period))
    $t_data .= td_no_form("&nbsp");
  else
    $t_data .= get_menu_defaults_form("Passwords");
  $t_data .= "</tr>";
  return $t_data;
}


function get_main_row()
{
  $t_data = "<tr>";
  $t_data .=
  td_button("/teachers/tests_admin/lock_unlock_db.php","Tests");
  $t_data .=
  td_button("/teachers/attendance/attendance.php", "Attendance");
  $t_data .=
  td_button("/teachers/grades/grades_search.php", "Grades");
  $t_data .=
  td_button("/teachers/seats/seating_edit.php", "Seats");
  $t_data .=
  td_button("/teachers/students_info/students_edit.php", "Student");
  $t_data .= "</tr>";
  return $t_data;
}


function get_monitor_row($period="")
{
  $t_data = "<tr>";
  $t_data .= 
td_button("/teachers/tests_admin/lock_unlock_db.php","Main");
  $t_data .=
  td_button("/teachers/tests_admin/test_monitor1.php","Monitor a Test");
  $t_data .= 
td_button("/teachers/tests_admin/lock_unlock_db.php","Tests");
  $t_data .= "</tr>";
  $t_data .= "<tr>";
  $temp_str = <<<EOQ
<input type=hidden name=period value="$period">
<input type=hidden name=menu_submit value="Monitor">
<input class=green_button type=submit name="monitor_update"
 value='Update "PERIOD $period" students not finished'>
EOQ;
  $t_data .= td_general_form($temp_str);
  $temp_str = <<<EOQ
<input type=hidden name=period value="$period">
<input type=hidden name=menu_submit value="Attendance">
<input class=green_button type=submit name="attendance"
 value='Take attendance for "PERIOD $period"'>
EOQ;
  $t_data .= 
td_general_form($temp_str, "/teachers/attendance/attendance.php");
  if(empty($period))
    $t_data .= td_no_form("&nbsp");
  else
    $t_data .=
    get_menu_defaults_form("Monitor a Test");
  $t_data .= "</tr>";

  return $t_data;
}


function get_student_info_row()
{
  $t_data = "<tr>";
  $t_data .= 
td_button("/teachers/tests_admin/lock_unlock_db.php","Main");
  $t_data .=
td_button("/teachers/students_info/students_edit.php",
                                         "Edit Student Information");
  $t_data .= "</tr>";
  return $t_data;
}


function get_menu_array()
{
  $submit_arr["Take Attendance"] = "attendance";
  $submit_arr["Absent Search"] = "attendance";
  $submit_arr["Absent search"] = "attendance";
  $submit_arr["Absent"] = "attendance";
  $submit_arr["Undo attendance for this period"] = "attendance";
  $submit_arr["Grades Search"] = "grade";
  $submit_arr["Grades search"] = "grade";
  $submit_arr["Hardcopy Search"] = "grade";
  $submit_arr["Make Up Grades"] = "grade";
  $submit_arr["Make up search"] = "grade";
  $submit_arr["Passwords"] = "test";
  $submit_arr["Lock or Unlock Tests"] = "test";
  $submit_arr["View Test Passwords"] = "test";
  $submit_arr["Set Test for a Period"] = "test";
  $submit_arr["Monitor a Test"] = "test";
  $submit_arr["Edit a Test"] = "test";
  $submit_arr["Create a Test"] = "test";
  $submit_arr["Find a Test"] = "test";
  $submit_arr["Review a Test"] = "test";
  $submit_arr["Edit Student Information"] = "student";
  $submit_arr["Save Changes"] = "seat";
  $submit_arr["Edit Seats"] = "seat";
  $submit_arr["Monitor"] = "monitor";

  $submit_arr["Tests"] = "test";
  $submit_arr["Attendance"] = "attendance";
  $submit_arr["Grades"] = "grade";
  $submit_arr["Seats"] = "seat";
  $submit_arr["Student"] = "student";
  $submit_arr["Main"] = "main";
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
    $bottom = get_main_row();
  elseif($menu_r["$act"] == "attendance")
    $bottom = get_attendance_row($period, $defaults);
  elseif($menu_r["$act"] == "grade")
    $bottom = get_grades_row($defaults);
  elseif($menu_r["$act"] == "test")
    $bottom = get_tests_row($period);
  elseif($menu_r["$act"] == "seat")
    $bottom = get_seats_row($update_inputs, $period);
  elseif($menu_r["$act"] == "student")
    $bottom = get_student_info_row();
  elseif($menu_r["$act"] == "monitor")
    $bottom = get_monitor_row($period);

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


function special_chars($type)
{
  $source = "";
  if($type == "Spanish")
  {
    $source = <<<EOQ
<INPUT TYPE="button" VALUE="  á  "
onclick="addText('á')">  
<INPUT TYPE="button" VALUE="  é  "
onclick="addText('é')">  
<INPUT TYPE="button" VALUE="  í  "
onclick="addText('í')">  
<INPUT TYPE="button" VALUE="  ó  "
onclick="addText('ó')">  
<INPUT TYPE="button" VALUE="  ú  "
onclick="addText('ú')">
<INPUT TYPE="button" VALUE="  ü  " 
onclick="addText('ü')"> 
<INPUT TYPE="button" VALUE="  ñ  "
onclick="addText('ñ')">
<INPUT TYPE="button" VALUE="  Á  "
onclick="addText('Á')">
<INPUT TYPE="button" VALUE="  É  "
onclick="addText('É')">
<INPUT TYPE="button" VALUE="  Í  "
onclick="addText('Í')">
<INPUT TYPE="button" VALUE="  Ó  "
onclick="addText('Ó')">
<INPUT TYPE="button" VALUE="  Ú  "
onclick="addText('Ú')">
<INPUT TYPE="button" VALUE="  Ü  " 
onclick="addText('Ü')"> 
<INPUT TYPE="button" VALUE="  Ñ  "
onclick="addText('Ñ')">
<INPUT TYPE="button" VALUE="  ¡  "
onclick="addText('¡')">
<INPUT TYPE="button" VALUE="  ¿  "
onclick="addText('¿')">
EOQ;
  }
elseif($type == "German")
  {
    $source = <<<EOQ
<INPUT TYPE="button" VALUE="  ä  "
onclick="addText('ä')"> 
<INPUT TYPE="button" VALUE="  ö  "
onclick="addText('ö')"> 
<INPUT TYPE="button" VALUE="  ü  "
onclick="addText('ü')"> 
<INPUT TYPE="button" VALUE="  Ä  " 
onclick="addText('Ä')"> 
<INPUT TYPE="button" VALUE="  Ö  "
onclick="addText('Ö')"> 
<INPUT TYPE="button" VALUE="  Ü  "
onclick="addText('Ü')"> 
<INPUT TYPE="button" VALUE="  ß  "
onclick="addText('ß')">
<INPUT TYPE="button" VALUE="  €  "
onclick="addText('€')">
EOQ;
  }
elseif($type == "French")
  {
    $source = <<<EOQ
<INPUT TYPE="button" VALUE="  À  "
onclick="addText('À')"> 
<INPUT TYPE="button" VALUE="  à  "
onclick="addText('à')"> 
<INPUT TYPE="button" VALUE="  Á  "
onclick="addText('Á')"> 
<INPUT TYPE="button" VALUE="  á  "
onclick="addText('á')"> 
<INPUT TYPE="button" VALUE="  Â  "
onclick="addText('Â')"> 
<INPUT TYPE="button" VALUE="  â  "
onclick="addText('â')"> 
<INPUT TYPE="button" VALUE="  Ä  "
onclick="addText('Ä')"> 
<INPUT TYPE="button" VALUE="  ä  "
onclick="addText('ä')"> 
<INPUT TYPE="button" VALUE="  É  "
onclick="addText('É')">  
<INPUT TYPE="button" VALUE="  é  "
onclick="addText('é')">
<INPUT TYPE="button" VALUE="  È  "
onclick="addText('È')">
<INPUT TYPE="button" VALUE="  è  "
onclick="addText('è')">
<INPUT TYPE="button" VALUE="  Ê  "
onclick="addText('Ê')"> 
<INPUT TYPE="button" VALUE="  ê  "
onclick="addText('ê')"> 
<INPUT TYPE="button" VALUE="  Ë  "
onclick="addText('Ë')"> 
<INPUT TYPE="button" VALUE="  ë  "
onclick="addText('ë')"> 
<INPUT TYPE="button" VALUE="  Î  "
onclick="addText('Î')"> 
<INPUT TYPE="button" VALUE="  î  "
onclick="addText('î')"> 
<INPUT TYPE="button" VALUE="  Ï  "
onclick="addText('Ï')"> 
<INPUT TYPE="button" VALUE="  ï  "
onclick="addText('ï')">
<INPUT TYPE="button" VALUE="  Ô  "
onclick="addText('Ô')">
<INPUT TYPE="button" VALUE="  ô  "
onclick="addText('ô')">
<INPUT TYPE="button" VALUE="  Œ  "
onclick="addText('Œ')">
<INPUT TYPE="button" VALUE="  œ  "
onclick="addText('œ')">
<INPUT TYPE="button" VALUE="  Ò  "
onclick="addText('Ò')"> 
<INPUT TYPE="button" VALUE="  ò  "
onclick="addText('ò')">
<INPUT TYPE="button" VALUE="  Ô  "
onclick="addText('Ô')"> 
<INPUT TYPE="button" VALUE="  ô  "
onclick="addText('ô')"> 
<INPUT TYPE="button" VALUE="  Ö  "
onclick="addText('Ö')"> 
<INPUT TYPE="button" VALUE="  ö  "
onclick="addText('ö')">
<INPUT TYPE="button" VALUE="  Ÿ  "
onclick="addText('Ÿ')">
<INPUT TYPE="button" VALUE="  ÿ  "
onclick="addText('ÿ')"> 
<INPUT TYPE="button" VALUE="  Ç  "
onclick="addText('Ç')">
<INPUT TYPE="button" VALUE="  ç  "
onclick="addText('ç')">
<INPUT TYPE="button" VALUE="  €  "
onclick="addText('€')">
EOQ;
  }
elseif($type == "Italian")
  {
    $source = <<<EOQ
<INPUT TYPE="button" VALUE="  á  "
onclick="addText('á')">
<INPUT TYPE="button" VALUE="  à  "
onclick="addText('à')">
<INPUT TYPE="button" VALUE="  â  "
onclick="addText('â')"> 
<INPUT TYPE="button" VALUE="  ã  "
onclick="addText('ã')"> 
<INPUT TYPE="button" VALUE="  ä  "
onclick="addText('ä')"> 
<INPUT TYPE="button" VALUE="  å  "
onclick="addText('å')"> 
<INPUT TYPE="button" VALUE="  æ  "
onclick="addText('æ')"> 
<INPUT TYPE="button" VALUE="  À  "
onclick="addText('À')"> 
<INPUT TYPE="button" VALUE="  Á  "
onclick="addText('Á')"> 
<INPUT TYPE="button" VALUE="  Â  "
onclick="addText('Â')"> 
<INPUT TYPE="button" VALUE="  Ã  "
onclick="addText('Ã')"> 
<INPUT TYPE="button" VALUE="  Ä  "
onclick="addText('Ä')"> 
<INPUT TYPE="button" VALUE="  Å  "
onclick="addText('Å')"> 
<INPUT TYPE="button" VALUE="  Æ  "
onclick="addText('Æ')"> 
<INPUT TYPE="button" VALUE="  è  "
onclick="addText('è')"> 
<INPUT TYPE="button" VALUE="  é  "
onclick="addText('ê')"> 
<INPUT TYPE="button" VALUE="  é  "
onclick="addText('ê')"> 
<INPUT TYPE="button" VALUE="  ë  "
onclick="addText('ë')"> 
<INPUT TYPE="button" VALUE="  È  "
onclick="addText('È')"> 
<INPUT TYPE="button" VALUE="  É  "
onclick="addText('É')"> 
<INPUT TYPE="button" VALUE="  Ê  "
onclick="addText('Ê')"> 
<INPUT TYPE="button" VALUE="  ì  "
onclick="addText('ì')"> 
<INPUT TYPE="button" VALUE="  í  "
onclick="addText('í')"> 
<INPUT TYPE="button" VALUE="  î  "
onclick="addText('î')"> 
<INPUT TYPE="button" VALUE="  ï  "
onclick="addText('ï')"> 
<INPUT TYPE="button" VALUE="  Ì  "
onclick="addText('Ì')"> 
<INPUT TYPE="button" VALUE="  Í  "
onclick="addText('Í')"> 
<INPUT TYPE="button" VALUE="  Î  "
onclick="addText('Î')"> 
<INPUT TYPE="button" VALUE="  Ï  "
onclick="addText('Ï')"> 
<INPUT TYPE="button" VALUE="  ò  "
onclick="addText('ò')"> 
<INPUT TYPE="button" VALUE="  ó  "
onclick="addText('ó')"> 
<INPUT TYPE="button" VALUE="  ô  "
onclick="addText('ô')"> 
<INPUT TYPE="button" VALUE="  õ  "
onclick="addText('õ')"> 
<INPUT TYPE="button" VALUE="  ö  "
onclick="addText('ö')"> 
<INPUT TYPE="button" VALUE="  Ò  "
onclick="addText('Ò')"> 
<INPUT TYPE="button" VALUE="  Ó  "
onclick="addText('Ó')"> 
<INPUT TYPE="button" VALUE="  Ô  "
onclick="addText('Ô')"> 
<INPUT TYPE="button" VALUE="  Õ  "
onclick="addText('Õ')"> 
<INPUT TYPE="button" VALUE="  Ö  "
onclick="addText('Ö')"> 
<INPUT TYPE="button" VALUE="  ù  "
onclick="addText('ù')"> 
<INPUT TYPE="button" VALUE="  ú  "
onclick="addText('ú')"> 
<INPUT TYPE="button" VALUE="  û  "
onclick="addText('û')"> 
<INPUT TYPE="button" VALUE="  ü  "
onclick="addText('ü')"> 
<INPUT TYPE="button" VALUE="  Ù  "
onclick="addText('Ù')"> 
<INPUT TYPE="button" VALUE="  Ú  "
onclick="addText('Ú')"> 
<INPUT TYPE="button" VALUE="  Û  "
onclick="addText('Û')"> 
<INPUT TYPE="button" VALUE="  Ü  "
onclick="addText('Ü')"> 
<INPUT TYPE="button" VALUE="  ç  "
onclick="addText('ç')"> 
<INPUT TYPE="button" VALUE="  Ç  "
onclick="addText('Ç')"> 
<INPUT TYPE="button" VALUE="  ñ  "
onclick="addText('ñ')"> 
<INPUT TYPE="button" VALUE="  Ñ  "
onclick="addText('Ñ')"> 
<INPUT TYPE="button" VALUE="  ß  "
onclick="addText('ß')"> 
<INPUT TYPE="button" VALUE="  ý  "
onclick="addText('ý')"> 
<INPUT TYPE="button" VALUE="  Ý  "
onclick="addText('Ý')"> 
<INPUT TYPE="button" VALUE="  ¿  "
onclick="addText('¿')"> 
<INPUT TYPE="button" VALUE="  ¡  "
onclick="addText('¡')"> 
EOQ;
  }
  return $source;
}


function get_subjects_select()
{
  $type_arr['Art'] = 0;
  $type_arr['English'] = 0;
  $type_arr['Geography'] = 0;
  $type_arr['History'] = 0;
  $type_arr['Mathematics'] = 0;
  $type_arr['Science'] = 0;
  $type_arr['Social Studies'] = 0;
  $type_arr['French'] = 0;
  $type_arr['German'] = 0;
  $type_arr['Italian'] = 0;
  $type_arr['Spanish'] = 0;
  $select_html = "<SELECT name=type><OPTION SELECTED> </OPTION>";
  while (list($key,) = each($type_arr))
  {
    $select_html .= "<OPTION>" . $key . "</OPTION>";
  }
  $select_html .= "</SELECT>";
  return $select_html;
}


function escape_single_quotes(&$post_r)
{
  while (list ($key ,$value) = each($post_r))
  {
    $value = str_replace("'", "\'", $value);
    $post_r["$key"] = $value;
  }
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


function img_div()
{
    echo <<<EOQ
<DIV ID="div_id1" STYLE="position:absolute; top:0; left:0;">
<img id="img_id1" src='/teachers/blank.png' ALT='blank picture'>
<input id="id1" type="hidden">
</DIV>
EOQ;
}

?>

