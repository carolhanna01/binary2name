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
$menu_submit=$_POST['menu_submit'];

if (empty($period))
  $yes_input = 0;


function get_students_arr( $db_name="", $period="", $defaults_r)
{
  $def_fname = $defaults_r['fname'];
  $def_mname = $defaults_r['mname'];
  $def_lname = $defaults_r['lname'];
  $def_name_lines = $defaults_r['name_lines'];
  $students_arr = array();
  $query = "select * from $db_name.spanish where period = '$period'";
  $result = mysql_query($query);
//  query_outcome_echo($query,$result);

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


function get_seating_data( $db_name="", $id=0, $period="" )
{
  $id_arr = array();
  $query = "select * from $db_name.seating where id = $id and
            period='$period'";
  $result = mysql_query($query);
//  query_outcome_echo($query,$result);
  $row = mysql_fetch_object($result);
  return $row;
}


function get_test_id($db_name, $period="")
{
  $period = "period" . $period;
  $query = "select $period from $db_name.today";
  $result = mysql_query($query);
//  query_outcome_echo($query,$result);
  $row = mysql_fetch_row($result);
  $test_id = $row[0];
  return $test_id;
}


function array_to_html($db_name="", $students_arr=array(), $period="")
{
  $dateTime = getdate();
  $date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";
  $test_id = get_test_id($db_name,$period);
  $source = "";
  while (list ($id , $name) = each($students_arr))
  {
    $status = is_done_with_test($db_name,$id,$period,$test_id,$date);
    if ($status == "not done")
      $class_str = "red_button";
    elseif ($status == "done")
      $class_str = "green2_button";
    elseif ($status == "absent")
      $class_str = "gray_button";
    else
    {
      $page_title = "Monitor a test";
      $comment = "You need to take attendance before monitoring a test.";
      title_comment_div_noend($page_title, $comment);
      return 0;
    }
    $seating_arr = get_seating_data( $db_name, $id, $period);
    $seat_x = $seating_arr->seat_x;
    $seat_y = $seating_arr->seat_y;

    $info_form = get_info_form($id, $test_id, $name, $period,
$class_str);
echo <<<EOQ
<DIV STYLE="position:absolute; top:$seat_y; left:$seat_x;">
$info_form
</DIV>
EOQ;
  }
  return 1;
}


function get_info_form ($id=0, $test_id=0, $name, $period, $class_str)
{
  $page = "/teachers/tests_admin/test_monitor3.php";
  $window_description =
"width=600,height=400,scrollbars=yes,resizable=yes,status=yes";

  $form_html = <<<EOQ
<form method=post target="newwindow" action=test_monitor3.php> 
<input type=hidden name=id value="$id">
<input type=hidden name=test_id value="$test_id">
<input type=hidden name=period value="$period">
<input type=submit name=name value='$name' class=$class_str
onClick="window.open('$page','newwindow','$window_description')">
</form>
EOQ;
  return $form_html;  
}


function is_done_with_test($db_name,$id,$period,$test_id,$date)
{
  $query = "select time_in from $db_name.attendance where date='$date'
            and id=$id and period='$period'";
  $result = mysql_query($query);
//  query_outcome_echo($query,$result);
  if (mysql_num_rows($result) == 0) return "zero";
  else $row = mysql_fetch_row($result);
  $time_in = $row[0];
  if ($time_in == "00:00:00") return "absent";
  else
  {
    $query = "select * from $db_name.scores where id=$id and
            date='$date' and test_id=$test_id and period='$period'";
    $result = mysql_query($query);
//  query_outcome_echo($query,$result);
    if (mysql_num_rows($result) == 0)
      return "not done";
    else
      return "done";
  }
}


$tag_title = "gnuschool.org Take attendance";


start_html($tag_title);
stylesheet_link("../style_sheet");

if ($yes_input)
{
/*
  echo <<<EOQ
<SCRIPT LANGUAGE="JavaScript">

var brOK = false, mie = false; 
if (document.layers || document.all) brOK = true; 
if (document.all) mie = true; 


function doneConfirm() {
var ok=confirm("checkbox is checked")
if (ok) return true; else return false;
}

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


function set_bgcolor(my_checkbox, ids_str, number_of_ids)
{
  id_arr = ids_str.split("_");
  if (my_checkbox.checked)
  {
    if (document.getElementById)
    {
      for( i = 0; i < number_of_ids; i++)
      {
        id = id_arr[i];
        document.getElementById(id).style.backgroundColor = 'red';
      }
    }
    else if (document.layers)
    {
      for( i = 0; i < number_of_ids; i++)
      {
        id = id_arr[i];
        document[id].bgColor = 'red';
      }
    }
    else if (document.all)
    {
      for( i = 0; i < number_of_ids; i++)
      {
        id = id_arr[i];
        document.all[id].style.backgroundColor = 'red';
      }
    }
  }
  else
  {
    if (document.getElementById)
    {
      for( i = 0; i < number_of_ids; i++)
      {
        id = id_arr[i];
        document.getElementById(id).style.backgroundColor = 'white';
      }
    }
    else if (document.layers)
    {
      for( i = 0; i < number_of_ids; i++)
      {
        id = id_arr[i];
        document[id].bgColor = 'white';
      }
    }
    else if (document.all)
    {
      for( i = 0; i < number_of_ids; i++)
      {
        id = id_arr[i];
        document.all[id].style.backgroundColor = 'white';
      }
    }
  }
}


function set_action(action, id)
{
 if (!mie)
   document[id].action = action;
 else
  document.getElementById(id).action = action;
}
</SCRIPT>
EOQ;
*/
}

start_body("main_background");

if(empty($menu_submit)) $menu_submit = "Tests";

$menu_r=array();
$menu_r['active_page']= $menu_submit;
$menu_r['period'] = "$period";
main_menu($menu_r);

if (!$yes_input)
{
  $page_title = "Monitor a test";
  $comment = "Please access this page using the site menu.";
  title_comment_div_noend($page_title, $comment);
  end_html_exit();
}

$def_r = get_defaults_r($db_name);
$students_arr = get_students_arr( $db_name, $period, $def_r);  

array_to_html($db_name, $students_arr, $period);
// img_div();

?>
</body></html>

