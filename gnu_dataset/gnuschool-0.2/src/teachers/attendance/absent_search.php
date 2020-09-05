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
include ('../search.php');

$menu_submit=$_POST['menu_submit'];


if ($period == "hr" || $period == "Hr" || $period == "hR") $period = "HR";


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
  if(mysql_num_rows($result) == 0) return 0;

  $period_idName_num=array();
  $id_name_arr=array();

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $period = $row['period'];

  $name = $row['fname'] . " " . $row['mname'] . " " . $row['lname'];
  $id_name_arr['name'] = $name;
  $id_name_arr['id'] = $row['id'];
  $period_idName_num[]=$id_name_arr;

  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  { 
    $next_period = $row['period'];
    $name = $row['fname'] . " " . $row['mname'] . " " . $row['lname'];
    $id_name_arr['name'] = $name;
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
//show_ids_arr($periods_idsNames_ass);
  return $periods_idsNames_ass;
}


function get_dates_arr ($db_name="", $date_clause="", $period="")
{
 $dates_arr = array();
 $query=""; 

 if($period == "HR")
   $query = "select distinct date from $db_name.hr_attendance where
             $date_clause order by date";
 else
   $query = "select distinct date from $db_name.attendance where
             $date_clause and period='$period' order by date";

 $result = mysql_query($query);
 while ($row = mysql_fetch_row($result))
 {  
   $dates_arr[] = $row[0];
 }
 return $dates_arr;
}


function get_attendance_arr($db_name="",$id=0,$date_clause="",$period="")
{
  $date_time_in = array();
  $query="";

  if($period == "HR")
    $query = "select date,time_in from $db_name.hr_attendance
              where id=$id and $date_clause";
  else
    $query = "select date,time_in from $db_name.attendance where id=$id
            and $date_clause and period='$period'";

  $result = mysql_query($query);
  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    $date = $row['date'];
    $date_time_in["$date"] = $row['time_in'];
  }
  return $date_time_in;
}


function show_table_column_titles($title_arr=array(), $period)
{
  echo startTable("border");
  $cell_data = tDataSource("Period $period attendance", "center", "",
                            sizeof($title_arr) +1);
  echo rowPrint($cell_data);
  $cell_data = tDataSource("&nbsp");

  for ($i=0; $i<sizeof($title_arr); $i++)
  {
    $date = $title_arr[$i];
    $cell_data .= tDataSource($date, "right");
  }
  echo rowPrint($cell_data);
}


function show_periods_attendance_table
                           ($db_name="",$spanish_query="",$date_clause="")
{
  $period_idsNames_ass = get_ids_arr ($spanish_query);
  if(!is_array($period_idsNames_ass)) 
    {print "No matches were found"; return 0;}

  while (list($period,$ids_names_num) = each($period_idsNames_ass))
  {
    $dates_arr = get_dates_arr ($db_name, $date_clause, $period);
    show_table_column_titles($dates_arr, $period);
    while (list(,$id_name_arr) = each($ids_names_num))
    {
      $name = $id_name_arr['name'];
      $id = $id_name_arr['id'];
      $cell_data = tDataSource($name, "left");

      $atten_in_ass=get_attendance_arr($db_name,$id,$date_clause,$period);
      for ($i=0; $i<sizeof($dates_arr); $i++)
      {
        $date = $dates_arr[$i];
        $time_in = $atten_in_ass["$date"];
        if (!empty($time_in))
          if ($time_in == "00:00:00")
            $cell_data .= tDataSource("absent", "right");
          else
            $cell_data .= tDataSource("in", "right");
        else
          $cell_data .= tDataSource("&nbsp", "right");
      }
      echo rowPrint($cell_data);
    }
    echo endTable();
    echo "<br>";
  }
}


$tag_title = "gnuschool.org Absent search";
$page_title = "Absent search";

start_html($tag_title);
stylesheet_link("../style_sheet");

echo <<<EOQ

<SCRIPT LANGUAGE="JavaScript">

var brOK = false, mie = false; 

if (document.layers || document.all) brOK = true; 
if (document.all) mie = true; 


function set_s_values(radio) {
  radio_val = radio.value;
  date_arr = radio_val.split("-");

  if (!mie)
  {
    if ( radio.checked == true)
    {
      document['s1'].value = date_arr[0];
      document['s2'].value = date_arr[1];
      document['s3'].value = date_arr[2];
    }
  }
  else
  {
    if ( radio.checked == true)
    {
      document.getElementById('s1').value = date_arr[0];
      document.getElementById('s2').value = date_arr[1];
      document.getElementById('s3').value = date_arr[2];
    }
  }
}

function set_e_values(radio) {
  radio_val = radio.value;
  date_arr = radio_val.split("-");

  if (!mie)
  {
    if ( radio.checked == true)
    {
      document['e1'].value = date_arr[0];
      document['e2'].value = date_arr[1];
      document['e3'].value = date_arr[2];
    }
  }
  else
  {
    if ( radio.checked == true)
    {
      document.getElementById('e1').value = date_arr[0];
      document.getElementById('e2').value = date_arr[1];
      document.getElementById('e3').value = date_arr[2];
    }
  }
}
</SCRIPT>
EOQ;

start_body("main_background");

if(empty($menu_submit))  $menu_submit = "Absent Search";
if($menu_submit == "Absent Search") $defaults = "yes";

$menu_r=array();
$menu_r['active_page']= $menu_submit;
$menu_r['defaults'] = $defaults;
main_menu($menu_r);

if (!$yes_input)
{
  $submit_val = "Absent search";
  $def_r = get_defaults_r($db_name);
  $comment = "Please complete the following information:";
  title_comment_div_noend($page_title, $comment);
  date_search_form($def_r, $submit_val);
  end_html_exit();
}



$page_title = "Attendance records";
title_comment_div_noend($page_title);

$search_r = glue_search_inputs($_POST);
$date_query = get_date_query($search_r);
show_periods_attendance_table ($db_name, $spanish_query, $date_query);

echo "</DIV>";
?>

</body></html>
