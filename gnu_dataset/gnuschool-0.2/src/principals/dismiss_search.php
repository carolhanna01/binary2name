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
$lname=$_POST['lname'];
$hr_num=$_POST['hr_num'];
$all_hr=$_POST['all_hr'];
$menu_submit=$_POST['menu_submit'];


if (empty($fname)&& empty($lname)&& empty($hr_num)&& empty($all_hr))
   $yes_input = 0;  

include ('./search.php');


function get_main_info_r($fname, $lname)
{
  $info_r = array();
  $where_query = "";
  $and_flag = 0;
  if (!empty($fname))
  {  $where_query = "fname = '$fname'"; $and_flag=1;}
  if (!empty($lname)) if($and_flag)
  { $where_query .= " and lname = '$lname'";}
  else { $where_query = "lname = '$lname'";}

  if (!empty($where_query))
    $query = "select * from main.spanish where $where_query order by
              hr, lname";
  else
    $query = "select * from main.spanish order by hr, lname";

  $result = mysql_query($query);
  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    $stu_r=array();
    $stu_r['id'] = $row['id'];
    $stu_r['hr'] = $row['hr'];
    $stu_r['periods'] = $row['periods'];
   
    $stu_r['name']=$row['fname'] ." ". $row['mname'] ." ". $row['lname'];
    $info_r[]=$stu_r;
  }
  return $info_r;
}


function get_databases_r()
{
  $db_r = array();
  $query = "select db_name from main.teachers order by db_name";
  $result = mysql_query($query);
   
  while ($row = mysql_fetch_row($result))
  {  
    $db_r[] =  $row[0];
  }

  return $db_r;
}


function set_cut_r($db_name="", $where_query="", &$cut_r)
{
  if(!empty($db_name))
  {
    $query = "select * from $db_name.cut where $where_query";

    $result = mysql_query($query);
    while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
    {
      $id = $row['id'];
      $stu_r=array();
      $stu_r[] = $row['date'];
      $stu_r[] = $row['period'];
      $id = $row['id'];
      $cut_r["$id"][]= $stu_r;
    }
  }
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


function get_teacher_db_r ()
{
  $db_r = array();
  $query = "select db_name from main.teachers";
  $result = mysql_query($query);
  while ($row = mysql_fetch_row($result))
  {  
    $db_r[] = $row[0];
  }
  return $db_r;
}


function get_dates_arr ($db_name="", $date_clause="")
{
  $dates_arr = array();
  $query = "select distinct date from $db_name.attendance where
            $date_clause order by date";
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
  $query = "select date,time_in from $db_name.attendance where id=$id
            and $date_clause";
  $result = mysql_query($query);
  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    $date = $row['date'];
    $date_time_in["$date"] = $row['time_in'];
  }
  return $date_time_in;
}


function show_table_hr_title($hr)
{
  echo startTable("border");
  $cell_data = tDataSource("Homeroom $hr cuts", "center", "",2);
  echo rowPrint($cell_data);
}


$tag_title = "gnuschool.org Cut search";
start_html($tag_title);
stylesheet_link("./style_sheet");

start_body("main_background");

$date = getdate();
$time = "$date[hours]:$date[minutes]:$date[seconds]";

$page_title = "Cut search&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp
               &nbsp&nbsp&nbsp&nbsp(this page was opened at $time)";
if(empty($menu_submit))  $menu_submit = "Cut Search";
if($menu_submit == "Cut Search") $defaults = "yes";

$menu_r=array();
$menu_r['active_page']= $menu_submit;
$menu_r['defaults'] = $defaults;
main_menu($menu_r);

if (!$yes_input)
{
  $submit_val = "Cut search";
  $def_r = get_defaults_r($db_name);
  $comment = "Please complete the following information:";
  title_comment_div_noend($page_title, $comment);
  date_search_form($def_r, $submit_val);
  end_html_exit();
}


$page_title = "Cut records&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp
               &nbsp&nbsp&nbsp&nbsp(this page was opened at $time)";
title_comment_div_noend($page_title);


$comments = "";
$info_r = array();
if (!empty($fname) || !empty($lname))
{
  $info_r = get_main_info_r($fname, $lname);
  if (!sizeof($info_r))
    $comments .= " Student: \"$fname $lname\" does not exist.";
}
  
if(!empty($comments))
{
  title_comment_div_noend("", $comments);
  end_html_exit();
}
elseif( !sizeof($info_r) )
  $info_r = get_main_info_r("", "");

$search_r = glue_search_inputs();
$search_start = $search_r['search_start'];
$search_end = $search_r['search_end'];

$beginning = 1;

if(sizeof($info_r))
{
  $db_r = get_databases_r();
  $all_cuts_r = array();
  for ($i=0;$i<sizeof($db_r);$i++)
  {
    $current_db = $db_r[$i];
    $where_query = "date >= '$search_start' and  date <= '$search_end'";
    set_cut_r($current_db, $where_query, $all_cuts_r);
  }

  $current_hr = $info_r[0]['hr'];
  $stu_r = array();
  for ($i=0;$i<sizeof($info_r);$i++)
  {
    $stu_r = $info_r[$i];
    $name = $stu_r['name'];
    $id = $stu_r['id'];
    $new_hr = $stu_r['hr'];

    $id_cuts_r = array();
    $id_cuts_r = $all_cuts_r["$id"];
    $cuts_r = array();
    for ($j=0;$j<sizeof($id_cuts_r);$j++)
    {
      $date = $id_cuts_r[$j][0];
      $period = $id_cuts_r[$j][1];
      $cuts_r[] = "$date" . " Period $period";
    }

    if(sizeof($cuts_r))
    {
      if($current_hr != $new_hr)
      {
        echo endTable();
        echo "<br>";
        show_table_hr_title($new_hr);
        $current_hr = $new_hr;
      }
      elseif ($beginning)
      {
        show_table_hr_title($current_hr);
        $beginning = 0;
      }

      $cell_data = tDataSource($name, "left");
      $cell_data .= tDataSource($cuts_r[0], "right");          
      echo rowPrint($cell_data);
      for ($k=1;$k<sizeof($cuts_r);$k++)
      {
        $cell_data = tDataSource("&nbsp", "left");
        $cell_data .= tDataSource($cuts_r[$k], "right");          
        echo rowPrint($cell_data);
      }
    }
  }
}
if (!$beginning)      
{
  echo endTable();
  echo "<br>";
}

echo "</DIV>";
?>
</body></html>
   
