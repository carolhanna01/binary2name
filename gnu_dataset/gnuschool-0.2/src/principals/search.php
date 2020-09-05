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

$spanish_query = get_spanish_query($db_name);
if (empty($spanish_query)) $yes_input = 0;


function get_spanish_query($db_name)
{
  $fname=$_POST['fname'];
  $lname=$_POST['lname'];
  $hr_num=$_POST['hr_num'];
  $all_hr=$_POST['all_hr'];

  $spanish_query = "";

  if (empty($all_hr))
  {
    $where_query = "";
    $and_flag = 0;
    if (!empty($fname)) if($and_flag)
    { $where_query .= " and fname = '$fname'";}
    else { $where_query .= " fname = '$fname'"; $and_flag=1;}

    if (!empty($lname)) if($and_flag)
    { $where_query .= " and lname = '$lname'";}
    else { $where_query .= " lname = '$lname'"; $and_flag=1;}

    if (!empty($hr_num)) if($and_flag)
    { $where_query .= " and hr = '$hr_num'";}
    else { $where_query .= " hr = '$hr_num'"; $and_flag=1;}
 
    if (!empty($where_query))
      $spanish_query = "select * from $db_name.spanish where $where_query  
                        order by hr,lname";
  }
  else
    $spanish_query = "select * from $db_name.spanish order by hr,lname";

  return $spanish_query;
}


function glue_search_inputs()
{
  $search_start = "y";
  $str_y = $_POST['search_start_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $search_start="";
  $str_m = $_POST['search_start_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $search_start ="";
  $str_d = $_POST['search_start_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $search_start ="";
  if ($search_start == "y")
    $search_start="$str_y"."-"."$str_m"."-"."$str_d";

  $search_end = "y";
  $str_y = $_POST['search_end_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $search_end = "";
  $str_m = $_POST['search_end_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $search_end = "";
  $str_d = $_POST['search_end_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $search_end = "";
  if ($search_end == "y")
    $search_end="$str_y"."-"."$str_m"."-"."$str_d";    

  $date = getdate();
  $today_date = "$date[year]-$date[mon]-$date[mday]";
  if (empty($search_start)) $search_start = $today_date; 
  if (empty($search_end)) $search_end = $today_date; 

  $search = array();
  $search['search_start'] = $search_start;
  $search['search_end'] = $search_end;

  return $search;
}


function get_date_query($search_r)
{
  $search_start = $search_r['search_start'];
  $search_end = $search_r['search_end'];

  $date_query=" (date >= '$search_start' and date <= '$search_end')";

  return $date_query;
}


function date_search_form($def, $submit_val)
{
  $date = getdate();
  $today_date = "$date[year]-$date[mon]-$date[mday]";
  $default_start = $def['search_start'];
  $default_end = $def['search_end'];
  $q1_start = $def['q1_start'];
  $q2_start = $def['q2_start'];
  $q3_start = $def['q3_start'];
  $q4_start = $def['q4_start'];
  $q1_end = $def['q1_end'];
  $q2_end = $def['q2_end'];
  $q3_end = $def['q3_end'];
  $q4_end = $def['q4_end'];


  $radios1 =<<<EOQ
<input type=radio name="search_start" value="$today_date" checked onclick="set_s_values(this)">
Today's date
<input type=radio name="search_start" value="$default_start" onclick="set_s_values(this)">
Default start date<br>
<input type=radio name="search_start" value="$q1_start" onclick="set_s_values(this)">
Start of 1st quarter
<input type=radio name="search_start" value="$q2_start" onclick="set_s_values(this)">
Start of 2nd quarter
<input type=radio name="search_start" value="$q3_start" onclick="set_s_values(this)">
Start of 3rd quarter
<input type=radio name="search_start" value="$q4_start" onclick="set_s_values(this)">
Start of 4th quarter <br>
<input type=radio name="search_start" value="$q1_end" onclick="set_s_values(this)">
End of 1st quarter
<input type=radio name="search_start" value="$q2_end" onclick="set_s_values(this)">
End of 2nd quarter
<input type=radio name="search_start" value="$q3_end" onclick="set_s_values(this)">
End of 3rd quarter
<input type=radio name="search_start" value="$q4_end" onclick="set_s_values(this)">
End of 4th quarter
EOQ;

  $radios2 =<<<EOQ
<input type=radio name="search_end" value="$today_date" checked onclick="set_e_values(this)">
Today's date
<input type=radio name="search_end" value="$default_end" onclick="set_e_values(this)">
Default end date<br>
<input type=radio name="search_end" value="$q1_start" onclick="set_e_values(this)">
Start of 1st quarter
<input type=radio name="search_end" value="$q2_start" onclick="set_e_values(this)">
Start of 2nd quarter
<input type=radio name="search_end" value="$q3_start" onclick="set_e_values(this)">
Start of 3rd quarter
<input type=radio name="search_end" value="$q4_start" onclick="set_e_values(this)">
Start of 4th quarter <br>
<input type=radio name="search_end" value="$q1_end" onclick="set_e_values(this)">
End of 1st quarter
<input type=radio name="search_end" value="$q2_end" onclick="set_e_values(this)">
End of 2nd quarter
<input type=radio name="search_end" value="$q3_end" onclick="set_e_values(this)">
End of 3rd quarter
<input type=radio name="search_end" value="$q4_end" onclick="set_e_values(this)">
End of 4th quarter
EOQ;

  echo <<<EOQ
<form method=post action="$PHP_SELF">
<b>Start date:</b><br>

<b>Month</b> (2 digits):
<input id="s2" type=text name="search_start_m" 
value="$date[mon]" size=3 maxlength=2>
&nbsp&nbsp&nbsp<b>Day</b> (2 digits):
<input id="s3" type=text name="search_start_d"
value="$date[mday]" size=3 maxlength=2>
&nbsp&nbsp&nbsp<b>Year</b> (4 digits): 
<input id="s1" type=text name="search_start_y"
value="$date[year]" size=6 maxlength=4>
<br>
$radios1
<br><br>
<b>End date:</b><br>
<b>Month</b> (2 digits):
<input id="e2" type=text name="search_end_m" 
value="$date[mon]" size=3 maxlength=2>
&nbsp&nbsp&nbsp<b>Day</b> (2 digits):
<input id="e3" type=text name="search_end_d" 
value="$date[mday]" size=3 maxlength=2>
&nbsp&nbsp&nbsp<b>Year</b> (4 digits): 
<input id="e1" type=text name="search_end_y" 
value="$date[year]" size=6 maxlength=4>
<br>
$radios2
<br><br>
<b>Homeroom number:</b><br>
<input type=text name=hr_num size=4>&nbsp &nbsp
<input type=checkbox name=all_hr value="absents"><b>All homerooms and 
all
students:</b>
<br><br>

<b>Student first name:</b><br>
<input type=text name=fname size=30>
<br><br>

<b>Student last name:</b><br>
<input type=text name=lname size=30>
<br><br>

<input type=submit name=menu_submit value="$submit_val">
</form>
EOQ;
}


function chars_are_nums($str = "")
{
  $str = trim($str);
  $char_r = array();
  $char_r['y0'] = "y";
  $char_r['y1'] = "y";
  $char_r['y2'] = "y";
  $char_r['y3'] = "y";
  $char_r['y4'] = "y";
  $char_r['y5'] = "y";
  $char_r['y6'] = "y";
  $char_r['y7'] = "y";
  $char_r['y8'] = "y";
  $char_r['y9'] = "y";
  $length = strlen($str);
  $yes_nums = 1;
  for ($i=0; $i<$length; $i++)
  { 
    $char = substr($str, $i, 1);
    $key = "y" . "$char";
    $result = $char_r["$key"];
    if ( $result != "y" ) $yes_nums = 0; 
  }
  return $yes_nums;
}

?>
