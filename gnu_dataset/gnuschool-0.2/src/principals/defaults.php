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

$menu_submit=$_POST['menu_submit'];
$update_search=$_POST['update_search'];


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


function get_new_defaults_r($post_r, $def)
{
  $q1_start = $post_r['q1_start'];
  $q1_end = $post_r['q1_end'];
  $q2_start = $post_r['q2_start'];
  $q2_end = $post_r['q2_end'];
  $q3_start = $post_r['q3_start'];
  $q3_end = $post_r['q3_end'];
  $q4_start = $post_r['q4_start'];
  $q4_end = $post_r['q4_end'];
  $search_start = $post_r['search_start'];
  $search_end = $post_r['search_end'];
  $time_in = $post_r['time_in'];

  if (!empty($q1_start)) $def['q1_start'] = $q1_start;
  if (!empty($q1_end)) $def['q1_end'] = $q1_end;
  if (!empty($q2_start)) $def['q2_start'] = $q2_start;
  if (!empty($q2_end)) $def['q2_end'] = $q2_end;
  if (!empty($q3_start)) $def['q3_start'] = $q3_start;
  if (!empty($q3_end)) $def['q3_end'] = $q3_end;
  if (!empty($q4_start)) $def['q4_start'] = $q4_start;
  if (!empty($q4_end)) $def['q4_end'] = $q4_end;
  if (!empty($search_start)) $def['search_start'] = $search_start;
  if (!empty($search_end)) $def['search_end'] = $search_end;
  if (!empty($time_in)) $def['time_in'] = $time_in;

  return $def;
}


function set_defaults($db_name,$def)
{
  $q1_start = $def['q1_start'];
  $q1_end = $def['q1_end'];
  $q2_start = $def['q2_start'];
  $q2_end = $def['q2_end'];
  $q3_start = $def['q3_start'];
  $q3_end = $def['q3_end'];
  $q4_start = $def['q4_start'];
  $q4_end = $def['q4_end'];
  $search_start = $def['search_start'];
  $search_end = $def['search_end'];
  $time_in = $def['time_in'];

  $query =<<<EOQ
update $db_name.defaults set
q1_start = '$q1_start',
q1_end = '$q1_end',
q2_start = '$q2_start',
q2_end = '$q2_end',
q3_start = '$q3_start',
q3_end = '$q3_end',
q4_start = '$q4_start',
q4_end = '$q4_end',
search_start = '$search_start',
search_end = '$search_end',
time_in = '$time_in'
EOQ;
  mysql_query($query);
}


function glue_search_inputs()
{
  $time_in = "y";
  $str_y = $_POST['time_in_h'];
  if ((strlen($str_y) > 2) || !chars_are_nums($str_y)) $time_in="";
  $str_m = $_POST['time_in_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $time_in="";
  if ($time_in == "y")
    $time_in="$str_y".":"."$str_m".":"."00";

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

  $q1_start = "y";
  $str_y = $_POST['q1_start_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $q1_start = "";
  $str_m = $_POST['q1_start_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $q1_start = "";
  $str_d = $_POST['q1_start_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $q1_start = "";
  if ($q1_start == "y") $q1_start="$str_y"."-"."$str_m"."-"."$str_d";

  $q1_end = "y";
  $str_y = $_POST['q1_end_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $q1_end = "";
  $str_m = $_POST['q1_end_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $q1_end = "";
  $str_d = $_POST['q1_end_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $q1_end = "";
  if ($q1_end == "y") $q1_end="$str_y"."-"."$str_m"."-"."$str_d";

  $q2_start = "y";
  $str_y = $_POST['q2_start_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $q2_start = "";
  $str_m = $_POST['q2_start_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $q2_start = "";
  $str_d = $_POST['q2_start_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $q2_start = "";
  if ($q2_start == "y") $q2_start="$str_y"."-"."$str_m"."-"."$str_d";

  $q2_end = "y";
  $str_y = $_POST['q2_end_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $q2_end = "";
  $str_m = $_POST['q2_end_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $q2_end = "";
  $str_d = $_POST['q2_end_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $q2_end = "";
  if ($q2_end == "y") $q2_end="$str_y"."-"."$str_m"."-"."$str_d";

  $q3_start = "y";
  $str_y = $_POST['q3_start_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $q3_start = "";
  $str_m = $_POST['q3_start_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $q3_start = "";
  $str_d = $_POST['q3_start_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $q3_start = "";
  if ($q3_start == "y") $q3_start="$str_y"."-"."$str_m"."-"."$str_d";

  $q3_end = "y";
  $str_y = $_POST['q3_end_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $q3_end = "";
  $str_m = $_POST['q3_end_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $q3_end = "";
  $str_d = $_POST['q3_end_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $q3_end = "";
  if ($q3_end == "y") $q3_end="$str_y"."-"."$str_m"."-"."$str_d";

  $q4_start = "y";
  $str_y = $_POST['q4_start_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $q4_start = "";
  $str_m = $_POST['q4_start_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $q4_start = "";
  $str_d = $_POST['q4_start_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $q4_start = "";
  if ($q4_start == "y") $q4_start="$str_y"."-"."$str_m"."-"."$str_d";

  $q4_end = "y";
  $str_y = $_POST['q4_end_y'];
  if ((strlen($str_y) > 4) || !chars_are_nums($str_y)) $q4_end = "";
  $str_m = $_POST['q4_end_m'];
  if ((strlen($str_m) > 2) || !chars_are_nums($str_m)) $q4_end = "";
  $str_d = $_POST['q4_end_d'];
  if ((strlen($str_d) > 2) || !chars_are_nums($str_d)) $q4_end = "";
  if ($q4_end == "y") $q4_end="$str_y"."-"."$str_m"."-"."$str_d";

  $search = array();
  $search['q1_start'] = $q1_start;
  $search['q1_end'] = $q1_end;
  $search['q2_start'] = $q2_start;
  $search['q2_end'] = $q2_end;
  $search['q3_start'] = $q3_start;
  $search['q3_end'] = $q3_end;
  $search['q4_start'] = $q4_start;
  $search['q4_end'] = $q4_end;
  $search['search_start'] = $search_start;
  $search['search_end'] = $search_end;
  $search['time_in'] = $time_in;

  return $search;
}


function make_search_inputs($def)
{
  $search = array();
  $str = $def['q1_start'];
  $arr = explode("-", $str);
  $search['q1_start_y'] = $arr[0];
  $search['q1_start_m'] = $arr[1];
  $search['q1_start_d'] = $arr[2];
  $str = $def['q1_end'];
  $arr = explode("-", $str);
  $search['q1_end_y'] = $arr[0];
  $search['q1_end_m'] = $arr[1];
  $search['q1_end_d'] = $arr[2];

  $str = $def['q2_start'];
  $arr = explode("-", $str);
  $search['q2_start_y'] = $arr[0];
  $search['q2_start_m'] = $arr[1];
  $search['q2_start_d'] = $arr[2];
  $str = $def['q2_end'];
  $arr = explode("-", $str);
  $search['q2_end_y'] = $arr[0];
  $search['q2_end_m'] = $arr[1];
  $search['q2_end_d'] = $arr[2];

  $str = $def['q3_start'];
  $arr = explode("-", $str);
  $search['q3_start_y'] = $arr[0];
  $search['q3_start_m'] = $arr[1];
  $search['q3_start_d'] = $arr[2];
  $str = $def['q3_end'];
  $arr = explode("-", $str);
  $search['q3_end_y'] = $arr[0];
  $search['q3_end_m'] = $arr[1];
  $search['q3_end_d'] = $arr[2];

  $str = $def['q4_start'];
  $arr = explode("-", $str);
  $search['q4_start_y'] = $arr[0];
  $search['q4_start_m'] = $arr[1];
  $search['q4_start_d'] = $arr[2];
  $str = $def['q4_end'];
  $arr = explode("-", $str);
  $search['q4_end_y'] = $arr[0];
  $search['q4_end_m'] = $arr[1];
  $search['q4_end_d'] = $arr[2];

  $str = $def['search_start'];
  $arr = explode("-", $str);
  $search['search_start_y'] = $arr[0];
  $search['search_start_m'] = $arr[1];
  $search['search_start_d'] = $arr[2];
  $str = $def['search_end'];
  $arr = explode("-", $str);
  $search['search_end_y'] = $arr[0];
  $search['search_end_m'] = $arr[1];
  $search['search_end_d'] = $arr[2];

  $str = $def['time_in'];
  $arr = explode(":", $str);
  $search['time_in_h'] = $arr[0];
  $search['time_in_m'] = $arr[1];

  return $search;  
}


function get_search_form($search, $menu_submit)
{
  $q1_start_y = $search['q1_start_y'];
  $q1_start_m = $search['q1_start_m'];
  $q1_start_d = $search['q1_start_d'];
  $q1_end_y = $search['q1_end_y'];
  $q1_end_m = $search['q1_end_m'];
  $q1_end_d = $search['q1_end_d'];
  $q2_start_y = $search['q2_start_y'];
  $q2_start_m = $search['q2_start_m'];
  $q2_start_d = $search['q2_start_d'];
  $q2_end_y = $search['q2_end_y'];
  $q2_end_m = $search['q2_end_m'];
  $q2_end_d = $search['q2_end_d'];
  $q3_start_y = $search['q3_start_y'];
  $q3_start_m = $search['q3_start_m'];
  $q3_start_d = $search['q3_start_d'];
  $q3_end_y = $search['q3_end_y'];
  $q3_end_m = $search['q3_end_m'];
  $q3_end_d = $search['q3_end_d'];
  $q4_start_y = $search['q4_start_y'];
  $q4_start_m = $search['q4_start_m'];
  $q4_start_d = $search['q4_start_d'];
  $q4_end_y = $search['q4_end_y'];
  $q4_end_m = $search['q4_end_m'];
  $q4_end_d = $search['q4_end_d'];
  $search_start_y = $search['search_start_y'];
  $search_start_m = $search['search_start_m'];
  $search_start_d = $search['search_start_d'];
  $search_end_y = $search['search_end_y'];
  $search_end_m = $search['search_end_m'];
  $search_end_d = $search['search_end_d'];
  $time_in_h = $search['time_in_h'];
  $time_in_m = $search['time_in_m'];

  $submit = <<<EOQ
<b>In Homeroom by:</b>&nbsp&nbspHour (2 digits):
<input type=text name="time_in_h"
value="$time_in_h" size=3 maxlength=2>
&nbsp&nbsp&nbspMinutes (2 digits):
<input type=text name="time_in_m"
value="$time_in_m" size=3 maxlength=2>
<br><br>
<b>Start search using:</b><br>
Month (2 digits):
<input type=text name="search_start_m"
value="$search_start_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="search_start_d"
value="$search_start_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="search_start_y"
value="$search_start_y" size=6 maxlength=4>
<br>
<b>End search using:</b><br>
Month (2 digits):
<input type=text name="search_end_m" 
value="$search_end_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="search_end_d" 
value="$search_end_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="search_end_y" 
value="$search_end_y" size=6 maxlength=4>
<br><br>

<b>First quarter start:</b><br>
Month (2 digits):
<input type=text name="q1_start_m"
value="$q1_start_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="q1_start_d"
value="$q1_start_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="q1_start_y" 
value="$q1_start_y" size=6 maxlength=4>
<br>
<b>First quarter end:</b><br>
Month (2 digits):
<input type=text name="q1_end_m"
value="$q1_end_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="q1_end_d"
value="$q1_end_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="q1_end_y"
value="$q1_end_y" size=6 maxlength=4>
<br><br>

<b>Second quarter start:</b><br>
Month (2 digits):
<input type=text name="q2_start_m"
value="$q2_start_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="q2_start_d"
value="$q2_start_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="q2_start_y" 
value="$q2_start_y" size=6 maxlength=4>
<br>
<b>Second quarter end:</b><br>
Month (2 digits):
<input type=text name="q2_end_m"
value="$q2_end_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="q2_end_d"
value="$q2_end_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="q2_end_y"
value="$q2_end_y" size=6 maxlength=4>
<br><br>
<b>Third quarter start:</b><br>
Month (2 digits):
<input type=text name="q3_start_m"
value="$q3_start_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="q3_start_d"
value="$q3_start_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="q3_start_y" 
value="$q3_start_y" size=6 maxlength=4>
<br>
<b>Third quarter end:</b><br>
Month (2 digits):
<input type=text name="q3_end_m"
value="$q3_end_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="q3_end_d"
value="$q3_end_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="q3_end_y"
value="$q3_end_y" size=6 maxlength=4>
<br><br>

<b>Fourth quarter start:</b><br>
Month (2 digits):
<input type=text name="q4_start_m"
value="$q4_start_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="q4_start_d"
value="$q4_start_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="q4_start_y" 
value="$q4_start_y" size=6 maxlength=4>
<br>
<b>Fourth quarter end:</b><br>
Month (2 digits):
<input type=text name="q4_end_m"
value="$q4_end_m" size=3 maxlength=2>
&nbsp&nbsp&nbspDay (2 digits):
<input type=text name="q4_end_d"
value="$q4_end_d" size=3 maxlength=2>
&nbsp&nbsp&nbspYear (4 digits): 
<input type=text name="q4_end_y"
value="$q4_end_y" size=6 maxlength=4>
<br><br>
<input type=hidden name=menu_submit value="$menu_submit">
<input type=submit name=update_search value="Update defaults">
EOQ;
  return $submit;
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
  $t_start =<<<EOQ
<TABLE $border $cellspacing $cellpadding width="100%">
EOQ;
  return $t_start;
}


function endTable()
{
  return "</TABLE>";
}


function show_form_table ($defaults_form)
{
  echo startTable("border");
  echo <<<EOQ
<form method=post action="$PHP_SELF">
EOQ;
  $cell_data = tDataSource("$defaults_form", "left");
  echo rowPrint($cell_data);
  echo '</form>';
  echo endTable();
}


$tag_title = "gnuschool.org Defaults";
start_html($tag_title);
stylesheet_link("./style_sheet");

start_body("main_background");
$page_title = "Defaults";
$comment = "Please access this page using the site menu.";

if($yes_input)
{
  title_comment_div_noend($page_title);

  $defaults_r = get_defaults_r($db_name);
  $new_defaults_r = array();
  if(!empty($update_search))
  {
    $search_r = glue_search_inputs();
    $new_defaults_r = get_new_defaults_r($search_r, $defaults_r);
  }
  else
    $new_defaults_r = get_new_defaults_r($_POST, $defaults_r);

  if(!empty($update_search))
    set_defaults($db_name, $new_defaults_r);

  if ($menu_submit == "Attendance Search")
  {
    $search_inputs_r = make_search_inputs($new_defaults_r);
    $defaults_form = get_search_form($search_inputs_r, $menu_submit);
  }

  echo startTable("",10,0);
  echo "<tr><td>";
  show_form_table ($defaults_form);
  echo "</td></tr>";
  echo endTable();
}
else
{
  title_comment_div_noend($page_title, $comment);
}
echo "</DIV>";
?>
</body></html>
