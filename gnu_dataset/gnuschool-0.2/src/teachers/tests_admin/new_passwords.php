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

$print=$_POST['print'];
$period=$_POST['period'];
$get_period=$_GET['period'];
$fname=$_POST['fname'];
$lname=$_POST['lname'];
$update_pws=$_POST['update_pws'];
$menu_submit=$_POST['menu_submit'];
$id=$_POST['id'];
$get_id=$_GET['id'];
$get_menu_submit=$_GET['menu_submit'];
if(empty($id)) $id = $get_id;
if(empty($period)) $period = $get_period;

include ('../login.php');

$where_str = get_where_str($id, $period);
if (empty($where_str)) $yes_input = 0;

$period_flag = 0;

function syllable_rand($start=0)
{
  $str = "";
  $vow = array('a','e','i','o','u');
  $vow_p = array('ai','au','ay','ea','ee','ia','ie'
                 ,'oa','oi','oy','oo','ou');
  $con = array('b','c','d','f','g','h','j','k','l','m','n','p'
               ,'r','s','t','v','w','z');
  $con_p = array('bl','br','ch','cl','cr','dr','fl','fr','gl'
                 ,'gr','ph','pl','pr','sc','sh','sl','sm','sn'
                 ,'sp','st','sw','th','tr','tw');

  $v = rand(0,sizeof($vow) - 1);
  $vp = rand(0,sizeof($vow_p) - 1);
  $c = rand(0,sizeof($con) - 1); 
  $cp = rand(0,sizeof($con_p) - 1); 

  $odds = rand(0,20);
  if($odds < 3 && $start)
  {
    $str = $vow[$v];
    return $str;
  }
  
  $odds = rand(0,20);
  if ($odds < 7)
    $str = $con_p[$cp];
  else
    $str = $con[$c];

  $odds = rand(0,20);
  if ($odds > 8)
    $str .= $vow[$v];
  elseif ($odds < 2)
    $str .= 'y';
  else
    $str .= $vow_p[$vp];
  
  return $str;
}


function get_long_where_str($get_id, $db_name)
{
  $period_makeup=$_POST['period_makeup'];
  $period=$_POST['period'];
  $fname=$_POST['fname'];
  $lname=$_POST['lname'];
  $id=$_POST['id'];
  if(empty($id)) $id = $get_id;

if (!empty($period_makeup)) $period = $period_makeup;

  $query = "";

  $and_flag = 0;

  if (!empty($fname)) if($and_flag)
  { $query .= " and $db_name.spanish.fname = '$fname'";}
  else
  {
    $query .= "where $db_name.spanish.fname = '$fname'";
    $and_flag=1;
  }

  if (!empty($lname)) if($and_flag)
  { $query .= " and $db_name.spanish.lname = '$lname'";}
  else
  {
    $query .= "where $db_name.spanish.lname = '$lname'";
    $and_flag=1;
  }

  if (!empty($id))
  {
    if($and_flag)
      $query .= " and $db_name.spanish.id = $id
                  and $db_name.seating.id = $id";
    else
    {
      $query .= "where $db_name.spanish.id = $id
                  and $db_name.seating.id = $id";
      $and_flag=1;
    }
  }
  else
  { if($and_flag)
     $query .= " and $db_name.spanish.id = $db_name.seating.id";
    else
    {
      $query .= "where $db_name.spanish.id = $db_name.seating.id";
      $and_flag=1;
    }
  }

  if (!empty($period)) if($and_flag)
  { $query .= " and $db_name.spanish.period = '$period'
                  and $db_name.seating.period = '$period'";
  }
  else
  {
    $query .= "where $db_name.spanish.period = '$period'
                  and $db_name.seating.period = '$period'";
    $and_flag=1;
  }
  return $query;
}


function get_where_str($id=0, $period="")
{
  $fname=$_POST['fname'];
  $lname=$_POST['lname'];

  $query = "";

  if (!empty($id)) if($and_flag)
  { $query .= " and id = $id";}
  else { $query .= "where id = $id"; $and_flag=1;}

  if (!empty($fname)) if($and_flag)
  { $query .= " and fname = '$fname'";}
  else { $query .= "where fname = '$fname'"; $and_flag=1;}

  if (!empty($lname)) if($and_flag)
  { $query .= " and lname = '$lname'";}
  else { $query .= "where lname = '$lname'"; $and_flag=1;}

  if (!empty($period)) if($and_flag)
  { $query .= " and period = '$period'";}
  else { $query .= "where period = '$period'"; $and_flag=1;}

  return $query;
}


function get_input_form()
{
  
  $login = <<<EOQ
<form method=post action="$PHP_SELF">
<b>Period:</b><br>
<input type=text name=period size=4>
<br><br>

<b>Student first name:</b><br>
<input type=text name=fname size=30>
<br><br>

<b>Student last name:</b><br>
<input type=text name=lname size=30>
<br><br>

<input type=hidden name=menu_submit value="Tests">
<input type=submit name=submit value="Get passwords">
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


function get_new_password()
{
  $digits_arr= array('2','3','4','5','6','7','8','9','A','B','C','D','E'
   ,'F','G','H','J','K','L','M','N','P','R','S','T','W','X','Y','Z');

  $chars_num_arr= array('4','5','6','7','8');

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


function update_password ($db_name="", $id=0, $period="")
{
  $password = "";
  $p_w_exists=1;
  while ($p_w_exists)
  {
    $password = get_new_password();
    $p_w_exists = password_exists ($db_name, $password);
  }
  $query = "update $db_name.passwords set password = '$password'
             where id = $id and period='$period'";
  $result = mysql_query($query);
//  query_outcome_echo($query,$result);
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


function get_ids_arr ($db_name="",$spanish_query="",$update_pws="")
{
  $periods_idsNames_ass = array();
  $result = mysql_query($spanish_query);
  // query_outcome_echo($query,$result);

  if (mysql_num_rows($result) == 0) return array();

  $period_idName_num=array();
  $id_name_arr=array();

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $period = $row['period'];

  $name = $row['fname'] . " " . $row['mname'] . " " . $row['lname'];
  $id = $row['id'];
  $id_name_arr['name'] = $name;
  $id_name_arr['id'] = $id;
  $id_name_arr['x'] = $row['seat_x'];
  $id_name_arr['y'] = $row['seat_y'];

  if (!empty($update_pws)) update_password ($db_name, $id, $period);
  
  $id_name_arr['password'] = get_password($db_name,$id, $period);
  $period_idName_num[]=$id_name_arr;

  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  { 
    $next_period = $row['period'];
    $name = $row['fname'] . " " . $row['mname'] . " " . $row['lname'];
    $id = $row['id'];
    $id_name_arr['name'] = $name;
    $id_name_arr['id'] = $id;
    $id_name_arr['x'] = $row['seat_x'];
    $id_name_arr['y'] = $row['seat_y'];

    if (!empty($update_pws)) update_password ($db_name,$id,$period);
    $id_name_arr['password'] = get_password($db_name,$id,$period);
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
      $name = $id_name_arr['name'];
      $id = $id_name_arr['id'];
      $password = $id_name_arr['password'];
      print "name=$name, id=$id, password=$password<br>";
    }
  }
  print "<br>";
}


function get_closest1($s_r, $size)
{
  $closest = 5000;
  $index = 0;
  for ($i=0; $i< $size; $i++)
  {
    $a = (int)$s_r[$i]["x"];
    $b = (int)$s_r[$i]["y"];
    $hypo=(int)(sqrt(pow($a,2)+pow($b,2)));
    if($hypo < $closest) {$closest = $hypo; $index = $i;} 
  }
  return $index;
}


function get_closest2($s_r, $size, $start)
{
  $newest = $start - 1;
  $x = (int) $s_r[$newest]["x"];
  $y = (int) $s_r[$newest]["y"];
  $closest = 5000;
  $index = $start;

  for ($i=$start; $i< $size; $i++)
  {
    $a = (int)$s_r[$i]["x"];
    $b = (int)$s_r[$i]["y"];
    $dif_x = abs($x - $a); 
    $dif_y = abs($y - $b); 
    $x_sq = pow($dif_x,2);
    $y_sq = pow($dif_y,2);

    $hypo=(int)(sqrt($x_sq + $y_sq));
    if($hypo < $closest) {$closest = $hypo; $index = $i;} 
  }
  return $index;
}


function order_r(&$period_idsNames_arr, $per)
{
  $ids_r = $period_idsNames_arr["$per"];
  $size = sizeof($ids_r);

  $index = get_closest1($ids_r, $size);

  $cur_pos = 0;

  $l_name = $period_idsNames_arr["$per"][$index]["name"];
  $l_id = $period_idsNames_arr["$per"][$index]["id"];
  $l_x = $period_idsNames_arr["$per"][$index]["x"];
  $l_y = $period_idsNames_arr["$per"][$index]["y"];
  $l_password = $period_idsNames_arr["$per"][$index]["password"];

  $h_name = $period_idsNames_arr["$per"][$cur_pos]["name"];
  $h_id = $period_idsNames_arr["$per"][$cur_pos]["id"];
  $h_x = $period_idsNames_arr["$per"][$cur_pos]["x"];
  $h_y = $period_idsNames_arr["$per"][$cur_pos]["y"];
  $h_password = $period_idsNames_arr["$per"][$cur_pos]["password"];

  $period_idsNames_arr["$per"][$index]["name"] = $h_name;
  $period_idsNames_arr["$per"][$index]["id"] = $h_id;
  $period_idsNames_arr["$per"][$index]["x"] = $h_x;
  $period_idsNames_arr["$per"][$index]["y"] = $h_y;
  $period_idsNames_arr["$per"][$index]["password"] = $h_password;

  $period_idsNames_arr["$per"][$cur_pos]["name"] = $l_name;
  $period_idsNames_arr["$per"][$cur_pos]["id"] = $l_id;
  $period_idsNames_arr["$per"][$cur_pos]["x"] = $l_x;
  $period_idsNames_arr["$per"][$cur_pos]["y"] = $l_y;
  $period_idsNames_arr["$per"][$cur_pos]["password"] = $l_password;

  $cur_pos++;

  for($cur_pos; $cur_pos<$size; $cur_pos++)
  {
    $ids_r = $period_idsNames_arr["$per"];
    $index = get_closest2($ids_r, $size, $cur_pos);
  
    $l_name = $period_idsNames_arr["$per"][$index]["name"];
    $l_id = $period_idsNames_arr["$per"][$index]["id"];
    $l_x = $period_idsNames_arr["$per"][$index]["x"];
    $l_y = $period_idsNames_arr["$per"][$index]["y"];
    $l_password = $period_idsNames_arr["$per"][$index]["password"];

    $h_name = $period_idsNames_arr["$per"][$cur_pos]["name"];
    $h_id = $period_idsNames_arr["$per"][$cur_pos]["id"];
    $h_x = $period_idsNames_arr["$per"][$cur_pos]["x"];
    $h_y = $period_idsNames_arr["$per"][$cur_pos]["y"];
    $h_password = $period_idsNames_arr["$per"][$cur_pos]["password"];

    $period_idsNames_arr["$per"][$index]["name"] = $h_name;
    $period_idsNames_arr["$per"][$index]["id"] = $h_id;
    $period_idsNames_arr["$per"][$index]["x"] = $h_x;
    $period_idsNames_arr["$per"][$index]["y"] = $h_y;
    $period_idsNames_arr["$per"][$index]["password"] = $h_password;

    $period_idsNames_arr["$per"][$cur_pos]["name"] = $l_name;
    $period_idsNames_arr["$per"][$cur_pos]["id"] = $l_id;
    $period_idsNames_arr["$per"][$cur_pos]["x"] = $l_x;
    $period_idsNames_arr["$per"][$cur_pos]["y"] = $l_y;
    $period_idsNames_arr["$per"][$cur_pos]["password"] = $l_password;
  }
}


function get_password ($db_name="", $id, $period="")
{
  $query = "select password from $db_name.passwords where id = $id
            and period='$period'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);

  $row = mysql_fetch_row($result);
  return $row[0];
}


function get_table_column_titles($period, $br)
{
  $source = startTable("border");
  $cell_data = tDataSource("Period $period passwords","center", "",2);
  $source .= rowPrint($cell_data);
  $cell_data = tDataSource("Name&nbsp$br", "right");
  $cell_data .= tDataSource("&nbspPassword$br", "left");
  $source .= rowPrint($cell_data);
  return $source;
}


function show_tables_array($period_idsNames_ass, $defaults_r)
{
  $periods_arr=array();
  $tables_num = $defaults_r['tables_across'];
  $col_space = $defaults_r['column_space'];
  $row_space = $defaults_r['row_space'];
  $br = "";
  for($i=0; $i<$row_space; $i++) $br .= "<br>"; 
  $max_rows = 0;
  $row_nums = 0;

  while (list($period,$ids_names_num) = each($period_idsNames_ass))
  {
    $sizeof_arr = sizeof($ids_names_num);   
    if ($sizeof_arr < $tables_num) $tables_num = $sizeof_arr;

    $tables_arr=array();
    for ($i=0; $i<$tables_num; $i++)
    {
      $tables_arr[$i]=get_table_column_titles($period, $br);
    }
    $index = 0;
    for ($i=0; $i<$sizeof_arr; $i++)
    {
      $name=$ids_names_num[$i]['name'];
      $password=$ids_names_num[$i]['password'];
      $cell_data = tDataSource("$name&nbsp$br", "right");
      $cell_data .= tDataSource("&nbsp$password$br", "left");
      $tables_arr[$index] .= rowPrint($cell_data);
      $index++;
      if($index == $tables_num) $index = 0;
    }
    for ($i=0; $i<$tables_num; $i++)
      $tables_arr[$i] .= endTable();
    $periods_html[]=$tables_arr;
  }
  display_tables_array($periods_html, $tables_num, $col_space);
}


function display_tables_array ($periods_html, $cells_across, $col_space=0)
{
  $cell_num = 0; $cell_data = ""; $nbsp_str="";
  for($i=0;$i<$col_space;$i++)
    $nbsp_str .= "&nbsp";
  echo startTable("");
  while (list(,$table_row) = each($periods_html))
  {
    $row_size = sizeof($table_row);
    for ($i=0; $i<$row_size; $i++)
    {      
      $cell_data .= tDataSource($table_row[$i], "center");
      if ($i != $row_size - 1)
        $cell_data .= tDataSource("$nbsp_str","center");
      $cell_num++;      
    }
    if($cell_num < $cells_across)
    {
      for($i=$cell_num;$i<$cells_across;$i++)
      {
        $cell_data .= tDataSource("&nbsp", "center");
        $cell_data .= tDataSource("$nbsp_str", "center");
      }
    }
    echo rowPrint($cell_data, "top");
    $cell_data = ""; 
  }
  echo endTable();
}


function teacher_info_select($db_name)
{
  $query = "select * from main.teachers where db_name = '$db_name'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  if($result === FALSE) return FALSE;
  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $name = $row['fname'] . " " . $row['lname'];
  $t_name = $row['t_name'];
  return array($t_name,$name);
}


function teacher_t_name_rand($db_name)
{
  $t_name = "";

  for ($i=0; $i < 50; $i++)
  {
    $temp = syllable_rand(1);
    $temp .= syllable_rand();
    $temp .= syllable_rand();
    $query = "select * from main.teachers where t_name = '$temp'";
    $result = mysql_query($query);
    // query_outcome_echo($query,$result);
    if($result === FALSE) return FALSE;
    if (mysql_num_rows($result) === 0)
    {
      $i=50;
      $t_name = $temp;
      $query = "update main.teachers set t_name = '$t_name' where 
                db_name = '$db_name'";
      $result = mysql_query($query);
      // query_outcome_echo($query,$result);
    }
  }
  return $t_name;
}


//function show_submit_form($user="", $period="", $fname="", $lname="")
function show_submit_form($period="", $fname="", $lname="")
{
//<input type=hidden name="user" value="$user">
  $submit = <<<EOQ
<form method=post action="$PHP_SELF">
<input type=hidden name="fname" value="$fname">
<input type=hidden name="lname" value="$lname">
<input type=hidden name="period" value="$period">
<input type=hidden name="menu_submit" value="Tests">
<input type=submit name="update_pws" value="Get new passwords">
EOQ;
  $submit .= "</form>";
  echo $submit;
}


function new_window_form ($period, $fname, $lname, $id)
{
  $page = "/teachers/tests_admin/new_passwords.php";
  $window_description =
"width=600,height=400,scrollbars=yes,resizable=yes,status=yes,menubar=yes";

  $form_html = <<<EOQ
<form method=post target="window23" action=new_passwords.php> 
<input type=hidden name=print value="yes">
<input type=hidden name=period value="$period">
<input type=hidden name=fname value="$fname">
<input type=hidden name=lname value="$lname">
<input type=hidden name=id value="$id">
<input type=submit name=submit value="Printer view"
onClick="window.open('$page','window23','$window_description')">
</form>
EOQ;
  echo $form_html;  
}


function simple_body()
{
  echo "</head><body>";
}


function start_div()
{
  echo "<DIV class=main_table>";
}


function end_div()
{
  echo "</DIV>";
}


$tag_title = "gnuschool.org Grades search";
$page_title = "";

start_html($tag_title);
stylesheet_link("../style_sheet");

if(empty($menu_submit)) $menu_submit = "Tests";

if (empty($get_id))
{
  $menu_r=array();
  $menu_r['active_page']= $menu_submit;
  $menu_r['period']= "$period";
}

$temp_user = "";
$teacher_r = teacher_info_select($db_name);
$teacher_name = $teacher_r[1];
if(empty($update_pws))
  $temp_user = $teacher_r[0];
else
  $temp_user = teacher_t_name_rand($db_name);

$temp_u_str = "";
if(empty($temp_user))
  $temp_u_str = "The database is LOCKED.";
else
  $temp_u_str = "Temporary user name for students: $temp_user";

if(empty($print))
{
  start_body("main_background");
  main_menu($menu_r);
  $page_title = "Password search";
}
else
{
  simple_body();
  echo "<b>Name: $teacher_name<br>$temp_u_str</b>";
}


if (!$yes_input)
{
  $comment = "Please fill the search fields as needed.";
  $comment .= get_input_form();
  title_comment_div_noend($page_title, $comment);
  end_html_exit();
}
else
{
  $spanish_query = "";

  $defaults_r = get_defaults_r($db_name);
  $order = $defaults_r['order'];

  if(!empty($fname) || !empty($lname) || !empty($id))
  {
    $spanish_query = "select * from $db_name.spanish 
                      $where_str order by period";
  }
  elseif($order=="fname" || $order=="lname")
  {
    $spanish_query = "select * from $db_name.spanish 
                      $where_str order by period,$order";
  }
  else
  {
    $period_flag = 1;
    $spanish_query = "select fname,mname,lname,$db_name.spanish.id,
                      $db_name.spanish.period,seat_x,seat_y
                      from $db_name.spanish,$db_name.seating";
    $where_str = get_long_where_str($get_id, $db_name);
    $spanish_query .= " $where_str order by $db_name.spanish.period";
  }
  $period_idsNames_arr=get_ids_arr($db_name,$spanish_query,$update_pws);

  if(sizeof($period_idsNames_arr) == 0)
  {
    $comment = "Sorry, no matches found.";
    title_comment_div_noend($page_title, $comment);
    end_html_exit();
  }
  else
  {
    if ($period_flag)
    {
      order_r($period_idsNames_arr, $period);
    }
    if(empty($print))
    {
      $comment = "<b>Name: $teacher_name<br>$temp_u_str
                                 <br><br>Matches found:";
      title_comment_div_noend($page_title, $comment);
      show_tables_array($period_idsNames_arr, $defaults_r);
//      show_submit_form($user, $period, $fname, $lname, $defaults_r);
      show_submit_form($period, $fname, $lname, $defaults_r);
      new_window_form($period, $fname, $lname, $id);
      end_div();
    }
    else
      show_tables_array($period_idsNames_arr, $defaults_r);
  }
}
?>

</body></html>

