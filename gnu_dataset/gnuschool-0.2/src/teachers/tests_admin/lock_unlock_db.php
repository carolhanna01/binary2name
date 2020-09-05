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

$submit=$_POST['submit'];
$menu_submit=$_POST['menu_submit'];

if(empty($submit)) $yes_input = 0;


function input_form()
{
  echo <<<EOQ
<form method=post action="$PHP_SELF">

<input type=hidden name=menu_submit value="Tests">
<input type=submit name=submit value="Unlock Database">
<input type=submit name=submit value="Lock Database">
</form>
EOQ;
}


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
      if($result === FALSE) return FALSE;
    }
  }
  return 0;
}


function db_is_locked($db_name)
{
  $query = "select t_name from main.teachers where db_name='$db_name'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  if($result === FALSE) return FALSE;
  $row = mysql_fetch_row($result);
  $t_name = $row[0];
  if (empty($t_name)) return 1;
  else return 0;
}


function lock_db($db_name)
{
  $query = "update main.teachers set t_name='' where db_name='$db_name'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  if($result === FALSE) return FALSE;
  else return 1;
}


$tag_title = "gnuschool.org Lock Unlock Database";
$page_title = "Lock or unlock your database";

start_html($tag_title);
stylesheet_link("../style_sheet");
start_body("main_background");

if(empty($menu_submit))  $menu_submit = "Tests";

$menu_r=array();
$menu_r['active_page']= $menu_submit;
main_menu($menu_r);

$comment = "";
$is_locked = 1;

if (!$yes_input)
{
  $is_locked = db_is_locked($db_name);
  if ($is_locked)
  {
    $comment = "The database is LOCKED. 
               Students will NOT be able to start assessments.";
  }
  elseif ($is_locked === 0)
  {
    $comment = "The database is UNLOCKED. Students may start 
               assessments.";
  }
  else
  {
    $comment = "There was a problem please contact your administrator.";
  }  

  title_comment_div_noend($page_title, $comment);
  input_form();
  end_html_exit();
}

if ($submit == "Lock Database")
{ 
  $is_locked = lock_db($db_name);
}
elseif ($submit == "Unlock Database")
{ 
  $is_locked = teacher_t_name_rand($db_name);
}
else
  $is_locked = db_is_locked($db_name);

if ($is_locked)
{
  $comment = "The database is LOCKED. 
        Students will NOT be able to start assessments.";
}
elseif ($is_locked === 0)
{
  $comment = "The database is UNLOCKED. Students may start 
assessments.";
}
else
{
  $comment = "There was a problem please contact your administrator.";
}  

title_comment_div_noend($page_title, $comment);
input_form();
echo "</DIV>";
?>

</body></html>



