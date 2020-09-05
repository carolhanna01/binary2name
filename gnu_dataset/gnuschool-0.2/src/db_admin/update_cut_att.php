<?php 
session_start();
session_register("s_password_main");
session_register("s_user_main");
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

include('login.php');

//  $query="update main.todate set date='0000-00-00'";
//  mysql_query($query);


function main_cut_insert ($id, $date, $period)
{
  $query="select * from main.cut where id=$id
          and period='$period' and date='$date'";
  $result = mysql_query($query);
  if( mysql_num_rows($result) === 0 )
  {
    $query="insert into main.cut (id,date,period)
            values ($id,'$date','$period')";
    mysql_query($query);
  }
}


function main_attendance_insert($id, $date, $time_in)
{
  $query = "select is_in from main.attendance
            where id=$id and date='$date'";
  $result = mysql_query($query);
  if (mysql_num_rows($result) == 0)
  {
    $query="insert into main.attendance (id,date,time_in,is_in)
            values ($id,'$date','$time_in',1)";
    mysql_query($query);
  }
}


// $p_names_rr[0]=in_school_flag [1]=time_in $hr_attendance
function is_in_hr_attendance($id,$hr_db,$date)
{
 $is_in_time_r=array();
 $is_in_time_r[0] = "";
 $is_in_time_r[1] = "00:00:00";

 $query = "select time_in, is_in from $hr_db.hr_attendance
           where id=$id and date='$date'";
 $result = mysql_query($query);
 if ($row=mysql_fetch_array($result, MYSQL_ASSOC))
 {
   $is_in_time_r[0] = $row['is_in'];
   $is_in_time_r[1] = $row['time_in'];
 }
 return $is_in_time_r;
}


// $periods_rr[0]=in_school_flag [1]=$periods_r
function get_periods_attended(
           $id,$cur_period,$period_db_rr,$date,$cur_is_in)
{
 $periods_rr=array();
 $periods_rr[0]=2;
 $periods_rr[1]=array();
 $periods_r=array();

 for($i=0;$i<sizeof($period_db_rr);$i++)
 {
   $is_in = "2";
   $period = $period_db_rr[$i][0];
   $db = $period_db_rr[$i][1];
       
   if($period === $cur_period)
   {
     $is_in = $cur_is_in;
   }
   else
   {
     $query="select is_in from $db.attendance where id=$id
             and period='$period' and date='$date'";
     $result = mysql_query($query);
     if ($row = mysql_fetch_row($result))
     {
       $is_in = $row[0];
     }
   }

   if($is_in === "1")
   {
     $periods_r["$period"] = 1;
     $periods_rr[0] = 1;
   }
   else
    if($is_in === "0")
      $periods_r["$period"] = 2;
 }
 $periods_rr[1]=$periods_r;

 return $periods_rr;
}


// the number 2 is a substitute for 0 meaning not in
function insert_attend_cuts($id,$periods_rr,$date,$in_hr,$time_in)
{
 $in_school = $periods_rr[0];
 $periods_r = $periods_rr[1];
 if ($in_school === 1)
 {
   main_attendance_insert($id, $date, $time_in);
   if ($in_hr === "0")
     main_cut_insert($id, $date, "HR");
 }
 while (list($period, $is_in) = each($periods_r))
 {
   if ($is_in === 2)
     if ($in_hr === "1")
       main_cut_insert($id, $date, $period);
     else
       if ($in_hr === "2")
         if ($in_school === 1)
           main_cut_insert($id, $date, $period);
 }
}

$date = date("Y-m-d");
$time = date("H:i:s");

$tag_title = "gnuschool.org Cut & Attendance Update";
$page_title = "Cut & Attendance Update <br>
               This page was opened $date at $time";


//  $query="update main.todate set date='0000-00-00'";
//  mysql_query($query);


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("index");

$query = "select date from main.todate where search='cut'";
$result = mysql_query($query);
$row = mysql_fetch_row($result);
$todate = $row[0];
if($todate !== $date)
{
  $date_clause=" (date>='$todate' and date<'$date')";
  $query = "select id, periods from main.spanish";
  $result = mysql_query($query);

  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  { 
    $id = $row['id'];
    $periods = $row['periods'];
    $length = strlen($periods);
    $periods = substr($periods,1,$length-2);
    $periods_r = explode("|",$periods);
    $period_db_rr = array();
    $hr_db = "";

    for($i=0;$i<sizeof($periods_r);$i++)
    {
      $period_db_str = $periods_r[$i];
      $period_db_r = array();
      $period_db_r = explode("+++",$period_db_str);
      $period = $period_db_r[0];
      if ($period == "HR" ) $hr_db = $period_db_r[1];
      else
      {
        $period_db_rr[] = $period_db_r;
      }
    }

    $dates_done_r = array();
    for($i=0;$i<sizeof($period_db_rr);$i++)
    {
      $is_in = "2";
      $period = $period_db_rr[$i][0];
      $db = $period_db_rr[$i][1];
     
      $query="select date, is_in from $db.attendance where id=$id
              and period='$period' and $date_clause";
      $result1 = mysql_query($query);
      while ($row1 = mysql_fetch_array($result1, MYSQL_ASSOC))
      { 
        $cur_date = $row1['date'];
        $done_date = $dates_done_r["$cur_date"];

        if (empty($done_date))
        {
          $is_in = $row1['is_in'];

          $is_in_hr_r = is_in_hr_attendance($id,$hr_db,$cur_date);
          $is_in_hr = $is_in_hr_r[0];
          $time_in = $is_in_hr_r[1];
          $periods_rr = get_periods_attended( 
               $id,$period,$period_db_rr,$cur_date,$is_in);         
          insert_attend_cuts(
               $id,$periods_rr,$cur_date,$is_in_hr,$time_in);
          $dates_done_r["$cur_date"] = 1;
        }
      }
    }
  }

  $comment=
    "Automate this script to run when no users are logged into the system.
     Opening this page updates the database.
     The database has been updated.";
  title_comment_div_noend($page_title, $comment);

  $query="update main.todate set date='$date'";
  mysql_query($query);
}
else
{
  $comment=
    "Automate this script to run when no users are logged into the system.
     Opening this page updates the database.
     The database was already updated today.";
  title_comment_div_noend($page_title, $comment);
}


echo "</DIV>";

?>
</body></html>

