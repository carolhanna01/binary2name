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

$textarea_data=$_POST['textarea_data'];
$proceed = $_POST['proceed'];
$delim = $_POST['delim'];
$mail_up = $_POST['mail_up'];
$show_up = $_POST['show_up'];
$e_mail = $_POST['e_mail'];

if(empty($mail_up)) $mail_up = 0;
else $mail_up = 1;
if(empty($show_up)) $show_up = 0;
else $show_up = 1;
if(empty($e_mail)) $e_mail = 0;
else $e_mail = 1;


include('login.php');


if(empty($proceed))
  $yes_input = 0;

if(empty($delim)) $delim = " ";


function proceed_delim_form()
{
  echo <<<EOQ
<form method=post action="$PHP_SELF">
<textarea name=textarea_data cols=80 rows=10></textarea>
<BR><BR>
<input type=checkbox name=e_mail value="yes"><b>Teacher column name 
TEMAIL exists. It holds the teacher's email address.</b>
<BR><BR>
<input type=checkbox name=mail_up value="yes"><b>Mail each user name
and password to that teacher's e-mail address.</b>
<BR><BR>
<input type=checkbox name=show_up value="yes"><b>Display all user name
and password data.</b>
<BR><BR><B>Field delimiter (the default is the tab or space).
<BR>
<input type=text size=8 name=delim maxlength=6>
<BR>If the delimiter is not a space(s) or a tab(s) you need to type in 
what it is.
<BR> 
It can be a single character such as -
<BR>Or a combination of characters such as &&&
<BR><BR>
<input type=submit name=proceed value="Proceed">
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


function user_rand()
{
  $user = "";

  for ($i = 0; $i < 50; $i++)
  {
    $user = syllable_rand(1);
    $user .= syllable_rand();
    $user .= syllable_rand();
    $query="select * from user.users where user='$user'";
    $result = mysql_query($query);
    query_outcome_echo($query,$result);
    if ($result === FALSE) return FALSE;
    if (mysql_num_rows($result) === 0) $i = 50;
  }
  return $user;
}


function password_rand()
{
  $pwd = syllable_rand(1);
  $pwd .= syllable_rand();
  $pwd .= rand(1,99);

  if(rand(0,1))
    $pwd .= '-';
  else
    $pwd .= '_';

  $pwd .= syllable_rand();
  $pwd .= syllable_rand();
  $pwd .= rand(1,99);
  return $pwd;
}


function create_db($db_name)
{
  $errors = 0;
  $query = "CREATE DATABASE $db_name";
  $result = mysql_query($query);
  query_outcome_echo($query,$result,$errors);
  return $errors;
}


function grant_access_S($db_name)
{
  $errors = 0;
  $query = "grant select on $db_name.* to 'access_S'@'127.0.0.1'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result,$errors);

  $query = "grant update, insert on $db_name.taken to 
                                            'access_S'@'127.0.0.1'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result,$errors);

  $query = "grant update, insert on $db_name.scores to 
                                            'access_S'@'127.0.0.1'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result,$errors);
  return $errors;
}


function create_tables($db_name)
{
  $fields = "(date date, score tinyint(4), test_id mediumint(8) unsigned,
      id int(11) unsigned,period varchar(12),makeup char(1),test text)";
  $query = "create table $db_name.scores " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  $fields="(fname varchar(30) default '' not null
           ,mname varchar(30) default '' not null
           ,lname varchar(30) default '' not null
           ,period varchar(12) default '' not null
           ,id int(11) unsigned)";
  $query = "create table $db_name.spanish " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  $fields = "(description varchar(255), type varchar(30), answers text, 
    test_id mediumint(8) unsigned auto_increment, isbn char(10),  
    title varchar(125), author varchar(125), page varchar(10), test text,
    primary key(test_id))";
  $query = "create table $db_name.tests " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  if ($result !== FALSE)
  {
    $query = "insert into $db_name.tests (description, type
              , answers, isbn,  title, author, page, test) 
              values ( 'empty test','','','','','','','')";
    $result = mysql_query($query);
    $errors = query_outcome_echo($query,$result,$errors);
  }

  $fields = "(period varchar(12),date date, time_in time,
              is_in tinyint(1) default 0 not null,id int(11) unsigned)";
  $query = "create table $db_name.attendance " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  $fields = "(date date, time_in time,
              is_in tinyint(1) default 0 not null,id int(11) unsigned)";
  $query = "create table $db_name.hr_attendance " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  $fields = "(period varchar(12), seat_x mediumint(8) unsigned,
       seat_y mediumint(8) unsigned, id int(11) unsigned)";
  $query = "create table $db_name.seating " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  $fields = "(id int(11) unsigned, password char(8), period varchar(12))";
  $query = "create table $db_name.passwords " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  $fields = "(test_id mediumint(8) unsigned, period varchar(12),
             date date, count tinyint(3) unsigned)";
  $query = "create table $db_name.taken " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  $fields = "(seating_fname char(2) default '10' not null,
              seating_mname char(1) default 'n' not null,
              seating_lname char(2) default '10' not null,
              seating_name_lines char(1) default 'f' not null,
              search_start date default '2004-03-15' not null,
              search_end date default '2004-03-16' not null,
              q1_start date default '2004-09-01' not null,
              q1_end date default '2004-11-13' not null,
              q2_start date default '2004-11-14' not null,
              q2_end date default '2005-02-01' not null,
              q3_start date default '2005-02-02' not null,
              q3_end date default '2005-04-15' not null,
              q4_start date default '2005-04-16' not null,
              q4_end date default '2005-06-22' not null,
              passwords_tables_across char(2) default '2' not null,
              passwords_row_space char(2) default '4' not null,
              passwords_order varchar(12) default 'seat_y' not null,
              passwords_column_space char(2) default '7' not null)";
  $query = "create table $db_name.defaults " . $fields;
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);

  if ($result !== FALSE)
  {
    $query = "insert into $db_name.defaults () values()";
    $result = mysql_query($query);
    $errors = query_outcome_echo($query,$result,$errors);
  }

  return $errors;
}


function trim_array($old_r)
{
  $length = sizeof($old_r);
  $new_r = array();
  for($i=0; $i<$length; $i++)
  {
    $val = $old_r[$i];
    $val = trim($val);
    if (!empty($val)) $new_r[] = $val;
  }
  return $new_r;
}


function match_and_order($def_rr, $line_r, $def_size, $line_size)
{ 
  $match = 0;
  $new_r = array();

  for ($i=0; $i< $def_size; $i++)
  {
    $match = 0;
    $name1 = $def_rr[$i][1];
    $col = $def_rr[$i][0];
    for ($j=0; $j< $line_size; $j++)
    { 
      $name2 = $line_r[$j];
      if ( $name1 == $name2)
      {
        $match = 1;
        if ($col == "fname")
          $new_r['fname'] = $j;
        elseif ($col == "mname")
          $new_r['mname'] = $j;
        elseif ($col == "lname")
          $new_r['lname'] = $j;
        elseif ($col == "email")
          $new_r['email'] = $j;
        else
          if ($col == "hr" || $col == "period")
            $new_r['hr'] = $j;
        $j = $line_size;
      }
    }
    if (!$match) $i=$def_size;
  }

  if ($match)
    return $new_r;
  else
    return FALSE;
}


function teacher_col_match($def_rr, $line_r, $def_size, $line_size)
{ 
  $match = 0;

  for ($i=0; $i< $def_size; $i++)
  {
    $match = 0;
    $name1 = $def_rr[$i][1];
    for ($j=0; $j< $line_size; $j++)
    { 
      $name2 = $line_r[$j];
      if ( $name1 == $name2) $match = 1;
    }
    if (!$match) $i=$def_size;
  }
  return $match;
}


function teacher_cols($email)
{
  $col_r = array();
  $col_r[] = array('fname','TFIRSTNAME');
  $col_r[] = array('mname','TMIDDLENAME');
  $col_r[] = array('lname','TLASTNAME');
  $col_r[] = array('hr','THOMEROOM');
  if($email) $col_r[] = array('email','TEMAIL');
  return $col_r;
}


function student_cols()
{
  $col_r = array();
  $col_r[] = array('fname','FIRSTNAME');
  $col_r[] = array('mname','MIDDLENAME');
  $col_r[] = array('lname','LASTNAME');
  $col_r[] = array('period','PERIOD');
  return $col_r;
}


function teacher_db_rand()
{
  $db_name = "";

  for ($i=0; $i < 50; $i++)
  {
    $temp = syllable_rand(1);
    $temp .= syllable_rand();
    $temp .= syllable_rand();
    $query = "select * from main.teachers where db_name = '$db_name'";
    $result = mysql_query($query);
    query_outcome_echo($query,$result);
    if($result === FALSE) return FALSE;
    if (mysql_num_rows($result) === 0)
    {
      $i=50;
      $db_name = $temp;
    }
  }
  return $db_name;
}


function teacher_t_name_rand()
{
  $t_name = "";

  for ($i=0; $i < 50; $i++)
  {
    $temp = syllable_rand(1);
    $temp .= syllable_rand();
    $temp .= syllable_rand();
    $query = "select * from main.teachers where t_name = '$temp'";
    $result = mysql_query($query);
    query_outcome_echo($query,$result);
    if($result === FALSE) return FALSE;
    if (mysql_num_rows($result) === 0)
    {
      $i=50;
      $t_name = $temp;
    }
  }
  return $t_name;
}


function teacher_select_db($fname,$mname,$lname)
{
  $query = "select db_name from main.teachers where fname='$fname'
            and mname='$mname' and lname='$lname'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;
  $row =  mysql_fetch_row($result);
  return $row[0];
}


function teacher_id_select($fname,$mname,$lname)
{
  $query = "select tid from main.teachers where fname='$fname' and 
            mname='$mname' and lname='$lname'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;
  $row =  mysql_fetch_row($result);
  return $row[0];
}


function user_tid_insert( $tid, $user, $pwd, $access)
{
  $query = "insert into user.users (tid,user,password,access)
            values ($tid,'$user',password('$pwd'),'$access')";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
}


function teacher_insert($db_name,$t_name,$fname
                                ,$mname,$lname,$hr, $email)
{
  $query =<<<EOQ
insert into main.teachers (db_name,t_name,fname,mname,lname,hr,email)
values ('$db_name','$t_name','$fname','$mname','$lname','$hr','$email')
EOQ;
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
}


function get_main_id_periods($fname,$mname,$lname)
{
  $query = "select id, periods from main.spanish where fname='$fname' 
            and mname='$mname' and lname='$lname'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;
  if (mysql_num_rows($result) === 0) return 0;

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  return $row;
}


function id_is_in_teacher_db($db_name, $id, $period)
{
  $query = "select id from $db_name.spanish where id=$id
             and period = '$period'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;
  if (mysql_num_rows($result)) return 1;
  else return 0;
}


function update_main_spanish($db_name,$id,$period,$hr_num,$periods)
{
  $per_db = "|$period+++$db_name|";
  if (strpos($periods,$per_db) === FALSE)
  {
    $periods .= "$period+++$db_name|";
    $query = "update main.spanish set periods='$periods' where
            id=$id";
    $result = mysql_query($query);
    query_outcome_echo($query,$result);
  }

  if ($period == "hr")
  {
    $query = "select hr from main.teachers where db_name = '$db_name'";
    $result = mysql_query($query);
    query_outcome_echo($query,$result);
    $row = mysql_fetch_row($result);
    $thr = $row[0];
    if ($thr != $hr_num)
    {
      echo "This student has a different homeroom number.<br>";
    }
  }
}


function email_teachers_up($up_r)
{
  $size = sizeof($up_r);

  $subject = "System access information";

  $body2 =<<<EOQ

Welcome to our school.
To access your class information you will need a user name and password.
A user name and password have been created for you. Please use these to 
log into the system. Once you're logged in you may change both your 
user name and your password to something more familiar to you.

EOQ;


  $body4 =<<<EOQ

For security reasons, please write the information on a piece of paper,
and delete this email from your inbox and trash. Please keep the user 
name and password in a safe place.

Thank you,
Peter E. Rios
EOQ;

  $from = "From: admin@mybooktrade.com\n";

  for ($i=0; $i < $size; $i++)
  {
    $hr = $up_r[$i][0];
    $name = $up_r[$i][1];
    $user = $up_r[$i][2];
    $pwd = $up_r[$i][3];
    $to = $up_r[$i][4];
    
    $body1 = "Dear $name,";

    $body3 =<<<EOQ
Your user name is: $user
Your password is: $pwd
EOQ;

    $body = $body1 . $body2 . $body3 . $body4;

    mail($to, $subject, $body, $from);
  }
}


function up_r_2_table($up_r)
{
  $size = sizeof($up_r);
  if ($size)
  {
    echo "<table border><td colspan=4>You will not be able to
    see this table again. This data is only displayed so that each
    educator may be supplied with a user name and password.
    Educators require privacy, so please destroy the output once
    educators have their user name and password.
    Saving this data is also a security liability.
    If an educator forgets a user name and or password, these can
    be easily reset later by the administrator.</td>
     <tr align=left><td><b>HOMEROOM</b></td><td><b>TEACHER'S NAME
     </b></td><td><b>USER NAME</b></td><td><b>PASSWORD</b></td></tr>";
  }

  for ($i=0; $i < $size; $i++)
  {
    $hr = $up_r[$i][0];
    $name = $up_r[$i][1];
    $user = $up_r[$i][2];
    $pwd = $up_r[$i][3];
    echo "<tr align=left><td>$hr</td><td>$name</td><td>$user</td>
          <td>$pwd</td></tr>";
  }

  if ($size)
    echo "</table><br><br>";
}


function get_db_r()
{
  $db_r = array();
  $query = "select db_name from main.teachers";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;
   
  while ($row = mysql_fetch_row($result))
  {  
    $database = $row[0];
    $db_r[] = $database; 
  }
  return $db_r;
}


function insert_all_passwords( $db_name="" )
{
  $query = "select id, period from $db_name.spanish order by period";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;

  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    $id = $row['id'];
    $period = $row['period'];

    $query = "select * from $db_name.passwords where id=$id and
              period='$period'";
    $result1 = mysql_query($query);
    query_outcome_echo($query,$result1);
    if ($result1 === FALSE) return FALSE;
    if (mysql_num_rows($result1) === 0)
    {
      $query = "insert into $db_name.passwords 
              (id,password,period)
              values ($id,'$id','$period')";
      $result2 = mysql_query($query);
      query_outcome_echo($query,$result2);
    }
  }
}


function insert_all_seats( $db_name="" )
{
  $students_r = array();
  $query = "select * from $db_name.spanish order by period";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $id = $row['id'];
  $period = $row['period'];
  $students_r[] = array($id, "$period");
  $period_cur = $period;
  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    $id = $row['id'];
    $period = $row['period'];
    if ($period != $period_cur)
    {
      insert_period_seats($students_r, $db_name);
      $period_cur = $period;
      $students_r = array();
    }
    $students_r[] = array($id, "$period");
  }
  insert_period_seats($students_r, $db_name);
}


function insert_period_seats($students_r, $db_name)
{
  $x_start = 10; $y_start = 55; $x_val = 0; $y_val = 0;
  $col_max = 5; $col_cur = 0; $row_cur = 0;
  $x_over = 150; $y_over = 70;
  for ($i=0;$i<sizeof($students_r);$i++)
  {
    $x_val = $col_cur * $x_over + $x_start;
    $y_val = $row_cur * $y_over + $y_start;
    $id = $students_r[$i][0];
    $period = $students_r[$i][1];
    if($col_cur == ($col_max-1))
    {
      $col_cur = 0;
      $row_cur++;
    }
    else $col_cur++;

    $query = "select * from $db_name.seating where 
              id=$id and period='$period'";
    $result = mysql_query($query);
    query_outcome_echo($query,$result);
    if ($result === FALSE) return FALSE;
    if (mysql_num_rows($result) === 0)
    {
      $query = "insert into $db_name.seating 
              (id,period,seat_x, seat_y)
              values ($id,'$period','$x_val','$y_val')";
      $result = mysql_query($query);
      query_outcome_echo($query,$result);
    }
  }
}


$tag_title = "gnuschool.org Paste into Teachers Database";
$page_title = "Paste or type into teachers' database";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("setup");


if (!$yes_input)
{
  $comment = "Paste or type text and populate teachers' databases";
  title_comment_div_noend($page_title, $comment);
  echo "<b>Notes:
        <ul>
       <LI>Paste formatted text into the textarea.
       <LI>The formatted text must contain teacher and student 
           information. A teacher section must be followed by that 
           teacher's student section.
       <LI>Teacher column names must contain
           TFIRSTNAME TMIDDLENAME TLASTNAME THOMEROOM
       <LI>Teacher data must follow the teacher column names.
       <LI>Student column names must follow the teacher data.
           The student column names must contain
           FIRSTNAME MIDDLENAME LASTNAME PERIOD
       <LI>Student data must follow the student column names.
       <LI>The THOMEROOM column is for the teacher's homeroom number.
       <LI>The PERIOD column is the class period when the student will 
           have the above named teacher.
       <LI>Students' homeroom period must be written as HR
       <LI>An example follows.</ul></b>";

echo "<TABLE>
<TR><TD>TFIRSTNAME&nbsp</TD><TD>TMIDDLENAME&nbsp</TD><TD>TLASTNAME&nbsp</TD>
<TD>TGENDER</TD><TD>THOMEROOM</TD></TR>
<TR><TD>Pedro</TD><TD>E.</TD><TD>Rios</TD><TD>M</TD><TD>D432</TD></TR>
<TR><TD>AVERAGE</TD><TD>FIRSTNAME</TD><TD>MIDDLENAME</TD><TD>LASTNAME</TD><TD>PERIOD</TD></TR>
<TR><TD>78</TD><TD>Toloro</TD><TD>Zorro</TD><TD>Senoro</TD><TD>HR</TD></TR>
<TR><TD>82</TD><TD>Flanito</TD><TD>X.</TD><TD>Pamplos</TD><TD>HR</TD></TR>
<TR><TD>98</TD><TD>John</TD><TD>H.</TD><TD>Smith</TD><TD>5</TD></TR>
<TR><TD>91</TD><TD>Mary</TD><TD>B.</TD><TD>Doe</TD><TD>3a</TD></TR>
<TR><TD>100</TD><TD>Jean</TD><TD>Lucid</TD><TD>Estudiante</TD><TD>3b</TD></TR>
</TABLE>
<TABLE>
<TR><TD>TFIRSTNAME&nbsp</TD><TD>TMIDDLENAME&nbsp</TD><TD>TLASTNAME&nbsp</TD>
<TD>THOMEROOM</TD></TR>
<TR><TD>Raymond</TD><TD>Walter</TD><TD>Maestro</TD><TD>D121</TD></TR>
<TR><TD>FIRSTNAME</TD><TD>MIDDLENAME</TD><TD>LASTNAME</TD><TD>PERIOD</TD></TR>
<TR><TD>Masca</TD><TD>Bien</TD><TD>Dentista</TD><TD>HR</TD></TR>
<TR><TD>Juan</TD><TD>Dulce</TD><TD>Smithers</TD><TD>HR</TD></TR>
<TR><TD>Maria</TD><TD>C.</TD><TD>Libro</TD><TD>1a</TD></TR>
<TR><TD>Marta</TD><TD>Lista</TD><TD>Studious</TD><TD>1b</TD></TR>
</TABLE>";

  proceed_delim_form();
  back_2_parentA("setup");
  end_html_exit();
}

$comment = "Attempting to edit the database using the textarea...";
title_comment_div_noend($page_title, $comment);

$lines_all_r = explode("\n",$textarea_data);
$lines_size = sizeof($lines_all_r);

$t_col_rr = teacher_cols($e_mail);
$s_col_rr = student_cols();

$t_cols_num = 4;
if($e_mail) $t_cols_num = 5;

$edited = 0;
$line = "";
$up_r = array();
$stu_cols = 0;
$stu_data = 0;
$tea_cols = 1;
$tea_data = 0;

for ( $i = 0; $i < $lines_size; $i++)
{
  $line_num = $i+1;
  $line = $lines_all_r[$i];
  $line = trim($line);

  if (!empty($line) && $tea_cols)
  {
    $line_r = explode($delim, $line);
    $line_r = trim_array($line_r);
    $t_cols = sizeof($line_r);

    $t_order_r = match_and_order($t_col_rr, $line_r, $t_cols_num
                               , $t_cols);
  
    echo "<br>LINE:$line_num<br>$line<br>";
    if($t_order_r === FALSE)
    {
      echo "<b> The teacher column names do not match with the 
                textarea's teacher column names.</b><br><br>";    
      back_2_parentA("setup");
      end_html_exit();
    }
    else
    {
      echo "<b> The teacher column names match with the textarea's
            column names. Continuing...</b><br><br>";    
    }
    $tea_cols = 0;
    $tea_data = 1;
  }
  elseif (!empty($line) && $tea_data)
  {
    $line_r = explode($delim, $line);
    $line_r = trim_array($line_r);
    $d_size = sizeof($line_r);

    if($t_cols != $d_size)
    {
      echo "<br>LINE:$line_num<br>$line<br>";
      echo "<b>The number of teacher data columns is not equal to the 
          column names.</b><br><br>";    
      back_2_parentA("setup");
      end_html_exit();
    }

    $k_f = $t_order_r['fname'];
    $k_m = $t_order_r['mname'];
    $k_l = $t_order_r['lname'];
    $k_h = $t_order_r['hr'];
    $k_e = $t_order_r['email'];

    $fname = $line_r[$k_f];
    $mname = $line_r[$k_m];
    $lname = $line_r[$k_l];
    $hr = $line_r[$k_h];
    $email = $line_r[$k_e];

    $t_exists = 0;
    $t_name = "";
    $db_name = teacher_select_db($fname,$mname,$lname);
    if (!empty($db_name))
    {
      echo "<br>LINE:$line_num<br>$line<br>";
      echo "<b>SKIPPING: Teacher $fname $mname $lname already 
          exists.</b><br><br>";
      $t_exists = 1;
    }
    else
    {
      $db_name = teacher_db_rand();
      if (empty($db_name))
      {
        echo "<b>The database name could not be generated 
                 in 50 tries.</b><br><br>";    
        back_2_parentA("setup");
        end_html_exit();
      }
      $t_name = teacher_t_name_rand();
    }

    if (!$t_exists)
    {
      teacher_insert($db_name,$t_name,$fname,$mname,$lname,$hr,$email);
      $user = user_rand();
      $pwd = password_rand();
      $name = "$fname $mname $lname";
      $up_r[] = array($hr, $name, $user, $pwd, $email);
      $tid = teacher_id_select($fname,$mname,$lname);
      user_tid_insert( $tid, $user, $pwd, "T");
      create_db($db_name);
      create_tables($db_name);
      grant_access_S($db_name);
    }    
    $tea_data = 0;
    $stu_cols = 1;
  }
  elseif (!empty($line) && $stu_cols)
  {
    $line_r = explode($delim, $line);
    $line_r = trim_array($line_r);
    $s_cols = sizeof($line_r);

    $s_order_r = match_and_order($s_col_rr, $line_r, 4, $s_cols);

    echo "<br>LINE:$line_num<br>$line<br>";
    if($s_order_r === FALSE)
    {
      echo "<b> The student column names do not match with the 
                textarea's student column names.</b><br><br>";    
      back_2_parentA("setup");
      end_html_exit();
    }
    else
    {
      echo "<b> The student column names match with the textarea's 
            column names. Continuing...</b><br><br>";    
    }
    $stu_cols = 0;
    $stu_data = 1;
  }
  elseif (!empty($line) && $stu_data)
  {
    $line_r = explode($delim, $line);
    $line_r = trim_array($line_r);
    $d_size = sizeof($line_r);
    if(teacher_col_match($t_col_rr, $line_r, $t_cols_num, $d_size))
    {
      $stu_data = 0;
      $tea_cols = 1;
      $i--;
    }
    else
    {
      echo "<br>LINE:$line_num<br>$line<br>";
      if($s_cols != $d_size)
      {
        echo "<b>SKIPPING: The number of student data columns
              is not equal to the number of column names.</b><br><br>";    
      }
      else
      {      
        $k_f = $s_order_r['fname'];
        $k_m = $s_order_r['mname'];
        $k_l = $s_order_r['lname'];
        $k_h = $s_order_r['hr'];

        $fname = $line_r[$k_f];
        $mname = $line_r[$k_m];
        $lname = $line_r[$k_l];
        $period = $line_r[$k_h];

        $id_r = get_main_id_periods($fname,$mname,$lname);
        $id = $id_r['id'];
        $periods = $id_r['periods'];
        if ($id)
        {
          if (!id_is_in_teacher_db($db_name, $id, $period))
          {
            update_main_spanish($db_name,$id,$period,$hr,$periods);
            $query = "insert into $db_name.spanish 
                  (id,fname,mname,lname,period) values
                  ($id,'$fname','$mname','$lname','$period')";
            $result = mysql_query($query);
            $errors = query_outcome_echo($query,$result,$errors);
            $edited++;
          }
          else echo "<b>SKIPPING: $fname $mname $lname
                   is already in $db_name.spanish</b><br>";
        }
        else echo "<b>SKIPPING: $fname $mname $lname
                   is not in main.spanish</b><br>";
      }
    } 
  }
}
echo "<b>End of textarea data reached.</b><br><br>
      <b>Arranging student seats and setting passwords...</b><br>";

$db_r = get_db_r();

for ($i=0;$i<sizeof($db_r);$i++)
{
  $db_name = $db_r[$i];
  insert_all_seats( $db_name ); 
  insert_all_passwords( $db_name ); 
}

echo "<br><br>";

if($show_up) up_r_2_table($up_r);
if($mail_up) email_teachers_up($up_r);

back_2_parentA("setup");
echo "</DIV>";

?>
</body></html>

