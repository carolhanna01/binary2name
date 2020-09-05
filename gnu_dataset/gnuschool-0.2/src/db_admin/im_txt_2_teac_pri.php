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
<input type=checkbox name=e_mail value="yes"><b>Principal column name 
EMAIL exists. It holds the principal's email address.</b>
<BR><BR>
<input type=checkbox name=mail_up value="yes"><b>Mail each user name
and password to that user's e-mail address.</b>
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


function principal_cols($email)
{
  $col_r = array();
  $col_r[] = array('fname','FIRSTNAME');
  $col_r[] = array('mname','MIDDLENAME');
  $col_r[] = array('lname','LASTNAME');
  $col_r[] = array('hr','ROOM');
  if($email) $col_r[] = array('email','EMAIL');
  return $col_r;
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


function teacher_insert($t_name,$fname,$mname,$lname,$hr, $email)
{
  $query =<<<EOQ
insert into main.teachers (t_name,fname,mname,lname,hr,email,db_name)
values ('$t_name','$fname','$mname','$lname','$hr','$email','')
EOQ;
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
}


function email_principals_up($up_r)
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
    principal may be supplied with a user name and password.
    Principals require privacy, so please destroy the output once
    the principals have their user name and password.
    Saving this data is also a security liability.
    If a principal forgets a user name and or password, these can
    be easily reset later by the administrator.</td>
     <tr align=left><td><b>HOMEROOM</b></td><td><b>PRINCIPAL'S NAME
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


$tag_title = "gnuschool.org Edit Database With Text File";
$page_title = "Edit database with a text file";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("setup");


if (!$yes_input)
{
  $comment = "Edit the database using a text file.";
  title_comment_div_noend($page_title, $comment);
  echo "<b>Notes:
        <ul>
       <LI>The file name and path must be: /tmp/principals.txt
       <LI>The file must contain principal information. 
       <LI>The first line must contain the column names
           FIRSTNAME MIDDLENAME LASTNAME
       <LI>Principal data must follow the column names.
       <LI>An example follows.</ul></b>";

echo "<TABLE>
<TR><TD>FIRSTNAME&nbsp</TD><TD>MIDDLENAME&nbsp</TD><TD>LASTNAME&nbsp</TD>
<TD>TGENDER</TD><TD>ROOM</TD></TR>
<TR><TD>William</TD><TD>K.</TD><TD>Thorough</TD><TD>M</TD><TD>B235</TD></TR>
<TR><TD>Maria</TD><TD>R.</TD><TD>Dedicada</TD><TD>F</TD><TD>A132</TD></TR>
<TR><TD>Jacobo</TD><TD>D.</TD><TD>Formal</TD><TD>M</TD><TD>D411</TD></TR>
</TABLE>";

  proceed_delim_form();
  back_2_parentA("setup");
  end_html_exit();
}

$comment = "Attempting to edit the database using the text file...";
title_comment_div_noend($page_title, $comment);


$path_file = "/tmp/principals.txt";

if ( file_exists($path_file) === FALSE )
{
  echo "<b>Error $path_file doesn't exist!</b></br><br>";
  back_2_parentA("setup");
  end_html_exit();    
}

$lines_all_r = file("$path_file");
if ($lines_all_r === FALSE)
{
  echo "<b>Error opening text file!</b><br><br>";
  back_2_parentA("setup");
  end_html_exit();
}

$lines_size = sizeof($lines_all_r);

$p_col_rr = principal_cols($e_mail);

$p_cols_num = 4;
if($e_mail) $p_cols_num = 5;

$line = "";
$up_r = array();
$data = 0;

for ( $i = 0; $i < $lines_size; $i++)
{
  $line_num = $i+1;
  $line = $lines_all_r[$i];
  $line = trim($line);

  if (!$data  && !empty($line))
  {
    $line_r = explode($delim, $line);
    $line_r = trim_array($line_r);
    $p_cols = sizeof($line_r);

    $p_order_r = match_and_order($p_col_rr, $line_r, $p_cols_num
                               , $p_cols);
  
    echo "<br>LINE:$line_num<br>$line<br>";
    if($p_order_r === FALSE)
    {
      echo "<b> The principal column names do not match with the text 
            file's principal column names.</b><br><br>";    

      if($show_up) up_r_2_table($up_r);
      if($mail_up) email_principals_up($up_r);
      back_2_parentA("setup");
      end_html_exit();
    }
    else
    {
      echo "<b> The principal column names match with the text file's 
            column names. Continuing...</b><br><br>";    
      $data = 1;
    }
  }
  elseif (!empty($line) && $data)
  {
    $line_r = explode($delim, $line);
    $line_r = trim_array($line_r);
    $d_size = sizeof($line_r);

    if($p_cols != $d_size)
    {
      echo "<br>LINE:$line_num<br>$line<br>";
      echo "<b>The number of principal data columns is not equal to the 
          column names.</b><br><br>";    
      
      if($show_up) up_r_2_table($up_r);
      if($mail_up) email_principals_up($up_r);
      back_2_parentA("setup");
      end_html_exit();
    }

    $k_f = $p_order_r['fname'];
    $k_m = $p_order_r['mname'];
    $k_l = $p_order_r['lname'];
    $k_h = $p_order_r['hr'];
    $k_e = $p_order_r['email'];

    $fname = $line_r[$k_f];
    $mname = $line_r[$k_m];
    $lname = $line_r[$k_l];
    $hr = $line_r[$k_h];
    $email = $line_r[$k_e];

    $p_exists = 0;
    $p_name = "";
    $tid = teacher_id_select($fname,$mname,$lname);
    if (!empty($tid))
    {
      echo "<br>LINE:$line_num<br>$line<br>";
      echo "<b>SKIPPING: Principal $fname $mname $lname already 
          exists.</b><br><br>";
      $p_exists = 1;
    }
    else
    {
      $p_name = teacher_t_name_rand();
    }

    if (!$p_exists)
    {
      teacher_insert($p_name,$fname,$mname,$lname,$hr,$email);
      $user = user_rand();
      $pwd = password_rand();
      $name = "$fname $mname $lname";
      $up_r[] = array($hr, $name, $user, $pwd, $email);
      $tid = teacher_id_select($fname,$mname,$lname);
      user_tid_insert( $tid, $user, $pwd, "P");
    }    
  }
}
echo "<b>End of file reached.</b><br><br>";

if($show_up) up_r_2_table($up_r);
if($mail_up) email_principals_up($up_r);

back_2_parentA("setup");
echo "</DIV>";

?>
</body></html>
