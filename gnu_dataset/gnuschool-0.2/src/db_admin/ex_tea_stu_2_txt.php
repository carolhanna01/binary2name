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
$e_mail = $_POST['e_mail'];

include('login.php');


if(empty($proceed))
  $yes_input = 0;


if(empty($e_mail)) $e_mail = 0;
else $e_mail = 1;


if(empty($delim)) $delim = " ";


function proceed_delim_form()
{
  echo <<<EOQ
<form method=post action="$PHP_SELF">
<input type=checkbox name=e_mail value="yes"><b>Include the teacher 
column name TEMAIL. It holds the teacher's email address.</b>
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


function teacher_cols($email, $delim)
{
  $col_str="TFIRSTNAME" . $delim . "TMIDDLENAME" . $delim .
           "TLASTNAME" . $delim . "THOMEROOM";
  if($email) $col_str .= $delim . "TEMAIL\n";
  else $col_str .= "\n";
  return $col_str;
}


function student_cols($delim)
{
  $col_str="FIRSTNAME" . $delim . "MIDDLENAME" . $delim .
           "LASTNAME" . $delim . "PERIOD\n";
  return $col_str;
}


$tag_title = "gnuschool.org Create Text File";
$page_title = "Create Text File";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("export");


if (!$yes_input)
{
  $comment = "Create the \"teacher_students_out.txt\" text file.";
  title_comment_div_noend($page_title, $comment);
  echo "<b>Notes:
        <ul>
       <LI>The path and file name will be: /tmp/teacher_students_out.txt
       <LI>The file will contain teacher and student information. 
           A teacher section will be followed by that teacher's 
           student section.
       <LI>The teacher column names will be
           TFIRSTNAME TMIDDLENAME TLASTNAME THOMEROOM
       <LI>Teacher data will follow the teacher's column names.
       <LI>Student column names will follow the teacher's data.
           The student column names will be
           FIRSTNAME MIDDLENAME LASTNAME PERIOD
       <LI>Student data will follow the student column names.
       <LI>The THOMEROOM column is for the teacher's homeroom number.
       <LI>The PERIOD column is the class period when the student will 
           have the above named teacher.
       <LI>Students' homeroom period will be written as HR
       <LI>An example follows.</ul></b>";

echo "<TABLE>
<TR><TD>TFIRSTNAME&nbsp</TD><TD>TMIDDLENAME&nbsp</TD><TD>TLASTNAME&nbsp</TD>
<TD>THOMEROOM</TD></TR>
<TR><TD>Pedro</TD><TD>E.</TD><TD>Rios</TD><TD>D432</TD></TR>
<TR><TD>FIRSTNAME</TD><TD>MIDDLENAME</TD><TD>LASTNAME</TD><TD>PERIOD</TD></TR>
<TR><TD>Toloro</TD><TD>Zorro</TD><TD>Senoro</TD><TD>HR</TD></TR>
<TR><TD>Flanito</TD><TD>X.</TD><TD>Pamplos</TD><TD>HR</TD></TR>
<TR><TD>John</TD><TD>H.</TD><TD>Smith</TD><TD>5</TD></TR>
<TR><TD>Mary</TD><TD>B.</TD><TD>Doe</TD><TD>3a</TD></TR>
<TR><TD>Jean</TD><TD>Lucid</TD><TD>Estudiante</TD><TD>3b</TD></TR>
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
  back_2_parentA("export");
  echo "</DIV>";
}
else
{
  
  $comment = "Attempting to create a text file using database data...";
  title_comment_div_noend($page_title, $comment);


  $t_col_str = teacher_cols($e_mail, $delim);
  $s_col_str = student_cols($delim);

  $file_dir = "/tmp";
  $path_file = "$file_dir/teacher_students_out.txt";

  if ( file_exists($file_dir) === FALSE )
  {
    echo "<b>Error $file_dir doesn't exist!</b></br><br>";
    back_2_parentA("setup");
    end_html_exit();    
  }

  $f_id = fopen( "$path_file", "w");
  if (!$f_id)
  {
    echo "<b>Error creating text file!</b>";
    end_html_exit();
  }

  $query = "select * from main.teachers";
  $result_t = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result_t === FALSE)
  {
    echo "<b>Error obtaining teacher data!</b>";
    end_html_exit();
  }

  while($row_t = mysql_fetch_array($result_t, MYSQL_ASSOC))
  {
    $f_result = fwrite ($f_id, $t_col_str);
    query_outcome_echo ( "fwrite to $path_file $t_col_str", $f_result);
    if($f_result === FALSE)
      end_html_exit();

    $db_name = $row_t['db_name'];
    $fname = $row_t['fname'];
    $mname = $row_t['mname'];
    $lname = $row_t['lname'];
    $hr = $row_t['hr'];
    $email = $row_t['email'];

    $line = "$fname$delim$mname$delim$lname";
    $line .= "$delim$hr$delim$email\n";
    $f_result = fwrite ($f_id, $line);
    query_outcome_echo ( "fwrite to $path_file $line", $f_result);
    if($f_result === FALSE)
      end_html_exit();

    $f_result = fwrite ($f_id, $s_col_str);
    query_outcome_echo("fwrite to $path_file $s_col_str", $f_result);
    if($f_result === FALSE)
      end_html_exit();

    $query = "select * from $db_name.spanish order by period, lname";
    $result = mysql_query($query);
    query_outcome_echo($query,$result);
    if ($result === FALSE)
    {
      echo "<b>Error obtaining student data!</b>";
      end_html_exit();
    }

    while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
    {  
      $fname = $row['fname'];
      $mname = $row['mname'];
      $lname = $row['lname'];
      $period = $row['period'];

      $line = "$fname$delim$mname$delim$lname$delim$period\n";
      $f_result = fwrite ($f_id, $line);
      query_outcome_echo ( "fwrite to $path_file $line", $f_result);
      if($f_result === FALSE)
        end_html_exit();
    }
  }
  fclose($f_id);
  echo "<b>Successfully created \"teacher_students_out.txt\"
        text file.</b><br><br>";
  back_2_parentA("export");
  echo "</DIV>";
}
?>

</body></html>

