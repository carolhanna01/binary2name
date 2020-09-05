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

$proceed=$_POST['proceed'];

if(empty($proceed))
  $yes_input = 0;


function default_dbf_name()
{
  $query = "select file_name, file_dir from main.defaults";
  $result = mysql_query($query);
  query_outcome_echo ( $query, $result);
  if($result === FALSE)
    return 0;

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $dir_file = $row['file_dir'] . $row['file_name'];
  if (empty($dir_file))
  {
    echo "<b>Empty file name</b><br><br>";
    back_2_parentA("display");
    end_html_exit();
  }

  return $dir_file;
}


$tag_title = "gnuschool.org Display Default File";
$page_title = "Display Default File";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("display");


if (!$yes_input)
{
  $comment = "Display the default file.";
  title_comment_div_noend($page_title, $comment);
  echo "<b>Text and dbase files are supported. You may
        experience unusual behaviour with other file types.</b>";
  proceed_form();
  back_2_parentA("display");
  echo "</DIV>";
}
else
{
  
  $comment = "Attempting to display the default file...";
  title_comment_div_noend($page_title, $comment);


  $dir_file = default_dbf_name();

  if ( file_exists($dir_file) === FALSE )
  {
    echo "<b>Error $dir_file doesn't exist!</b></br><br>";
    back_2_parentA("display");
    end_html_exit();    
  }

  $r_h = fopen($dir_file, "r");

  if ($r_h === FALSE)
  {
    echo "<b>Error opening default file $dir_file!</b><br><br>";
    back_2_parentA("display");
    end_html_exit();
  }
  
  $char1 = fgetc($r_h); 
  fclose($r_h);

  if (is_valid_file_name($char1))
  {
    $lines_all_r = file($dir_file);
    if ($lines_all_r === FALSE)
    {
      echo "<b>Error opening text file!</b><br><br>";
      back_2_parentA("display");
      end_html_exit();
    }
    $lines_all_r = file("$dir_file");
    $lines_size = sizeof($lines_all_r);


    echo "<br><b>This is the text file $dir_file</b></br>";
    for($i=0; $i< $lines_size; $i++)
    {
      $line = $lines_all_r[$i];
      echo "$line <br>";
    }
  }
  else
  {
    $dbf = dbase_open("$dir_file", 0);

    if ($dbf === FALSE)
    {
      echo "<b>Error opening dbase file!</b><br><br>";
      end_html_exit();
    }

    $column_info = dbase_get_header_info($dbf);

    if ($column_info === FALSE)
    {
      echo "<b>Error obtaining header information from the dbase 
           file!</b><br><br>";
      end_html_exit();
    }
  
    echo "<br><b>This record is the column/header data for 
          $dir_file</b><br>";
    print_r($column_info);
    print "<br><br>";
    $record_numbers = dbase_numrecords($dbf);
    if ($record_numbers === FALSE)
    {
      echo "<b>Error obtaining record numbers from the dbase 
            file!</b><br><br>";
      end_html_exit();
    }

    echo "<b>These are the records.</b><br><br>";
    for ($i=1; $i<$record_numbers + 1; $i++)
    {
      $row = dbase_get_record_with_names($dbf, $i);
      if ($row === FALSE)
      {
        echo "<b>Error obtaining record with names from the dbase 
             file!</b><br>";
      }
      print_r($row);
      print "<br><br>";
    }
  }
  echo "<br><br>";
  back_2_parentA("display");
  echo "</DIV>";
}
?>

</body></html>

