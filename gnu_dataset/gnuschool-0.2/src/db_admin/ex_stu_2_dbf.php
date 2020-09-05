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


$proceed=$_POST['proceed'];

include('login.php');


if(empty($proceed))
  $yes_input = 0;


$tag_title = "gnuschool.org Create Dbase";
$page_title = "Create Dbase File";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("export");


if (!$yes_input)
{
  $comment = "Create the default dbase file.";
  title_comment_div_noend($page_title, $comment);
  echo "<b>File requirements:
        <ul>
        <LI>The dbase file will have at minimum a full name column 
            or separate first, middle, and last name columns.
       <LI>The period column will be ignored.
       <LI>All other columns are optional.</ul>
    Warning: If a file exists in the default 
       directory with the default name, it will be overwritten.</b>
";
  proceed_form();
  back_2_parentA("export");
  echo "</DIV>";
}
else
{
  
  $comment = "Attempting to create a dbase file using database data...";
  title_comment_div_noend($page_title, $comment);


  $def_cols_r = file_def_col_names();
  if (empty($def_cols_r))
  {
    echo "<br>";
    back_2_parentA("export");
    end_html_exit();
  }

  $file_name = $def_cols_r['file_name'];
  $file_dir = $def_cols_r['file_dir'];
  $path_file = "$file_dir" . "$file_name";
  $delim = $def_cols_r['field_delim'];
  $col_id_opt = $def_cols_r['col_id_opt'];
  $col_name_opt = $def_cols_r['col_name_opt'];
  $col_hr_opt = $def_cols_r['col_hr_opt'];
  $col_id = $def_cols_r['col_id'];
  $col_all_name = $def_cols_r['col_all_name'];
  $col_fname = $def_cols_r['col_fname'];
  $col_mname = $def_cols_r['col_mname'];
  $col_lname = $def_cols_r['col_lname'];
  $col_hr = $def_cols_r['col_hr'];

  $cols = "";
  $select_str = "";
  $y_full = 0;
  $y_hr = 0;
  $y_id = 0;

  $definition = array();

  if ( file_exists($file_dir) === FALSE )
  {
    echo "<b>Error $file_dir doesn't exist!</b></br><br>";
    back_2_parentA("setup");
    end_html_exit();    
  }

  if ( $col_id_opt == "yes") 
  {
    $definition[] = array("$col_id", "N", 11, 0);
    $select_str .= "id,";
    $y_id = 1;
  }

  if ( $col_name_opt == "yes") 
  {
    $definition[] = array("$col_all_name", "C", 60);
    $select_str .= "fname,mname,lname";
    $y_full = 1;
  }
  else
  {
    $definition[] = array("$col_fname", "C", 20);
    $definition[] = array("$col_mname", "C", 20);
    $definition[] = array("$col_lname", "C", 20);
    $select_str .= "fname,mname,lname"; 
  }

  if ( $col_hr_opt == "yes") 
  {
    $definition[] = array("$col_hr", "C", 20);
    $select_str .= ",hr";
    $y_hr = 1;
  }

  if (!dbase_create($path_file, $definition))
  {
    echo "<b>Error creating dbase file!</b><br>Note: If the default file 
     exists, you may need alter the user or permissions. Try creating 
    a non-existing file.";
    end_html_exit();
  }

  $dbf = dbase_open("$path_file", 2);
  query_outcome_echo ( "dbase_open $path_file", $dbf);
  if ($dbf === FALSE)
    end_html_exit();

  $query = "select $select_str from main.spanish";
  $result = mysql_query($query);
  query_outcome_echo ( $query, $result);
  if($result === FALSE)
    end_html_exit();

  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {  
    $fname = $row['fname'];
    $mname = $row['mname'];
    $lname = $row['lname'];
    $f_str = "$fname $mname $lname";

    $field_r = array();

    if ($y_id)
    {
      $f_str = $row['id'] . " $f_str";
      $field_r[] = $row['id'];
    }

    if ($y_full)
    {
      $field_r[] = "$fname $mname $lname";
    }
    else
    {
      $field_r[] = "$fname";
      $field_r[] = "$mname";
      $field_r[] = "$lname";
    }

    if ($y_hr)
    {
      $f_str = "$f_str " . $row['hr'];
      $field_r[] = $row['hr'];
    }

    $d_result = dbase_add_record( $dbf, $field_r );
    query_outcome_echo ( "dbase_add_record $f_str", $d_result);

  }
  echo "<b>Successfully created default dbase file.</b><br><br>";
  back_2_parentA("export");
  echo "</DIV>";
}
?>
</body></html>


