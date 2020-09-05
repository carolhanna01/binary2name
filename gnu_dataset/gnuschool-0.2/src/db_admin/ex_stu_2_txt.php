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


$tag_title = "gnuschool.org Create Text File";
$page_title = "Create Text File";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("export");


if (!$yes_input)
{
  $comment = "Create the default text file using the student data.";
  title_comment_div_noend($page_title, $comment);
  echo "<b>Note:
        <ul>
        <LI>The text file will have at minimum a full name column 
            or separate first, middle, and last name columns.
       <LI>The period column will be ignored.
       <LI>All other columns are optional.</ul></b>";
  proceed_form();
  back_2_parentA("export");
  echo "</DIV>";
}
else
{
  
  $comment = "Attempting to create a text file using database data...";
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
  $col_pe_opt = $def_cols_r['col_pe_opt'];
  $col_id = $def_cols_r['col_id'];
  $col_all_name = $def_cols_r['col_all_name'];
  $col_fname = $def_cols_r['col_fname'];
  $col_mname = $def_cols_r['col_mname'];
  $col_lname = $def_cols_r['col_lname'];
  $col_hr = $def_cols_r['col_hr'];
  $col_pe = $def_cols_r['col_pe'];

  $cols = "";
  $select_str = "";
  $y_full = 0;
  $y_hr = 0;
  $y_id = 0;

  if ( file_exists($file_dir) === FALSE )
  {
    echo "<b>Error $file_dir doesn't exist!</b></br><br>";
    back_2_parentA("setup");
    end_html_exit();    
  }

  if ( $col_id_opt == "yes") 
  {
    $cols .= "$col_id$delim";
    $select_str .= " id,";
    $y_id = 1;
  }

  if ( $col_name_opt == "yes") 
  {
    $cols .= "$col_all_name";
    $select_str .= "fname,mname,lname";
    $y_full = 1;
  }
  else
  {
    $cols .= "$col_fname$delim$col_mname$delim$col_lname";
    $select_str .= "fname,mname,lname"; 
  }

  if ( $col_hr_opt == "yes") 
  {
    $cols .= "$delim$col_hr";
    $select_str .= ",hr";
    $y_hr = 1;
  }

  $cols .= "\n";

  $f_id = fopen( "$path_file", "w");

  if (!$f_id)
  {
    echo "<b>Error creating text file!</b>";
    end_html_exit();
  }

  $f_result = fwrite ($f_id, $cols);
  query_outcome_echo ( "fwrite to $path_file $cols", $f_result);
  if($f_result === FALSE)
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
    $line = "";

    if ( $y_id ) $line = $row['id'] . $delim;

    if ( $y_full )
      $line .= "$fname $mname $lname";
    else
      $line .= "$fname$delim$mname$delim$lname";

    if ( $y_hr )
    {
      $hr_val = $row['hr'];
      if (empty($hr_val)) echo "<b>Warning: empty homeroom value. </b>";
      $line .= $delim . $row['hr'];
    }

    $line .= "\n";

    $f_result = fwrite ($f_id, $line);
    query_outcome_echo ( "fwrite to $path_file $line", $f_result);
    if($f_result === FALSE)
      end_html_exit();
  }
  fclose($f_id);
  echo "<b>Successfully created default text file.</b><br><br>";
  back_2_parentA("export");
  echo "</DIV>";
}
?>

</body></html>

