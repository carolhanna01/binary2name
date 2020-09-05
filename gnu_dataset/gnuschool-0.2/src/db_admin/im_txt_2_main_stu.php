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

include('login.php');


if(empty($proceed))
  $yes_input = 0;


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
  for ($i=0; $i< $line_size; $i++)
  {
    $new_r[]="";
  }

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
        if ($col == "col_id")
          $new_r[$j] = "id";
        elseif ($col == "col_fname")
          $new_r[$j] = "fname";
        elseif ($col == "col_mname")
          $new_r[$j] = "mname";
        elseif ($col == "col_lname")
          $new_r[$j] = "lname";
        elseif ($col == "col_all_name")
          $new_r[$j] = "all_name";
        else
          if ($col == "col_hr")
            $new_r[$j] = "hr";
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


function get_name_r($fname, $mname, $lname)
{
  $name_r = array();
  $temp_r = explode(" ", $name);
  for ($i=0; $i<sizeof($temp_r); $i++)
  { 
    $name_part = $temp_r[$i];
    $name_part = trim($name_part);
    if(!empty($name_part))
      $name_r[] = $name_part;
  }
  if(sizeof($name_r) == 2) 
  {
    $name_r[2] = $name_r[1];
    $name_r[1] = "";
  }
  if(sizeof($name_r) != 3)
    return FALSE;
  else
    return $name_r;
}


function query_id($id)
{
  $query = "select id from main.spanish where id = $id";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;
  $num = mysql_num_rows($result);
  return $num;
}


function query_name($fname, $mname="", $lname)
{
  $mname_str = "";
  if (!empty($mname)) $mname_str = " and mname='$mname' ";
  $query = "select id from main.spanish where
            fname='$fname' $mname_str and lname='$lname'";
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
  if ($result === FALSE) return FALSE;
  $num = mysql_num_rows($result);
  return $num;
}


function num_2_assoc($order_r, $size)
{
  $new_r = array();
  $new_r['id'] = FALSE;
  $new_r['all_name'] = FALSE;
  $new_r['fname'] = FALSE;
  $new_r['mname'] = FALSE;
  $new_r['lname'] = FALSE;
  $new_r['hr'] = FALSE;

  for ($i=0; $i< $size; $i++)
  { 
    $key = $order_r[$i];
    if(!empty($key))
      $new_r["$key"] = $i;
  }
  return $new_r;
}


function used_def_cols($def_cols_r)
{
  $col_id_opt = $def_cols_r['col_id_opt'];
  $col_hr_opt = $def_cols_r['col_hr_opt'];
  $col_name_opt = $def_cols_r['col_name_opt'];
  $col_id_val = $def_cols_r['col_id'];
  $col_fname_val = $def_cols_r['col_fname'];
  $col_mname_val = $def_cols_r['col_mname'];
  $col_lname_val = $def_cols_r['col_lname'];
  $col_hr_val = $def_cols_r['col_hr'];
  $col_all_name_val = $def_cols_r['col_all_name'];

  $col_id_opt = trim($col_id_opt);
  $col_hr_opt = trim($col_hr_opt);
  $col_name_opt = trim($col_name_opt);
  $col_id_val = trim($col_id_val);
  $col_fname_val = trim($col_fname_val);
  $col_mname_val = trim($col_mname_val);
  $col_lname_val = trim($col_lname_val);
  $col_hr_val = trim($col_hr_val);
  $col_all_name_val = trim($col_all_name_val);
  $col_r = array();
  if($col_name_opt == "no")
  {
    $col_r[] = array('col_fname', $col_fname_val);
    $col_r[] = array('col_mname', $col_mname_val);
    $col_r[] = array('col_lname', $col_lname_val);
  }
  else
  {
    $col_r[] = array('col_all_name', $col_all_name_val);
  }    

  if($col_hr_opt == "yes")
  {
    $col_r[] = array('col_hr', $col_hr_val);
  }

  if($col_id_opt == "yes")
  {
    $col_r[] = array('col_id', $col_id_val);
  }
  return $col_r;
}


$tag_title = "gnuschool.org Edit Database With Text File";
$page_title = "Edit database with a text file";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("setup");


if (!$yes_input)
{
  $comment = "Edit the database using the default text file.";
  title_comment_div_noend($page_title, $comment);
  echo "<b>Notes:
        <ul>
        <LI>The text file must have at
        minimum the first name and last name columns.
       <LI>The first line must have the column names.
       <LI>The default period column will be ignored.
       <LI>All other columns are optional.</ul></b>";
  proceed_form();
  back_2_parentA("setup");
  end_html_exit();
}

$comment = "Attempting to edit the database using the default text file...";
title_comment_div_noend($page_title, $comment);


$def_cols_r = file_def_col_names();
if (empty($def_cols_r))
{
  echo "<br>";
  back_2_parentA("setup");
  end_html_exit();
}

$file_name = $def_cols_r['file_name'];
$file_dir = $def_cols_r['file_dir'];
$path_file = "$file_dir" . "$file_name";
$delim = $def_cols_r['field_delim'];
$col_id_opt = $def_cols_r['col_id_opt'];
$col_name_opt = $def_cols_r['col_name_opt'];
$col_hr_opt = $def_cols_r['col_hr_opt'];

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

$line = "";
$index = 0;
$lines_size = sizeof($lines_all_r);

for($i=0; $i< $lines_size; $i++)
{
  $line = $lines_all_r[$i];
  $line = trim($line);
  if (!empty($line))
  {
    $index = $i;
    $i = $lines_size;
  }
}
$index++;

if (empty($line))
{
  echo "<b>Empty file!</b><br><br>";
  back_2_parentA("setup");
  end_html_exit();
}

$used_col_rr = used_def_cols($def_cols_r);
$used_size = sizeof($used_col_rr);

$line_col_r = explode("$delim", $line);
$line_col_r = trim_array($line_col_r);
$line_cols = sizeof($line_col_r);

$order_r = match_and_order($used_col_rr, $line_col_r,
                           $used_size, $line_cols);

if($order_r === FALSE)
{
  echo "<b> The default column names do not match with the text file 
        column names.</b><br><br>";    
  back_2_parentA("setup");
  end_html_exit();
}
else
{
  echo "<b> The default column names match with the text file column 
            names. Continuing...</b><br><br>";    
}

$keys_r = num_2_assoc($order_r, $line_cols);

$k_id = $keys_r['id'];
$k_all_name = $keys_r['all_name'];
$k_fname = $keys_r['fname'];
$k_mname = $keys_r['mname'];
$k_lname = $keys_r['lname'];
$k_hr = $keys_r['hr'];

$kt_id = 0;
$kt_all_name = 0;
$kt_fname = 0;
$kt_mname = 0;
$kt_lname = 0;
$kt_hr = 0;

if ( is_int($k_id) )   $kt_id = 1;
if ( is_int($k_all_name) )  $kt_all_name = 1;
if ( is_int($k_fname) )  $kt_fname = 1;
if ( is_int($k_mname) )  $kt_mname = 1;
if ( is_int($k_lname) )  $kt_lname = 1;
if ( is_int($k_hr) )  $kt_hr = 1;

if ( !$kt_all_name )
{
  if ( !($kt_fname && $kt_lname) )
  {
    echo "<b>The required first and last name fields are not 
             present.</b><br><br>";
    back_2_parentA("setup");
    end_html_exit();
  }
}
else
{
  if ($kt_fname || $kt_lname)
  {
    echo "<b>There are too many name fields present.</b><br><br>";
    back_2_parentA("setup");
    end_html_exit();
  }
}

$edited = 0;
for ($w=$index; $w< $lines_size; $w++)
{
  $line = $lines_all_r[$w];
  $line_r = explode($delim, $line);
  $line_r = trim_array($line_r);
  $line_size = sizeof($line_r);

  if ($line_size != $line_cols)
  {
    echo "<b>SKIPPING:The number of default columns is not equal 
      to the number of columns in this line.</b><br>$line<br><br>";
  }
  else
  {
    $ok_line = 1;
    $id = "";
    $fname = "";
    $mname = "";
    $lname = "";
    $hr = "";
  
    if ($kt_all_name)
    {
      $all_name = $line_r[$k_all_name];
      $name_r = get_name_r($all_name);
      if ($name_r === FALSE) 
      {
        $ok_line = 0;
        echo "<b>SKIPPING: Bad name $all_name.</b><br><br>";
      }
      else
      {
        $fname = $name_r[0];
        $mname = $name_r[1];
        $lname = $name_r[2];
      }
    }
    else
    {
      $fname = $line_r[$k_fname];
      $mname = $line_r[$k_mname];
      $lname = $line_r[$k_lname];
      if (empty($fname) || empty($lname))
      {
        $ok_line = 0;
        echo "<b>SKIPPING: Bad name $fname $mname $lname.</b><br><br>";
      }
    }
      
    if ($ok_line && !name_alpha_dot($fname))
    {
        echo "<b>SKIPPING: Bad character found in the first name of 
              $fname $mname $lname.</b><br><br>";
        $ok_line = 0;
    }

    if ($ok_line && !name_alpha_dot($mname, 1))
    {
        echo "<b>SKIPPING: Bad character found in the middle name of 
              $fname $mname $lname.</b><br><br>";
        $ok_line = 0;
    }

    if ($ok_line && !name_alpha_dot($lname))
    {
        echo "<b>SKIPPING: Bad character found in the last name of 
              $fname $mname $lname.</b><br><br>";
        $ok_line = 0;
    }

    if ($kt_id && $ok_line)
    {
      $id = $line_r[$k_id];
      $temp = query_id($id);
      if ($temp)
      { 
          echo "<b>SKIPPING: ID $id already exists.</b><br><br>";
          $ok_line = 0;
      }
      else
      { 
        if($temp === FALSE) 
        {
          echo "<b>SKIPPING: MySQL was unable to process id 
               query.</b><br><br>";
          $ok_line = 0;
        }
        else
          echo "<b>CONTINUING: ID $id does not exist.</b><br>";
      }
    }

    $name_warn = "";
    if ($ok_line)
    {
      $temp = query_name($fname, "", $lname);
      if ($temp)
      {
        $name_warn = "<b>WARNING: $fname $lname exists.</b><br>";
        $temp = query_name($fname, $mname, $lname);
        if ($temp)
        {
          echo "<b>SKIPPING: $fname $mname $lname already 
                exists.</b><br><br>";
          $ok_line = 0;
        }
        else
        {
          if ($temp === FALSE)
          {
            echo "<b>SKIPPING: MySQL was unable to process names 
                  query.</b><br><br>";
            $ok_line = 0;
          }
          else
          {
            echo $name_warn;
            echo "<b>CONTINUING: $fname $mname $lname doesn't 
                exist.</b><br>";
          }
        }
      }
      else
      {
        echo "<b>CONTINUING: $fname $mname $lname doesn't 
            exist.</b><br>";
      }
    }

    if ($ok_line)
    {
      $query = "insert into main.spanish (";
      $values = " values (";
      $comma = 0;
      
      if ($kt_id)
      {
        $query .= "id,";
        $values .= "$line_r[$k_id],";
      }

      if ($kt_hr)
      {
        $query .= "hr,";
        $values .= "'$line_r[$k_hr]',";
      }
       
      $query .= "fname,mname,lname)";
      $values .= "'$fname','$mname','$lname')";
      $query .= $values;
      $result = mysql_query($query);
      query_outcome_echo($query,$result);
      $edited = 1;
      echo "<br>";
    } 
  }
}
if ($edited)
  echo "<b>You have successfully updated the 
        database.<b><br><br>";
else
  echo "<b>No changes have been made to the database.<b><br><br>";

back_2_parentA("setup");
echo "</DIV>";

?>
</body></html>

