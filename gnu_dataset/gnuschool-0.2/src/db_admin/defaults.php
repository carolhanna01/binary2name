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


$update=$_POST['update'];
$parent=$_POST['parent'];

include('login.php');

//  0=48,9=57
function all_nums($str = "")
{
  $str = trim($str);
  $length = strlen($str);
  if ($length < 1) return FALSE;

  for ($i=0; $i<$length; $i++)
  { 
    $char = substr($str, $i, 1);
    $char_val = ord($char);
    if (!($char_val > 47 && $char_val < 58) )
        return 0;
  }
  return 1;
}


// A=65,Z=90  a=97,z=122
function all_alpha($str = "")
{
  $length = strlen($str);
  if ($length < 1) return FALSE;

  for ($i=0; $i<$length; $i++)
  { 
    $char = substr($str, $i, 1);
    $char_val = ord($char);
    if (!(($char_val > 64 && $char_val < 91) ||
       ($char_val > 96 && $char_val < 123)) )
       return 0;
  }
  return 1;
}


function get_valid_file_dir(&$str = "")
{
  $dir_r = explode("/", $str);
  $new_dir = "";

  $length = strlen($str);
  $first_char = substr($str, 0, 1);
  $last_char = substr($str, $length-1, 1);

  for ($i=0; $i<sizeof($dir_r); $i++)
  {
    $temp_dir = $dir_r[$i];
    if (!empty($temp_dir))
    {
      $valid = is_valid_file_name($temp_dir);
      if ( !$valid ) return $valid;
      else $new_dir .= "$temp_dir/";
    }
  }
  if (empty($new_dir)) return FALSE;
  else $str = "/$new_dir";
  
  return 1;
}


function clean_new_defaults_r($post_r, &$def)
{
  $file_name = $post_r['file_name'];
  $file_dir = $post_r['file_dir'];
  $field_delim = $post_r['field_delim'];
  $line_delim = $post_r['line_delim'];

  $col_all_name = $post_r['col_all_name'];
  $col_name_opt = $post_r['col_name_opt'];
  $col_id = $post_r['col_id'];
  $col_id_opt = $post_r['col_id_opt'];
  $col_fname = $post_r['col_fname'];
  $col_mname = $post_r['col_mname'];
  $col_lname = $post_r['col_lname'];
  $col_hr = $post_r['col_hr'];
  $col_hr_opt = $post_r['col_hr_opt'];
  $col_pe = $post_r['col_pe'];
  $col_pe_opt = $post_r['col_pe_opt'];

if ((strlen($file_name) > 28) || (!is_valid_file_name($file_name)))
    $file_name = "";

if ((strlen($file_dir) > 48) || (!get_valid_file_dir($file_dir))) 
    $file_dir = "";

if ((strlen($line_delim) > 5)) 
    $line_delim = "";
if ((strlen($field_delim) > 5)) 
    $field_delim = "";

  if ( $col_name_opt != "yes" )
    $col_name_opt = "no";
  if ((strlen($col_all_name) > 10) || 
                             (!is_valid_file_name($col_all_name)) )
    $col_all_name = "";

  if ( $col_id_opt != "yes" )
    $col_id_opt = "no";
  if ((strlen($col_id) > 10) || (!is_valid_file_name($col_id)) )
    $col_id = "";
  if ((strlen($col_fname) > 10) || (!is_valid_file_name($col_fname)) )
    $col_fname = "";
  if ((strlen($col_mname) > 10) || (!is_valid_file_name($col_mname)) )
    $col_mname = "";
  if ((strlen($col_lname) > 10) || (!is_valid_file_name($col_lname)) )
    $col_lname = "";

  if ( $col_hr_opt != "yes" )
    $col_hr_opt = "no";
  if ((strlen($col_hr) > 10) || (!is_valid_file_name($col_hr)) )
    $col_hr = "";
  
  if ( $col_pe_opt != "yes" )
    $col_pe_opt = "no";
  if ((strlen($col_pe) > 10) || (!is_valid_file_name($col_pe)) )
    $col_pe = "";
  
  if (!empty($file_name)) $def['file_name'] = $file_name;
  if (!empty($file_dir)) $def['file_dir'] = $file_dir;
  if (!empty($line_delim)) $def['line_delim'] = $line_delim;
  if (!empty($field_delim)) $def['field_delim'] = $field_delim;

  if (!empty($col_name_opt)) $def['col_name_opt'] = $col_name_opt;
  if (!empty($col_all_name)) $def['col_all_name'] = $col_all_name;
  if (!empty($col_id)) $def['col_id'] = $col_id;
  if (!empty($col_id_opt)) $def['col_id_opt'] = $col_id_opt;
  if (!empty($col_fname)) $def['col_fname'] = $col_fname;
  if (!empty($col_mname)) $def['col_mname'] = $col_mname;
  if (!empty($col_lname)) $def['col_lname'] = $col_lname;
  if (!empty($col_hr)) $def['col_hr'] = $col_hr;
  if (!empty($col_hr_opt)) $def['col_hr_opt'] = $col_hr_opt;
  if (!empty($col_pe)) $def['col_pe'] = $col_pe;
  if (!empty($col_pe_opt)) $def['col_pe_opt'] = $col_pe_opt;

  return $def;
}


function set_defaults(&$def)
{
  $file_name = $def['file_name'];
  $file_dir = $def['file_dir'];
  $line_delim = $def['line_delim'];
  $field_delim = $def['field_delim'];

  $col_name_opt  = $def['col_name_opt'];
  $col_all_name  = $def['col_all_name'];
  $col_id_opt = $def['col_id_opt'];
  $col_id = $def['col_id'];
  $col_fname = $def['col_fname'];
  $col_mname = $def['col_mname'];
  $col_lname = $def['col_lname'];
  $col_hr_opt = $def['col_hr_opt'];
  $col_hr = $def['col_hr'];
  $col_pe_opt = $def['col_pe_opt'];
  $col_pe = $def['col_pe'];

  $query =<<<EOQ
update main.defaults set
file_name='$file_name',
file_dir='$file_dir',
line_delim='$line_delim',
field_delim='$field_delim',
col_name_opt='$col_name_opt',
col_all_name='$col_all_name',
col_id_opt='$col_id_opt',
col_id='$col_id',
col_fname='$col_fname',
col_mname='$col_mname',
col_lname='$col_lname',
col_hr_opt='$col_hr_opt',
col_hr='$col_hr',
col_pe_opt='$col_pe_opt',
col_pe='$col_pe'
EOQ;
  $result = mysql_query($query);
  query_outcome_echo($query,$result);
}


function data_transfer_form(&$def, $parent)
{
  $file_name = $def['file_name'];
  $file_dir = $def['file_dir'];
  $line_delim = $def['line_delim'];
  $field_delim = $def['field_delim'];

  $name_opt = $def['col_name_opt'];
  $id_checked = "";
  $name_checked = "";
  $hr_checked = "";
  $id_opt = $def['col_id_opt'];
  $name_opt = $def['col_name_opt'];
  $hr_opt = $def['col_hr_opt'];
  $pe_opt = $def['col_pe_opt'];
  if ($id_opt == "yes") $id_checked = "checked";
  if ($name_opt == "yes") $name_checked = "checked";
  if ($hr_opt == "yes") $hr_checked = "checked";
  if ($pe_opt == "yes") $pe_checked = "checked";
  $all_name = $def['col_all_name'];
  $id = $def['col_id'];
  $fname = $def['col_fname'];
  $mname = $def['col_mname']; 
  $lname = $def['col_lname'];
  $hr = $def['col_hr'];
  $pe = $def['col_pe'];


  echo <<<EOQ
<form method="post" action="$PHP_SELF">
<br><br><hr>
<b>File name and directory used to import and/or export data.</b><br>
File name: <input type=text name="file_name"
value="$file_name" size=38 maxlength=28>
<br><br>
File directory: <input type=text name="file_dir"
value="$file_dir" size=38 maxlength=28>
<br><br><hr>
<b>Delimiters for text files.</b><br>
Field delimiter: <input type=text name="field_delim"
value="$field_delim" size=8 maxlength=5>
<br><br><hr>
<input type=checkbox name=col_name_opt value="yes" $name_checked>
<b>The default file has a column with a full name. If checked,
the "Full name column" is used. The first, middle, 
and last name default columns will be ignored.</b>
<br><br>
<input type=checkbox name=col_id_opt value="yes" $id_checked>
<b>The default file has a column with the student ID. If not checked
when importing data, the student IDs will be created 
automatically. If not checked when exporting data, the ID column 
will be omitted.</b>
<br><br>
<input type=checkbox name=col_hr_opt value="yes" $hr_checked>
<b>The default file has a room column. If checked, the data would be
a classroom name/number such as D432, 224, 142B, etc.</b>
<br><br>
<input type=checkbox name=col_pe_opt value="yes" $pe_checked>
<b>The default has a block/period column.
 If checked, the data would be a block/period name such as 1A
 , 2, 4B, etc.</b>
<br><br><hr>
<b>Default column names for an import/export 
file.</b><br>
ID column: <input type=text name="col_id"
value="$id" size=15 maxlength=10>
<br><br>
Full name column: <input type=text name="col_all_name"
value="$all_name" size=15 maxlength=10>
<br><br>
First name column: <input type=text name="col_fname"
value="$fname" size=15 maxlength=10>
<br><br>
Middle name column: <input type=text name="col_mname"
value="$mname" size=15 maxlength=10>
<br><br>
Last name column: <input type=text name="col_lname"
value="$lname" size=15 maxlength=10>
<br><br>
Room name column: <input type=text name="col_hr"
value="$hr" size=15 maxlength=10>
<br><br>
Block name column: <input type=text name="col_pe"
value="$pe" size=15 maxlength=10>
<br><br>
<input type=hidden name="parent" value="$parent">
<input type=submit name="update" value="Update defaults">
</form>
EOQ;
}


$tag_title = "gnuschool.org Defaults";
$page_title = "Defaults";


start_html($tag_title);

stylesheet_link("admin_stylesheet");

start_body("admin_colors");
back_2_parentA($parent);


$comment = "These are the current file defaults. To change them, please 
            edit the form and click the update button.";
title_comment_div_noend($page_title, $comment);

$defaults_r = file_def_col_names();

if(!empty($update))
{
  clean_new_defaults_r($_POST, $defaults_r);
  set_defaults($defaults_r);
}

data_transfer_form($defaults_r, $parent);
back_2_parentA($parent);

echo "</DIV>";
?>

</body></html>

