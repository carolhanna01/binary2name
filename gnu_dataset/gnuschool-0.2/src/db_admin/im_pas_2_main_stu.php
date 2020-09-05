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
$field_order=$_POST['field_order'];
$delimiter=$_POST['delimiter'];

if (empty($delimiter)) $delimiter = " ";

include('login.php');

$field_order_r = array();
for($i=0; $i < sizeof($field_order); $i++)
{
  $field_name = trim($field_order[$i]);
  if(!empty($field_name))
    $field_order_r[]=$field_name;
}


if(empty($textarea_data) || empty($field_order_r))
  $yes_input = 0;

function field_order_form()
{
  echo <<<EOQ
<form method=post action="$PHP_SELF">
EOQ;

spanish_field_select();

echo <<<EOQ
<BR><BR>
<textarea name=textarea_data cols=80 rows=10></textarea>
<BR><BR><B>Field delimiter (the default is the tab or space).
<BR>
<input type=text size=6 name=delimiter>
<BR>If the delimiter is not a space(s) or a tab(s) you need to type in 
what it is.
<BR> 
It can be a single character such as -
<BR>Or a combination of characters such as &&&
<BR><BR>
<input type=submit name=submit value="Populate Tables">
</form>
EOQ;
}


function spanish_field_select()
{
  $field_str="id int(11)|fname varchar(30)|mname char(30)
              |lname varchar(30)|hr varchar(30)";
  $field_arr = explode("|",$field_str);
  $arr_size=sizeof($field_arr);

  for ($j=0;$j< $arr_size ; $j++)
  {
    print '<select name="field_order[]" size=1>';
    for ($k=0;$k< $arr_size; $k++)
    {
      print "<option> $field_arr[$k] </option>";
    }
    print "<option selected> </option>";
    print "</select>";
  }
}


function add_mname_to_arr($field_order,$values_arr)
{
  $new_values_r = array();
  $add_one = 0;
  $field_r = explode (",",$field_order);
  for ($i=0;$i<sizeof($field_r);$i++)
  {
    if ($field_r[$i] == "mname")
    {
      $new_values_r[$i] = "''";
      $add_one = 1;
    }
    else
    {
      $new_index = $i - $add_one;
      $new_values_r[$i] = $values_arr[$new_index];
    }
  }
  return $new_values_r;
}


function select_from_main($field_order, $values_r
                          ,$mname_is_empty, $errors)
{
  $field_r = explode (",",$field_order);
  $field_order = ",$field_order,";
  $id_in = strpos($field_order,",id,");
  $where_str = "";
  if ($id_in !== FALSE)
  {
    for ($i=0;$i<sizeof($values_r);$i++)
    {
      $field_name = $field_r[$i];

      if ($field_name == "id")
      { 
        $where_str = "id = $values_r[$i]";
        $i = sizeof($values_r);
      }
    }
    $query = "select * from main.spanish where $where_str";
    $result = mysql_query($query);
    $errors = query_outcome_echo($query,$result,$errors);
    if(mysql_num_rows($result)) return 1;
    $where_str = "";
  }
    
  for ($i=0;$i<sizeof($values_r);$i++)
  {
    $field_name = $field_r[$i];

    if(!($field_name == "mname" && $mname_is_empty))
      if ($field_name != "hr" && $field_name != "id")
      {  
        $where_str .= "$field_name = $values_r[$i] and ";
      }
  }
  $where_str = substr($where_str, 0, strlen($where_str) - 5);

  $query = "select * from main.spanish where $where_str";
  $result = mysql_query($query);
  $errors = query_outcome_echo($query,$result,$errors);
  $return_r[]=$errors;
  if(mysql_num_rows($result)) $return_r[] = 1;
  else $return_r[] = 0;
  return $return_r;
}


function must_have_fields($insert_fields)
{
  $insert_fields = "," . $insert_fields . ",";
  if( strpos($insert_fields, ",fname,") === FALSE) return 0;
  if( strpos($insert_fields, ",lname,") === FALSE) return 0;
  return 1;
}


$tag_title = "gnuschool.org Paste or Type and Populate Database";
$page_title = "Paste or type text and populate database";


start_html($tag_title);
stylesheet_link("admin_stylesheet");
start_body("admin_colors");
back_2_parentA("setup");


if (!$yes_input)
{
  $comment = "1. Paste or type formatted text in the textarea.<br>
        2. Please choose combo-box selections that match
        the columns in the textarea.<br><br>
        Example:<br>
        Let's say column 1 (the left-most column) in the textarea has 
        John, column 2 has Smith, and column 3 has an identification
        number 2345876<br>
        In this case, the combo-box selections from left to right 
        should be fname varchar(30), lname varchar(30),and id 
        int(11).";

  title_comment_div_noend($page_title, $comment);
  field_order_form();
  back_2_parentA("setup");
  echo "</DIV>";
}
else
{
  $errors = 0;
  $inserted_item = 0;
  $comment = "Attempting to populate School Database...";
  title_comment_div_noend($page_title, $comment);
  $mname_index = FALSE;
  $insert_fields = "";
  $is_a_number = array(); 
  $arr_size = sizeof($field_order_r);

  for ($i=0; $i< $arr_size; $i++)
  {
    $field = $field_order_r[$i];
    $mname_pos = strpos($field, "mname");
    if ($mname_pos !== FALSE)
      $mname_index = $i;
    $space_pos = strpos($field, " ");
    $insert_fields .= substr($field, 0, $space_pos) . ",";
    $type = substr($field, $space_pos);
    $number_pos = strpos($type, "int");
    if ($number_pos === FALSE) $number_pos = strpos($type, "float");
    if ($number_pos === FALSE) $number_pos = strpos($type,"double");
    if ($number_pos === FALSE) $is_a_number[]=0;
    else $is_a_number[]=1;
  }
  $is_num_size = sizeof($is_a_number);

  if(!must_have_fields($insert_fields))
  {
    echo "<b>ABRUPT END: Minimum fields are fname (first name), and 
          lname (last name).</b><br><br>";
    back_2_parentA("setup");    
    end_html_exit();
  }
  $insert_fields = substr($insert_fields, 0,
                  strlen($insert_fields)-1);
  $query = "insert into main.spanish ($insert_fields) ";
  $query .= "values (";
  $line_number = 0;
  $data_arr = explode("\n",$textarea_data);
  for ($j=0; $j<sizeof($data_arr);$j++)
  {
    $mname_is_empty = 0;
    $line_number++;
    $current_line = trim($data_arr[$j]);
    $current_line = str_replace("\t", " ", $current_line);
    $values_arr_temp = explode($delimiter, $current_line);
    $values_arr = array();
    for ($h=0; $h<sizeof($values_arr_temp);$h++)
    {
      $temp_val = trim($values_arr_temp[$h]);
      if ($temp_val != "") $values_arr[] = $temp_val;
    }
    $val_size = sizeof($values_arr);
    if ( ($val_size == $is_num_size - 1) || $val_size == 2)
      $mname_is_empty = 1;

    if (($val_size != $is_num_size) && !$mname_is_empty)
    {      
      print "SKIPPING: Insert fields and values do not match at line 
                    number $line_number:<b> $data_arr[$j] </b><br>";
    }
    else
    {
      $confirmed_n=1;
      for ($k=0; $k<$val_size; $k++)
      {
        if($is_a_number[$k])
        {
          if (is_numeric($values_arr[$k]) !== TRUE)
          {
            $confirmed_n = 0;
            $k = $val_size;
          }
        }
        else
          $values_arr[$k]="'$values_arr[$k]'"; 
      }

      if ($confirmed_n)
      {
        if($mname_is_empty)
        {
          $values_arr = add_mname_to_arr($insert_fields,$values_arr);
        }
        else
        {
          $mname_temp = $values_arr[$mname_index];
          $mname_temp = substr($mname_temp, 1, 1);
          $values_arr[$mname_index] = "'" . $mname_temp . ".'";
        }
        $return_r = select_from_main($insert_fields, $values_arr, 
                                         $mname_is_empty, $errors);
        $errors = $return_r[0];
        $value_exists = $return_r[1];
        if (!$value_exists)
        {
          $values_str = implode(",", $values_arr);
          $query_full = "$query $values_str )";
          $result = mysql_query($query_full);
          $errors = query_outcome_echo($query_full,$result, $errors);
          if($result !== FALSE)
            $inserted_item = 1;
        }
      }
      else
      {
        print "There is a field number and value non-number at line
              number $line_number:<b> $data_arr[$j] </b> <br>";
      }
    }
  }
  if ($inserted_item) echo "<b>The database was edited.</b>";
  else echo "<b>The database was not edited.</b>";
  echo "<br><br>";
  back_2_parentA("setup");
  echo "</DIV>"; 
}
?>

</body></html>

