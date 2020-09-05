<?php
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

function title_comment_div_noend($page_title="", $comment="")
{
if (!empty($comment))
  $comment="<h3>$comment</h3>";
echo "<DIV class=page_title><h2>$page_title</h2>";
echo $comment;
}


function end_html_exit()
{
  echo "</DIV></BODY></HTML>";
  exit(1);
}


function startA($file)
{
  echo '<a href="$file">';
}


function back_2_parentA($parent)
{
  echo "<b>";
  if ($parent == "export")
  echo <<<EOQ
<a href="./ex_2_def_files.php"
>Go to the export to files page.</a></b>
EOQ;
  elseif ($parent == "setup")
  echo <<<EOQ
<a href="./setup_school.php"
>Go to the main school setup page.</a>
EOQ;
  elseif ($parent == "index")
  echo <<<EOQ
<a href="./"
>Go to the home page.</a>
EOQ;
  elseif ($parent == "display")
  echo <<<EOQ
<a href="./display_files.php"
>Go to the view files page.</a>
EOQ;
  else echo "";
  echo "</b>";
}


function start_html($title)
{
echo <<<EOQ
<html>
<head>
<title> $title </title>
EOQ;
}


function stylesheet_link($file)
{
echo <<<EOQ
<LINK REL="STYLESHEET" TYPE="text/css" HREF="$file">
EOQ;
}


function start_body($colors="admin_colors")
{
echo <<<EOQ
</head>
<body class="$colors">
EOQ;
}


function escape_single_quotes(&$post_r)
{
  while (list ($key ,$value) = each($post_r))
  {
    $value = str_replace("'", "\'", $value);
    $post_r["$key"] = $value;
  }
}


function query_outcome_echo($query,$result,$errors=0)
{
  if($result === FALSE)
  {
    echo "ERROR " . mysql_errno() . ": " . mysql_error() .
          "<br>$query<br>";
    $errors++;
  }
  else echo "SUCCESS:$query<br>";
  return $errors;
}


function td_get_form($file, $text, $parent="")
{
  if (!empty($parent))
    $parent = "<input type=hidden name=parent value=$parent>";
  echo <<<EOQ
<form method=post action="$file"> 
<td align="left">
$parent
<input type=submit name=submit value="$text">
</td>
</form>
EOQ;
}


// -=45  .=46  0=48,9=57  A=65,Z=90  _=95  a=97,z=122
function is_valid_file_name($str = "")
{
  $str = trim($str);
  $length = strlen($str);
  if ($length < 1) return FALSE;

  $char = substr($str, 0, 1);
  $char_val = ord($char);
  if (!(($char_val > 64 && $char_val < 91) ||
      ($char_val > 96 && $char_val < 123)) )
      return 0;

  for ($i=1; $i<$length; $i++)
  { 
    $char = substr($str, $i, 1);
    $char_val = ord($char);
    if (!(($char_val > 47 && $char_val < 58) || 
        ($char_val > 64 && $char_val < 91) ||
        ($char_val > 96 && $char_val < 123) ||
        $char_val == 95 || $char_val == 45 || $char_val == 46) )
        return 0;  
  }
  return 1;
}


// A=65,Z=90  a=97,z=122
function name_alpha_dot($str = "", $end_dot = 0)
{
  $length = strlen($str);
  if ($length < 1) return FALSE;
  if ($end_dot) $length--;

  for ($i=0; $i<$length; $i++)
  { 
    $char = substr($str, $i, 1);
    $char_val = ord($char);
    if (!(($char_val > 64 && $char_val < 91) ||
       ($char_val > 96 && $char_val < 123)) )
       return 0;
  }

  if ($end_dot) 
  {
    $char = substr($str, $i, 1);
    $char_val = ord($char);
    if (!(($char_val > 64 && $char_val < 91) ||
      ($char_val > 96 && $char_val < 123) || $char_val == 46) )
      return 0;
  }

  return 1;
}


function file_def_col_names()
{
  $query = "select col_id, col_id_opt, col_hr, col_hr_opt
            ,col_fname, col_mname, col_lname
            ,col_pe, col_pe_opt
            ,file_name, file_dir, field_delim, line_delim
            ,col_all_name, col_name_opt from main.defaults";
  $result = mysql_query($query);
  query_outcome_echo ( $query, $result);

  if($result === FALSE)
    return 0;

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  return $row;
}


function proceed_form()
{
  echo <<<EOQ
<form method=post action="$PHP_SELF">
<input type=submit name=proceed value="Proceed">
</form>
EOQ;
}

?>

