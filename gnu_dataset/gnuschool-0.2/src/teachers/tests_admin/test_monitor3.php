<?php 
session_start();
session_register("s_password");
session_register("s_user");
session_register("s_db_name");
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

include ('../login.php');

$name=$_POST['name'];
$id=$_POST['id'];
$test_id=$_POST['test_id'];
$period=$_POST['period'];
$set_score=$_POST['set_score'];

if(empty($id) || empty($test_id) || empty($period))
  $yes_input = 0;


$dateTime = getdate();
$date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";


function get_edit_inputs($id=0,$test_id=0,$name="",$period="")
{
  $edit_form = <<<EOQ

<b>Student's name:</b><br>
<input type=text name=name size=30 value="$name" READONLY><br><br>

<b>Period:</b>
<input type=text name=period size=3 value="$period" READONLY>
<br>

<b>Score given for this test:</b>
<input type=number name=set_score size=3>
<br>

<input type=hidden name=id value="$id">
<input type=hidden name=test_id value="$test_id">

<input type=submit name=grade_in value="Set student's score">
</TD></TR>
EOQ;
  return $edit_form;
}


function get_name ($db_name="",$id="",$period="")
{
  $queryString = "select * from $db_name.spanish where id = $id and
                  period = '$period'";
  $result = mysql_query($queryString);
//  query_outcome_echo($query,$result);

  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $name = $row['fname'] . " " . $row['mname'] . " " . $row['lname'];
  return $name;
}


function check_scores_table ($db_name="",$id="",$test_id="",$date="",$period)
{
  $queryString = "select * from $db_name.scores where id = $id and
         test_id = $test_id and date = '$date' and period = '$period'";
  $result = mysql_query($queryString);
//  query_outcome_echo($query,$result);

  if (mysql_num_rows($result) == 0)
    return "no";
  else
  {
    $row = mysql_fetch_array($result, MYSQL_ASSOC);
    if ( $row['makeup'] == 'y') return "makeup";
    else return "yes";
  }
}


function update_data
     ($db_name="", $id=0, $test_id=0, $period="", $score=0, $date="")
{
  $queryString="update $db_name.scores set score='$score', makeup='n' 
            where id = $id and test_id = $test_id and date = '$date'";
  $result = mysql_query($queryString);
//  query_outcome_echo($query,$result);
  $count = in_taken_table($db_name, $test_id, $period, $date);
  if ($count == 0)
  {
    $query = "insert into $db_name.taken (test_id,period,date,count)
                          values ($test_id, '$period', '$date', 1)";
    $result = mysql_query($query);
//  query_outcome_echo($query,$result);
  }
  else
  {
    $count++;
    $query = "update $db_name.taken set count = $count where 
            test_id=$test_id and date='$date' and period='$period'";
    $result = mysql_query($query);
//  query_outcome_echo($query,$result);
  }
}


function insert_data
     ($db_name="", $id=0, $test_id=0, $period="", $score=0, $date="")
{
  $query="insert into $db_name.scores (date,id,score,test_id,period,makeup)
          values ('$date', $id, $score, $test_id, '$period', 'n')";
  $result = mysql_query($query);
//  query_outcome_echo($query,$result);
  $count = in_taken_table($db_name, $test_id, $period, $date);
  if ($count == 0)
  {
    $query = "insert into $db_name.taken (test_id,period,date,count) "
               . "values ($test_id, '$period', '$date', 1)";
    $result = mysql_query($query);
//  query_outcome_echo($query,$result);
  }
  else
  {
    $count++;
    $query = "update $db_name.taken set count = $count where
              test_id=$test_id and date='$date' and period='$period'";
    $result = mysql_query($query);
//  query_outcome_echo($query,$result);
  }
}


function in_taken_table ($db_name="", $test_id=0, $period="",$date="")
{
  $query = "select count from $db_name.taken where
            test_id = $test_id and date = '$date' and period ='$period'";
  $result = mysql_query($query);
//  query_outcome_echo($query,$result);

  if (mysql_num_rows($result) == 0)
    return 0;
  else
  {
    $row = mysql_fetch_row($result);
    return $row[0];
  }
}


function tDataSource($data="", $align="", $width="", $colspan="")
{
  if ($align != "") $align = "align=" . $align;
  if ($colspan != "") $colspan = "colspan=" . $colspan;
  if ($width != "") $width = "width=" . $width;
  $data_html = <<<EOQ
<TD $align $colspan $width>
$data
</TD>
EOQ;
  return $data_html;
}


function rowPrint($t_data="", $align="")
{
  if ($align != "") $align = "align=" . $align;
  return "<TR $align> $t_data </TR>";
}


function startTable($border="", $cellpadding=0, $cellspacing=0)
{
  $cellpadding = "cellpadding=" . "$cellpadding";
  $cellspacing = "cellspacing=" . "$cellspacing";
  return "<TABLE $border $cellspacing $cellpadding>";
}


function endTable()
{
  return "</TABLE>";
}


function start_main_div ()
{
  echo '<DIV class=main_table>';
}


function end_main_div ()
{
  echo '</DIV>';
}


function show_form_table ($score_form)
{
  echo startTable("border");
  echo '<form method=post action="test_monitor3.php">';
  $cell_data = tDataSource("$score_form", "left");
  echo rowPrint($cell_data);
  echo '</form>';
  echo endTable();
}


function show_test_table ($test_arr)
{
  echo startTable("border");
  $description = $test_arr['description'];
  $isbn = $test_arr['isbn'];
  $title = $test_arr['title'];
  $author = $test_arr['author'];
  $page = $test_arr['page'];
  $cell_data = tDataSource("<b>Test Information</b>", "center","",2);
  echo rowPrint($cell_data);
  $cell_data = tDataSource("Description", "left");
  $cell_data .= tDataSource($description, "left");
  echo rowPrint($cell_data);
  $cell_data = tDataSource("Author", "left");
  $cell_data .= tDataSource($author, "left");
  echo rowPrint($cell_data);
  $cell_data = tDataSource("Title", "left");
  $cell_data .= tDataSource($title, "left");
  echo rowPrint($cell_data);
  $cell_data = tDataSource("Page", "left");
  $cell_data .= tDataSource($page, "left");
  echo rowPrint($cell_data);
  $cell_data = tDataSource("ISBN", "left");
  $cell_data .= tDataSource($isbn, "left");
  echo rowPrint($cell_data);
  echo endTable();
}


function get_test_arr($db_name = "", $test_id = 0)
{
  $fields_arr = array();
  $query = "select * from $db_name.tests where test_id=$test_id";
  $result = mysql_query($query);  
//  query_outcome_echo($query,$result);
  $row = mysql_fetch_array($result, MYSQL_ASSOC);
  $fields_arr["description"]=$row['description'];
  $fields_arr["isbn"]=$row['isbn'];
  $fields_arr["title"]=$row['title'];
  $fields_arr["author"]=$row['author'];
  $fields_arr["page"]=$row['page'];
  return $fields_arr;
}


$tag_title = "gnuschool.org Test Monitor";
$page_title = "Monitor a test";

start_html($tag_title);
stylesheet_link("../style_sheet");
start_body("main_background");

if (!$yes_input)
{
  $comment = "Please access this page using the site menu.";
  title_comment_div_noend($page_title, $comment);
  end_html_exit();
}

$name = get_name ($db_name,$id,$period);

if(empty($set_score))
{
  $page_title = "Set a test score";
  $comment = "Please enter the score you wish to give for this test.";
  title_comment_div_noend($page_title, $comment);  

  $score_form = get_edit_inputs($id,$test_id,$name,$period);
  $test_arr = get_test_arr($db_name, $test_id);
  start_main_div ();
  echo startTable("",10,0);
  echo "<tr><td>";
  show_form_table ($score_form);
  echo "</td><td>";
  show_test_table ($test_arr);
  echo "</td></tr>";
  echo endTable();
  end_main_div();
}
else
{
  $set_score = (int) $set_score;
  if(is_int($set_score))
  {
    $score_in=check_scores_table($db_name,$id,$test_id,$date,$period);

    if ( $score_in == "makeup")
      update_data($db_name, $id, $test_id, $period, $set_score, $date);
    else
      insert_data($db_name, $id, $test_id, $period, $set_score, $date);

    $page_title = "Score is set";
    $comment = "$name has a $set_score for this test.";
    title_comment_div_noend($page_title, $comment);  
  }
  else
  {
    $page_title = "Score is not set";
    $comment = 'The score "$set_score" is not a valid entry';
    title_comment_div_noend($page_title, $comment);  
  }
}

?>
</body></html>

