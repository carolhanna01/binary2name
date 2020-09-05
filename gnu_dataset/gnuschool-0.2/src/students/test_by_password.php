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
$password=$_GET['password'];
$t_name=$_GET['t_name'];
$submit=$_GET['submit'];

$yes_input=1;
if ( empty($password) && empty($t_name)) $yes_input=0;

$dateTime = getdate();
$date = "$dateTime[year]-$dateTime[mon]-$dateTime[mday]";

function get_id_period ($db_name="", $password="")
{
  $query= "select id,period from $db_name.passwords
           where password = '$password'";
  $result = mysql_query($query);
  if($result === FALSE) return 0;
  if (empty($result)) return -2;
  if ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    $info_r = array();
    $info_r[0] = $row['id'];
    $info_r[1] = $row['period'];
    return $info_r;
  }
  else
    return -1;
}


function makeup_in_scores ($db_name="",$id="")
{
  $queryString = "select * from $db_name.scores where id = $id and
         makeup = 'y'";
  $result = mysql_query($queryString);

  if (mysql_num_rows($result) == 0)
    return "no";
  else
  {
    $row = mysql_fetch_array($result, MYSQL_ASSOC);
    $info = $row['test_id'];
    $info .= "_" . $row['date'];
    return $info;
  }
}


function get_student_name ($db_name, $id="", $period="")
{
  $query = "select * from $db_name.spanish where id = $id
            and period = '$period'";
  $result = mysql_query($query);
  $row = mysql_fetch_object($result);
  $name = "$row->fname $row->mname $row->lname";
  return $name;
}


function get_test_id($db_name="", $period="" )
{
  $t_field = "period" . $period;
  $query = "select * from $db_name.today";
  $result = mysql_query($query);
  if($result === FALSE) return 0;
  $row = mysql_fetch_object($result);
  $test_id = $row->$t_field;
  return $test_id;
}


function get_test_arr($db_name="", $test_id=0 )
{
  $query = "select * from $db_name.tests where test_id = $test_id";
  $result = mysql_query($query);
if($result === FALSE) return array();
  $row = mysql_fetch_object($result);
  $test_arr["test"] = $row->test;
  $test_arr["answers"] = $row->answers;
  $test_arr["description"] = $row->description;
  $test_arr["type"] = $row->type;
  return $test_arr;
}


function get_questions_sequence( $arr = array() )
{
  $new_arr = array();
  while (list ($key, $value) = each($arr))
  {  
     $pos = strpos($key, "|");
     $question_number = substr($key, 0, $pos);
     $new_arr[] = $question_number;
  }
  return $new_arr;
}


function randomize_array( $test_arr )
{
	$arr = array();
        
	while (list( , $value) = each ($test_arr))
	{
	  $pos = strpos($value, "~");
	  if ($pos)
	  {
	    $question = substr($value, 0, $pos);
	    $answers = substr($value, $pos + 1);
	    $arr[$question] = $answers;
	  }
          else
	  {
	    $arr[$value] = "";
	  }
	}

	$items = count ($arr);
	$new_arr = array();
	$arr_rand_keys = array_rand($arr, $items);
        
      if(is_array($arr_rand_keys))
      {
        shuffle($arr_rand_keys);

	for ( $i=0; $i<$items; $i++)
	{
	  $new_arr[$arr_rand_keys[$i]] = $arr[$arr_rand_keys[$i]];
	}
      }
      else
        $new_arr[$arr_rand_keys] = $arr[$arr_rand_keys];
      return $new_arr;
}


function isin_scores_table 
             ($db_name="", $id="", $test_id="", $date="", $period="")
{
  $queryString = "select makeup from $db_name.scores where id = $id and
         test_id = $test_id and period='$period' and date = '$date'";

  $result = mysql_query($queryString);
  if($result === FALSE) return "no";
  if (mysql_num_rows($result) == 0)
    return "no";
  else
    return "yes";
}


function array_to_html($arr=array(), $source="", $type)
{
  $sequence = get_questions_sequence($arr);
  $index = 1;
  $source .= "<hr>";
  
  while (list ($key, $value) = each($arr))
  {  
     $pos = strpos($key, "|");
     $question = substr($key, $pos + 1);
     $mult_choice_source = "";
     if ($value == "")
     {       
       $quest_num = $sequence[$index - 1];
       $quest_num = trim($quest_num);
       $name = "v" . "$quest_num";
       $text_field = <<<EOQ
&nbsp<input type=text size=34 name="$name" onclick="set_global_pos(this)">
EOQ;
       $question = str_replace("|", " | ", $question);
       $question = str_replace("|", $text_field, $question);
$question .= "<br>" . special_chars($type);
     }
     else
     {
     $ans_arr = explode("~", $value);
     $count1 = 1;
     while (list ($key1, $value1) = each($ans_arr))
     {
       $quest_num = $sequence[$index - 1];
       $quest_num = trim($quest_num);
       $name = "v" . "$quest_num";
       $mult_choice_source .= <<<EOQ
<input type="radio" name='$name' value="$count1">$value1<br>
EOQ;
       $count1++;
     }
     }
     $source .= <<<EOQ
<b>$index .) $question </b><br>
EOQ;
     $source .= $mult_choice_source;
     $source .= "<br>";
     $index++;
  }
return $source;
}


function array_print_html($desc = "", $arr = array(), $db_name="",
$id="", $test_id="", $period="", $name="", $date="", $type)
{
$name = "Date: $date &nbsp &nbsp &nbsp &nbsp Period: $period<br>Name:
$name";
$title = "Test Title: $desc";
title_comment_div_noend($name, $title);
echo "<DIV>";
$source_top = <<<EOQ
<DIV class=student_test_table>
<table><tr><td>
<form name=test method=post action="test_to_db.php">
<input type=hidden name=db_name value="$db_name">
<input type=hidden name=id value="$id">
<input type=hidden name=name value="$name">
<input type=hidden name=date value="$date">
<input type=hidden name=test_id value="$test_id">
<input type=hidden name=period value="$period">
EOQ;

$source = array_to_html($arr, $source_top, $type);

$source .= <<<EOQ
<input type=submit name=submit value="I'm done, please compute the grade" 
onClick="return doneConfirm3()">
<br>
</form>
</td></tr></table>
</div>
EOQ;

print $source;
}


function t_name_2_db($t_name="")
{
  $query="select db_name from main.teachers
          where t_name = '$t_name'";
  $result = mysql_query("$query");
  $row = mysql_fetch_row($result);
  $db_name = $row[0];
  return $db_name; 
}


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


function start_html($title)
{
echo <<<EOQ
<html>
<head>
<title> $title </title>
EOQ;
}


function start_body($color="main_background")
{
echo <<<EOQ
</head>
<body class="$color">
EOQ;
}


function special_chars($type)
{
  $source = "";
  if($type == "Spanish")
  {
    $source = <<<EOQ
<INPUT TYPE="button" VALUE="  á  "
onclick="addText('á')">  
<INPUT TYPE="button" VALUE="  é  "
onclick="addText('é')">  
<INPUT TYPE="button" VALUE="  í  "
onclick="addText('í')">  
<INPUT TYPE="button" VALUE="  ó  "
onclick="addText('ó')">  
<INPUT TYPE="button" VALUE="  ú  "
onclick="addText('ú')">
<INPUT TYPE="button" VALUE="  ü  " 
onclick="addText('ü')"> 
<INPUT TYPE="button" VALUE="  ñ  "
onclick="addText('ñ')">
<INPUT TYPE="button" VALUE="  Á  "
onclick="addText('Á')">
<INPUT TYPE="button" VALUE="  É  "
onclick="addText('É')">
<INPUT TYPE="button" VALUE="  Í  "
onclick="addText('Í')">
<INPUT TYPE="button" VALUE="  Ó  "
onclick="addText('Ó')">
<INPUT TYPE="button" VALUE="  Ú  "
onclick="addText('Ú')">
<INPUT TYPE="button" VALUE="  Ü  " 
onclick="addText('Ü')"> 
<INPUT TYPE="button" VALUE="  Ñ  "
onclick="addText('Ñ')">
<INPUT TYPE="button" VALUE="  ¡  "
onclick="addText('¡')">
<INPUT TYPE="button" VALUE="  ¿  "
onclick="addText('¿')">
EOQ;
  }
elseif($type == "German")
  {
    $source = <<<EOQ
<INPUT TYPE="button" VALUE="  ä  "
onclick="addText('ä')"> 
<INPUT TYPE="button" VALUE="  ö  "
onclick="addText('ö')"> 
<INPUT TYPE="button" VALUE="  ü  "
onclick="addText('ü')"> 
<INPUT TYPE="button" VALUE="  Ä  " 
onclick="addText('Ä')"> 
<INPUT TYPE="button" VALUE="  Ö  "
onclick="addText('Ö')"> 
<INPUT TYPE="button" VALUE="  Ü  "
onclick="addText('Ü')"> 
<INPUT TYPE="button" VALUE="  ß  "
onclick="addText('ß')">
<INPUT TYPE="button" VALUE="  €  "
onclick="addText('€')">
EOQ;
  }
elseif($type == "French")
  {
    $source = <<<EOQ
<INPUT TYPE="button" VALUE="  À  "
onclick="addText('À')"> 
<INPUT TYPE="button" VALUE="  à  "
onclick="addText('à')"> 
<INPUT TYPE="button" VALUE="  Á  "
onclick="addText('Á')"> 
<INPUT TYPE="button" VALUE="  á  "
onclick="addText('á')"> 
<INPUT TYPE="button" VALUE="  Â  "
onclick="addText('Â')"> 
<INPUT TYPE="button" VALUE="  â  "
onclick="addText('â')"> 
<INPUT TYPE="button" VALUE="  Ä  "
onclick="addText('Ä')"> 
<INPUT TYPE="button" VALUE="  ä  "
onclick="addText('ä')"> 
<INPUT TYPE="button" VALUE="  É  "
onclick="addText('É')">  
<INPUT TYPE="button" VALUE="  é  "
onclick="addText('é')">
<INPUT TYPE="button" VALUE="  È  "
onclick="addText('È')">
<INPUT TYPE="button" VALUE="  è  "
onclick="addText('è')">
<INPUT TYPE="button" VALUE="  Ê  "
onclick="addText('Ê')"> 
<INPUT TYPE="button" VALUE="  ê  "
onclick="addText('ê')"> 
<INPUT TYPE="button" VALUE="  Ë  "
onclick="addText('Ë')"> 
<INPUT TYPE="button" VALUE="  ë  "
onclick="addText('ë')"> 
<INPUT TYPE="button" VALUE="  Î  "
onclick="addText('Î')"> 
<INPUT TYPE="button" VALUE="  î  "
onclick="addText('î')"> 
<INPUT TYPE="button" VALUE="  Ï  "
onclick="addText('Ï')"> 
<INPUT TYPE="button" VALUE="  ï  "
onclick="addText('ï')">
<INPUT TYPE="button" VALUE="  Ô  "
onclick="addText('Ô')">
<INPUT TYPE="button" VALUE="  ô  "
onclick="addText('ô')">
<INPUT TYPE="button" VALUE="  Œ  "
onclick="addText('Œ')">
<INPUT TYPE="button" VALUE="  œ  "
onclick="addText('œ')">
<INPUT TYPE="button" VALUE="  Ò  "
onclick="addText('Ò')"> 
<INPUT TYPE="button" VALUE="  ò  "
onclick="addText('ò')">
<INPUT TYPE="button" VALUE="  Ô  "
onclick="addText('Ô')"> 
<INPUT TYPE="button" VALUE="  ô  "
onclick="addText('ô')"> 
<INPUT TYPE="button" VALUE="  Ö  "
onclick="addText('Ö')"> 
<INPUT TYPE="button" VALUE="  ö  "
onclick="addText('ö')">
<INPUT TYPE="button" VALUE="  Ÿ  "
onclick="addText('Ÿ')">
<INPUT TYPE="button" VALUE="  ÿ  "
onclick="addText('ÿ')"> 
<INPUT TYPE="button" VALUE="  Ç  "
onclick="addText('Ç')">
<INPUT TYPE="button" VALUE="  ç  "
onclick="addText('ç')">
<INPUT TYPE="button" VALUE="  €  "
onclick="addText('€')">
EOQ;
  }
elseif($type == "Italian")
  {
    $source = <<<EOQ
<INPUT TYPE="button" VALUE="  á  "
onclick="addText('á')">
<INPUT TYPE="button" VALUE="  à  "
onclick="addText('à')">
<INPUT TYPE="button" VALUE="  â  "
onclick="addText('â')"> 
<INPUT TYPE="button" VALUE="  ã  "
onclick="addText('ã')"> 
<INPUT TYPE="button" VALUE="  ä  "
onclick="addText('ä')"> 
<INPUT TYPE="button" VALUE="  å  "
onclick="addText('å')"> 
<INPUT TYPE="button" VALUE="  æ  "
onclick="addText('æ')"> 
<INPUT TYPE="button" VALUE="  À  "
onclick="addText('À')"> 
<INPUT TYPE="button" VALUE="  Á  "
onclick="addText('Á')"> 
<INPUT TYPE="button" VALUE="  Â  "
onclick="addText('Â')"> 
<INPUT TYPE="button" VALUE="  Ã  "
onclick="addText('Ã')"> 
<INPUT TYPE="button" VALUE="  Ä  "
onclick="addText('Ä')"> 
<INPUT TYPE="button" VALUE="  Å  "
onclick="addText('Å')"> 
<INPUT TYPE="button" VALUE="  Æ  "
onclick="addText('Æ')"> 
<INPUT TYPE="button" VALUE="  è  "
onclick="addText('è')"> 
<INPUT TYPE="button" VALUE="  é  "
onclick="addText('ê')"> 
<INPUT TYPE="button" VALUE="  é  "
onclick="addText('ê')"> 
<INPUT TYPE="button" VALUE="  ë  "
onclick="addText('ë')"> 
<INPUT TYPE="button" VALUE="  È  "
onclick="addText('È')"> 
<INPUT TYPE="button" VALUE="  É  "
onclick="addText('É')"> 
<INPUT TYPE="button" VALUE="  Ê  "
onclick="addText('Ê')"> 
<INPUT TYPE="button" VALUE="  ì  "
onclick="addText('ì')"> 
<INPUT TYPE="button" VALUE="  í  "
onclick="addText('í')"> 
<INPUT TYPE="button" VALUE="  î  "
onclick="addText('î')"> 
<INPUT TYPE="button" VALUE="  ï  "
onclick="addText('ï')"> 
<INPUT TYPE="button" VALUE="  Ì  "
onclick="addText('Ì')"> 
<INPUT TYPE="button" VALUE="  Í  "
onclick="addText('Í')"> 
<INPUT TYPE="button" VALUE="  Î  "
onclick="addText('Î')"> 
<INPUT TYPE="button" VALUE="  Ï  "
onclick="addText('Ï')"> 
<INPUT TYPE="button" VALUE="  ò  "
onclick="addText('ò')"> 
<INPUT TYPE="button" VALUE="  ó  "
onclick="addText('ó')"> 
<INPUT TYPE="button" VALUE="  ô  "
onclick="addText('ô')"> 
<INPUT TYPE="button" VALUE="  õ  "
onclick="addText('õ')"> 
<INPUT TYPE="button" VALUE="  ö  "
onclick="addText('ö')"> 
<INPUT TYPE="button" VALUE="  Ò  "
onclick="addText('Ò')"> 
<INPUT TYPE="button" VALUE="  Ó  "
onclick="addText('Ó')"> 
<INPUT TYPE="button" VALUE="  Ô  "
onclick="addText('Ô')"> 
<INPUT TYPE="button" VALUE="  Õ  "
onclick="addText('Õ')"> 
<INPUT TYPE="button" VALUE="  Ö  "
onclick="addText('Ö')"> 
<INPUT TYPE="button" VALUE="  ù  "
onclick="addText('ù')"> 
<INPUT TYPE="button" VALUE="  ú  "
onclick="addText('ú')"> 
<INPUT TYPE="button" VALUE="  û  "
onclick="addText('û')"> 
<INPUT TYPE="button" VALUE="  ü  "
onclick="addText('ü')"> 
<INPUT TYPE="button" VALUE="  Ù  "
onclick="addText('Ù')"> 
<INPUT TYPE="button" VALUE="  Ú  "
onclick="addText('Ú')"> 
<INPUT TYPE="button" VALUE="  Û  "
onclick="addText('Û')"> 
<INPUT TYPE="button" VALUE="  Ü  "
onclick="addText('Ü')"> 
<INPUT TYPE="button" VALUE="  ç  "
onclick="addText('ç')"> 
<INPUT TYPE="button" VALUE="  Ç  "
onclick="addText('Ç')"> 
<INPUT TYPE="button" VALUE="  ñ  "
onclick="addText('ñ')"> 
<INPUT TYPE="button" VALUE="  Ñ  "
onclick="addText('Ñ')"> 
<INPUT TYPE="button" VALUE="  ß  "
onclick="addText('ß')"> 
<INPUT TYPE="button" VALUE="  ý  "
onclick="addText('ý')"> 
<INPUT TYPE="button" VALUE="  Ý  "
onclick="addText('Ý')"> 
<INPUT TYPE="button" VALUE="  ¿  "
onclick="addText('¿')"> 
<INPUT TYPE="button" VALUE="  ¡  "
onclick="addText('¡')"> 
EOQ;
  }
  return $source;
}


function escape_single_quotes(&$post_r)
{
  while (list ($key ,$value) = each($post_r))
  {
    $value = str_replace("'", "\'", $value);
    $post_r["$key"] = $value;
  }
}


function get_defaults_r($db_name)
{
  $defaults_r = array();
  $query = "select * from $db_name.defaults";
  $result = mysql_query($query);
  $row = mysql_fetch_array($result, MYSQL_ASSOC);

  $defaults_r['tables_across'] = $row['passwords_tables_across'];
  $defaults_r['row_space'] = $row['passwords_row_space'];
  $defaults_r['column_space'] = $row['passwords_column_space'];
  $defaults_r['order'] = $row['passwords_order'];

  $defaults_r['fname'] = $row['seating_fname'];
  $defaults_r['mname'] = $row['seating_mname'];
  $defaults_r['lname'] = $row['seating_lname'];
  $defaults_r['name_lines'] = $row['seating_name_lines'];

  $defaults_r['q1_start'] = $row['q1_start'];
  $defaults_r['q1_end'] = $row['q1_end'];
  $defaults_r['q2_start'] = $row['q2_start'];
  $defaults_r['q2_end'] = $row['q2_end'];
  $defaults_r['q3_start'] = $row['q3_start'];
  $defaults_r['q3_end'] = $row['q3_end'];
  $defaults_r['q4_start'] = $row['q4_start'];
  $defaults_r['q4_end'] = $row['q4_end'];

  $defaults_r['search_start'] = $row['search_start'];
  $defaults_r['search_end'] = $row['search_end'];

  return $defaults_r;
}

$tag_title = "gnuschool.org Assessment";
start_html($tag_title);

echo <<<EOQ
<style type="text/css">
DIV.page_title {
  position: absolute;
  top: 50;
  left: 10;
}

DIV.main_table {
  position: absolute;
  top: 120;
  left: 10;
}

DIV.student_test_table {
  position: absolute;
  top: 160;
  left: 10;
}

.main_background {
  background: rgb(200,200,255)
}
</style>
EOQ;


if ($yes_input)
{
echo <<<EOQ
<SCRIPT LANGUAGE="JavaScript">

var inputA=null;


function set_global_pos(myobject)
{ if(myobject) inputA = myobject; }


function doneConfirm3() {
var ok=confirm("Are you done with Test?")
if (ok) return true; else return false;
}


function addText( insText )
{
if (inputA)
{
if (inputA.type == "textarea" || inputA.type == "text")
{
 inputA.focus();
 if( inputA.createTextRange ) {
   document.selection.createRange().text += insText;
 } else if( inputA.setSelectionRange ) {
   var len = inputA.selectionEnd;
   inputA.value=inputA.value.substr(0,len) + insText +
inputA.value.substr(len);
   inputA.setSelectionRange(len+insText.length,len+insText.length);
 } else { inputA.value += insText; }
}
}
}
</script>
EOQ;
}

start_body();

$page_title = "Assessment";

if (!$yes_input)
{
  $comment = "The teacher name and student password are empty. Please
              close this window and try again.";
  title_comment_div_noend($page_title, $comment);
}
elseif (empty($submit))
{
  $comment = "Please access this page using the home page.";
  title_comment_div_noend($page_title, $comment);
}
else
{
  $mylink = 
  mysql_connect("127.0.0.1","access_S","nivelS_en_el_sistema");


  if($mylink)
  {
    $db_name = t_name_2_db($t_name);
    if (empty($db_name))
    {
      $comment = "The user name was incorrect. Please close this window
                and try again.";
      title_comment_div_noend($page_title, $comment);
    }
    else
    {
      $test_id = 0;
      $score_in = "";

      $info = get_id_period ($db_name, $password);
      $id = $info[0];
      $period = $info[1];

      if($id == -1)
      {
        $comment = "Your password was incorrect. Please close this
                  window and try again.";
        title_comment_div_noend($page_title, $comment);
      }
      elseif($id == -2)
      {
        $comment = "The database is locked, please wait for your 
                  instructor to open it.";
        title_comment_div_noend($page_title, $comment);
      }
      else
      {
        $name = get_student_name($db_name, $id, $period);
  
        $makeup_testid_date = makeup_in_scores ($db_name,$id);
        if ($makeup_testid_date != "no")
        {
          $info_r = explode("_", $makeup_testid_date);
          $test_id = $info_r[0];
          $date = $info_r[1];
          $score_in = "no";
        }
        else
        {
          $test_id = get_test_id($db_name, $period);
          if(!empty($test_id))
          $score_in=isin_scores_table 
                            ($db_name,$id,$test_id,$date,$period);
        }
     
        if ($test_id == 1)
        {      
          print "<b>No Test</b></body></html>";
          exit(1);      
        }

        if ( $score_in == "yes" )
        {
          $comment = "You already have a grade for this test.";
          title_comment_div_noend($page_title, $comment);
        }
        else
        {
          $all_test_arr = get_test_arr($db_name, $test_id);
          $desc = $all_test_arr["description"];
          $type = $all_test_arr["type"];
          $quest_str = str_replace("__________",
                           " __________ ", $all_test_arr['test']);
          $quest_arr = explode("&&", $quest_str);
          srand ((double) microtime() * 1000000);
          $arr = randomize_array($quest_arr);
    
          array_print_html($desc, $arr, $db_name, $id, $test_id,$period,  
                        $name, $date, $type);
        }
      }
    }
  }
  else
  {
    $comment = "Failure to connect to MySQL server.";
    title_comment_div_noend($page_title, $comment);
  }  
}
echo "<DIV>";
?>
</body></html>
