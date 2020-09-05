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

$period=$_POST['period'];
$user=$_POST['user'];
$password=$_POST['password'];
$new_seats=$_POST['new_seats'];
$menu_submit=$_POST['menu_submit'];


if (empty($period) && empty($new_seats))
  $yes_input = 0;


function input_form()
{
  $login = <<<EOQ
<form method=post action="$PHP_SELF">
<b>Period:</b><br>
<input type=text size=4 name=period>
<br><br>

<input type=hidden name=menu_submit value="Seats">
<input type=submit name=submit value="Edit seating plan">
</form>
EOQ;
  return $login;
}


function get_students_arr( $db_name="", $period="", $defaults_r)
{
  $def_fname = $defaults_r['fname'];
  $def_mname = $defaults_r['mname'];
  $def_lname = $defaults_r['lname'];
  $def_name_lines = $defaults_r['name_lines'];
  $students_arr = array();
  $query = "select * from $db_name.spanish where period = '$period'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  while ($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    $fname = $row['fname'];
    $mname = $row['mname'];
    $lname = $row['lname'];
    $id = $row['id'];
    $continues = "";
    if ($def_fname == 'n') $fname = "";
    else
    {
      $length = strlen($fname);
      if ($length > (int)$def_fname)
        {$length = (int)$def_fname; $continues = "~";}
      $fname = substr($fname, 0, $length);
      $fname .= $continues;
      $continues = "";
      
      if($def_mname!='n' || $def_lname!='n')
      {
        if($def_name_lines=='f' || $def_name_lines=='a') $fname .= "\n";
        elseif($def_name_lines=='n'||$def_name_lines=='m') $fname .= " ";
      }
      elseif ($def_name_lines=='m' && $def_mname=='n') $fname .= "\n";
    }

    if ($def_mname == 'n') $mname = "";
    elseif($def_lname!='n')
    {
      if ($def_name_lines=='m' || $def_name_lines=='a') $mname .= "\n";
      elseif ($def_name_lines=='n' || $def_name_lines=='f') $mname .= " ";
    }

    if ($def_lname == 'n') $lname = "";
    else
    {
      $length = strlen($lname);
      if ($length > (int)$def_lname)
        { $length = (int)$def_lname; $continues = "~";}
      $lname = substr($lname, 0, $length);
      $lname .= $continues;
    }
    $students_arr["$id"] = "$fname" . "$mname" . "$lname";
  }
  return $students_arr;
}


function get_seating_data( $db_name="", $id="", $period="")
{
  $id_num = (int)($id);
  $query = "select * from $db_name.seating where id = $id_num
            and period = '$period'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  $row = mysql_fetch_object($result);
  return $row;
}


function get_img_coords($db_name, $period = "")
{
  $biggest_coords = array();
  $biggest_x = 10;
  $biggest_y = 10;

  $query = "select * from $db_name.seating where period = '$period'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  while($row = mysql_fetch_array($result, MYSQL_ASSOC))
  {
    if ($row['seat_x'] > $biggest_x) $biggest_x = $row['seat_x'];
    if ($row['seat_y'] > $biggest_y) $biggest_y = $row['seat_y'];
  }
  $biggest_coords['seat_x'] = $biggest_x;
  $biggest_coords['seat_y'] = $biggest_y;
  return $biggest_coords;
}


function update_seating($db_name,$post_arr = array())
{
  $period = $post_arr['period'];
  $query = "select id from $db_name.spanish where period = '$period'";
  $result = mysql_query($query);
  // query_outcome_echo($query,$result);
  while ($row = mysql_fetch_row($result))
  {
    $id = $row[0];
    $id_num = (int)($id);
    $v_id = "v" . $id;
    $coords = $post_arr["$v_id"];
    if (!empty($coords))
    {
      $pos = strpos($coords, "_");
      $x_val = substr($coords, 1, $pos-1);
      $y_val = substr($coords, $pos+1);
      $query1 = "update $db_name.seating ";
      $query1 .= "set seat_x='$x_val', seat_y='$y_val' where id=$id_num
                  and period = '$period'";
      mysql_query($query1);
  // query_outcome_echo($query,$result);
    }
  }
}


function array_to_html($db_name="", $students_arr=array(), $period)
{
  $source = "";
  $index = 2;
  while (list ($id , $name) = each($students_arr))
  {
    $seating_arr = get_seating_data( $db_name, $id, $period);
    $seat_x = $seating_arr->seat_x;
    $seat_y = $seating_arr->seat_y;
    $div_id = "div_" . $id;
    $but_id = "but_" . $id;
    $v_id = "v" . $id;
    $index++;
echo <<<EOQ
<DIV ID="$div_id" STYLE="position:absolute; top:$seat_y; left:$seat_x; 
z-index: $index">
<input id="$but_id" type="button" class="gray_button" name="$but_id"
value="$name" 
onMouseDown="set_globals('$but_id', '$div_id');"
onMouseUp="unset_globals();">
</DIV>
EOQ;
    $source .= <<<EOQ
<input id="$id" type="hidden" name="$v_id">
EOQ;
  }
  return $source;
}

function img_div_html($coords_r)
{
  $seat_x = $coords_r['seat_x'] + 151;
  $seat_y = $coords_r['seat_y'] + 101;
  echo <<<EOQ
<DIV ID="div_id1" STYLE="position:absolute; top:$seat_y; left:$seat_x;">
<img id="img_id1" src='/blank.png' ALT='blank picture'>
<input id="id1" type="hidden">
</DIV>
EOQ;
}


function get_update_seats_inputs($hidden_divs="", $period)
{
  $inputs = <<<EOQ
$hidden_divs
<input type=hidden name="menu_submit" value="Seats">
<input type=submit class=green_button name="new_seats"
value='Save Seat Changes for "PERIOD $period"'>
EOQ;
  return $inputs;
}



function update_seats_div($hidden_divs="")
{
  echo <<<EOQ
<DIV STYLE="position:absolute; top:30; left:0; z-index: 2">
<form method=post action="$PHP_SELF">
$hidden_divs
<input type=submit class=green_button name="new_seats"
value="Save changes">
</form>
</DIV>
EOQ;
}


$tag_title = "gnuschool.org Edit seating plan";

start_html($tag_title);
stylesheet_link("../style_sheet");

if ($yes_input)
{
  echo <<<EOQ
<SCRIPT LANGUAGE="JavaScript">

var brOK = false, mie = false; 
if (document.layers || document.all) brOK = true; 
if (document.all) mie = true; 


var old_event_x = 0, old_event_y = 0;
var w_width, w_height;
var max_x=0, max_y=0;
var cur_div_obj_x, cur_div_obj_y;
var cur_div_obj, cur_but_obj;
var cur_div_id;
var unset=0;


function setHandlers()
{
  start_globals();
  window.document.onmousemove = move; 
} 


function coords_to_value()
{
 var str_len = cur_div_id.length - 4;
 var id = cur_div_id.substr(4, str_len);
 if (!mie)
   document[id].value = 'v' +cur_div_obj_x+ '_' +cur_div_obj_y;
 else
  document.getElementById(id).value = 'v'+cur_div_obj_x+'_'+cur_div_obj_y;
}


function start_globals()
{     
    set_but_obj('img_id1');
    set_div_obj('div_id1');
    set_div_obj_xy();
    max_x = cur_div_obj_x;
    max_y = cur_div_obj_y;
    unset = 1;
} 


function unset_globals()
{ 
    if ((cur_div_obj_x + 150) > max_x)
      max_x = cur_div_obj_x + 150;
    if ((cur_div_obj_y + 100) > max_y)
      max_y = cur_div_obj_y + 100;
    
    set_but_obj('img_id1');
    set_div_obj('div_id1');
    move_div_obj_xy(max_x, max_y);
    cur_but_obj.blur();
    unset = 1;
} 


function set_globals(but_id, div_id)
{ 
    set_div_obj(div_id);
    set_div_obj_xy();
    set_but_obj(but_id);
    unset = 0;
} 


function set_but_obj(id)
{ 
 if (!mie)
   cur_but_obj = document[id];
 else
   cur_but_obj = document.getElementById(id);
} 


function set_div_obj(id)
{ 
 cur_div_id = id;
 if (!mie)
   cur_div_obj = document[id];
 else
   cur_div_obj = document.getElementById(id).style;
} 


function set_div_obj_xy()
{ 
  if (!mie)
  { 
    cur_div_obj_x = cur_div_obj.left; 
    cur_div_obj_y = cur_div_obj.top; 
  } 
  else
  { 
    cur_div_obj_x = cur_div_obj.pixelLeft; 
    cur_div_obj_y = cur_div_obj.pixelTop; 
  } 
}


function move_div_obj_xy(new_x, new_y)
{ 
  if (!mie)
  { 
    cur_div_obj.left = new_x; 
    cur_div_obj.top = new_y; 
  } 
  else
  { 
    cur_div_obj.pixelLeft = new_x; 
    cur_div_obj.pixelTop = new_y; 
  } 
  coords_to_value();
}


function set_height_width()
{
  if( typeof( window.innerWidth ) == 'number' )
  {
    //netscape
    w_width = window.innerWidth;
    w_height = window.innerHeight;
  }
  else if( document.documentElement &&
      ( document.documentElement.clientWidth || document.documentElement.clientHeight ) )
  {
    //IE 6+
    w_width = document.documentElement.clientWidth;
    w_height = document.documentElement.clientHeight;
  }
  else if( document.body && ( document.body.clientWidth || document.body.clientHeight ) )
  {
    //IE 4
    w_width = document.body.clientWidth;
    w_height = document.body.clientHeight;
  }
}


function move()
{ 
    new_event_x = event.x; 
    new_event_y = event.y;

    set_height_width();

    if (unset == 0)
    if ((new_event_x < 3)||(new_event_y < 3)||(new_event_x>w_width)||(new_event_y>w_height))
      unset_globals();

    new_div_x = cur_div_obj_x + (new_event_x - old_event_x);
    new_div_y = cur_div_obj_y + (new_event_y - old_event_y);
             
    if (new_div_x < 0 )
      new_div_x = 0;
 
    if (new_div_y < 0 )
      new_div_y = 0;
 


    if (unset == 0)
    {
      move_div_obj_xy(new_div_x , new_div_y);
      cur_div_obj_x = new_div_x;
      cur_div_obj_y = new_div_y;
    }
    old_event_x = new_event_x;
    old_event_y = new_event_y;
} 

window.onload = setHandlers; 
</script>
EOQ;
}

start_body("main_background");


if(empty($menu_submit))  $menu_submit = "Seats";
if(!empty($new_seats))  $menu_submit = "Seats";

$menu_r=array();
$menu_r['active_page']= $menu_submit;
$menu_r['period'] = "$period";

$page_title = "Edit seating plan";

if (!$yes_input)
{
  main_menu($menu_r);
  $comment = "Please complete the following information:";
  title_comment_div_noend($page_title, $comment);
  echo input_form();
  end_html_exit();
}

$def_r = get_defaults_r($db_name);

$students_arr = get_students_arr( $db_name, $period, $def_r);  

if (!empty($new_seats))
  update_seating($db_name,$_POST);

$img_coords = get_img_coords($db_name, $period );
img_div_html($img_coords);

$hidden_inputs_html = array_to_html($db_name, $students_arr, $period);

$hidden_inputs_html .= <<<EOQ
<input type="hidden" name="period" value="$period">
EOQ;
$update_inputs = get_update_seats_inputs($hidden_inputs_html, $period);

$menu_r['update_inputs'] = $update_inputs;
main_menu($menu_r);

?>
</body></html>

