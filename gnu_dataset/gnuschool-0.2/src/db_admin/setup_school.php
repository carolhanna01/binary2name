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

include('functions.php');


function tr_td_get_form($file, $text)
{
  echo "<tr>";
  td_get_form("$file","$text", "setup");
  echo "</tr>";
}

function tr_td_empty()
{
  echo "<tr><td>&nbsp</td></tr>";
}


function anchors_table()
{
  echo <<<EOQ
    <table cellpadding="0" cellspacing="0" >
EOQ;

  echo "<tr><td><b>1: Create the user database.</b></td></tr>";
  $file = "./create_user.php";
  $text = "Create the user database";
  tr_td_get_form("$file","$text");
  tr_td_empty();

  echo "<tr><td><b>2: Create the school database. The first time you 
    use this button, you will need the user name and password of the
    new web administrator created above.</b></td></tr>";
  $file = "./create_main.php";
  $text = "Create the school database";
  tr_td_get_form("$file","$text");
  tr_td_empty();
  
  echo "<tr><td><b>After completing step 2, you may edit the database's
                    default settings using this button.</b></td></tr>";
  $file="./defaults.php";
  $text = "Edit default settings";
  tr_td_get_form("$file","$text");
  tr_td_empty();

  echo "<tr><td><b>3: Update the school's student data using
        any of the following 3 methods.</b></td></tr>";
  $file = "./im_dbf_2_main_stu.php";
  $text = "Use a default dbase file";
  tr_td_get_form("$file","$text");
  $file = "./im_txt_2_main_stu.php";
  $text = "Use a default text file";
  tr_td_get_form("$file","$text");
  $file = "./im_pas_2_main_stu.php";
  $text = "Place text in a textarea";
  tr_td_get_form("$file","$text");
  tr_td_empty();

  echo "<tr><td><b>4: Update the teacher's classes data using
        any of the following 2 methods.</b></td></tr>";
  $file = "./im_txt_2_teac_stu.php";
  $text = "Use a text file";
  tr_td_get_form("$file","$text");
  $file = "./im_pas_2_teac_stu.php";
  $text = "Place text in a textarea";
  tr_td_get_form("$file","$text");
  tr_td_empty();


  echo "<tr><td><b>5: Update the school's principal data
        using the default text file.</b></td></tr>";
  $file = "./im_txt_2_teac_pri.php";
  $text = "Use the text file";
  tr_td_get_form("$file","$text");
  tr_td_empty();


  echo "<tr><td><b>If you've completed the five steps 
         without major errors, congratulations!<br>
         You have completed the school system setup.</b></td></tr>";
  echo "</table>";
}


$tag_title = "gnuschool.org Setup Procedure";
$page_title = "Setup procedure for the school system.";

start_html($tag_title);

stylesheet_link("admin_stylesheet");

start_body("admin_colors");
back_2_parentA("index");


$comment = "Please complete these steps in order from top to bottom.";
title_comment_div_noend($page_title, $comment);
anchors_table();
echo "<br>";
back_2_parentA("index");
echo "</DIV>";

?>

</body></html>

