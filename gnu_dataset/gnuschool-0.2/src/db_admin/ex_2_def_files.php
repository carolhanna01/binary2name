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
  td_get_form("$file","$text", "export");
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

  echo "<tr><td><b>Create a default DBF file using the 
        database's student data.</b></td></tr>";
  $file = "./ex_stu_2_dbf.php";
  $text = "Create the dbase file";
  tr_td_get_form("$file","$text");
  tr_td_empty();

  echo "<tr><td><b>Create a default text file using the
        database's student data.</b></td></tr>";
  $file = "./ex_stu_2_txt.php";
  $text = "Create the text file";
  tr_td_get_form("$file","$text");
  tr_td_empty();

  echo "<tr><td><b>Create \"/tmp/teacher_students_out.txt\"
        using the database's teacher and student data.</b></td></tr>";
  $file = "./ex_tea_stu_2_txt.php";
  $text = "Create teachers and students text file";
  tr_td_get_form("$file","$text");
  tr_td_empty();

  echo "<tr><td><b>You may edit the database's
        default settings using this button.</b></td></tr>";
  $file="defaults.php";
  $text = "Edit default settings";
  tr_td_get_form("$file","$text");

  echo "</table>";
}


$tag_title = "gnuschool.org Export to Default File";
$page_title = "Export data to a file.";

start_html($tag_title);

stylesheet_link("admin_stylesheet");

start_body("admin_colors");
back_2_parentA("index");


$comment = "Export data to a file using one of the options below. If a 
default file is created, the default settings will be used.";
title_comment_div_noend($page_title, $comment);
anchors_table();
echo "<br>";
back_2_parentA("index");
echo "</DIV>";

?>
</body></html>

