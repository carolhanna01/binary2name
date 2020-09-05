<?php
/*

Copyright (C) 2012-2015 GNU remotecontrol authors.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

function render_foot( $script = null ) {

  $clear_link = new_link( null )->to_string();

  $clear_script = <<<EOF
    function submit_clear() {
      document.location = '$clear_link';
      return false;
    }
EOF;
  
  $node = html()->tail;
  
  $node->
            div_end()->
          div_end()->
        div_end()->
      div_end()->
      test( $script )->
        script()->markup( $script )->script_end()->
      test_end()->
      script()->markup( $clear_script )->script_end()->
      /*
      pre()->
        text( serialize( dal()->log ) )->
      pre_end()->
      */
    body_end()->
  html_end();  
  
}
