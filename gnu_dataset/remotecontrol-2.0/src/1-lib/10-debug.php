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

function dump() {
  
  static $load = true;
  
  if ( $load ) {
    
    touch( DUMP_PATH );
    
    $load = false;
    
  }
  
  $args = func_get_args();
  
  if ( count( $args ) === 0 ) {
    
    file_put_contents( DUMP_PATH, "END\n\n", FILE_APPEND );
    
    return;
    
  }
  
  ob_start();

  var_dump( $args );

  $output = ob_get_clean();

  file_put_contents( DUMP_PATH, ':' . $output . "\n", FILE_APPEND );
    
}

/*
function grc_dump_end() { dump(); }

register_shutdown_function( 'grc_dump_end' );
*/
