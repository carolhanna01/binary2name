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

set_error_handler( 'handle_error' );

set_exception_handler( 'handle_exception' );

function handle_error( $num, $str, $file, $line, $context = null ) {

  throw new ErrorException( $str, 0, $num, $file, $line );
  
}

function handle_exception( $ex ) {

  //echo "<pre>";
  //var_dump( $ex ); die;
  
  $id = 0;
  
  try {

    $id = dal()->exception->log( $ex );
    
  }
  catch ( Exception $ex ) {

    // hey, we tried...
    
  }
  
  if ( function_exists(  'http_response_code' ) ) {
  
    http_response_code( 500 );
  
  }
  else {
  
    header( 'HTTP/1.1 500 Internal Server Error' );
    
  }
  
  if ( AJAX ) {
    
    //echo 'Error: ' . $ex->getMessage();
    echo $ex->getMessage();
    
    //var_dump( $ex );
    
    exit;
    
  }
  
  render_500( $ex, $id );

  exit;
  
}
