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

require_once __DIR__ . '/../src/include.php';

global $error;

verify( is_administrator() );

if ( is_post() ) {
  
  try {

    bom()->thermostat->set_times();
    
  }
  catch( Exception $ex ) {
    
    dal()->exception->log( $ex );
    
    $error = $ex->getMessage();
    
  }
}

html( intl()->get_lang() );

render_head( 'Time Administration' );

render_page( $error );

render_foot();

html()->render();

function render_page( $error ) {

  html()->tail->
    //p()->text( session()->get_xsrf_token() )->p_end()->
    test( is_post() )->
      test( $error )->
        p( 'style', 'color:red' )->
          text( $error )->
        p_end()->
      test_end()->
      test( $error === null )->
        p( 'style', 'color:green' )->
          markup( A( 'Times updated successfully.' ) )->
        p_end()->
      test_end()->
    test_end()->
    p()->
      text( 'Click to update times on all active thermostats:' )->
    p_end()->
    form( 'method', 'POST' )->
      //input( 'type', 'hidden', 'name', 'xsrf', 'value', session()->get_xsrf_token() )->
      hidden( 'xsrf', session()->get_xsrf_token() )->
      input( 'type', 'submit', 'value', A( 'Update Time' ) )->
    form_end();
  
}
