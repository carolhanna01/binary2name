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

verify( is_admin() );

$id = intval( read_g( 'id', null ) );

if ( $id <= 0 ) {
  
  redirect( '/admin-exception-list.php' );
  
}

html( intl()->get_lang() );

$css = <<<EOF

  tbody th {
    
    vertical-align: top !important;
 
    padding: 0.4em !important;
    margin: 0px !important;
    padding-top: 0.8em !important;
 
  }
  
  pre { padding: 0.5em; margin: 0px; }
  
EOF;

render_head( 'Exception Details', $css );
  
render_page( $id, $error );

$script = <<<EOF
  
EOF;

render_foot( $script );

html()->render();

function render_page( $id, &$error ) {
  
  $data = dal()->exception->get_by_id( $id );

  //echo "<pre>"; var_dump( $data ); die;
  
  $tail = html()->tail;

  $tail->
    test( count( $error ) > 0 )->
      p( 'class', 'error' )->
        text( map_1st_value( $error ) )->
      p_end()->
    test_end()->
    form()->
      fieldset( 'id', 'exception_details' )->
        legend()->
          T_TH( 'Exception Details' )->
        legend_end()->
        table()->
          tbody()->
    
            tr()->
              th(
                'title', A( 'Exception ID' )
              )->
                label( 'for', 'exception_id' )->
                  T_TH( 'Exception ID' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Exception ID' )
              )->
                span(
                  'id', 'exception_id'
                )->
                  text( read_a( $data, 'id' ) )->
                span_end()->
              td_end()->
            tr_end()->
    
            tr()->
              th(
                'title', A( 'Timestamp' )
              )->
                label( 'for', 'timestamp' )->
                  T_TH( 'Timestamp' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Timestamp' )
              )->
                span(
                  'id', 'timestamp'
                )->
                  text( read_a( $data, 'timestamp' ) )->
                span_end()->
              td_end()->
            tr_end()->
    
            tr()->
              th(
                'title', A( 'User' )
              )->
                label( 'for', 'exception_user' )->
                  T_TH( 'User' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'User' )
              )->
                span(
                  'id', 'exception_user'
                )->
                  text( read_a( $data, 'username' ) )->
                span_end()->
              td_end()->
            tr_end()->
    
            tr()->
              th(
                'title', A( 'Type' )
              )->
                label( 'for', 'exception_type' )->
                  T_TH( 'Type' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Type' )
              )->
                span(
                  'id', 'exception_type'
                )->
                  text( read_a( $data, 'type' ) )->
                span_end()->
              td_end()->
            tr_end()->
    
    
            decl(
              $previous_id,
              intval( read_a( $data, 'previous_id' ) )
            )->
    
            tr()->
              th(
                'title', A( 'Previous' )
              )->
                label( 'for', 'previous' )->
                  T_TH( 'Previous' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Previous' )
              )->
                test( $previous_id > 0 )->
                  span(
                    'id', 'type'
                  )->
                    new_link(
                      '/admin-exception.php',
                      'previous',
                      'Click for details of previous exception.',
                      null, // class
                      array( 'id' => $previous_id )
                    )->
                  span_end()->
                test_end()->
              td_end()->
            tr_end()->

            tr()->
              th(
                'title', A( 'Message' )
              )->
                label( 'for', 'message' )->
                  T_TH( 'Message' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Message' )
              )->
                pre(
                  'id', 'message'
                )->
                  text( read_a( $data, 'message' ) )->
                pre_end()->
              td_end()->
            tr_end()->
    
            tr()->
              th(
                'title', A( 'Code' )
              )->
                label( 'for', 'exception_code' )->
                  T_TH( 'Code' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Code' )
              )->
                span(
                  'id', 'exception_code'
                )->
                  text( read_a( $data, 'code' ) )->
                span_end()->
              td_end()->
            tr_end()->
    
            tr()->
              th(
                'title', A( 'File' )
              )->
                label( 'for', 'exception_file' )->
                  T_TH( 'File' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'File' )
              )->
                span(
                  'id', 'exception_file'
                )->
                  text( read_a( $data, 'file' ) )->
                span_end()->
              td_end()->
            tr_end()->

            tr()->
              th(
                'title', A( 'Line' )
              )->
                label( 'for', 'exception_line' )->
                  T_TH( 'Line' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Line' )
              )->
                span(
                  'id', 'exception_line'
                )->
                  text( read_a( $data, 'line' ) )->
                span_end()->
              td_end()->
            tr_end()->

            tr()->
              th(
                'title', A( 'Messages' )
              )->
                label( 'for', 'message_stack' )->
                  T_TH( 'Messages' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Messages' )
              )->
                span(
                  'id', 'message_stack'
                )->
                  text( read_a( $data, 'message_stack' ) )->
                span_end()->
              td_end()->
            tr_end()->
    
            tr()->
              th(
                'title', A( 'Trace' )
              )->
                label( 'for', 'exception_trace' )->
                  T_TH( 'Trace' )->
                  markup( ':' )->
                label_end()->
              th_end()->
              td(
                'title', A( 'Trace' )
              )->
                span(
                  'id', 'exception_trace'
                )->
                  pre()->
                    text( trim( read_a( $data, 'trace' ) ) )->
                  pre_end()->
                span_end()->
              td_end()->
            tr_end()->
    
    
    
          tbody_end()->
        table_end()->
      fieldset_end()->
    form_end();
  
}
