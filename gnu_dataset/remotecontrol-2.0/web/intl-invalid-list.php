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

html( intl()->get_lang() );

render_head( 'Invalid Langtag List' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  //$tail->add( get_intl_menu() );

  $tail = $tail->
    p()->
      T_H( 'Invalid langtags are langtags which have been requested ' .
        'by web browsers but which are in an invalid format.' )->
    p_end();
  
  $table = new GrcTableView(
    'invalid_langtag',
    array(
      'langtag' => array(
        'heading' => A( 'Langtag', CONTEXT_TABLE_HEADING ),
        'type' => STRING_COLUMN
      ),
     'count' => array(
        'heading' => 'Count',
        'type' => INT_COLUMN
      ),
    )
  );
  
  $data = dal()->intl->get_langtag_invalid();
  $total = dal()->intl->get_langtag_invalid_total();

  $table->render( $data, $tail, $total );
  
}
