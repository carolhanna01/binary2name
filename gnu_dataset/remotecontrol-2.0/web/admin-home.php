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

render_head( 'Administration Dashboard' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;
    
  $tail->
    p()->
      T_H( 'Welcome to the administration section. ' )->
      T_H( 'Please select from the following options.' )->
    p_end()->
    ul()->
      li()->
        new_link(
          '/admin-location.php',
          'Location Administration'
        )->
      li_end()->
      li()->
        new_link(
          '/admin-group.php',
          'Group Administration'
        )->
      li_end()->
      li()->
        new_link(
          '/admin-user.php',
          'User Administration'
        )->
      li_end()->
      li()->
        new_link(
          '/admin-exception-list.php',
          'Exception List'
        )->
      li_end()->
      li()->
        new_link(
          '/admin-thermostat.php',
          'New Thermostat'
        )->
      li_end()->
      li()->
        new_link(
          '/admin-undelete.php',
          'Undelete Thermostats'
        )->
      li_end()->
      li()->
        new_link(
          '/admin-time.php',
          'Time Administration'
        )->
      li_end()->
      li()->
        new_link(
          '/admin-config.php',
          'System Configuration'
        )->
      li_end()->
    ul_end();
  
}
