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

$css = <<<EOF
  
  .header {

    background-color: #555555 !important;
    /*border-color: #555555 !important;*/
    color: #eacc76 !important;
    text-align: center !important;   
  }
  
EOF;

html( intl()->get_lang() );

render_head( 'System Configuration', $css );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  $tail->
    p()->
      T_H( 'These are the system configuration settings. They are ' .
        'configured by editing the config.php file in the web root.' )->
    p_end()->
    table()->
      tbody()->
    
        tr()->
          th( 'class', 'header', 'colspan', 3 )->
            T_TH( 'Debug Settings' )->
          th_end()->
        tr_end()->
        tr()->
          th()->markup( 'DEBUG' )->th_end()->
          td()->text( format_bool( DEBUG ) )->td_end()->
          td()->T_H(
            'Whether debugging is supported or not.'
          )->td_end()->
        tr_end()->

        tr()->
          th( 'class', 'header', 'colspan', 3 )->
            T_TH( 'Database Settings' )->
          th_end()->
        tr_end()->
        tr()->
          th()->markup( 'DB_HOST' )->th_end()->
          td()->text( DB_HOST )->td_end()->
          td()->T_H(
            'IP address or domain name of MySQL server.'
          )->td_end()->
        tr_end()->
        tr()->
          th()->markup( 'DB_USER' )->th_end()->
          td()->text( DB_USER )->td_end()->
          td()->T_H(
            'Username of MySQL server user.'
          )->td_end()->
        tr_end()->
        tr()->
          th()->markup( 'DB_PASS' )->th_end()->
          td()->text( DB_PASS )->td_end()->
          td()->T_H(
            'Password of MySQL server user.'
          )->td_end()->
        tr_end()->
        tr()->
          th()->markup( 'DB_NAME' )->th_end()->
          td()->text( DB_NAME )->td_end()->
          td()->T_H(
            'Name of MySQL database.'
          )->td_end()->
        tr_end()->
        tr()->
          th()->markup( 'DB_TZ_NAME' )->th_end()->
          td()->text( format_bool( DB_TZ_NAME ) )->td_end()->
          td()->T_H(
            'MySQL named time zone support.'
          )->td_end()->
        tr_end()->
    
        tr()->
          th( 'class', 'header', 'colspan', 3 )->
            T_TH( 'Web Settings' )->
          th_end()->
        tr_end()->
        tr()->
          th()->markup( 'WEB_ROOT' )->th_end()->
          td()->text( WEB_ROOT )->td_end()->
          td()->T_H(
            'URL prefix.'
          )->td_end()->
        tr_end()->
    
        tr()->
          th( 'class', 'header', 'colspan', 3 )->
            T_TH( 'HTML Settings' )->
          th_end()->
        tr_end()->
        tr()->
          th()->markup( 'HTML_VERSION' )->th_end()->
          td()->text( HTML_VERSION )->td_end()->
          td()->T_H(
            '4: HTML 4; X: XHTML; 5: HTML 5.'
          )->td_end()->
        tr_end()->
        tr()->
          th()->markup( 'HTML_PRETTY' )->th_end()->
          td()->text( format_bool( HTML_PRETTY ) )->td_end()->
          td()->T_H(
            'true: pretty print HTML; false: compress HTML'
          )->td_end()->
        tr_end()->
    
        tr()->
          th( 'class', 'header', 'colspan', 3 )->
            T_TH( 'Temperature Settings' )->
          th_end()->
        tr_end()->
        tr()->
          th()->markup( 'DEFAULT_SCALE' )->th_end()->
          td()->text( DEFAULT_SCALE )->td_end()->
          td()->T_H( '1: Fahrenheit, 2: Celsius' )->td_end()->
        tr_end()->
    
        tr()->
          th( 'class', 'header', 'colspan', 3 )->
            T_TH( 'Date/Time Settings' )->
          th_end()->
        tr_end()->
        tr()->
          th()->markup( 'DEFAULT_TIMEZONE' )->th_end()->
          td()->text( DEFAULT_TIMEZONE )->td_end()->
          td()->td_end()->
        tr_end()->
    
        tr()->
          th( 'class', 'header', 'colspan', 3 )->
            T_TH( 'Network Settings' )->
          th_end()->
        tr_end()->
        tr()->
          th()->markup( 'WRITE_ATTEMPTS' )->th_end()->
          td()->text( WRITE_ATTEMPTS )->td_end()->
          td()->
            T_H( 'Number of times to retry device writes.' )->
          td_end()->
        tr_end()->
        tr()->
          th()->markup( 'READ_ATTEMPTS' )->th_end()->
          td()->text( READ_ATTEMPTS )->td_end()->
          td()->
            T_H( 'Number of times to retry device reads.' )->
          td_end()->
        tr_end()->
        tr()->
          th()->markup( 'NET_TIMEOUT' )->th_end()->
          td()->text( NET_TIMEOUT )->td_end()->
          td()->
            T_H( 'Number of seconds to wait for device operation to ' .
              'succeed.' )->
          td_end()->
        tr_end()->
        tr()->
          th()->markup( 'RECOVER_FROM_FACTORY_RESET' )->th_end()->
          td()->text( format_bool( RECOVER_FROM_FACTORY_RESET ) )->td_end()->
          td()->
            T_H( 'Whether to auto-recover from factory reset or not.' )->
          td_end()->
        tr_end()->
    
      tbody_end()->
    table_end();
  
}
