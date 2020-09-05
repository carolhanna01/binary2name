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

function get_most_hidden() {
  
  $result = GrcHtmlComposite::Create();
  
  foreach ( $_GET as $key => &$val ) {
    
    if ( preg_match( '/^q($|_)/', $key ) ) { continue; }
    
    if ( is_array( $val ) ) {
      
      foreach ( $val as $subkey => $subval ) {
        
        $result->input(
          'type', 'hidden',
          'name', $key . '[]',
          'value', $subval
        );
        
      }
    }
    else {
    
      $result->input(
        'type', 'hidden',
        'name', $key,
        'value', $val
      );
      
    }
  }
  
  return $result;
  
}

function get_global_hidden_html() {
  
  $include = func_get_args();
  
  $query = array();

  global $global_attrs;
  
  foreach ( $global_attrs as $term ) {
    
    $value = read_g( $term, null );
    
    if ( $value !== null ) { $query[ $term ] = $value; }
    
  }
  
  foreach ( $include as $term ) {
    
    $value = read_g( $term, null );
    
    if ( $value !== null ) { $query[ $term ] = $value; }
    
  }
  
  //$elem = elem( 'span' );
  $elem = GrcHtmlComposite::Create();
  
  foreach ( $query as $key => $value ) {
    
    if ( is_array( $value ) ) {

      foreach ( $value as $inner_value ) {

        $elem->input(
          'type', 'hidden',
          'name', $key . '[]',
          'value', $inner_value
        );
        
      }
    }
    else {
      
      $elem->input( 'type', 'hidden', 'name', $key, 'value', $value );
      
    }
  }
  
  return $elem;
  
}

function get_query_hidden_html() {
  
  $args = func_get_args();
  
  $query = $_GET;
  
  foreach ( $args as $arg ) {
    
    unset( $query[ $arg ] );
    
  }
  
  //$elem = elem( 'span' );
  $elem = GrcHtmlComposite::Create();
  
  foreach ( $query as $key => $value ) {
    
    if ( is_array( $value ) ) {

      foreach ( $value as $inner_value ) {

        $elem->input(
          'type', 'hidden',
          'name', $key . '[]',
          'value', $inner_value
        );
        
      }
    }
    else {
      
      $elem->input( 'type', 'hidden', 'name', $key, 'value', $value );
      
    }
  }
  
  return $elem;
  
}

function get_timezone_select( $id, $selected = null ) {
  
  $select = elem( 'select', 'id', $id );

  $select->style = 'width:14em;';
  
  $timezones = timezone_identifiers_list();
  
  foreach ( $timezones as $timezone ) {
    
    $tz = new DateTimeZone( $timezone );

    $label = get_timezone_label( $tz );
            
    $select->option(
      'value', $timezone,
      'title', $label,
      'selected', ( $timezone === $selected )
    )->
      text( $timezone )->
    option_end();
    
  }
  
  return $select;
  
}

function get_location_select( $id, $selected = null ) {
  
  $select = elem( 'select', 'id', $id );

  $select->style = 'width:7em;';
  
  $locations = dal()->location->report();
  
  foreach ( $locations as $location ) {
    
    $location_id = $location[ 'id' ];
    $location_name = $location[ 'name' ];
    
    $select->option(
      'value', $location_id,
      'title', $location_name,
      'selected', ( $location_id === $selected )
    )->
      text( $location_name )->
    option_end();
    
  }
  
  return $select;
  
}

function get_language_select( $id, $selected = null ) {
  
  $select = elem( 'select', 'id', $id );

  $select->style = 'width:12em;';
  
  $languages = dal()->language->report();
  
  foreach ( $languages as $language ) {
    
    $langtag = $language[ 'langtag' ];
    $language_name = $language[ 'english_name' ];
    
    $select->option(
      'value', $langtag,
      'title', $language_name,
      'selected', ( $langtag === $selected )
    )->
      text( $language_name )->
    option_end();
    
  }
  
  return $select;
  
}

function get_user_menu( $id, $username ) {
  
  $menu = elem(
    'ul',
    'class', 'menu',
    'style', 'display:inline;clear:none;float:right;margin-left:1em;'
  );
  
  return $menu->
    li()->
      span( 'class', 'underline' )->
        text( $username )->
      span_end()->
      ul()->
        li()->
          add_link(
            '/admin-user.php'
          )->
        li_end()->
      ul_end()->
    li_end();

  return;
  
  foreach ( $timezones as $timezone ) {
    
    $link = get_link(
      henc( $timezone ),
      A( 'Click to change timezone.' )
    );
    $link->set( 'tz', $timezone );
    
    $tail->
      li()->
        add( $link->to_anchor() )->
      li_end();
    
  }
  
  return $tail->
        ul_end()->
      li_end()->
    ul_end();

}

function get_timezones_combo_html() {

  $headers = intl()->parse_accept_header();
    
  $countries = array();
  
  foreach ( $headers as $langtag ) {
  
    if ( preg_match( '/[A-Z]{2}/', $langtag, $matches ) ) {
      
      $countries[] = $matches[ 0 ];
      
    }
  }

  $timezones = array();
  
  foreach ( $countries as $country ) {

    $timezones = array_merge(
      $timezones,
      timezone_identifiers_list( DateTimeZone::PER_COUNTRY, $country )
    );
    
  }

  if ( count( $timezones ) === 0 ) {

    $timezones = timezone_identifiers_list( DateTimeZone::ALL );
    
  }

  $menu = elem(
    'ul',
    'class', 'menu timezone',
    'style', 'display:inline;clear:none;float:right;margin-left:1em;'
  );
  
  $tail = $menu->
    li()->
      span( 'class', 'underline' )->
        T_H( 'Timezone' )->
      span_end()->
      ul();

  foreach ( $timezones as $timezone ) {
    
    $link = get_link(
      henc( $timezone ),
      A( 'Click to change timezone.' )
    );
    $link->set( 'tz', $timezone );
    
    $tail->
      li()->
        add( $link->to_anchor() )->
      li_end();
    
  }
  
  $tail = $tail->
        ul_end()->
      li_end()->
    ul_end();
  
  return $menu;
    
}

function get_languages_combo_html() {
  
  $menu = elem(
    'ul',
    'class', 'menu language',
    'style', 'display:inline;clear:none;float:right;margin-left:1em;'
  );

  $tail = $menu->
    li()->
      span( 'class', 'underline' )->
        T_H( 'Language' )->
      span_end()->
      ul();

  foreach ( intl()->lang as $langtag => &$lang ) {
    
    $local_name = $lang[ 'local_name' ];
    
    $link = get_link(
      $local_name,
      A( 'Click to view content in %language%', 'language', $local_name )
    );
    $link->set( 'lang', $langtag );
    
    $tail->
      li()->
        add( $link->to_anchor() )->
      li_end();
    
  }
  
  $tail = $tail->
        ul_end()->
      li_end()->
    ul_end();
  
  return $menu;
  
  $elem = elem( 'div', 'id', 'languagediv' );
  
  $elem->
    form(
      'method', 'GET',
      'action', $uri
    )->
      add( get_query_hidden_html( 'lang' ) )->
      label(
        'for', 'languageddl'
      )->
        T_H( 'Display language:' )->
      label_end()->
      select(
        'id', 'languageddl',
        'name', 'lang'
      )->
        option(
          'value', 'en',
          'selected', ( $lang === 'en' )
        )->
          markup( 'English' )->
        option_end()->
        option(
          'value', 'es',
          'selected', ( $lang === 'es' )
        )->
          markup( 'Español' )->
        option_end()->
        option(
          'value', 'fr',
          'selected', ( $lang === 'fr' )
        )->
          markup( 'français' )->
        option_end()->
        option(
          'value', 'it',
          'selected', ( $lang === 'it' )
        )->
          markup( 'Italiano' )->
        option_end()->
        option(
          'value', 'de',
          'selected', ( $lang === 'de' )
        )->
          markup( 'Deutsch' )->
        option_end()->
      select_end()->
      input(
        'type', 'submit',
        'value', A( 'Go &raquo;' )
      )->
    form_end();
    
  return $elem;
  
}

function get_select_langtag() {
  
  $args = func_get_args();
  
  $arg_count = count( $args );
  
  if ( $arg_count > 0 ) {
    
    $selected = $args[ 0 ];
    
    array_shift( $args );
    
  }
  else {
    
    $selected = null;
    
  }
  
  $select = elem( 'select' );
  
  $select->attr_args( $args );
  
  foreach ( intl()->lang as $lang ) {
    
    $langtag = $lang[ 'langtag' ];
    
    $select->option(
      'value', $langtag,
      'selected', ( $langtag === $selected )
    )->
      text( $lang[ 'local_name' ] );
    
  }
    
  return $select;
  
}

function get_select_timezone() {
  
  $args = func_get_args();
  
  $arg_count = count( $args );
  
  if ( $arg_count > 0 ) {
    
    $selected = $args[ 0 ];
    
    array_shift( $args );
    
  }
  else {
    
    $selected = null;
    
  }
  
  $select = elem( 'select' );
  
  $select->attr_args( $args );
  
  $list = timezone_identifiers_list();
  
  foreach ( $list as &$timezone ) {
    
    $select->option(
      'value', $timezone,
      'selected', ( $timezone === $selected )
    )->
      text( $timezone );
    
  }
    
  return $select;
  
}

function get_group_select_html( &$thermostat ) {
  
  $thermostat_id = $thermostat[ 'id' ];
  $group_id = $thermostat[ 'group_id' ];
  
  //$groups = dal()->get_groups();
  $groups = dal()->group->report();
  
  $elem = elem(
    'select',
    'id', 'Group_' . $thermostat_id . '_Input',
    'class', 'view'
  );
  
  //$elem->option()->T_H( '-- Select Group --' );
  
  foreach ( $groups as &$group ) {
    
    $elem->option(
      'value', $group[ 'id' ],
      'selected', ( $group[ 'id' ] == $group_id )
    )->
      markup( $group[ 'name' ] );
    
  }
  
  return $elem;
  
}

function get_new_group_select_html() {
  
  //$groups = dal()->get_groups();
  $groups = dal()->group->report();
  
  $elem = elem(
    'select',
    'id', 'Group_0_Input',
    'class', 'required'
  );
  
  //$elem->option()->T_H( '-- Select Group --' );
  
  foreach ( $groups as &$group ) {
    
    $elem->option(
      'value', $group[ 'id' ]
      //, 'selected', ( $group[ 'GroupId' ] == $group_id )
    )->
      markup( $group[ 'name' ] );
    
  }
  
  return $elem;
  
}

function get_timezone_select_html( &$thermostat, $class = 'view' ) {

  $timezones = timezone_identifiers_list( DateTimeZone::ALL );
  
  $thermostat_id = $thermostat[ 'id' ];
  $timezone = $thermostat[ 'timezone' ];
    
  $elem = elem(
    'select',
    'id', 'TimeZone_' . $thermostat_id . '_Input',
    'class', $class
  );
  
  //$elem->option()->T_H( '-- Select Offset --' );
  
  foreach ( $timezones as $tz ) {
    
    $elem->option(
      'value', $tz,
      'selected', ( $tz == $timezone )
    )->
      text( $tz );
    
  }
  
  return $elem;
  
}

function get_new_timezone_select_html() {
    
  $thermostat = array( 'id' => 0, 'timezone' => '' );
  
  return get_timezone_select_html( $thermostat, null );
  
}

function get_thermostat_col_html( $heading, $column ) {

  $thermostat_sort = get_thermostat_sort( $sort_column );
  
  $asc_sort = $column;
  $desc_sort = '-' . $column;
  
  $asc_query = $_GET;
  $asc_query[ 'thermostat_sort' ] = $asc_sort;
  
  $desc_query = $_GET;
  $desc_query[ 'thermostat_sort' ] = $desc_sort;
  
  $asc_link = WEB_ROOT . '/home.php?' . http_build_query( $asc_query );
  $desc_link = WEB_ROOT . '/home.php?' . http_build_query( $desc_query );
  
  $class = ( $column === $sort_column ? 'sorted' : null );
  $asc_class = ( $thermostat_sort === $asc_sort ? 'sorted' : null );
  $desc_class = ( $thermostat_sort === $desc_sort ? 'sorted' : null );
  
  $elem = elem( 'div' )->
    div( 'class', $class )->
      markup( $heading )->
    div_end()->
    a( 'href', $asc_link, 'class', $asc_class )->
      markup( '&#9650;' )->
    a_end()->
    a( 'href', $desc_link, 'class', $desc_class )->
      markup( '&#9660;' )->
    a_end();
  
  return $elem;
  
}

function get_thermostats_tbody_html( &$thermostats ) {
    
  //$elem = elem( 'tbody' );
  $elem = GrcHtmlComposite::Create();
  
  $counter = 1;
  
  foreach ( $thermostats as &$thermostat ) {

    $class = ( $counter % 2 === 0 ? 'alt' : null );
    
    $id = $thermostat[ 'id' ];
    
    $row = $elem->tr( 'id', '~' . $id . '~' );
    
    $row->
      td()->
        input(
          'type', 'checkbox',
          'id', 'Select_' . $id,
          'onclick', 'toggleSelectThermostat(this);'
        )->
      td_end()->
      td()->
        a(
          'href', 'javascript:void(0);',
          'id', 'Edit_' . $id,
          'onclick', 'editThermostat(' . $id . ');',
          'class', 'edit'
        )->
          T_H( 'Edit' )->
        a_end()->
        a(
          'href', 'javascript:void(0);',
          'id', 'Update_' . $id,
          'onclick', 'update_thermostat(' . $id . ');',
          'class', 'update'
        )->
          T_H( 'Update' )->
        a_end()->
        a(
          'href', 'javascript:void(0);',
          'id', 'Cancel_' . $id,
          'onclick', 'cancelEditThermostat(' . $id . ');',
          'class', 'cancel'
        )->
          T_H( 'Cancel' )->
        a_end()->
      td_end()->
      td()->
        span()->markup( $counter . '.&nbsp;' )->span_end()->
      td_end()->
      td()->
        span()->text( read_a( $thermostat, 'device_name' ) )->span_end()->
      td_end()->
      td()->
        span()->text( read_a( $thermostat, 'site_name' ) )->span_end()->
      td_end()->
      td()->
        span()->text( read_a( $thermostat, 'model_number' ) )->span_end()->
      td_end()->
      td()->
        span(
          'id', 'Group_' . $id . '_Span'
        )->
          text( read_a( $thermostat, 'group' ) )->
        span_end()->
        add( get_group_select_html( $thermostat ) )->
      td_end()->
      td()->
        span(
          'id', 'Name_' . $id . '_Span',
          'class', 'view'
        )->
          text( $thermostat[ 'name' ] )->
        span_end()->
        input(
          'type', 'text',
          'id', 'Name_' . $id . '_Input',
          'value', aenc( $thermostat[ 'name' ] ),
          'class', 'view'
        )->
      td_end()->
      td()->
        span(
          'id', 'Description_' . $id . '_Span',
          'class', 'view'
        )->
          text( $thermostat[ 'description' ] )->
        span_end()->
        input(
          'type', 'text',
          'id', 'Description_' . $id . '_Input',
          'value', aenc( $thermostat[ 'description' ] ),
          'class', 'view'
        )->
      td_end()->
      td()->
        span(
          'id', 'Host_' . $id . '_Span',
          'class', 'view'
        )->
          text( $thermostat[ 'host' ] )->
        span_end()->
        input(
          'type', 'text',
          'id', 'Host_' . $id . '_Input',
          'value', aenc( $thermostat[ 'host' ] ),
          'class', 'view'
        )->
      td_end()->
      td()->
        span(
          'id', 'Port_' . $id . '_Span',
          'class', 'view'
        )->
          text( $thermostat[ 'port' ] )->
        span_end()->
        input(
          'type', 'text',
          'id', 'Port_' . $id . '_Input',
          'value', aenc( $thermostat[ 'port' ] ),
          'class', 'view'
        )->
      td_end()->
      td()->
        span(
          'id', 'Location_' . $id . '_Span',
          'class', 'view'
        )->
          text( $thermostat[ 'location' ] )->
        span_end()->
        //add( get_location_select_html( $thermostat ) )->
      td_end();
  
    $user = read_a( $thermostat, 'user' );
    $pass = read_a( $thermostat, 'pass' );
    
    $auth_string = "$user:$pass";
    
    if ( $user !== null && $pass !== null ) {
      
      $row->
        td()->
          a(
            'href', 'javascript:void(0);',
            'id', 'ChangeAuthInfo_' . $id,
            'onclick', 'getAuthInfo(' . $id . ',this);',
            'class', 'change'
          )->
            markup( 'Change' )->
          a_end()->
        td_end();
      
    }
    else {
      
      $row->
        td()->
          a(
            'href', 'javascript:void(0);',
            'id', 'SetAuthInfo_' . $id,
            'onclick', 'toggleAuthInfo(true,' . $id . ',this);',
            'class', 'set'
          )->
            markup( 'Set' )->
          a_end()->
        td_end();
      
    }
    
    $row->
      td()->
        a(
          'href', 'javascript:void(0);',
          'id', 'View_' . $id,
          'onclick', 'populateForm(' . $id . ',true);',
          'class', 'viewsettings'
        )->
          T_NBSP( 'View Settings' )->
        a_end()->
      /*
        span(
          'id', 'CurrView_' . $id,
          'class', 'currentlyviewing'
        )->
          markup( 'Currently Viewing' )->
        span_end()->
        span(
          'id', 'Success_' . $id,
          'class', 'success'
        )->
          markup( 'Done' )->
        span_end()->
        span(
          'id', 'Failure_' . $id,
          'class', 'failure'
        )->
          markup( 'Error' )->
        span_end()->
        span(
          'id', 'NoTrans_' . $id,
          'class', 'notrans'
        )->
          markup( 'NoTrans' )->
        span_end()->
      */
      td_end()->
      td()->
        a(
          'href', 'javascript:void(0);',
          'id', 'Delete_' . $id,
          'onclick', 'deleteThermostat(' . $id . ');',
          'class', 'delete'
        )->
          markup( 'Delete' )->
        a_end()->
      td_end()->
    tr_end();
    
    $counter++;
    
  }
  
  return $elem;
  
}

function get_thermostats_tfoot_html( &$thermostats ) {
  
  $max_thermostats = dal()->thermostat->get_max_thermostats_for_username(
    $_SERVER[ 'PHP_AUTH_USER' ]
  );
  
  if ( count( $thermostats ) >= $max_thermostats ) {
    
    return null;
    
  }
  
  //$tfoot = elem( 'tfoot' );
  $tfoot = GrcHtmlComposite::Create();

  $tfoot->
    tr( 'id', 'addrow' )->
      td( 'colspan', '6' )->
        div()->
          span( 'class', 'requirednote' )->
            markup( 'Highlighted fields are required.' )->
          span_end()->
        div_end()->
      td_end()->
      td()->
        add( get_new_group_select_html() )->
      td_end()->
      td()->
        input(
          'type', 'text',
          'id', 'Name_0_Input',
          'class', 'required'
        )->
      td_end()->
      td()->
        input(
          'type', 'text',
          'id', 'Description_0_Input'
        )->
      td_end()->
      td()->
        input(
          'type', 'text',
          'id', 'Host_0_Input',
          'class', 'required'
          //'onkeyup', 'RestrictToDomainChars(this);'
        )->
      td_end()->
      td()->
        input(
          'type', 'text',
          'id', 'Port_0_Input',
          'class', 'portinput required'
          //'onkeyup', 'return RestrictToInteger(this);'
        )->
      td_end()->
      td()->
        //add( get_new_timezone_select_html() )->
        add( get_location_select( 'Location_0_Input' ) )->
      td_end()->
      td()->
        comment()->text( 'login...' )->comment_end()->
      td_end()->
      td(
        'id', 'addlinkcell',
        'colspan', '4'
      )->
        a(
          'href', '#',
          'onclick', 'add_thermostat();',
          'class', 'add'
        )->
          markup( 'Add' )->
        a_end()->
      td_end()->
    tr_end();

  return $tfoot;
  
}

function get_setback_temp_select_html( $id, $name ) {
  
  $select = elem(
    'select',
    'id', $id,
    'name', $name,
    'class', 'fieldvalue'
  );
  
  for ( $i = 99; $i >= 40; $i-- ) {
    
    $select->option( 'value', $i )->text( $i );
    
  }
  
  return $select;
  
}

function get_remotesensor_correction_select_html( $id, $name ) {
  
  $select = elem(
    'select',
    'id', $id,
    'name', $name,
    'class', 'fieldvalue'
  );

  for ( $i = 50; $i >= -50; $i -= 5 ) {
    
    $select->option( 'value', $i )->text( $i / 10 );
    
  }
  
  return $select;

}

function get_menu() {
    
  $result = html_div( 'style', 'width:100%;clear:both' );
  
  $result->add( get_main_menu() );
  
  if ( is_translator() ) {
  
    //TODO: put this back in when intl ready...
    $result->add( get_intl_menu() );
  
  }
  
  if ( is_admin() ) {

    $result->add( get_admin_menu() );
    
  }
  
  $result->br( 'style', 'clear:both' );
  
  return $result;
  
}

function get_main_menu() {
  
  //$div = elem( 'div', 'style', 'margin:1em;display:block;clear:both;width:100%;' );
  
  return html_div(
    //'style', 'margin:1em;display:inline;clear:both;width:100%;float:left'
    'style', 'float:left;'
  )->
    ul( 'class', 'menu' )->
      li()->
        span( 'class', 'underline' )->
          T_H( 'Main Menu' )->
        span_end()->
        ul()->
          li()->
            new_link(
              '/home.php',
              H( 'Home Page' ),
              A( 'Return to the GNU Remote Control home page.' )
            )->
          li_end()->
          li()->
            new_link(
              '/history.php',
              H( 'Historical Reporting' ),
              A( 'Report on thermostat history.' )
            )->
          li_end()->
          li()->
            new_link(
              '/user-settings.php',
              H( 'User Settings' ),
              A( 'Configure your account.' )
            )->
          li_end()->
          /*
          test( is_administrator() )->
            li()->
              new_link(
                '/admin-home.php',
                H( 'Admin Home' ),
                A( 'Return to the GNUrc administration section.' )
              )->
            li_end()->
          test_end()->
          */
        ul_end()->
      li_end()->
    ul_end(); //->
  //div_end();
    
}

function get_intl_menu() {
      
  //$div = elem( 'div', 'style', 'margin:1em;display:block;clear:both;width:100%;' );
  
  return html_div(
    'style', 'float:left;'
  )->
    ul( 'class', 'menu' )->
      li()->
        span( 'class', 'underline' )->
          T_H( 'Language Admin Menu' )->
        span_end()->
        ul()->
          li()->
            new_link(
              '/intl-home.php',
              H( 'Language Administration Dashboard' ),
              A( 'Open the language administration dashboard.' )
            )->
          li_end()->
          li()->
            new_link(
              '/intl-language-list.php',
              H( 'Browse Languages' ),
              A( 'View registered languages.' )
            )->
          li_end()->
          li()->
            new_link(
              '/intl-language-add.php',
              H( 'Add a Language' ),
              A( 'Register a new supported language.' )
            )->
          li_end()->
          li()->
            new_link(
              '/intl-message-list.php',
              H( 'Browse Messages' ),
              A( 'View registered messages available for translation.' )
            )->
          li_end()->
          li()->
            new_link(
              '/intl-context-list.php',
              H( 'Browse Message Contexts' ),
              A( 'View registered message contexts.' )
            )->
          li_end()->
          li()->
            new_link(
              '/intl-help.php',
              H( 'Help' ),
              A( 'Get help with the language administration system.' )
            )->
          li_end()->
        ul_end()->
      li_end()->
    ul_end()->
    br( 'style', 'clear:both' );
  
}

function get_admin_menu() {
    
  return html_div(
    'style', 'float:left;'
  )->
    ul( 'class', 'menu' )->
      li()->
        span( 'class', 'underline' )->
          T_H( 'Admin Menu' )->
        span_end()->
        ul()->
          li()->
            new_link(
              '/admin-home.php',
              H( 'Administration Dashboard' ),
              A( 'Return to the GNUrc administration section.' )
            )->
          li_end()->
          li()->
            new_link(
              '/admin-location.php',
              H( 'Location Administration' ),
              A( 'Edit available locations.' )
            )->
          li_end()->
          li()->
            new_link(
              '/admin-group.php',
              H( 'Group Administration' ),
              A( 'Edit available thermostat groups.' )
            )->
          li_end()->
          li()->
            new_link(
              '/admin-user.php',
              H( 'User Administration' ),
              A( 'Edit active users.' )
            )->
          li_end()->
          li()->
            new_link(
              '/admin-exception-list.php',
              H( 'Exception List' ),
              A( 'Review system errors.' )
            )->
          li_end()->
          li()->
            new_link(
              '/admin-thermostat.php',
              H( 'New Thermostat' ),
              A( 'Register a new thermostat.' )
            )->
          li_end()->
          li()->
            new_link(
              '/admin-undelete.php',
              H( 'Undelete Thermostats' ),
              A( 'Restore deleted thermostats.' )
            )->
          li_end()->
          li()->
            new_link(
              '/admin-time.php',
              H( 'Time Administration' ),
              A( 'Set the time on active thermostats.' )
            )->
          li_end()->
          li()->
            new_link(
              '/admin-config.php',
              H( 'System Configuration' ),
              A( 'Report on system configuration settings.' )
            )->
          li_end()->
        ul_end()->
      li_end()->
    ul_end();
  
}

function get_intl_fallback_select() {
  
  //$languages = dal()->intl_get_languages();
  $languages = intl()->lang;
  
  $select = elem( 'select', 'id', 'fallback_select', 'name', 'fallback' );
  
  if ( count( $languages ) === 0 ) {
    
    $select->option( 'value', 'en' )->markup( 'English' );
    
  }
  else {
    
    foreach ( $languages as $language ) {
      
      $select->option( 'value', $language[ 'langtag' ] )->text(
        $language[ 'english_name' ]
      );
      
    }
    
  }
  
  return $select;
  
}

function render_page_style() {

  html()->
    find_tag( 'head' )->children[ 0 ]->
      link(
        'rel', 'stylesheet',
        'type', 'text/css',
        'href', WEB_ROOT . '/styles/page.css'
      )->
    restore();
  
}

function render_500( $ex, $id = 0 ) {

  html()->reset();

  render_head( 'Error' );

  $tail = html()->tail;
    
  $trace = $ex->getTraceAsString();
  
  $tail->
    p()->
      T_H( 'Error: ' )->
      span( 'class', 'error' )->
        text( $ex->getMessage() )->
      span_end()->
    p_end()->
    test( DEBUG )->
      pre()->
        markup( $trace )->
      pre_end()->
    test_end();

  render_foot();

  html()->render();
  
}

function get_user_select( $selected ) {
  
  $map = dal()->user->get_map();
  
  $result = elem( 'select' );
  
  foreach ( $map as $id => $username ) {
    
    $result->
      option( 'value', $id, 'selected', $id == $selected )->
        text( $username )->
      option_end();
    
  }
  
  return $result;
  
}