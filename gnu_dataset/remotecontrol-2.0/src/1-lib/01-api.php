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

function msg( $new = null ) {
  static $msg = null;
  if ( $new !== null ) {
    $msg = $new;
  }
  else if ( $msg === null ) {
    $msg = GrcMessage::Create();
  }
  return $msg;
}

function err( $new = null ) {
  static $err = null;
  if ( $new !== null ) {
    $err = $new;
  }
  else if ( $err === null ) {
    $err = GrcError::Create();
  }
  return $err;
}

function Error( /* code, paramters..., context?, $previous? */ ) {
  $args = func_get_args();
  return err()->get_error( $args );
}

function validator() {
  static $validator = null;
  if ( $validator === null ) {
    $validator = GrcValidator::Create();
  }
  return $validator;
}

function validate( $type, $name, &$data ) {
  global $error;
  //$val = $data[ $name ];
  $val = read_a( $data, $name );
  try {
    validator()->$type( $val );
  }
  catch ( Exception $ex ) {
    dal()->exception->log( $ex );
    $error[ $name ] = $ex->getMessage();
  }
}

function validate_array( $subtype, $name, &$data ) {
  $array = read_a( $data, $name );
  if ( ! is_array( $array ) ) {
    $error[ $name ] = S( 'Data is not an array.' );
    return;
  }
  foreach ( $array as $key => $val ) {
    validate( $subtype, $key, $array );
  }
}

function dal() {
  static $dal = null;
  if ( $dal === null ) {
    $dal = GrcDal::Create();
  }
  return $dal;
}

function bom() {
  static $bom = null;
  if ( $bom === null ) {
    $bom = GrcBom::Create();
  }
  return $bom;
}

function intl( $new = null ) {
  static $intl = null;
  if ( $new !== null ) {
    $intl = $new;
  }
  else if ( $intl === null ) {
    $intl = GrcIntl::Create();
  }
  return $intl;
}

function session() {
  static $session = null;
  if ( $session === null ) {
    $session = GrcSession::Load();
  }
  return $session;
}

function user() {
  static $user = null;
  if ( $user === null ) {
    $user = GrcUser::Load();
  }
  return $user;
}


function html() {
  static $html = null;
  if ( $html === null ) {
    $html = GrcHtmlElement::Create( 'html' );
    //$html->lang = intl()->get_lang();
    $html->lang = user()->get_lang();
  }
  return $html;
}

function html_div() {
  $elem = GrcHtmlElement::Create( 'div' );
  $args = func_get_args();
  $elem->attr_args( $args );
  return $elem;
}

function elem( $tag ) {
  $elem = GrcHtmlElement::Create( $tag );
  $args = func_get_args();
  array_shift( $args );
  $elem->attr_args( $args );
  return $elem;
}

function url() {
  static $url;
  if ( ! $url ) { $url = GrcUrl::Read(); }
  return $url;
}

function new_link(
  $path = null,
  $inner_html = null,
  $title = null,
  $class = null,
  $query = null,
  $fragment = null
) {
  
  $result = get_link( $inner_html, $title, $class, null, $fragment );
  
  $result->clear();
  
  if ( $path !== null ) { $result->set_path( $path ); }

  if ( $query ) {
    
    $result->set( $query );
    
  }
  
  return $result;
  
}

function get_link(
  $inner_html = null,
  $title = null,
  $class = null,
  $query = null,
  $fragment = null
) {

  $result = url()->to_builder( $inner_html, $title, $class );
  
  if ( $query ) {
    
    $result->set( $query );
    
  }
  
  if ( $fragment !== null ) {
    
    $result->set_fragment( $fragment );
    
  }

  return $result;
  
}

function ajax() {
  static $ajax = null;
  if ( $ajax === null ) {
    $ajax = GrcAjax::Create();
  }
  return $ajax;
}
