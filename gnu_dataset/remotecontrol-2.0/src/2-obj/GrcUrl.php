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

function get_url_request() {

  return GrcUrl::Read();

}

function get_url_referrer() {

  if ( isset( $_SERVER[ 'HTTP_REFERER' ] ) ) {

    return GrcUrl::Parse( $_SERVER[ 'HTTP_REFERER' ] );

  }

  return GrcUrl::GetEmpty();

}

class GrcUrl {

  private static $EMPTY = null;

  public $service_count;
  public $scheme;
  public $domain;
  public $base;
  public $path;
  public $query;
  public $fragment;
  public $inner_html;
  public $title;
  public $class_list;

  public function __construct(
    $scheme,
    $domain,
    $base,
    $path,
    $query,
    $fragment,
    $inner_html,
    $title,
    $class_list
  ) {
  
    $this->service_count = 1;
    $this->scheme = $scheme;
    $this->domain = $domain;
    $this->base = $base;
    $this->path = $path;
    $this->query = $query;
    $this->fragment = $fragment;
    $this->inner_html = $inner_html;
    $this->title = $title;
    $this->class_list = $class_list;

  }

  public static function GetServerScheme() {

    if ( isset( $_SERVER[ "HTTPS" ] ) and $_SERVER[ "HTTPS" ] === "on" ) {

      return "https";

    }

    return "http";

  }

  public static function GetServerDomain() {

    if ( isset( $_SERVER[ "HTTP_HOST" ] ) ) {
  
      return $_SERVER[ "HTTP_HOST" ];

    }

    return "localhost";

  }

  public static function GetServerSchemeAndDomain() {

    return self::GetServerScheme() . "://" . self::GetServerDomain();

  }

  public static function Read() {

    $scheme = self::GetServerScheme();
    $domain = self::GetServerDomain();

    //$base = $_SERVER[ "SCRIPT_NAME" ];
    $base = WEB_ROOT;
    $path = "";
    $query = "";

    if ( isset( $_SERVER[ "PATH_INFO" ] ) ) {

      $path = $_SERVER[ "PATH_INFO" ];

    }
    else {
      
      $path = $_SERVER[ 'SCRIPT_NAME' ];
      
      $path = substr( $path, strlen( $base ) );
      
    }

    if ( isset( $_SERVER[ "QUERY_STRING" ] ) ) {

      $query = $_SERVER[ "QUERY_STRING" ];

    }

    if ( $path !== "" ) {
      $inner_html = henc( $path );
      $title = $path;
    }
    else {
      $inner_html = henc( $base );
      $title = $base;
    }

    $path = GrcUrlPath::Parse( $path );
    $query = GrcUrlQuery::Parse( $query );

    $fragment = null;
    
    $class = array( "applink" );

    return new GrcUrl(
      $scheme,
      $domain,
      $base,
      $path,
      $query,
      $fragment,
      $inner_html,
      $title,
      $class
    );

  }

  public static function Parse(
    $url,
    $base = "",
    $inner_html = "",
    $title = "",
    $class = ""
  ) {

    if ( $base === null ) { $base = ""; }
    if ( $inner_html === null ) { $inner_html = ""; }
    if ( $title === null ) { $title = ""; }
    if ( $class === null ) { $class = ""; }

    $base = trim( strval( $base ) );
    $inner_html = trim( strval( $inner_html ) );
    $title = trim( strval( $title ) );
    $class = trim( strval( $class ) );

    $scheme = null;
    $domain = null;
    $path = "";
    $query = "";
    $fragment = null;

    if ( $url !== null ) {

      $url = trim( strval( $url ) );

    }

    if ( $url === null or $url === "" ) {

      return self::GetEmpty();

    }

    $is_http = preg_match( "/^http\\:\\/\\/.*/", $url );
    $is_https = preg_match( "/^https\\:\\/\\/.*/", $url );

    if ( $is_http or $is_https ) {

      $scheme = $is_http ? "http" : "https";

      $url = substr( $url, $is_http ? 7 : 8 );

      $parts = explode( "/", $url );

      $domain = $parts[ 0 ];

      //unset( $parts[ 0 ] );
      $parts[ 0 ] = '';

      $url = implode( "/", $parts );

      $parts = explode( '?', $domain );

      if ( count( $parts ) > 1 ) {

        $domain = $parts[ 0 ];

        $parts[ 0 ] = '';

        $url = implode( '?', $parts ) . $url;

      }

      $parts = explode( '#', $domain );

      if ( count( $parts ) > 1 ) {

        $domain = $parts[ 0 ];

        $parts[ 0 ] = '';

        $url = implode( '#', $parts ) . $url;

      }

    }

    if ( strlen( $base ) > 0 && strpos( $url, $base ) >= 0 ) {

      $url = substr( $url, strlen( $base ) );

    }
    else { 

      $base = "";

    }

    preg_match( '/^([^\?\#]*)\??([^\#]*)\#?(.*)$/', $url, $matches );

    $path = $matches[ 1 ];
    $query = $matches[ 2 ];
    $fragment = $matches[ 3 ];

    if ( $fragment === '' && ! preg_match( '/#$/', $fragment ) ) {
      
      $fragment = null;
      
    }
    
    $full_path = $base . $path;

    if ( $inner_html === "" ) { $inner_html = henc( $full_path ); }
    if ( $title === "" ) { $title = $full_path; }

    $path = GrcUrlPath::Parse( $path );
    $query = GrcUrlQuery::Parse( $query );

    if ( $class === "" ) {

      $class = array();

    }
    else {

      $class = preg_split( "/\\s+/", $class );

    }

    return new GrcUrl(
      $scheme,
      $domain,
      $base,
      $path,
      $query,
      $fragment,
      $inner_html,
      $title,
      $class
    );

  }

  public function GetEmpty() {

    if ( self::$EMPTY === null ) {

      self::$EMPTY = new GrcUrl(
        null,
        null,
        "",
        GrcUrlPath::GetEmpty(),
        GrcUrlQuery::GetEmpty(),
        "",
        "",
        "",
        array()
      );

    }

    return self::$EMPTY;

  }

  public function register_global( /* values */ ) {

    global $global_attrs;
    
    if ( $global_attrs === null ) { $global_attrs = array(); }
    
    foreach ( func_get_args() as $value ) {
    
      if ( ! in_array( $value, $global_attrs ) ) {

        $global_attrs[] = $value;

      }
    }
  }
  
  public function is_empty() { return $this->to_string() === ""; }

  public function set_service_count( $value ) {
    $this->service_count = $value;
  }

  public function is_fully_qualified() {
    return ! ( $this->scheme === null or $this->domain === null );
  }

  public function get_scheme() { return $this->scheme; }
  public function get_domain() { return $this->domain; }
  public function get_scheme_and_domain() {
    if ( $this->scheme === null and $this->domain === null ) {
      return "";
    }
    if ( $this->scheme === null ) {
      return "http://" . $this->domain;
    }
    if ( $this->domain === null ) {
      return $this->scheme . "://localhost";
    }
    return $this->scheme . "://" . $this->domain;
  }
  public function get_full_path() { return $this->base . $this->path->to_string(); }
  public function get_base() { return $this->base; }
  public function get_path() { return $this->path->to_string(); }
  public function get_path_length() { return $this->path->get_length(); }
  public function get_path_part( $index ) {
    return $this->path->get_part( $index );
  }
  public function get_filename() { return $this->path->get_filename(); }
  public function get_extension() { return $this->path->get_extension(); }

  public function get_service( $index = -1 ) {
    if ( $index < 0 ) { $index += $this->service_count; }
    return $this->get_path_part( $index + 1 );
  }
  public function get_mode() { return $this->get_path_part( $this->service_count + 1 ); }
  public function get_selector() { return $this->get_path_part( $this->service_count + 2 ); }
  public function get_key() { return $this->get_path_part( $this->service_count + 3 ); }

  public function get_query() { return $this->query->to_string(); }
  public function get_fragment() { return $this->fragment; }
  public function get_inner_html() { return $this->inner_html; }
  public function get_title() { return $this->title; }
  public function get_class_list() { return $this->class_list; }
  public function get_class_text() { return implode( " ", $this->class_list->data ); }

  public function get( $key, $default = false ) {
    return $this->query->get_string( $key, $default );
  }
  public function get_bool( $key, $default = false ) {
    return $this->query->get_bool( $key, $default );
  }
  public function get_int( $key, $default = 0 ) {
    return $this->query->get_int( $key, $default );
  }
  public function get_string( $key, $default = "" ) {
    return $this->query->get_string( $key, $default );
  }
  public function get_html( $key, $default = "" ) {
    return $this->query->get_html( $key, $default );
  }
  public function get_first( $key ) {
    return $this->query->get_first( $key );
  }
  public function get_last( $key ) {
    return $this->query->get_last( $key );
  }
  public function get_all( $key ) {
    return $this->query->get_all( $key );
  }

  public function get_sort( $name = null, $default = null ) {
    if ( $name === null ) {
      $name = "sort";
    }
    else {
      $name = $name . "_sort";
    }
    $list = $this->get_all( $name );
    if ( count( $list ) === 0 ) {
      $list = $default;
    }
    return new GrcDataSort( $list );
  }

  public function get_page( $name = null, $default_page = 1, $default_size = 10 ) {
    if ( $name === null ) {
      $page_name = "page";
      $size_name = "size";
    }
    else {
      $page_name = $name . "_page";
      $size_name = $name . "_size";
    }
    $page = $this->get_first( $page_name );
    $size = $this->get_first( $size_name );
    if ( $page === null ) {
      $page = $default_page;
    }
    else {
      $page = intval( $page );
    }
    if ( $size === null ) {
      $size = $default_size;
    }
    else {
      $size = intval( $size );
    }
    return new GrcDataPage( $page, $size );
  }

  public function has_class( $class ) {

    return array_search( $class, $this->class_list ) >= 0;

  }

  public function to_builder(
    $inner_html = null,
    $title = null,
    $class = null
  ) {

    if ( $inner_html === null ) { $inner_html = $this->inner_html; }
    if ( $title === null ) { $title = $this->title; }
    if ( $class === null ) { $class = $this->class_list; }

    if ( ! is_array( $class ) ) {
      
      $class = preg_split( "/\\s+/", trim( $class ) );
      
    }

    return new GrcUrlBuilder(
      $this->scheme,
      $this->domain,
      $this->base,
      $this->path->to_builder(),
      $this->query->to_builder(),
      $this->fragment,
      $inner_html,
      $title,
      $class
    );

  }

  public function to_string() {

    $scheme_and_domain = $this->get_scheme_and_domain();

    $base = $this->base;
    $path = $this->path->to_string();
    $base_and_path = $base . $path;
    $query = $this->query->to_string();
    $fragment = $this->fragment;

    $url = $scheme_and_domain;

    if ( $scheme_and_domain === '' and $base_and_path === '' ) {

      $url .= '/';

    }
    else {

      $url .= $base_and_path;

    }

    if ( $query !== "" ) {

      $url .= "?" . $query;

    }

    if ( $fragment !== null ) {

      $url .= "#" . $fragment;

    }

    return $url;

  }

  public function to_html() {

    return aenc( $this->to_string() );

  }

  public function to_anchor( $inner_html = null, $title = null ) {

    $href = $this->to_string();
    $current = get_current_url();
    
    //if ( $href ===  $current ) {
    if ( strpos( $current, $href ) === 0 ) {

      if ( ! in_array( 'active', $this->class_list ) ) {

        if ( ! in_array( 'inactive', $this->class_list ) ) {

          $this->class_list[] = 'active';
          
        }
      }
    }
    
    if ( $inner_html === null ) { $inner_html = $this->inner_html; }
    if ( $title === null ) { $title = $this->title; }

    $class = implode( ' ', $this->class_list );
    $href = aenc( $href );

    $a = elem( 'a', 'class', $class, 'title', $title, 'href', $href );

    $a->markup( $inner_html );

    return $a;
    
  }

  public function render(
    $tail,
    $html_before = "",
    $html_after = ""
  ) {

    if ( $html_before ) { $tail->markup( $html_before ); }
    
    $tail->add( $this->to_anchor() );
    
    if ( $html_after ) { $tail->markup( $html_after ); }

  }

  public function __toString() {

    return $this->to_string();

  }
}

class GrcUrlBuilder extends GrcUrl {

  public function __construct(
    $scheme,
    $domain,
    $base,
    $path,
    $query,
    $fragment,
    $inner_html,
    $title,
    $class
  ) {

    parent::__construct(
      $scheme,
      $domain,
      $base,
      $path,
      $query,
      $fragment,
      $inner_html,
      $title,
      $class
    );

  }

  public function set_scheme( $value ) { $this->scheme = strval( $value ); }
  public function set_domain( $value ) { $this->domain = strval( $value ); }
  public function set_base( $value ) { $this->base = strval( $value ); }
  public function set_path( $value ) { $this->path->set_path( $value ); }
  public function set_full_path( $value ) {
    $this->set_base( "" );
    $this->set_path( $value );
  }
  public function set_fragment( $value ) { $this->fragment = $fragment; }
  public function set_inner_html( $value ) { $this->inner_html = strval( $value ); }
  public function set_title( $value ) { $this->title = strval( $value ); }
  public function set_class( $value ) {
    $this->class_list = array();
    $this->add_class( $value );
  }
  public function add_class( $value ) {
    if ( ! is_array( $value ) ) {
      $value = preg_split( "/\\s+/", trim( $value ) );
    }
    $this->class_list = array_unique( array_merge( $this->class_list, $value ) );
  }
  public function set_info( $inner_html = null, $title = null, $class = null ) {
    if ( $inner_html === null ) { $inner_html = henc( $this->get_full_path() ); }
    $inner_html = trim( strval( $inner_html ) );
    if ( $title === null ) { $title = strip_tags( $inner_html ); }
    $this->set_inner_html( $inner_html );
    $this->set_title( $title );
    if ( $class !== null ) {
      $class = trim( strval( $class ) );
      if ( $class === "" ) {
        $this->class_list = array();
      }
      else {
        $this->class_list = preg_split( "/\\s+/", $class );
      }
    }
  }

  public function copy( $inner_html = null, $title = null, $class = null ) {

    if ( $inner_html === null ) { $inner_html = $this->inner_html; }
    if ( $title === null ) { $title = $this->title; }
    if ( $class === null ) { $class = $this->class_list; }

    if ( is_string( $class ) ) {

      $class = preg_split( "/\\s+/", trim( $class ) );

    }

    return new GrcUrlBuilder(
      $this->scheme,
      $this->domain,
      $this->base,
      $this->path->copy(),
      $this->query->copy(),
      $this->fragment,
      $inner_html,
      $title,
      $class
    );

  }

  public function clear( $key = null ) {

    $this->query->clear( $key );

  }
  
  public function clear_all() {

    $this->query->clear_all();

  }
  
  public function set( $key, $value = null ) {
    
    $this->query->set( $key, $value );

  }
  
  public function set_page( $name = null, $page = 1, $size = 10 ) {
    if ( $name === null ) {
      $page_name = "page";
      $size_name = "size";
    }
    else {
      $page_name = $name . "_page";
      $size_name = $name . "_size";
    }
    $this->set( $page_name, $page );
    $this->set( $size_name, $size );
  }
  public function add( $key, $value ) {

    $this->query->add( $key, $value );

  }
}

class GrcUrlPath {

  private static $EMPTY = null;

  protected $parts;
  protected $extension;

  public function __construct( $parts, $extension ) {

    $this->parts = $parts;
    $this->extension = $extension;

  }

  public static function Parse( $path ) {

    $parts = explode( '/', trim( $path ) );

    foreach ( $parts as $index => $part ) {

      $parts[ $index ] = urldecode( $part );

    }

    $extension = null;

    if ( count( $parts ) > 1 ) {

      $last_index = count( $parts ) - 1;
      $last_part = $parts[ $last_index ];

      $dot_index = strrpos( $last_part, '.' );

      if ( $dot_index !== false ) {

        // a.b
        // 012

        $file_name = substr( $last_part, 0, $dot_index );
        $extension = substr( $last_part, $dot_index + 1 );

        $parts[ $last_index ] = $file_name;

      }
    }

    return new GrcUrlPath( $parts, $extension );

  }

  public static function GetEmpty() {

    if ( self::$EMPTY === null ) {

      self::$EMPTY = new GrcUrlPath( array( '' ), null );

    }

    return self::$EMPTY;

  }

  public function is_empty() { return count( $this->parts ) < 2; }

  public function get_filename() {

    return $this->parts[ count( $this->parts ) - 1 ];

  }

  public function get_extension() {

    return $this->extension;

  }

  public function get_service() { return $this->get_part( 1 ); }
  public function get_mode() { return $this->get_part( 2 ); }
  public function get_selector() { return $this->get_part( 3 ); }
  public function get_key() { return $this->get_part( 4 ); }

  public function get_part( $index ) {
    if ( isset( $this->parts[ $index ] ) ) { return $this->parts[ $index ]; }
    return null;
  }
  public function get_length() { return count( $this->parts ); }

  public function to_builder() {

    return new GrcUrlPathBuilder( array_values( $this->parts ), $this->extension );

  }

  public function to_array() { return array_values( $this->parts ); }

  public function __toString() { return $this->to_string(); }

  public function to_string() {

    $parts = array_values( $this->parts );

    foreach ( $parts as $index => $part ) {

      $parts[ $index ] = rawurlencode( $part );

    }

    $result = implode( "/", $parts );

    if ( strlen( $result ) > 0 and ! preg_match( "#^/#", $result ) ) {

      $result = "/" . $result;

    }

    if ( $this->extension !== null ) {

      $result .= '.' . $this->extension;

    }

    return $result;

  }
}

class GrcUrlPathBuilder extends GrcUrlPath {

  public function __construct( $parts, $extension ) {

    parent::__construct( $parts, $extension );

  }

  public function set_path( $value ) {

    if ( $value === null ) {

      $this->parts = array( "" );

      return;

    }

    $value = trim( $value );

    $is_relative = true;

    if ( strpos( $value, "/" ) === 0 ) {

      $is_relative = false;

    }

    if ( $is_relative ) {

      $parts = array_values( $this->parts );
                                 
      $new_parts = explode( "/", $value );

      $offset = count( $parts ) - 1;

      if ( $offset === 0 ) { $offset = 1; }

      foreach ( $new_parts as $index => $new_part ) {

        $parts[ $offset + $index ] = $new_part;

      }
    }
    else {

      $parts = explode( "/", $value );

    }

    $new_parts = array( '' );

    for ( $i = 1; $i < count( $parts ); $i++ ) {

      if ( $parts[ $i ] === '.' ) { continue; }
      if ( $parts[ $i ] === '..' ) {

        array_pop( $new_parts );
        continue;

      }

      $new_parts[] = $parts[ $i ];

    }

    if ( count( $new_parts ) === 0 ) { $new_parts = array( '' ); }

    $parts = $new_parts;

    $extension = null;

    if ( count( $parts ) > 0 ) {

      $last_index = count( $parts ) - 1;
      $last_part = $parts[ $last_index ];

      $dot_index = strrpos( $last_part, '.' );

      if ( $dot_index !== false ) {

        // a.b
        // 012

        $file_name = substr( $last_part, 0, $dot_index );
        $extension = substr( $last_part, $dot_index + 1 );

        $parts[ $last_index ] = $file_name;

      }
    }

    $this->parts = $parts;
    $this->extension = $extension;

  }

  public function set_filename( $value ) {

    $count = count( $this->parts );

    if ( $count < 1 ) {
      
      throw Error(
        'Invalid path parts.'
      );
      
    }

    if ( $count === 1 ) {

      $this->parts[] = $value;

    }
    else {

      $this->parts[ $count - 1 ] = $value;

    }
  }

  public function set_extension( $value ) {

    $this->extension = $value;

  }

  public function copy() {

    return new GrcUrlPathBuilder( array_values( $this->parts ), $this->extension );

  }
}

class GrcUrlQuery {

  private static $EMPTY = null;

  protected $head;

  public function __construct( $head ) {
    
    $this->head = $head;

  }

  public static function Parse( $query ) {
  
    $parts = explode( "&", $query );

    $head = new GrcUrlQueryNode();
    $last = $head;

    foreach ( $parts as $part ) {

      if ( $part === "" ) { continue; }

      $curr = new GrcUrlQueryNode();

      $field_parts = explode( "=", $part );

      $key = "";
      $value = "";

      $count = count( $field_parts );

      if ( $count > 0 ) { $key = urldecode( $field_parts[ 0 ] ); }
      
      if ( $count > 1 ) { $value = urldecode( $field_parts[ 1 ] ); }
      else { $value = null; }

      $curr->set_key( $key );
      $curr->set_value( $value );

      $last->set_next( $curr );
      $last = $curr;

    }

    return new GrcUrlQuery( $head );

  }

  public static function GetEmpty() {

    if ( self::$EMPTY === null ) {

      self::$EMPTY = new GrcUrlQuery( new GrcUrlQueryNode() );

    }

    return self::$EMPTY;

  }

  public function is_empty() { return $this->head->get_next() === null; }

  public function contains( $key ) {

    $curr = $this->head;

    while ( ( $curr = $curr->get_next() ) != null ) {

      if ( $curr->get_key() === $key ) { return true; }

    }

    return false;

  }

  public function get_bool( $key, $default = false ) {

    $value = $this->get_first( $key );

    if ( $value === null ) { return $default; }

    return boolval( $value );

  }

  public function get_int( $key, $default = 0 ) {

    $value = $this->get_first( $key );

    if ( $value === null ) { return $default; }

    return intval( $value );

  }

  public function get_string( $key, $default = "" ) {

    $value = $this->get_first( $key );

    if ( $value === null ) { return $default; }

    return strval( $value );

  }

  public function get_html( $key, $default = "" ) {

    return aenc( $this->get_string( $key, $default ) );

  }

  public function get_first( $key ) {

    $curr = $this->head;

    while ( ( $curr = $curr->get_next() ) != null ) {

      if ( $curr->get_key() === $key ) { return $curr->get_value(); }

    }

    return null;

  }

  public function get_last( $key ) {

    $result = null;

    $curr = $this->head;

    while ( ( $curr = $curr->get_next() ) != null ) {

      if ( $curr->get_key() === $key ) { $result = $curr->get_value(); }

    }

    return $result;

  }

  public function get_all( $key ) {

    $curr = $this->head;

    $result = array();

    while  ( ( $curr = $curr->get_next() ) != null ) {

      if ( $curr->get_key() === $key ) { $result[] = $curr->get_value(); }

    }

    return $result;

  }

  public function to_builder() {

    $head = $this->head->copy();

    return new GrcUrlQueryBuilder( $head );

  }

  public function to_string() {

    $result = "";

    $curr = $this->head;
    $is_first = true;

    while ( ( $curr = $curr->get_next() ) != null ) {

      if ( $is_first ) {

        $is_first = false;

      }
      else {

        $result .= "&";

      }

      $result .= rawurlencode( $curr->get_key() );

      if ( $curr->get_value() != null ) {

        $result .= "=" . rawurlencode( $curr->get_value() );

      }
    }

    return $result;

  }
}

class GrcUrlQueryBuilder extends GrcUrlQuery {

  public function __construct( $head ) {

    parent::__construct( $head );

  }

  public function copy() {

    return new GrcUrlQueryBuilder( $this->head->copy() );

  }

  public function clear_non_global() {

    global $global_attrs;
    
    $curr = $this->head;
    $prev = $curr;

    while ( ( $curr = $curr->get_next() ) !== null ) {

      if ( in_array( $curr->get_key(), $global_attrs ) ) {

        $prev = $curr;

      }
      else {
    
        $prev->set_next( $curr->get_next() );

      }
    }
  }

  public function clear( $key = null ) {

    if ( $key === null ) {

      $this->clear_non_global();
      return;

    }

    $curr = $this->head;
    $prev = $curr;

    while ( ( $curr = $curr->get_next() ) !== null ) {

      if ( $curr->get_key() === $key ) {

        $prev->set_next( $curr->get_next() );

      }
      else {
    
        $prev = $curr;

      }
    }
  }

  public function clear_all() {

    $this->head->set_next( null );

  }

  public function set( $key, $value = null ) {

    if ( is_array( $key ) ) {
    
      foreach( $key as $inner_key => &$value ) {
        
        $this->set( $inner_key, $value );
        
      }
      
      return;
      
    }
    
    if ( is_array( $value ) ) {
      
      $this->clear( $key );
      
      foreach ( $value as $inner_value ) {
        
        $this->add( $key, $inner_value );
        
      }

      return;
      
    }

    if ( $value === null ) {
      
      $this->clear( $key );
      
      return;
      
    }
    
    $key = strval( $key );
    $value = strval( $value );

    $curr = $this->head;

    while ( ( $curr = $curr->get_next() ) !== null ) {

      if ( $curr->get_key() === $key ) {

        $curr->set_value( $value );

        $prev = $curr;
        $curr = $curr->get_next();

        while ( $curr !== null ) {

          if ( $curr->get_key() === $key ) {

            $prev->set_next( $curr->get_next() );

          }

          $prev = $curr;
          $curr = $curr->get_next();

        }

        return;

      }
    }

    $this->tail()->set_next( new GrcUrlQueryNode( $key, $value ) );

  }

  public function add( $key, $value ) {

    if ( is_array( $key ) ) {
    
      foreach( $key as $inner_key => &$value ) {
        
        $this->add( $inner_key, $value );
        
      }
      
      return;
      
    }
    
    if ( is_array( $value ) ) {
      
      foreach ( $value as $inner_value ) {
        
        $this->add( $key, $inner_value );
        
      }

      return;
      
    }
    
    $key = strval( $key );
    $value = strval( $value );

    $last = $this->head;

    while ( $last->get_next() !== null ) {

      $last = $last->get_next();

    }

    $last->set_next( new GrcUrlQueryNode( $key, $value ) );

  }

  private function tail() {

    $curr = $this->head;

    while ( ( $next = $curr->get_next() ) !== null ) {

      $curr = $next;

    }

    return $curr;

  }
}

class GrcUrlQueryNode {

  private $key;
  private $value;
  private $next;

  public function __construct( $key = null, $value = null ) {

    $this->set_key( $key );
    $this->set_value( $value );

  }

  public function get_key() { return $this->key; }
  public function set_key( $value ) { $this->key = strval( $value ); }

  public function get_value() { return $this->value; }
  public function set_value( $value ) {
 
    if ( $value === null ) { $this->value = null; return; }

    switch ( gettype( $value ) ) {

      case "boolean" :

        $this->value = format_bool( $value );
        return;

      default :

        $this->value = strval( $value );
        return;

    }
  }

  public function get_next() { return $this->next; }
  public function set_next( $value ) { $this->next = $value; }

  public function copy() {

    $result = new GrcUrlQueryNode();
    $result->set_key( $this->key );
    $result->set_value( $this->value );
    if ( $this->next !== null ) {
      $result->set_next( $this->next->copy() );
    }
    return $result;

  }
}
