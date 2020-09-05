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

class GrcDataPage {

  public $page;
  public $size;
  public $name;
  public $start;
  public $finish;
  public $count;

  public function __construct( $page = 1, $size = DEFAULT_PAGE_SIZE, $name = null ) {

    $page = intval( $page );
    $size = intval( $size );

    if ( $size < 0 ) { $size = 0; }

    $this->page = $page;
    $this->size = $size;
    $this->name = $name;
    $this->start = 0;
    $this->finish = 0;
    $this->count = -1;

  }

  public static function Read( $default_page = 1, $default_size = DEFAULT_PAGE_SIZE, $name = null ) {

    $page_name = $name ? $name . '_page' : 'page';
    $size_name = $name ? $name . '_size' : 'size';
    
    $class = get_called_class();
        
    return new $class(
      array_key_exists( $page_name, $_GET ) ? intval( $_GET[ $page_name ] ) : $default_page,
      array_key_exists( $size_name, $_GET ) ? intval( $_GET[ $size_name ] ) : $default_size,
      $name
    );

  }
  
  public static function None() {

    $class = get_called_class();
    
    return new $class( 1, 0 );

  }

  public function get_page_query_term() {
    return $this->get_query_term( 'page' );
  }
  public function get_size_query_term() {
    return $this->get_query_term( 'size' );
  }
  
  public function set_count( $count ) {

    $this->count = $count;

    $this->init();

  }

  private function init() {

    $page = $this->page;
    $size = $this->size;

    $max_page = $this->max_page();

    if ( $page > $max_page ) {
      $page = $max_page;
    }
    if ( $page < 1 ) {
      $page = $max_page + $page;
    }
    if ( $page < 1 ) {
      $page = 1;
    }

    $start = ( $page - 1 ) * $size;
    $finish = $start + $size;

    $this->page = $page;
    $this->size = $size;
    $this->start = $start;
    $this->finish = $finish;

  }

  public function max_page() {

    if ( $this->size <= 0 ) {
      return 1;
    }

    if ( $this->count <= 0 ) {
      return 1;
    }

    $result = intval( $this->count / $this->size );

    if ( $this->count % $this->size > 0 ) {
      $result++;
    }

    if ( $result < 1 ) {
      $result = 1;
    }

    return $result;

  }

  public function to_sql() {

    if ( $this->size === 0 ) {
      return "";
    }

    return "\nLIMIT " . $this->start . ", " . $this->size;

  }

  public function get_page_links( $name ) {

    return PageLinks::Create( $this, $name );

  }

  public function render( $name ) {

    $this->get_page_links( $name )->render();

  }
  
  protected function get_query_term( $type ) {
    $name = $this->name;
    $term = $name ? $name . '_' . $type : $type;
    return $term;
  }
}
