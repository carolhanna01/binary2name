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

// supported column types:

$i = 1;

define( 'CHECKBOX_COLUMN', $i++ );
define( 'BOOL_COLUMN', $i++ );
define( 'ID_COLUMN', $i++ );
define( 'INT_COLUMN', $i++ );
define( 'FLOAT_COLUMN', $i++ );
define( 'STRING_COLUMN', $i++ );
define( 'TEXT_COLUMN', $i++ );
define( 'FILE_COLUMN', $i++ );
define( 'HTML_COLUMN', $i++ );
define( 'LINK_COLUMN', $i++ );
define( 'LINK_DOMAIN_COLUMN', $i++ );
define( 'LINK_PATH_COLUMN', $i++ );
define( 'DATE_COLUMN', $i++ );
define( 'TIME_COLUMN', $i++ );
define( 'DATETIME_COLUMN', $i++ );
define( 'IPv4_COLUMN', $i++ );
define( 'HASH_COLUMN', $i++ );
define( 'SECONDS_COLUMN', $i++ );

class GrcTableView {

  public static $type_spec;
  
  public $name;
  public $schema;
  public $tfoot_spec;
  public $index_column;
  public $sort;
  public $page;
  public $gather;
  public $query_columns;
  public $selected_columns;
  public $included_columns;
  
  public function __construct(
    $name,
    $schema,
    $tfoot_spec = null,
    $index_column = 'id',
    $default_sort_column = null
  ) {

    $page_size = DEFAULT_PAGE_SIZE;
    
    if ( self::$type_spec === null ) {
      self::$type_spec = array(
        CHECKBOX_COLUMN => array(
          'compare' => 'boolcmp',
          'format' => function( &$row, $column, $schema, $tail, $view ) {
            $value = $row[ $column ];
            $selected = $value ? true : false;
            $tail->input(
              'id', $view->name . '_' . $column . '_' . $row[ 'id' ] . '_input',
              'type', 'checkbox',
              'checked', $selected,
              'autocomplete', 'off'
            );
            return;
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return yes_no( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return yes_no( $value );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : boolval( $value );
          }
        ),
        BOOL_COLUMN => array(
          'compare' => 'boolcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return yes_no( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return yes_no( $value );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : boolval( $value );
          }
        ),
        ID_COLUMN => array(
          'compare' => 'intcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $units = read_a( $schema, $column, 'units' );
            //$units = $units ? ' ' . $units : '';
            //return intval( $value ) . $units;
            //return format_int( $value, $units );
            return strval( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return strval( $value );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : intval( $value );
          }
        ),
        INT_COLUMN => array(
          'compare' => 'intcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $units = read_a( $schema, $column, 'units' );
            //$units = $units ? ' ' . $units : '';
            //return format_int( $value ) . $units;
            return format_int( $value, $units );
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $units = read_a( $schema, $column, 'units' );
            return format_int( $value, $units );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : intval( $value );
          }
        ),
        FLOAT_COLUMN => array(
          'compare' => 'floatcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $units = read_a( $schema, $column, 'units' );
            //$units = $units ? ' ' . $units : '';
            $result = '<span title="' . $value . ' precisely.">' .
              format_float( $value, $units ) . '</span>';
            return $result;
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $units = read_a( $schema, $column, 'units' );
            $result = '<span title="' . $value . ' precisely.">' .
              format_float( $value, $units ) . '</span>';
            return $result;
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : floatval( $value );
          }
        ),
        STRING_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $tail->
              span( 'title', aenc( $value ) )->
                T_T( truncate_text( $value ) )->
              span_end();
            //return nbsp( henc( $value ) );
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $tail->
              span( 'title', aenc( $value ) )->
                T_T( truncate_text( $value ) )->
              span_end();
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        TEXT_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $tail->
              span( 'title', aenc( $value ) )->
                T_T( truncate_text( $value ) )->
              span_end();
            //return henc( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $tail->
              span( 'title', aenc( $value ) )->
                T_T( truncate_text( $value ) )->
              span_end();
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        FILE_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $parts = explode( '/', $value );
            $count = count( $parts );
            if ( $count === 1 ) {
              $parts = explode( '\\', $value );
              $count = count( $parts );
            }
            $tail->
              span( 'title', aenc( implode( DIRECTORY_SEPARATOR, $parts ) ) )->
                T_T( $parts[ $count - 1 ] )->
              span_end();
            //return henc( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $parts = explode( '/', $value );
            $count = count( $parts );
            if ( $count === 1 ) {
              $parts = explode( '\\', $value );
              $count = count( $parts );
            }
            $tail->
              span( 'title', aenc( implode( DIRECTORY_SEPARATOR, $parts ) ) )->
                T_T( $parts[ $count - 1 ] )->
              span_end();
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        HTML_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            //return $value;
            return truncate_html( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $value = read_a( $row, $column );
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return truncate_html( $value );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        LINK_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $link_name = $schema[ $column ][ 'link-text' ];
            if ( $link_name instanceof Closure ) {
              $link_name = $link_name( $row, $column, $schema );
            }
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $url = aenc( $value );
            $url = '<a href="' . $url . '">' . $link_name . '</a>';
            return $url;
          },
          'gather' => function( &$row, $column, $schema, $tail, $view ) {
            $link_name = $schema[ $column ][ 'link-text' ];
            if ( $link_name instanceof Closure ) {
              $link_name = $link_name( $row, $column, $schema );
            }
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $url = aenc( $value );
            $url = '<a href="' . $url . '">' . $link_name . '</a>';
            return $url;
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        LINK_DOMAIN_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $parts = explode( '/', $value );
            $link_name = $parts[ 2 ];
            $url = aenc( $value );
            $url = '<a href="' . $url . '">' . $link_name . '</a>';
            return $url;
          },
          'gather' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $parts = explode( '/', $value );
            $link_name = $parts[ 2 ];
            $url = aenc( $value );
            $url = '<a href="' . $url . '">' . $link_name . '</a>';
            return $url;
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            if ( ! is_null( $value ) ) {
              $parts = explode( '/', $value );
              return $parts[ 2 ];
            }
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        LINK_PATH_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $parts = explode( '?', $value, 2 );
            $parts = explode( '/', $parts[ 0 ], 4 );
            $link_name = read_a( $parts, 3 );
            if ( strpos( $link_name, ltrim( CONTROLLER, '/' ) ) === 0 ) {
              $link_name = substr( $link_name, strlen( CONTROLLER ) );
            }
            $url = aenc( $value );
            $url = '<a href="' . $url . '">' . $link_name . '</a>';
            return $url;
          },
          'gather' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            $parts = explode( '?', $value, 2 );
            $parts = explode( '/', $parts[ 0 ], 4 );
            $link_name = read_a( $parts, 3 );
            if ( strpos( $link_name, ltrim( CONTROLLER, '/' ) ) === 0 ) {
              $link_name = substr( $link_name, strlen( CONTROLLER ) );
            }
            $url = aenc( $value );
            $url = '<a href="' . $url . '">' . $link_name . '</a>';
            return $url;
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            if ( ! is_null( $value ) ) {
              $parts = explode( '?', $value, 2 );
              $parts = explode( '/', $parts[ 0 ], 4 );
              $link_name = read_a( $parts, 3 );
              if ( strpos( $link_name, ltrim( CONTROLLER, '/' ) ) === 0 ) {
                $link_name = substr( $link_name, strlen( CONTROLLER ) );
              }
              return $link_name;
            }
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        DATE_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $override = read_a( $schema, $column, 'column' );
            if ( $override ) { $column = $override; }
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            //return nbsp( utc_to_local( $value )->format( 'Y-m-d' ) );
            return nbsp( format_value( $value ) );
          },
          'gather' => function( &$row, $column, $schema, $tail ) {
            $override = read_a( $schema, $column, 'column' );
            if ( $override ) { $column = $override; }
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            //return nbsp( utc_to_local( $value )->format( 'Y-m-d' ) );
            return nbsp( format_value( $value ) );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        TIME_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $override = read_a( $schema, $column, 'column' );
            if ( $override ) { $column = $override; }
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            //return nbsp( utc_to_local( $value )->format( 'H:i:s' ) );
            return nbsp( format_value( $value ) );
          },
          'gather' => function( &$row, $column, $schema, $tail ) {
            $override = read_a( $schema, $column, 'column' );
            if ( $override ) { $column = $override; }
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            //return nbsp( utc_to_local( $value )->format( 'H:i:s' ) );
            return nbsp( format_value( $value ) );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        DATETIME_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            //return nbsp( utc_to_local( $value )->format( DB_DATETIME ) );
            return nbsp( format_value( $value ) );
          },
          'gather' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            //return nbsp( utc_to_local( $value )->format( DB_DATETIME ) );
            return nbsp( format_value( $value ) );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : strval( $value );
          }
        ),
        IPv4_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return bin2ip( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return bin2ip( $value );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : bin2ip( $value );
          }
        ),
        HASH_COLUMN => array(
          'compare' => 'strcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return bid2uid( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return bid2uid( $value );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : bid2uid( $value );
          }
        ),
        SECONDS_COLUMN => array(
          'compare' => 'floatcmp',
          'format' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return format_seconds( $value );
          },
          'gather' => function( &$row, $column, $schema, $tail ) {
            $value = $row[ $column ];
            if ( is_null( $value ) ) {
              $value = read_a( $schema, $column, 'null' );
            }
            if ( is_null( $value ) ) {
              return '';
            }
            return format_seconds( $value );
          },
          'parse' => function(  &$row, $column, $schema ) {
            $value = $row[ $column ];
            $null = read_a( $schema, $column, 'null' );
            return is_null( $value ) ? $null : 0;
          }
        )
      );
    }
    
    foreach ( $schema as $column_name => &$spec ) {
            
      $type = read_a( $spec, 'type' );
      
      if ( $type === null ) {
        
        $spec[ 'type' ] = $type = STRING_COLUMN;
        
      }
      
      $depends = read_a( $spec, 'depends' );
      
      if ( $depends === null ) {
        
        $spec[ 'depends' ] = $depends = array();
        
      }
      else {
        
        $depends = &self::ReadDepends( $depends );

        foreach ( $depends as $depends_column ) {
          
          if ( ! array_key_exists( $depends_column, $schema ) ) {
            
            throw Error(
              'Missing column "%column%".',
              'column', $column
            );
            
          }
        }        
      }

      $heading = read_a( $spec, 'heading' );
            
      if ( $heading === null ) {
        
        $heading = $this->compute_heading( $column_name, $type );
                
        $spec[ 'heading' ] = $heading;
        
      }
      else if (
        is_object( $heading ) &&
        get_class( $heading ) === 'Closure'
      ) {
        
        $heading = $heading();
        
      }

      $spec[ 'heading' ] = $heading = nbsp( $heading );

      $default = read_a( $spec, 'default' );
      
      if ( $default === null ) {
        
        $spec[ 'default' ] = $default = true;
        
      }
      
      $compare = read_a( $spec, 'compare' );
      
      if ( $compare === null ) {
        
        $spec[ 'compare' ] = $compare = self::$type_spec[ $type ][ 'compare' ];
        
      }
      
      $format = read_a( $spec, 'format' );
      
      if ( $format === null ) {

        $spec[ 'format' ] = $format = self::$type_spec[ $type ][ 'format' ];
        
      }
      
      $gather = read_a( $spec, 'gather' );
      
      if ( $gather === null ) {

        $spec[ 'gather' ] = $gather = self::$type_spec[ $type ][ 'gather' ];
        
      }
      
      $parse = read_a( $spec, 'parse' );
      
      if ( $parse === null ) {

        $spec[ 'parse' ] = $parse = self::$type_spec[ $type ][ 'parse' ];
        
      }
      
      $link_text = read_a( $spec, 'link-text' );
      
      if ( $link_text === null && $type === LINK_COLUMN ) {
        
        $spec[ 'link-text' ] = $link_text = 'link';

      }
      
      $is_computed = read_a( $spec, 'is-computed' );
      
      if ( $is_computed === null ) {
        
        $spec[ 'is-computed' ] = false;
        
      }
      
      $no_sort = read_a( $spec, 'no-sort' );
      
      if ( $no_sort === null ) {
        
        $spec[ 'no-sort' ] = false;
        
      }
      
      $no_menu = read_a( $spec, 'no-menu' );
      
      if ( $no_menu === null ) {
        
        $spec[ 'no-menu' ] = false;
        
      }
      
      $align = read_a( $spec, 'align' );
      
      if ( $align === null ) {
        
        $spec[ 'align' ] = 'center';
        
      }
      
      $sql = read_a( $spec, 'sql' );
      
      if ( $sql === null ) {
        
        $spec[ 'sql' ] = $column_name;
        
      }
    }

    $this->name = $name;
    $this->schema = $schema;
    $this->tfoot_spec = $tfoot_spec;
    $this->index_column = $index_column;
    
    $this->query_columns = null;
    $this->selected_columns = null;
    $this->included_columns = null;

    // note: this sets selected_columns and included_columns:
    $included_columns = &$this->get_included();
    
    if ( $default_sort_column === null ) {
    
      if ( count( $included_columns ) > 0 ) {

        foreach ( $included_columns as $included_column ) {

          if ( ! $schema[ $included_column ][ 'is-computed' ] ) {

            $default_sort_column = '-' . $included_column;

            break;

          }
        }
      }
    }
    
    $this->sort = GrcDataSort::Read( $default_sort_column, $name );
    
    $this->sort->set_schema( $schema );
    
    $this->page = GrcDataPage::Read( 1, $page_size, $name );
    
    $this->gather = read_a( $_GET, $this->get_gather_query_term() );
    
    $this->validate_column_list(
      $this->sort->field_list,
      $this->get_sort_query_term(),
      true
    );

  }
  
  public static function Create( $name, $schema, $tfoot_spec ) {
    
    //$page_size = DEFAULT_PAGE_SIZE\
    
    $class = get_called_class();
    
    return new $class( $name, $schema, $tfoot_spec );
    
  }

  public static function &ReadDepends( &$depends ) {
    
    if ( ! is_array( $depends ) ) {

      $result = array_map( 'trim', explode( ',', $depends ) );
      
      return $result;

    }

    return $depends;
    
  }
  
  public function compute_heading(
    $column_name,
    $type = STRING_COLUMN
  ) {
    
    $heading = implode(
      ' ',
      array_map(
        'ucfirst',
        explode(
          '_',
          $column_name
        )
      )
    );

    if ( $type === CHECKBOX_COLUMN || $type === BOOL_COLUMN ) {
      
      $heading .= '?';
      
    }

    return TABLE_HEADING( $heading );
    
  }
  
  public function set_query_columns() {
    
    foreach ( func_get_args() as $column ) {
      
      if ( ! in_array( $column, $this->included_columns ) ) {
        
        $this->included_columns[] = $column;
        
      }
    }
  }
  
  
  public function get_gather_query_term() {
    return $this->get_query_term( 'gather' );
  }
  public function get_cols_query_term() {
    return $this->get_query_term( 'cols' );
  }
  public function get_sort_query_term() {
    return $this->get_query_term( 'sort' );
  }
  public function get_page_query_term() {
    return $this->get_query_term( 'page' );
  }
  
  // gets a list of columns to include in output
  public function &get_included() {
  
    if ( $this->included_columns === null ) {

      $included_columns = $this->get_selected();
      
      $column_list = array_keys( $this->schema );

      foreach ( $column_list as $column ) {

        $depends = read_a( $this->schema, $column, 'depends' );

        if ( $depends ) {

          $depends = &self::ReadDepends( $depends );

          foreach ( $depends as $depends_column ) {

            if ( ! in_array( $depends_column, $included_columns ) ) {

              $included_columns[] = $depends_column;

            }
          }          
        }
      }

      $gather_column = $this->gather;

      if ( strlen( $gather_column ) > 0 ) {

        $included_columns[] = $gather_column;

      }

      $this->included_columns = &$included_columns;

      //$this->validate_column_list( $this->included_columns );
      
    }
   
    return $this->included_columns;
    
  }
    
  // gets the list of columns selected by the user
  public function &get_selected() {

    if ( $this->selected_columns === null ) {

      $spec = read_a( $_GET, $this->get_cols_query_term() );

      if ( $spec ) {

        $this->selected_columns = array_map( 'trim', explode( ' ', $spec ) );

        $this->validate_column_list(
          $this->selected_columns,
          $this->get_cols_query_term(),
          true // check included
        );
        
      }
      else {

        $selected_columns = array();

        $column_list = array_keys( $this->schema );

        foreach ( $column_list as $column ) {

          $default = read_d( $this->schema, $column, 'default', true );

          if ( $default ) { $selected_columns[] = $column; }

        }

        $this->selected_columns = &$selected_columns;

      }      
    }
    
    return $this->selected_columns;
    
  }
  
  public function render_columns( $tail ) {

    $selected_columns =  &$this->get_selected();
    
    $cols_term = $this->get_cols_query_term();
    
    $column_list = array_keys( $this->schema );

    $tail = $tail->
      ul(
        'class', 'menu right'
        //'style', 'display:inline;clear:none;float:right;'
      )->
        li(
          //'style', 'margin-right:0.4em;display:inline;clear:none;'
        )->
          span( 'class', 'underline' )->
            //add( $columns->to_anchor() )->
            T_H( 'Columns' )->
          span_end();
    
    $tail = $tail->
      ul( 'style', 'display:inline;clear:none' );
    
    $default = get_link( H( 'Default' ), A( 'Show default columns.' ) );
    $default->clear( $cols_term );
    
    $all = get_link( H( 'All' ), A( 'Show all columns.' ) );
    $all->set( $cols_term, implode( ' ', $column_list ) );

    $tail = $tail->li();

    $all->render( $tail );
    
    //$tail = $tail->li_end()->li( 'class', 'underline' );
    $tail = $tail->li_end()->li();
    
    $default->render( $tail );

    $tail = $tail->
      li_end();
    
    foreach ( $column_list as $column ) {

      $default = read_d( $this->schema, $column, 'default', true );

      $selected = in_array( $column, $selected_columns );

      $no_menu = read_a( $this->schema, $column, 'no-menu' );
      
      $heading = read_a( $this->schema, $column, 'heading' );
      
      if (
        $heading === '' || 
        $no_menu
      ) {

        $type = read_a( $this->schema, $column, 'type' );
        
        $heading = $this->compute_heading( $column, $type );
        
      }
      
      $html = ( $selected ? H( 'Remove ' ) : H( 'Add ' ) ) .
        $heading;
      
      $new_selected = $selected_columns;
      
      if ( $selected ) {

        $index = array_search( $column, $new_selected );
        
        unset( $new_selected[ $index ] );
        
      }
      else {

        $new_selected[] = $column;
        
      }      
      
      $link = get_link( $html, '' );
      $link->set( $cols_term, implode( ' ', $new_selected ) );

      $tail = $tail->li();

      $link->render( $tail );

      $tail = $tail->li_end();
      
    }
    
    $tail->ul_end()->li_end()->ul_end();
    
  }
  
  public function to_div(
    &$data,
    &$total = null,
    $highlight_spec = null,
    $top_nav = true,
    $bottom_nav = true,
    $sort_links = true,
    $search_row = false
  ) {

    $div = elem( 'div', 'class', 'table' );
    
    $this->render(
      $data,
      $div,
      $total,
      $highlight_spec,
      $top_nav,
      $bottom_nav,
      $sort_links,
      $search_row
    );
    
    return $div;
    
  }
  
  public function &search() {
    
    $search = array();
    
    $selected_columns = &$this->get_selected();

    foreach ( $selected_columns as $column ) {
      
      $search[ $this->schema[ $column ][ 'sql' ] ] =
        read_g( 'q_' . $column, '' );
      
    }
    
    return $search;
    
  }
  
  public function render(
    &$data,
    $tail,
    &$total = null,
    $highlight_spec = null,
    $top_nav = true,
    $bottom_nav = true,
    $sort_links = true,
    $search_row = false,
    $clear_function = 'submit_clear'
  ) {

    $i = 0;
    
    /*
    if ( count( $data ) === 0 ) {
      
      $tail->p( 'style', 'margin-left:1em' )->markup( H( 'No data.' ) )->p_end();
      
      return;
      
    }
    */
    
    $sort = $this->sort;
    
    $selected_columns = &$this->get_selected();
    
    $gather_column = $this->gather;

    $page_links = GrcPageLinks::Create( $this->page, $this->name );

    $tail = $tail->div( 'class', 'table-nav-top' );

    $this->render_columns( $tail );

    if ( $top_nav ) {
    
      $page_links->render( $tail );  

    }
    
    $tail = $tail->div_end();

    $column_list = array_keys( $this->schema );

    $shuffler = GrcListShuffler::Create( $selected_columns );
    
    $tail = $tail->
      table(
        'id', $this->name . '_table',
        'class', 'table table-stiped table-bordered table-condensed'
      )->
        thead()->
          tr();
        
    $cols_term = $this->get_cols_query_term();
    $gather_term = $this->get_gather_query_term();
    $sort_term = $this->get_sort_query_term();

    foreach ( $selected_columns as $column ) {

      $is_computed = $this->schema[ $column ][ 'is-computed' ];
      $no_sort = $this->schema[ $column ][ 'no-sort' ];
      $no_menu = $this->schema[ $column ][ 'no-menu' ];
      
      if ( $is_computed ) { $no_sort = true; }
      
      $sort_ascending = get_link(
        //'Sort Ascending',
        '&#8595;', // downwards arrow
        A( 'Sort this table by this column in ascending order.' )
      );
      $sort_ascending->set( $sort_term, $column );
      $sort->activate( $sort_ascending, $sort_term, $column, true );

      $sort_descending = get_link(
        //'Sort Descending',
        '&#8593;', // upwards arrow
        A( 'Sort this table by this column in descending order.' )
      );
      $sort_descending->set( $sort_term, '-' . $column );
      $sort->activate( $sort_descending, $sort_term, '-' . $column, true );

      $append_sort_ascending = get_link(
        //'Append Sort Ascending',
        '&#8659;', // downwards double arrow
        A( 'Add this column to the list of sorted columns in ascending order.' )
      );
      /*
      $append_sort_ascending->set(
        $sort_term,
        $sort->to_string() === '' ?
          $column :
          $sort->to_string() . ' ' . $column
      );
      */
      //$append_sort_ascending->add( $sort_term, $column );
      $append_sort_ascending->set(
        $sort_term,
        $sort->push( $column, 'asc', false )
      );
      $sort->activate(
        $append_sort_ascending,
        $sort_term,
        $column,
        false
      );

      $append_sort_descending = get_link(
        //'Append Sort Descending',
        '&#8657;', // upwards double arrow
        A( 'Add this column to the list of sorted columns in descending order.' )
      );
      /*
      $append_sort_descending->set(
        $sort_term,
        $sort->to_string() === '' ? '-' . $column : $sort->to_string() . ' -' . $column
      );
      */
      $append_sort_descending->set(
        $sort_term,
        $sort->push( $column, 'desc', false )
      );
      $sort->activate(
        $append_sort_descending,
        $sort_term,
        '-' . $column,
        false
      );

      $gather = get_link( H( 'Gather' ), A( 'Gather results by this column.' ) );
      $gather->set( $gather_term, $column );
      
      $shuffler->remove( $column, $cols );
      $remove = get_link( H( 'Remove' ), A( 'Remove this column.' ) );
      $remove->set( $cols_term, implode( ' ', $cols ) );
      $remove->clear( $sort_term );
      
      $shuffler->first( $column, $cols );
      $move_first = get_link( H( 'Move first' ), A( 'Move this column to the first column.' ) );
      $move_first->set( $cols_term, implode( ' ', $cols ) );
      
      $shuffler->left( $column, $cols );
      $move_left = get_link( H( 'Move left' ), A( 'Move this column to the left.' ) );
      $move_left->set( $cols_term, implode( ' ', $cols ) );
      
      $shuffler->right( $column, $cols );
      $move_right = get_link( H( 'Move right' ), A( 'Move this column to the right.' ) );
      $move_right->set( $cols_term, implode( ' ', $cols ) );
      
      $shuffler->last( $column, $cols );
      $move_last = get_link( H( 'Move last' ), A( 'Move this column to the last column.' ) );
      $move_last->set( $cols_term, implode( ' ', $cols ) );

      $width = read_a( $this->schema, $column, 'width' );
      $style = null;

      if ( $width ) { $style = 'width:' . $width; }

      if ( $no_menu ) {

        $tail = $tail->
          th(
            'style', $style
          )->
            markup( $this->schema[ $column ][ 'heading' ] )->
          th_end();
        
      } 
      else {
        
        $tail = $tail->
          th(
            'style', $style
          )->
            ul( 'class', 'menu' )->
              li()->
                span( 'class', 'underline' )->
                  markup( $this->schema[ $column ][ 'heading' ] )->
                span_end()->
                test( false )->
                //test( $sort_links && ! $no_sort )->
                  markup( '&nbsp;' )->
                  add( $sort_ascending->to_anchor() )->
                  markup( '&nbsp;' )->
                  add( $sort_descending->to_anchor() )->
                  markup( '&nbsp;' )->
                  add( $append_sort_ascending->to_anchor() )->
                  markup( '&nbsp;' )->
                  add( $append_sort_descending->to_anchor() )->
                test_end();

        $sort_ascending->inner_html .= nbsp( A( ' Sort Ascending' ) );
        $sort_descending->inner_html .= nbsp( A( ' Sort Descending' ) );
        $append_sort_ascending->inner_html .= nbsp( A( ' Append Sort Ascending' ) );
        $append_sort_descending->inner_html .= nbsp( A( ' Append Sort Descending' ) );

        $tail = $tail->ul();

        if ( ! $is_computed ) {

          if ( $sort_links && ! $no_sort ) {

            $tail = $tail->li();

            $sort_ascending->render( $tail );

            $tail = $tail->li_end()->li();

            $sort_descending->render( $tail );

            $tail = $tail->li_end()->li();

            $append_sort_ascending->render( $tail );

            $tail = $tail->li_end()->li( 'class', 'underline' );

            $append_sort_descending->render( $tail );

            $tail = $tail->
              li_end();

          }

          $tail = $tail->
            li();

          $gather->render( $tail );

          $tail = $tail->li_end();

        }

        $tail = $tail->li( 'class', 'underline' );

        $remove->render( $tail );

        $tail = $tail->
          li_end()->
          li();

        $move_first->render( $tail );

        $tail = $tail->li_end()->li();

        $move_left->render( $tail );

        $tail = $tail->li_end()->li();

        $move_right->render( $tail );

        $tail = $tail->li_end()->li( 'class', 'underline' );

        $move_last->render( $tail );

        $tail = $tail->
          li_end();

        for ( $i = 0, $il = count( $selected_columns ); $i < $il; $i++ ) {
          if ( $shuffler->pos( $i + 1, $column, $cols, $label ) ) {
            $move_pos = get_link(
              H( 'Move %position%', 'position', $label ),
              A( 'Move this column to the %position% column.', 'position', $label )
            );
            $move_pos->clear( $cols_term );
            $move_pos->set( $cols_term, implode( ' ', $cols ) );

            $tail = $tail->li();

            $move_pos->render( $tail );

            $tail = $tail->li_end();

          }
        }

        // sort links back to icons:
        $sort_ascending->inner_html = '&#8595;';
        $sort_descending->inner_html = '&#8593;';
        $append_sort_ascending->inner_html = '&#8659;';
        $append_sort_descending->inner_html = '&#8657;';

        $tail = $tail->
                ul_end()->
              li_end()->
            ul_end()->

            test( $sort_links && ! $no_sort )->
              markup( '<br>' )->
              add( $sort_ascending->to_anchor() )->
              markup( '&nbsp;' )->
              add( $sort_descending->to_anchor() )->
              markup( '&nbsp;' )->
              add( $append_sort_ascending->to_anchor() )->
              markup( '&nbsp;' )->
              add( $append_sort_descending->to_anchor() )->
            test_end()->

          th_end();
      }
    }

    $tail = $tail->
      test( $search_row )->
        th()->
          nbsp()->
          add( get_most_hidden() )->
        th_end()->
      test_end()->
      tr_end();
    
    if ( $search_row ) {
    
      $tail = $tail->tr();

      foreach ( $selected_columns as $column ) {

        $tail = $tail->
          th()->
            test( ! $this->schema[ $column ][ 'is-computed' ] )->
              input(
                'type', 'text',
                'name', 'q_' . $column,
                'value', read_g( 'q_' . $column, '' ),
                //'style', 'width:7em;',
                'style', 'width:97%;',
                'class', 'query'
              )->
            test_end()->
          th_end();

      }

      $tail = $tail->
          th()->
            input( 'type', 'submit', 'value', 'Search' )->
            nbsp()->
            test( $clear_function == null )->
              new_link(
                null,
                A( 'Clear' ),
                null
              )->
            test_end()->
            test( $clear_function != null )->
              button(
                'onclick', "return $clear_function()"
              )->
                text( 'Clear' )->
              button_end()->
            test_end()->
          th_end()->
        tr_end();
      
    }
    
    $tail = $tail->thead_end();

    if ( count( $data ) ) {

      $tail = $tail->tbody();
      
    }
            
    if ( $gather_column !== null ) {
    
      $compare = $this->schema[ $gather_column ][ 'compare' ];
      $format = $this->schema[ $gather_column ][ 'gather' ];
      $schema = $this->schema;
      
      usort( $data, function( &$a, &$b ) use ( $compare, $gather_column, $sort, $schema ) {
        $direction =
          read_a( $sort->direction_list, $gather_column ) === 'desc' ?
          -1 :
          1;
        $result = $direction * $compare( $a[ $gather_column ], $b[ $gather_column ] );
        if ( $result !== 0 ) { return $result; }
        foreach ( $sort->field_list as $index => $sort_field ) {
          $direction = $sort->direction_list[ $sort_field ] === 'asc' ? 1 : -1;
          $compare = $schema[ $sort_field ][ 'compare' ];
          $result = $compare( $a[ $sort_field ], $b[ $sort_field ] ) * $direction;
          if ( $result !== 0 ) { return $result; }
        }
        return 0;
      });
      
    }
    
    $prev_gather = null;
    //$curr_gather = '**** ZERO VALUE ****';
    $curr_gather = new DateTime(); // <- this won't match anything
    
    foreach ( $data as &$row ) {
      
      if ( $gather_column ) {
        
        $parse = $this->schema[ $gather_column ][ 'parse' ];
        
        $prev_gather = $curr_gather;
        $curr_gather = $parse( $row, $gather_column, $this->schema );
        
        if ( $prev_gather !== $curr_gather ) {

          $format = $this->schema[ $gather_column ][ 'gather' ];
          
          $html_node = elem( 'span', 'class', 'gather' );
          
          $html = $format( $row, $gather_column, $this->schema, $html_node, $this );

          if ( count( $html_node->children ) ) {
           
            $html = $html_node->to_html();
            
          }
          else if ( strlen( $html ) === 0 ) {
            
            $html = "&lt;missing&gt;";
           
          }
          
          $link = get_link(
            $this->schema[ $gather_column ][ 'heading' ],
            A( 'Click to stop gathering by this column.' )
          );
          $link->clear( $gather_term );
    
          $tail = $tail->
            tr()->
              th(
                'colspan', count( $selected_columns ),
                'style', 'width:100%;text-align:left !important;'
              );
          
          $link->render( $tail );
          
          $tail = $tail->
                markup( ': ' . $html )->
              th_end()->
            tr_end();
          
        }
        
      }
      
      $row_id = null;
      
      if ( $this->index_column ) {
        
        $row_id =
          $this->name . '_' . $this->index_column . '_' .
            $row[ $this->index_column ];
        
      }

      $tail = $tail->tr(
        'id', $row_id,
        'class', $i++ % 2 ? 'alt' : null
      );

      $is_first = true;
      
      foreach ( $selected_columns as $column ) {

        /*
        $index = null;
        
        if ( $is_first ) {
        
          $index = read_a( $row, 'index' );
          
          if ( $index !== null ) {
            
            $index = $this->name . '_' . ( $index + 1 );
            
          }
          
          $is_first = false;
          
        }
        */
        
        $highlight = true;

        if ( $highlight_spec === null ) {
          
          $highlight = false;
          
        }
        else {
         
          foreach ( $highlight_spec as $hspec_key => $hspec_value ) {
            
            $highlight = ( read_a( $row, $hspec_key ) === $hspec_value );
            
            if ( $highlight === false ) { break; }
            
          }
        }
        
        if ( $highlight ) {
          
          $tail->add_class( 'active' );
          
        }
                
        $tail = $tail->td(
          'style', 'text-align:' . $this->schema[ $column ][ 'align' ] .
            ' !important'
          //'id', $index, 
          //'style', $style
        );

        $format = $this->schema[ $column ][ 'format' ];

        $html = $format( $row, $column, $this->schema, $tail, $this );
        
        if ( count( $tail->children ) === 0 ) {
        
          $tail->markup( $html );

        }
          
        $tail = $tail->td_end();

      }

      $tail = $tail->tr_end();

    }

    if ( count( $data ) ) {

      $tail = $tail->
          tbody_end();
      
    }
                
    if ( $this->tfoot_spec ) {

      $tfoot = &$this->tfoot_spec;
      
      $tail = $tail->tfoot()->tr();

      foreach ( $selected_columns as $column ) {

        $tail = $tail->td(
          'style', 'text-align:' . $this->schema[ $column ][ 'align' ] .
            ' !important'
        );

        $format = $tfoot[ $column ][ 'format' ];
        
        $format( $column, $this->schema, $tail, $this );

        $tail = $tail->td_end();

      }

      $tail = $tail->tr_end()->tfoot_end();
      
    }
    else if ( $total ) {
      
      $tail = $tail->tfoot()->tr();

      foreach ( $selected_columns as $column ) {

        $tail = $tail->td();

        if ( ! read_a( $this->schema, $column, 'is-computed' ) ) {

          $tail->text(
            format_value(
              $total[ $column ],
              read_d( $this->schema, $column, 'total_units', '' )
            )
          );

        }
        
        $tail = $tail->td_end();

      }

      $tail = $tail->tr_end()->tfoot_end();
      
    }
    
    if ( $this->tfoot_spec ) {
      
      //$tail->add( $tfoot );
      
    }
    
    $tail = $tail->
      table_end()->
      div( 'class', 'table-nav-bottom' );
    
    if ( $bottom_nav ) {

      $page_links->render( $tail );
      
    }
    
    //$tail = $tail->div_end();
    return $tail->div_end();

  }

  protected function get_query_term( $type ) {
    $name = $this->name;
    $term = $name ? $name . '_' . $type : $type;
    return $term;
  }
  
  protected function validate_column_list(
    &$column_list,
    $query_term = null,
    $check_included = false
  ) {
    
    $included_columns = null;
    
    if ( $check_included ) {
      
      $included_columns = &$this->get_included();
      
    }
    
    foreach ( $column_list as $column_name ) {

      if ( ! array_key_exists( $column_name, $this->schema ) ) {

        $this->try_clear_and_redirect( $query_term );
        
        throw Error(
          'Missing column "%column%".',
          'column', $column_name
        );
        
      }
      
      if ( $check_included && ! in_array( $column_name, $included_columns ) ) {
        
        $this->try_clear_and_redirect( $query_term );
        
        throw Error(
          'Column "%column%" not included.',
          'column', $depends_column
        );
                
      }
    }
  }
  
  private function try_clear_and_redirect( $query_term ) {
    
    if ( $query_term === null ) { return; }
        
    if ( strlen( $query_term ) === 0 ) { return; }
    
    $url = get_link();
    
    $query_value = trim( strval( $url->get( $query_term ) ) );
    
    if ( strlen( $query_value ) === 0 ) { return; }
    
    $url->clear( $query_term );
    
    //request()->redirect( $url );
    redirect( $url->to_string() );
    
  }
}
