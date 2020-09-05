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

//define( 'CONTEXT_MISSING', null );

intl()->define_context( 'CONTEXT_GLOBAL', 'Global Context' );

intl()->define_context( 'CONTEXT_CONTEXT', 'Message Context' );
intl()->define_context( 'CONTEXT_LINK', 'HTML Link' );
intl()->define_context( 'CONTEXT_FIELD', 'Field Name' );
intl()->define_context( 'CONTEXT_LANGUAGE', 'Language' );

intl()->define_context( 'CONTEXT_PAGE_TITLE', 'HTML Page Title' );
intl()->define_context( 'CONTEXT_HEADING', 'HTML Heading' );
intl()->define_context( 'CONTEXT_TABLE_HEADING', 'HTML Table Heading' );
intl()->define_context( 'CONTEXT_UNITS', 'Units' );

intl()->define_context( 'CONTEXT_PUNCTUATION', 'Punctuation' );

intl()->define_context( 'CONTEXT_ERROR', 'Error Message' );

intl()->define_context( 'CONTEXT_DEFAULT', 'Default Message' );

intl()->define_context( 'CONTEXT_INTL', 'Internationalization Subsystem' );
intl()->define_context( 'CONTEXT_INTL_HELP', 'Internationalization Help File' );

function TABLE_HEADING( $message ) {
  return nbsp( A( $message, CONTEXT_TABLE_HEADING ) );
}

// see: https://support.google.com/webmasters/answer/6059209

class GrcIntl {

  public $lang_dat_path;
  public $lang_ser_path;
  
  public $file;
  public $lang;
  public $message;
  public $translation;
  
  public $context_list;
  public $context;
  public $context_translated;
  
  protected function __construct() {
    
    $dat_path = realpath( __DIR__ . '/../../dat' );
    
    $this->lang_dat_path = $dat_path . '/lang.dat';
    $this->lang_ser_path = $dat_path . '/lang.ser';
    
    $this->file = null;
    
    $this->lang = array(
      'en' => array(
        'langtag' => 'en',
        'fallback' => 'en',
        'english_name' => 'English',
        'local_name' => 'English',
        'active' => 1
      ),
    );
    $this->lang = dal()->intl->get_language_map();

    $this->message = array();
    $this->translation = array();

    $this->context_list = array();

    //$this->context = array( CONTEXT_DEFAULT );
    $this->context = array( 'default' );
    $this->context_translated = false;
        
  }
  
  public static function Create() {
    
    return new GrcIntl();
    
  }
  
  public function &get_lang_map() {
    
    $result = array();
    
    foreach ( $this->lang as $langtag => $setting ) {
    
      //$result[ $langtag ] = serialize( $setting );
      $result[ $langtag ] = read_a( $setting, 'english_name' );

    }
    
    return $result;
    
  }
  
  function define_context( $define, $label ) {
    if ( ! is_valid_ascii_const( $define ) ) {
      throw Error(
        'Invalid context "%context%".',
        'context', $define
      );
    }
    $text = strtolower( substr( $define, 8 ) );
    define( $define, $text );
    $this->context_list[ $text ] = $label;
  }
  
  function get_context_label( $context ) {

    return A( $this->context_list[ $context ], CONTEXT_CONTEXT );
    
    //global $intl_context_list, $intl_context_translated;

    if ( $this->context_translated !== true ) {

      $this->context_translated = true;

      $seq = 1;

      foreach ( $this->context_list as $context => $context_label ) {

        dal()->intl->order_context( $context, $seq++ );

        $this->context_list[ $context ] = H( $context_label, CONTEXT_CONTEXT );

      }

    }

    return read_d( $this->context_list, $context, $context );

  }

  public function add_context( $context ) {

    $this->context[] = $context;

  }

  public function get_context( $context = CONTEXT_GLOBAL ) {
    
    if ( $context === CONTEXT_GLOBAL ) {
      
      return vector_nth_value( $this->context );
      
    }
    
    return $context;
    
  }
  
  public function pop_context() {

    return array_pop( $this->context );

  }
  
  public function is_valid_context( $context ) {
    
    return array_key_exists( $context, $this->context_list );  
    
  }
  
  public function &load_file() {

    if ( $this->file !== null ) { return $this->file; }
    
    if ( file_exists(  $this->lang_ser_path ) ) {

      $this->parse_lang_ser( $this->lang_ser_path, $this->file );
      
    }
    else {
      
      $this->parse_lang_dat( $this->lang_dat_path, $this->file );
      
    }
    
    return $this->file;
    
  }
  
  public function init() {
    
    msg()->translate();
    
  }
  
  public function is_valid_fallback( $langtag, $fallback ) {
    
    if ( $fallback === 'en' ) { return true; }
    
    $chain = $this->get_fallback_chain( $fallback );

    return ! in_array( $langtag, $chain );
    
  }
  
  public function &get_fallback_chain( $langtag ) {

    $result = array();
    
    while ( $langtag !== 'en' ) {
      
      $result[] = $langtag;
      
      $langtag = $this->lang[ $langtag ][ 'fallback' ];
      
    }
    
    $result[] = 'en';

    return $result;
    
  }
  
  public function get_lang() {
    
    static $lang = null;
    
    if ( $lang !== null ) { return $lang; }
    
    $lang = read_d( $_GET, 'lang', false );
    
    if ( $lang === false ) {
      
      $lang = user()->langtag;

    }
    
    if ( ! array_key_exists( $lang, $this->lang ) ) {
      
      $lang = 'en';
      
    }
    
    return $lang;
    
  }
  
  public function get_name( $langtag ) {
    
    if ( ! $this->parse_langtag( $langtag, $spec ) ) { return false; }
      
    $langtag = $this->format_langtag( $spec );

    $this->load_file();
    
    switch ( $langtag ) {
      
      case 'zh-CN': return 'Chinese (Simplified)';
      case 'zh-TW': return 'Chinese (Traditional)';
        
    }
    
    if ( strlen( $spec[ 'extlang' ] ) ) {

      $extlang = $spec[ 'extlang' ];

      $descriptions = read_a( $this->file, 'extlang_descriptions', $extlang );
      
      $result = vector_1st_value( $descriptions );

    }
    else {

      $language = $spec[ 'language' ];

      $descriptions = read_a( $this->file, 'language_descriptions', $language );
      
      $result = vector_1st_value( $descriptions );

    }

    if ( strlen( $spec[ 'script' ] ) ) {

      $script = $spec[ 'script' ];

      $descriptions = read_a( $this->file, 'script_descriptions', $script );
      
      $result .= ' ' . vector_1st_value( $descriptions );

    }

    if ( strlen( $spec[ 'region' ] ) ) {

      $region = $spec[ 'region' ];

      $descriptions = read_a( $this->file, 'region_descriptions', $region );
      
      $result .= ' (' . vector_1st_value( $descriptions ) . ')';

    }

    return $result;
 
  }
  
  public function get_type_context_label( $type ) {

    static $labels = array(
      'html' => 'HTML Content (%context%)',
      'attr' => 'HTML Attribute (%context%)',
      'text' => 'Plain Text (%context%)',
      'safe' => 'Safe Text (%context%)',
    );
    
    return read_a( $labels, $type );
    
  }
  
  public function get_type_label( $type ) {

    static $labels = array(
      'html' => 'HTML Content',
      'attr' => 'HTML Attribute',
      'text' => 'Plain Text',
      'safe' => 'Safe Text',
    );
    
    return read_a( $labels, $type );
    
  }
  
  public function canonicalize_langtag( $langtag, &$error = null ) {
    
    if ( $this->parse_langtag( $langtag, $spec, $error ) ) {
      
      return $this->format_langtag( $spec );
      
    }
    
    return false;
    
  }
  
  public function format_langtag( &$spec ) {
    
    $result = '';
    
    foreach ( $spec as $key => &$value ) {
      
      if ( is_string( $value ) && strlen( $value ) === 0 ) { continue; }
      
      switch ( $key ) {
        
        case 'language' :
          
          $result .= $value;
          
          break;
        
        case 'extlang' :
        case 'script' :
        case 'region' :
          
          $result .= "-$value";
          
          break;
        
        case 'variant':
        case 'extension' :
          
          foreach ( $value as $item ) { $result .= "-$item"; }
        
          break;
        
        default:
          
          // ignore other stuff, this allows us to process $_GET...
          //throw new Exception( "Unknown language spec '$key'." );
          
      }
    }
    
    return $result;
    
  }
  
  public function parse_langtag( $langtag, &$result, &$error = null ) {
    
    $this->load_file();

    $result = array(
      'language' => '',
      'extlang' => '',
      'script' => '',
      'region' => '',
      'variant' => array(),
      'extension' => array(),
    );
    
    $parts = explode( '-', strtolower( trim( $langtag ) ) );
    
    $language = $parts[ 0 ];
    
    if ( preg_match( '/^[a-z]{2,3}$/', $language ) ) {
      if ( read_a( $this->file, 'languages', $language ) ) {
        $result[ 'language' ] = $language;
      }
      else {
        $error = "Missing language '$language'.";
        return false;
      }
    }
    else {
      $error = "Invalid language '$language'.";
      return false;
    }

    if ( count( $parts ) < 2 ) { return true; }
    
    $i = 1;
    
    while ( preg_match( '/^[a-z]{3}$/', $parts[ $i ] ) ) {
      if ( read_a( $result, 'extlang' ) ) {
        if ( $i < 3 ) {
          // ignore reserved extlang
          $i++;
          if ( count( $parts ) < $i + 1 ) { return true; }
        }
        else {
          $error = "Invalid extlang '{$parts[ $i ]}'.";
          return false;
        }
      }
      else {
        $result[ 'extlang' ] = $parts[ $i ];
      }
      $i++;
      if ( count( $parts ) < $i + 1 ) { return true; }
    }
    
    if ( preg_match( '/^[a-z]{4}$/', $parts[ $i ] ) ) {
      $script = ucfirst( $parts[ $i++ ] );
      if ( read_a( $this->file, 'scripts', $script ) ) {
        if (
          read_a( $this->file, 'languages', $language, 'Suppress-Script' ) !==
          $script
        ) {
          $result[ 'script' ] = $script;
        }
      }
      else {
        $error = "Missing script '$script'.";
        return false;
      }
      if ( count( $parts ) < $i + 1 ) { return true; }
    }
    
    while (
      preg_match( '/^[a-z]{2}$/', $parts[ $i ] ) ||
      preg_match( '/^[0-9]{3}$/', $parts[ $i ] )
    ) {
      $region = strtoupper( $parts[ $i++ ] );
      if ( read_a( $this->file, 'regions', $region ) ) {
        $result[ 'region' ] = $region;
      }
      else {
        $error = "Missing region '$region'.";
        return false;
      }
      if ( count( $parts ) < $i + 1 ) { return true; }
    }
    
    while (
      preg_match( '/^[a-z0-9]{5,8}$/', $parts[ $i ] ) ||
      preg_match( '/^[0-9][a-z0-9]{3}$/', $parts[ $i ] )
    ) {
      $variant = $parts[ $i++ ];
      $prefix = $this->format_langtag( $result );
      if ( read_a( $this->file, 'variants', $prefix ) ) {
        $result[ 'variant' ][] = $variant;
      }
      else {
        $error = "Missing variant '$variant'.";
        return false;
      }
      if ( count( $parts ) < $i + 1 ) { return true; }
    }
    
    while(
      preg_match(
        '/^[0-9\x41-\x57\x59-\x5a\x61-\x77\x79-\x7a]$/',
        $parts[ $i ]
      )
    ) {
      $singleton = $parts[ $i++ ];
      if ( count( $parts ) < $i + 1 ) {
        $error = "Missing extension for singleton '$singleton'.";
        return false;
      }
      if ( preg_match( '/^[a-z0-9]{2,8}$/', $parts[ $i ] ) ) {
        $extension = $singleton . '-' . $parts[ $i++ ];
        $result[ 'extension' ][] = $extension;
        if ( count( $parts ) < $i + 1 ) { return true; }
      }
      else {
        $error = "Invalid extension for singleton '$singleton'.";
        return false;
      }
    }
    
    return true;
    
  }

  public function normalize_message( &$content, &$space_before, &$space_after ) {
    
    $space_before = '';
    $space_after = '';
    
    if ( preg_match( '/^\s/', $content ) ) {
      
      $space_before = ' ';
      
    }
    
    if ( preg_match( '/\s$/', $content ) ) {
      
      $space_after = ' ';
      
    }
    
    $content = trim( $content );
    $content = preg_replace( '/\s{2}/', ' ', $content );
    $content = preg_replace( '/\s/', ' ', $content );
    
    
    if ( strlen( $content ) === 0 ) {
      
      //throw Error( 'Empty message.' );
      
    }
  }
  
  public function hash( &$content, &$space_before, &$space_after ) {

    $this->normalize_message( $content, $space_before, $space_after );
    
    return md5( $content );
    
  }
  
  public function register_message(
    $type,
    $context,
    $content,
    &$space_before,
    &$space_after,
    &$hash = null
  ) {
        
    $context = $this->get_context( $context );

    $hash = $this->hash( $content, $space_before, $space_after );

    if ( isset( $this->message[ $type ][ $context ][ $hash ] ) ) {
      
      return $content;
      
    }
    
    dal()->intl->register_message( $type, $context, $hash, $content );

    $this->message[ $type ][ $context ][ $hash ] = true;

    return $content;
    
  }
  
  public function register_html(
    $context,
    $content,
    &$space_before = null,
    &$space_after = null
  ) {
    
    return $this->register_message( 'html', $context, $content, $space_before, $space_after );
    
  }
  
  public function register_attr(
    $context,
    $content,
    &$space_before = null,
    &$space_after = null
  ) {
    
    return $this->register_message( 'attr', $context, $content, $space_before, $space_after );
    
  }
  
  public function register_text(
    $context,
    $content,
    &$space_before = null,
    &$space_after = null
  ) {
    
    return $this->register_message( 'text', $context, $content, $space_before, $space_after );
    
  }
  
  public function register_safe(
    $context,
    $content,
    &$space_before = null,
    &$space_after = null
  ) {
    
    return $this->register_message( 'safe', $context, $content, $space_before, $space_after );
    
  }
  
  public function format_01n(
    $type,
    $msg_0,
    $msg_1,
    $msg_n,
    $number,
    $context = CONTEXT_GLOBAL
  ) {
    
    $this->register_message(
      $type,
      $context,
      $msg_0,
      $space_before,
      $space_after,
      $hash
    );
    
    $this->register_message(
      $type,
      $context,
      $msg_1,
      $space_before,
      $space_after,
      $hash
    );
    
    $this->register_message(
      $type,
      $context,
      $msg_n,
      $space_before,
      $space_after,
      $hash
    );
    
    if ( $number == 0 ) {
      
      $msg = $msg_0;
      
    }
    else if ( $number == 1 ) {
      
      $msg = $msg_1;
      
    }
    else {
      
      $msg = $msg_n;
      
    }

    $args = array( $msg, 'number', format_number( $number ) );
    
    return $this->format( $type, $args, $context );
    
  }
  
  public function format_translation( $translation, &$args ) {

    $argc = count( $args );

    if ( $argc > 1 && ( ( $argc % 2 ) === 0 ) ) { $argc--; }
    
    for ( $i = 1; $i < $argc; $i += 2 ) {

      $key = $args[ $i ];

      $value = $args[ $i + 1 ];

      $translation = str_replace( '%' . $key . '%', $value, $translation );

    }

    return $translation;
    
  }
  
  public function format( $type, &$args, $context = CONTEXT_GLOBAL ) {

    $context = $this->get_context( $context );

    $argc = count( $args );

    if ( $argc === 0 ) {
      
      throw Error(
        'Missing translation args for format().'
      );
      
    }
    
    $message = $args[ 0 ];
    
    if ( $argc > 1 && ( ( $argc % 2 ) === 0 ) ) {

      $context = $args[ --$argc ];

    }
    
    $translation = $this->translate( $type, $message, $context );

    return $this->format_translation( $translation, $args );
    
  }
  
  public function get_fallback( $langtag ) {
    
    $result = read_a( $this->lang, $langtag, 'fallback' );
    
    return $result ? $result : 'en';
    
  }
  
  public function translate(
    $type,
    $message,
    $context = CONTEXT_GLOBAL,
    $langtag = null
  ) {

    if ( is_int( $message ) ) {
      
      return msg()->get( $message );
      
    }
    
    $context = $this->get_context( $context );

    $message = $this->register_message(
      $type,
      $context,
      $message,
      $space_before,
      $space_after,
      $hash
    );
    
    if ( $langtag === null ) { $langtag = $this->get_lang(); }

    $translation = $message;
    
    for ( $lang = $langtag; $lang !== ''; $lang = $this->get_fallback( $lang ) ) {

      if ( isset( $this->translation[ $lang ][ $type ][ $context ][ $hash ] ) ) {
        
        $result = $this->translation[ $lang ][ $type ][ $context ][ $hash ];

        // if the translation is an empty string that indicates we
        // inherit the parent language translation.
        if ( $result === '' ) { continue; }
        
        $translation = $result;
        
        break;
        
      }
      
      $result = dal()->intl->translate( $lang, $type, $context, $hash );
      
      if ( $result !== null ) {
                
        $this->translation[ $lang ][ $type ][ $context ][ $hash ] = $result;

        if ( $result === '' ) { continue; }
        
        $translation = $result;
        
        break;
        
      }
      
      if ( $lang === 'en' ) { break; }
      
    }
    
    return $space_before . $translation . $space_after;
    
  }
  
  public function translate_html(
    $message,
    $context = CONTEXT_GLOBAL,
    $langtag = null
  ) {
    
    return $this->translate( 'html', $message, $context, $langtag );
    
  }
  
  public function translate_attr(
    $message,
    $context = CONTEXT_GLOBAL,
    $langtag = null
  ) {
    
    return $this->translate( 'attr', $message, $context, $langtag );
    
  }
  
  public function translate_text(
    $message,
    $context = CONTEXT_GLOBAL,
    $langtag = null
  ) {
    
    return $this->translate( 'text', $message, $context, $langtag );
    
  }
  
  public function translate_safe(
    $message,
    $context = CONTEXT_GLOBAL,
    $langtag = null
  ) {
    
    return $this->translate( 'safe', $message, $context, $langtag );
    
  }
  
  public function parse_accept_header() {

    static $result = null;
    
    if ( $result !== null ) { return $result; }
    
    $header = read_a( $_SERVER, 'HTTP_ACCEPT_LANGUAGE' );
    
    if ( $header === null ) {
      
      return $result = array( 'en' );
      
    }
    
    $parts = explode( ',', $header );
        
    $temp = array();
    
    foreach ( $parts as $part ) {
      
      $spec = explode( ';', $part );
      
      $langtag = $spec[ 0 ];
      
      // drop character code spec that may be provided, e.g. ru-RU.UTF-8:
      $langtag_parts = explode( '.', $langtag );
      
      $langtag = $langtag_parts[ 0 ];

      $canonical_langtag = $this->canonicalize_langtag( $langtag, $error );
            
      if ( $canonical_langtag === false ) {
        
        dal()->intl->log_langtag_invalid( $langtag );
        
        continue;
        
      }
      else if ( ! array_key_exists( $canonical_langtag, $this->lang ) ) {
        
        dal()->intl->log_langtag_missing( $canonical_langtag );

        continue;
        
      }
      
      if ( count( $spec ) === 1 ) {
        
        $temp[ $canonical_langtag ] = 1.0;
        
        continue;
        
      }
      
      $q = explode( '=', $spec[ 1 ] );
      
      if ( count( $q ) === 2 ) {
        
        $q = $q[ 1 ];
        
      }
      else {
        
        $q = $q[ 0 ];
        
      }
      
      $temp[ $canonical_langtag ] = floatval( $q );
      
    }
        
    if ( ! array_key_exists( 'en', $temp ) ) {
      
      $temp[ 'en' ] = 0.0;
      
    }
    
    arsort( $temp, SORT_NUMERIC );
    
    $result = array_keys( $temp );
    
    return $result;
    
  }
  
  public function get_root() {
    
    $path = __DIR__ . '/../../dat/lang';
    
    $root = realpath( $path );

    if ( $root === false ) {

      throw Error(
        'Missing /dat/lang directory "%path%".',
        'path', $path
      );
      
    }
    
    return $root;
    
  }
  
  public function dump() {

    $msg_list = dal()->intl->get_messages();
    
    foreach ( $msg_list as &$msg ) {

      $langtag = 'en';
      $type = $msg[ 'type' ];
      $context = $msg[ 'context' ];
      $hash = $msg[ 'hash' ];
      $message = $msg[ 'message' ];
      
      if ( ! $this->get_path( $langtag, $type, $context, $hash, $dir, $path ) ) {

        //???
        
      }
    }
    
    $lang_list = dal()->intl->get_languages();
    
    foreach ( $lang_list as &$lang ) {
      
      $langtag = $lang[ 'langtag' ];
      
      if ( $langtag === 'en' ) { continue; }
      
      $tran_list = dal()->intl->get_translations_for_langtag( $langtag );
      
    }
  }

  private function parse_lang_ser( $path, &$file ) {

    $data = file_get_contents( $path );

    $file = unserialize( $data );

  }

  private function write_lang_ser( $path, &$file ) {

    file_put_contents( $path, serialize( $file ) );

  }

  private function parse_lang_dat( $path, &$file ) {

    $data = file_get_contents( $path );

    $data = str_replace( "\r", '', $data );
    $data = str_replace( "\n  ", ' ', $data );

    $specs = explode( '%%', $data );

    $file = array();

    foreach ( $specs as $spec ) {

      $lines = explode( "\n", $spec );

      $record = array();

      foreach ( $lines as $line ) {

        $line = trim( $line );

        if ( $line === '' ) {

          $type = read_a( $record, 'Type' );
          $subtype = read_a( $record, 'Subtag' );
          $suppress_script = read_a( $record, 'Suppress-Script' );
          $descriptions = read_a( $record, 'Description' );
          $prefixes = read_a( $record, 'Prefix' );
          $comments = read_a( $record, 'Comments' );

          if ( ! is_array( $descriptions ) ) {

            $descriptions = $descriptions ? array( $descriptions ) : array();

          }

          if ( ! is_array( $prefixes ) ) {

            $prefixes = $prefixes ? array( $prefixes ) : array();

          }

          $record[ 'Descriptions' ] = $descriptions;
          $record[ 'Prefixes' ] = $prefixes;

          unset( $record[ 'Type' ] );
          unset( $record[ 'Description' ] );
          unset( $record[ 'Prefix' ] );
          unset( $record[ 'Added' ] );
          //unset( $record[ 'Comments' ] );

          if ( $type === 'language' ) {

            $file[ 'languages' ][ $subtype ] = $record;

            $file[ 'language_descriptions' ][ $subtype ]  = $descriptions;

            $file[ 'language_comments' ][ $subtype ] = $comments;

          }
          else if ( $type === 'extlang' ) {

            foreach ( $prefixes as $prefix ) {

              $file[ 'extlangs' ][ $prefix ][] = $record;

            }

            $file[ 'extlang_descriptions' ][ $subtype ] = $descriptions;

            $file[ 'extlang_comments' ][ $subtype ] = $comments;

          }
          else if ( $type === 'script' ) {

            $file[ 'scripts' ][ $subtype ] = $record;

            $file[ 'script_descriptions' ][ $subtype ] = $descriptions;

            $file[ 'script_comments' ][ $subtype ] = $comments;

          }
          else if ( $type === 'region' ) {

            $file[ 'regions' ][ $subtype ] = $record;

            $file[ 'region_descriptions' ][ $subtype ] = $descriptions;

            $file[ 'region_comments' ][ $subtype ] = $comments;

          }
          else if ( $type === 'variant' ) {

            foreach ( $prefixes as $prefix ) {

              $file[ 'variants' ][ $prefix ][] = $record;

            }

            $file[ 'variant_descriptions' ][ $subtype ] = $descriptions;

            $file[ 'variant_comments' ][ $subtype ] = $comments;

          }
          else if ( $type === 'extension' ) {

            foreach ( $prefixes as $prefix ) {

              $file[ 'extensions' ][ $prefix ][] = $record;

            }

            $file[ 'extension_descriptions' ][ $subtype ] = $descriptions;

            $file[ 'extension_comments' ][ $subtype ] = $comments;

          }

          $record = array();

          continue;

        }

        $parts = explode( ':', $line, 2 );

        $label = trim( read_a( $parts, 0 ) );
        $value = trim( read_a( $parts, 1 ) );

        if ( isset( $record[ $label ] ) ) {

          if ( ! is_array( $record[ $label ] ) ) {

            $first_value = $record[ $label ];

            $record[ $label ] = array( $first_value );

          }
          else {

            $record[ $label ][] = $value;

          }
        }
        else {

          $record[ $label ] = $value;

        }
      }
    }
  }
}
