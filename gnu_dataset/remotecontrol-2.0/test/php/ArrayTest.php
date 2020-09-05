<?php

class PhpArrayTest extends PHPUnit_Framework_TestCase {

  public function setUp() { }

  public function test_array_access() {

    // results: either way is fine...
    
    return;
    
    $items = array( 'one', 'two', 'three' );
    
    $hash = function( $item ) {
      static $data = array(
        'one' => true,
        'two' => true,
        'three' => true,
      );
      return array_key_exists( $item, $data );
    };
    
    $search = function( $item ) {
      static $data = array( 'one', 'two', 'three' );
      return in_array( $item, $data );
    };
    
    $i = 1000;
    
    $hash_time = $this->time( $hash, $i, $items );
    
    $search_time = $this->time( $search, $i, $items );

    $this->assertTrue( $search_time < $hash_time );
    
  }
  
  private function time( $fn, $iterations, $items ) {
    
    $start = microtime( true );
    
    for ( $i = 0; $i < $iterations; $i++ ) {
      
      foreach ( $items as $item ) {

        $fn( $item );
        
      }
    }
    
    return microtime( true ) - $start;
    
  }
}
