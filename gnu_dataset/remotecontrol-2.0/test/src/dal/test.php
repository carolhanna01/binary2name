<?php

require_once __DIR__ . '/GrcDalUrlTest.php';

function ensure( $name, $value ) {
  
  if ( defined( $name ) ) { return; }
  
  define( $name, $value );
  
}
