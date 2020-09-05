<?php

/**
 * @ runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcValidationTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
  }
  
  public function test_valid_html() {

    $this->assertTrue( is_valid_html(
      '<code>&lt;&lt; &gt;&gt; &apos;&apos; &quot; &amp;&amp; &nbsp; &pound;</code>'
    ));
    
    $this->assertTrue( is_valid_html( '\'' ) );
    $this->assertTrue( is_valid_html( '"' ) );
    $this->assertTrue( is_valid_html( '&amp;' ) );
    
  }

  public function test_invalid_html() {

    $this->assertFalse( is_valid_html( '<<a href="whatever>">link</a>>' ) );
    $this->assertFalse( is_valid_html( '<<' ) );
    $this->assertFalse( is_valid_html( '>>' ) );
    $this->assertFalse( is_valid_html( '<' ) );
    $this->assertFalse( is_valid_html( '>' ) );
    $this->assertFalse( is_valid_html( '&' ) );
    
  }
  
  public function test_valid_attr() {

    $this->assertTrue( is_valid_attr( '&amp;' ) );
    
  }

  public function test_invalid_attr() {

    $this->assertFalse( is_valid_attr( '<<a href="whatever>">link</a>>' ) );
    $this->assertFalse( is_valid_attr( '<<' ) );
    $this->assertFalse( is_valid_attr( '>' ) );
    $this->assertFalse( is_valid_attr( '<' ) );
    $this->assertFalse( is_valid_attr( '>>' ) );
    $this->assertFalse( is_valid_attr( '&' ) );
    $this->assertFalse( is_valid_attr( '\'' ) );
    $this->assertFalse( is_valid_attr( '"' ) );
    
  }
}
