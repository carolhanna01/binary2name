<?php

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcHtmlTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
  }

  public function test_nbsp() {

    $this->nbsp( '123', '123' );
    
    $this->nbsp( '<a href="123">test</a>', '<a href="123">test</a>' );
    
    $this->nbsp(
      '<a href="123">one two three</a>',
      '<a href="123">one&nbsp;two&nbsp;three</a>'
    );
    
    $this->nbsp(
      '123<span> <a href="123">one two three</a> </span>', 
      '123<span>&nbsp;<a href="123">one&nbsp;two&nbsp;three</a>&nbsp;</span>'
    );
    
  }
  
  private function nbsp( $input, $expected ) {
    
    $this->assertSame( $expected, nbsp( $input ) );
    
  }
}
