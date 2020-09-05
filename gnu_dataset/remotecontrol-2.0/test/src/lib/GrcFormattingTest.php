<?php

/**
 * @runTestsInSeparateProcesses
 * @preserveGlobalState disabled
 */
class GrcFormattingTest extends PHPUnit_Framework_TestCase {

  public function setUp() {

    require_once __DIR__ . '/../../../src/test.php';
    
  }

  public function test_truncate_html() {
    
    $this->truncate_html(
      '123456789012345678901234567890123456789012345',
      '123456789012345678901234567890123456789012...'
    );
    
    $this->truncate_html(
      '1234567890123456789012345678901234567890&amp;',
      '1234567890123456789012345678901234567890&amp;'
    );
    
    $this->truncate_html(
      '123456789012345678901234567890123456&amp;7890',
      '123456789012345678901234567890123456&amp;7890'
    );
    
    $this->truncate_html(
      '123456789012345678901234567890123456&amp;78901',
      '123456789012345678901234567890123456&amp;78901'
    );
    
    $this->truncate_html(
      '123456789012345678901234567890123456&amp;',
      '123456789012345678901234567890123456&amp;'
    );

    $this->truncate_html(
      '<a href="..">1234567890123456789012345678901234567890</a>',
      '1234567890123456789012345678901234567890'
    );
    
  }
  
  public function testTruncateText() {

    $this->truncate_text( null, '' );
    $this->truncate_text( '', '' );
    $this->truncate_text( '1', '1' );
    $this->truncate_text(
      '1234567890123456789012345678901234567',
      '1234567890123456789012345678901234567'
    );
    $this->truncate_text(
      '1234567890123456789012345678901234567890',
      '1234567890123456789012345678901234567890'
    );
    $this->truncate_text(
      '<a href="..">1234567890123456789012345678901234567890</a>',
      '<a href...</a>'
    );
    $this->truncate_text(
      '1 3 5 7 9 1 3 5 7 9 1 3 5 7 9 1 3 5 7 9 1 3 5',
      '1 3 5 7 9 1 3 5 7 9 1 ...5 7 9 1 3 5 7 9 1 3 5'
    );
    $this->truncate_text(
      ' 1 3 5 7 9 1 3 5 7 9 1 3 5 7 9 1 3 5 7 9 1',
      '1 3 5 7 9 1 3 5 7 9 1 3 5 7 9 1 3 5 7 9 1'
    );
  }
  
  private function truncate_html( $input, $expected ) {
    
    $this->assertSame( $expected, truncate_html( $input ) );
    
  }
  
  private function truncate_text( $input, $expected ) {
    
    $this->assertSame( $expected, truncate_text( $input ) );
    
  }
}
