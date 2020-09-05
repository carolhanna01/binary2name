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

// see: https://simon.html5.org/html-elements

global
  $html5_attr,
  $html5_global_attr,
  $html5_global_event,
  $html5_spec;

// keep a list of all HTML5 attributes (generated below):
$html5_attr = array();

$html5_global_attr = array(
  'accesskey' => array(
    'join' => ' ',
  ),
  // aria-*
  'aria' => true,
  'class' => true,
  'contenteditable' => true,
  'contextmenu' => true,
  // data-*
  'data' => true,
  'dir' => true,
  'draggable' => true,
  'dropzone' => true,
  'hidden' => true,
  'id' => true,
  'inert' => true,
  'itemid' => true,
  'itemprop' => true,
  'itemref' => true,
  'itemscope' => true,
  'itemtype' => true,
  'lang' => true,
  'role' => true,
  'spellcheck' => true,
  'style' => array(
    'keys' => ':',
    'join' => ';',
  ),
  'tabindex' => true,
  'title' => true,
  'translate' => true,
);

$html5_global_event = array(
  'onabort' => true,
  'onblur' => true,
  'oncanplay' => true,
  'oncanplaythrough' => true,
  'onchange' => true,
  'onclick' => true,
  'oncontextmenu' => true,
  'ondblclick' => true,
  'ondrag' => true,
  'ondragend' => true,
  'ondragenter' => true,
  'ondragleave' => true,
  'ondragover' => true,
  'ondragstart' => true,
  'ondrop' => true,
  'ondurationchange' => true,
  'onemptied' => true,
  'onended' => true,
  'onerror' => true,
  'onfocus' => true,
  'onformchange' => true,
  'onforminput' => true,
  'oninput' => true,
  'oninvalid' => true,
  'onkeydown' => true,
  'onkeypress' => true,
  'onkeyup' => true,
  'onload' => true,
  'onloadeddata' => true,
  'onloadedmetadata' => true,
  'onloadstart' => true,
  'onmousedown' => true,
  'onmousemove' => true,
  'onmouseout' => true,
  'onmouseover' => true,
  'onmouseup' => true,
  'onmousewheel' => true,
  'onpause' => true,
  'onplay' => true,
  'onplaying' => true,
  'onprogress' => true,
  'onratechange' => true,
  'onreset' => true,
  'onreadystatechange' => true,
  'onseeked' => true,
  'onseeking' => true,
  'onselect' => true,
  'onshow' => true,
  'onstalled' => true,
  'onsubmit' => true,
  'onsuspend' => true,
  'ontimeupdate' => true,
  'onvolumechange' => true,
  'onwaiting' => true,
);

$html5_spec = array(
  'test' => array(
    'default' => 'test',
    'attr' => array(
      'test' => true
    ),
  ),
  'comment' => array(
    'attr' => array(),
  ),
  'html' => array(
    'spec' => array(
      'space-before' => false
    ),
    'attr' => array(
      'manifest' => true,
      'xmlns' => true,
    ),
  ),
  'head' => array(
    'attr' => array()
  ),
  'title' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array()
  ),
  'base' => array(
    'empty' => true,
    'default' => 'href',
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array(
      'href' => true,
      'target' => true,
    ),
  ),
  'link' => array(
    'empty' => true,
    'attr' => array(
      'href' => true,
      'rel' => true,
      'media' => true,
      'hreflang' => true,
      'type' => true,
      'sizes' => true,
    )
  ),
  'meta' => array(
    'empty' => true,
    'attr' => array(
      'name' => true,
      'http-equiv' => true,
      'content' => array(
        array( '', '', array( 'keys' => false, 'join' => ' ' ) ),
        array( 'name', 'viewport', array( 'keys' => '=', 'join' => ',' ) ),
      ),
      'charset' => true,
      'property' => true,
    )
  ),
  'style' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => true,
      'newline' => true,
    ),
    'attr' => array(
      'media' => true,
      'type' => true,
      'scoped' => true,
    )
  ),
  'script' => array(
    'spec' => array(
      'space-after' => true,
    ),
    'attr' => array(
      'src' => true,
      'async' => true,
      'defer' => true,
      'type' => true,
      'charset' => true,
    ),
  ),
  'noscript' => array(
    'attr' => array(),
  ),
  'template' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'body' => array(
    'attr' => array(
      'onafterprint' => true,
      'onbeforeprint' => true,
      'onbeforeunload' => true,
      'onblur' => true,
      'onerror' => true,
      'onfocus' => true,
      'onhashchange' => true,
      'onload' => true,
      'onmessage' => true,
      'onoffline' => true,
      'ononline' => true,
      'onpagehide' => true,
      'onpageshow' => true,
      'onpopstate' => true,
      'onresize' => true,
      'onscroll' => true,
      'onstorage' => true,
      'onunload' => true,
    ),
  ),
  'section' => array(
    'version' => HTML5,
    'default' => 'id',
    'attr' => array(),
  ),
  'nav' => array(
    'version' => HTML5,
    'default' => 'id',
    'attr' => array(),
  ),
  'article' => array(
    'version' => HTML5,
    'default' => 'id',
    'attr' => array(),
  ),
  'aside' => array(
    'attr' => array(),
  ),
  'h1' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'h2' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'h3' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'h4' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'h5' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'h6' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'hgroup' => array(
    'attr' => array(),
  ),
  'header' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'footer' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'address' => array(
    'attr' => array(),
  ),
  'main' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'p' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'hr' => array(
    'empty' => true,
    'default' => 'class',
    'attr' => array(),
  ),
  'pre' => array(
    'attr' => array(),
  ),
  'blockquote' => array(
    'default' => 'class',
    'attr' => array( 'cite' => true ),
  ),
  'ol' => array(
    'default' => 'class',
    'attr' => array(
      'reversed' => true,
      'start' => true,
    ),
  ),
  'ul' => array(
    'default' => 'class',
    'attr' => array(),
  ),
  'li' => array(
    'spec' => array(
      'space-after' => false,
    ),
    'attr' => array( 'value' => true ),
  ),
  'dl' => array(
    'default' => 'class',
    'attr' => array(),
  ),
  'dt' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'dd' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'figure' => array(
    'version' => HTML5,
    'default' => 'id',
    'attr' => array(),
  ),
  'figcaption' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'div' => array(
    'default' => 'id',
    'attr' => array(),
  ),
  'a' => array(
    'default' => 'href',
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(
      'href' => true,
      'target' => true,
      'ping' => true,
      'rel' => true,
      'media' => true,
      'hreflang' => true,
      'type' => true,
      'title' => true,
    ),
  ),
  'em' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'strong' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'small' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  's' => array(
    'attr' => array(),
  ),
  'cite' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'q' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array( 'cite' => true ),
  ),
  'dfn' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'abbr' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'data' => array(
    'version' => HTML5,
    'attr' => array( 'value' => true ),
  ),
  'time' => array(
    'version' => HTML5,
    'attr' => array(
      'datetime' => true,
      'pubdate' => true,
    ),
  ),
  'code' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'var' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'samp' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'kbd' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'sub' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'sup' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'i' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'b' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'u' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'mark' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'ruby' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'rt' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'rp' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'bdi' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'bdo' => array(
    'attr' => array(),
  ),
  'span' => array(
    //'default' => 'id',
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'br' => array(
    'empty' => true,
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'wbr' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'ins' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(
      'cite' => true,
      'datetime' => true,
    ),
  ),
  'del' => array(
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(
      'cite' => true,
      'datetime' => true,
    ),
  ),
  'img' => array(
    'empty' => true,
    'attr' => array(
      'alt' => true,
      'title' => true,
      'src' => true,
      'srcset' => true,
      'crossorigin' => true,
      'usemap' => true,
      'ismap' => true,
      'width' => true,
      'height' => true,
    ),
  ),
  'iframe' => array(
    'attr' => array(
      'src' => true,
      'srcdoc' => true,
      'name' => true,
      'sandbox' => true,
      'seamless' => true,
      'width' => true,
      'height' => true,
    ),
  ),
  'embed' => array(
    'version' => HTML5,
    'attr' => array(
      'src' => true,
      'type' => true,
      'width' => true,
      'height' => true,
    ),
  ),
  'object' => array(
    'attr' => array(
      'data' => true,
      'type' => true,
      'typemustmatch' => true,
      'name' => true,
      'usemap' => true,
      'form' => true,
      'width' => true,
      'height' => true,
    ),
  ),
  'param' => array(
    'attr' => array(
      'name' => true,
      'value' => true,
    ),
  ),
  'video' => array(
    'version' => HTML5,
    'attr' => array(
      'src' => true,
      'crossorigin' => true,
      'preload' => true,
      'autoplay' => true,
      'mediagroup' => true,
      'loop' => true,
      'muted' => true,
      'controls' => true,
      'width' => true,
      'height' => true,
    ),
  ),
  'audio' => array(
    'version' => HTML5,
    'attr' => array(
      'src' => true,
      'crossorigin' => true,
      'preload' => true,
      'autoplay' => true,
      'mediagroup' => true,
      'loop' => true,
      'muted' => true,
      'controls' => true,
    ),
  ),
  'source' => array(
    'version' => HTML5,
    'attr' => array(
      'src' => true,
      'type' => true,
      'media' => true,
    ),
  ),
  'track' => array(
    'version' => HTML5,
    'attr' => array(
      'default' => true,
      'kind' => true,
      'label' => true,
      'src' => true,
      'srclang' => true,
    ),
  ),
  'canvas' => array(
    'version' => HTML5,
    'attr' => array(
      'width' => true,
      'height' => true,
    ),
  ),
  'map' => array(
    'attr' => array(
      'name' => true,
    ),
  ),
  'area' => array(
    'attr' => array(
      'alt' => true,
      'coords' => true,
      'shape' => true,
      'href' => true,
      'target' => true,
      'ping' => true,
      'rel' => true,
      'media' => true,
      'hreflang' => true,
      'type' => true,
    ),
  ),
  'svg' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'math' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'table' => array(
    'attr' => array(),
  ),
  'caption' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'colgroup' => array(
    'attr' => array(
      'span' => true,
    ),
  ),
  'col' => array(
    'attr' => array(
      'span' => true,
    ),
  ),
  'tbody' => array(
    'attr' => array(),
  ),
  'thead' => array(
    'attr' => array(),
  ),
  'tfoot' => array(
    'attr' => array(),
  ),
  'tr' => array(
    'attr' => array(),
  ),
  'td' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(
      'colspan' => true,
      'rowspan' => true,
      'headers' => true,
    ),
  ),
  'th' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(
      'colspan' => true,
      'rowspan' => true,
      'headers' => true,
      'scope' => true,
      'abbr' => true,
    ),
  ),
  'form' => array(
    'attr' => array(
      'accept-charset' => true,
      'action' => true,
      'autocomplete' => true,
      'enctype' => true,
      'method' => true,
      'name' => true,
      'novalidate' => true,
      'target' => true,
    ),
  ),
  'fieldset' => array(
    'attr' => array(
      'disabled' => true,
      'form' => true,
      'name' => true,
    ),
  ),
  'legend' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(),
  ),
  'label' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(
      'form' => true,
      'for' => true,
    ),
  ),
  'input' => array(
    'empty' => true,
    'spec' => array(
      'space-before' => false,
      'space-after' => false,
    ),
    'attr' => array(
      'accept' => true,
      'alt' => true,
      'autocomplete' => true,
      'autofocus' => true,
      'checked' => true,
      'dirname' => true,
      'disabled' => true,
      'form' => true,
      'formaction' => true,
      'formenctype' => true,
      'formmethod' => true,
      'formnovalidate' => true,
      'formtarget' => true,
      'height' => true,
      'inputmode' => true,
      'list' => true,
      'max' => true,
      'maxlength' => true,
      'min' => true,
      'multiple' => true,
      'name' => true,
      'pattern' => true,
      'placeholder' => true,
      'readonly' => true,
      'required' => true,
      'size' => true,
      'src' => true,
      'step' => true,
      'type' => array(
        'whitelist' => array(
          'hidden',
          'text',
          'search',
          'tel',
          'url',
          'email',
          'password',
          'datetime',
          'date',
          'month',
          'week',
          'time',
          'datetime-local',
          'number',
          'range',
          'color',
          'checkbox',
          'radio',
          'file',
          'submit',
          'image',
          'reset',
          'button',
        )
      ),
      'value' => true,
      'width' => true,
      'title' => true,
    ),
  ),
  'button' => array(
    'attr' => array(
      'autofocus' => true,
      'disabled' => true,
      'form' => true,
      'formaction' => true,
      'formenctype' => true,
      'formmethod' => true,
      'formnovalidate' => true,
      'formtarget' => true,
      'name' => true,
      'type' => true,
      'value' => true,
    ),
  ),
  'select' => array(
    'attr' => array(
      'autofocus' => true,
      'disabled' => true,
      'form' => true,
      'multiple' => true,
      'name' => true,
      'required' => true,
      'size' => true,
    ),
  ),
  'datalist' => array(
    'version' => HTML5,
    'attr' => array(
      'option' => true,
    ),
  ),
  'optgroup' => array(
    'attr' => array(
      'disabled' => true,
      'label' => true,
    ),
  ),
  'option' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(
      'disabled' => true,
      'label' => true,
      'selected' => true,
      'value' => true,
    ),
  ),
  'textarea' => array(
    'spec' => array(
      'space-before' => true,
      'space-after' => false,
    ),
    'attr' => array(
      'autocomplete' => true,
      'autofocus' => true,
      'cols' => true,
      'dirname' => true,
      'disabled' => true,
      'form' => true,
      'inputmode' => true,
      'maxlength' => true,
      'name' => true,
      'placeholder' => true,
      'readonly' => true,
      'required' => true,
      'rows' => true,
      'wrap' => true,
    ),
  ),
  'keygen' => array(
    'version' => HTML5,
    'attr' => array(
      'autofocus' => true,
      'challenge' => true,
      'disabled' => true,
      'form' => true,
      'keytype' => true,
      'name' => true,
    ),
  ),
  'output' => array(
    'version' => HTML5,
    'attr' => array(
      'for' => true,
      'form' => true,
      'name' => true,
    ),
  ),
  'progress' => array(
    'version' => HTML5,
    'attr' => array(
      'value' => true,
      'max' => true,
    ),
  ),
  'meter' => array(
    'version' => HTML5,
    'attr' => array(
      'value' => true,
      'min' => true,
      'max' => true,
      'low' => true,
      'high' => true,
      'optimum' => true,
    ),
  ),
  'details' => array(
    'version' => HTML5,
    'attr' => array(
      'open' => true,
    ),
  ),
  'summary' => array(
    'version' => HTML5,
    'attr' => array(),
  ),
  'command' => array(
    'version' => HTML5,
    'attr' => array(
      'type' => true,
      'label' => true,
      'icon' => true,
      'disabled' => true,
      'checked' => true,
      'radiogroup' => true,
      'command' => true,
    ),
  ),
  'menu' => array(
    'version' => HTML5,
    'attr' => array(
      'type' => true,
      'label' => true,
    ),
  ),
  'dialog' => array(
    'version' => HTML5,
    'attr' => array(
      'open' => true,
    ),
  ),
);

foreach ( $html5_spec as $element => &$spec ) {
  
  $attr = read_d( $html5_spec, $element, 'attr', array() );
  $attr = $attr + $html5_global_attr;
  $attr = $attr + $html5_global_event;
  
  $html5_attr += $attr;
  
  $html5_spec[ $element ][ 'attr' ] = $attr;
  
}

class GrcHtmlElement {

  public $tag;
  public $attributes;
  
  public $parent;
  public $children;
  
  public $tail;
  
  // the 'mark' is the old tail for restore()
  public $mark;
  
  // used for i18n context
  private $context;
  
  public function __construct( $tag, $parent ) {
    
    $this->tag = $tag;
    $this->attributes = array();
    
    $this->parent = $parent;
    $this->children = array();
    
    $this->tail = $this;
    $this->mark = $this;
    
    $this->context = array( CONTEXT_GLOBAL );
    
  }
  
  public static function Create( $tag = null, $parent = null ) {
    
    if ( $tag === null ) {
      
      throw Error( 'Tag must be specified.' );
      
    }
    
    return new GrcHtmlElement( $tag, $parent );
    
  }
  
  public function get_attribute( $name ) { return read_a( $this->attributes, $name ); }
    
  public function root() {
        
    $element = $this;
    
    while ( $element->parent !== null ) {
      
      $element = $element->parent;
      
    }
    
    return $element;
    
  }
  
  public function __call( $method, $args ) {

    return $this->dispatch( $method, $args );
    
  }
  
  public function __get( $attr ) {
    
    return read_a( $this->attributes, $attr );
    
  }
  
  public function __set( $attr, $value ) {
    
    $this->validate_attr( $attr, $value );
    
    $this->attributes[ $attr ] = $value;
    
  }
  
  public function __toString() {
    
    return $this->to_html();
    
  }
  
  public function decl( &$var, $val = null ) {
    
    $var = $val;
    
    return $this;
    
  }
  
  public function dispatch( $method, &$args ) {
    
    $parts = explode( '_', $method, 2 );

    $tag = $parts[ 0 ];
    
    if ( count( $parts ) === 2 ) {
      
      if ( $parts[ 1 ] !== 'end' ) {
        
        throw Error(
          'Expecting "end" on method "%method%".',
          'method', $method
        );
        
      }
      
      if ( $this->tag !== $tag ) {
        
        $path = array();
        $parent = $this;
        
        while ( $parent ) {
          
          $path[] = $parent->tag;
          
          $parent = $parent->parent;
          
        }
        
        $path = implode( '/', array_reverse( $path ) );
        
        dump( $path );
        //die;
        
        throw Error(
          'Mismatched close element "%method%" for tag "%tag%" expected ' .
            '"%expected%".',
          'method', $method,
          'tag', $tag,
          'expected', $this->tag
        );
        
      }

      $this->root()->tail = $this->parent;
      
      return $this->parent;
      
    }

    if ( $tag === 'text' || $tag === 'markup' || $tag === 'literal' ) {
      
      $new_element = GrcHtmlElement::Create( $tag, $this );

      $new_element->attributes[ 'content' ] = $args[ 0 ];
      
      $this->children[] = $new_element;

      $this->root()->tail = $this;
      
      return $this;
      
    }
    
    global $html5_spec;
    
    if ( ! read_a( $html5_spec, $tag ) ) {
      
      throw Error(
        'Invalid HTML element "%tag%".',
        'tag', $tag
      );
      
    }
    
    $new_element = GrcHtmlElement::Create( $tag, $this );
    
    $new_element->attr_args( $args );
    
    $this->children[] = $new_element;

    if ( read_a( $html5_spec, $tag, 'empty' ) ) {
      
      $this->root()->tail = $this;
      
      return $this;
      
    }
    
    $this->root()->tail = $new_element;

    return $new_element;
    
  }
  
  public function nbsp() {
    
    return $this->markup( '&nbsp;' );
    
  }
  
  public function add( $element ) {
    
    if ( $element === null ) { return $this; }
    
    if ( is_a( $element, 'GrcHtmlComposite' ) ) {
      
      $children = $element->children;
      
      foreach ( $children as $el ) {
        
        $this->add( $el );
        
      }
      
      return $this;
      
    }

    if ( $element->parent ) {
      
      $element->parent->remove( $element );
      
    }
    
    $element->parent = $this;
    
    $this->children[] = $element;
    
    return $this;
    
  }
  
  public function remove( $element ) {

    $key = array_search( $element, $this->children );
    
    if ( $key !== false ) {
      
      unset( $this->children[ $key ] );
      
    }
  }

  public function attr() {
    
    $args = func_get_args();
    
    return $this->attr_args( $args );
    
  }
  
  public function add_context( $context ) {
    
    $this->root()->context[] = $context;
    
  }
  
  public function get_context() {
    
    return vector_nth_value( $this->root()->context );
    
  }
  
  public function pop_context() {
    
    return array_pop( $this->root()->context );
    
  }
  
  public function reset( $context = CONTEXT_GLOBAL ) {
    
    //$this->tag = $tag;
    $this->attributes = array();
    
    $this->parent = null;
    $this->children = array();
    
    $this->tail = $this;
    
    $this->context = array( $context );
    
  }
  
  public function T_Hn() {
    
    $args = func_get_args();
    
    return $this->add_translation(
      'html',
      $args,
      CONTEXT_HEADING
    );
    
  }

  public function T_TH() {
    
    $args = func_get_args();
    
    return $this->add_translation(
      'nbsp',
      $args,
      CONTEXT_TABLE_HEADING
    );
    
  }

  public function T_H() {
    
    $args = func_get_args();
    
    return $this->add_translation( 'html', $args );
    
  }
  
  public function T_H01n(
    $zero_message,
    $one_message,
    $other_message,
    $number,
    $context = CONTEXT_GLOBAL
  ) {
    
    return $this->markup(
      H01n( $zero_message, $one_message, $other_message, $number, $context )
    );
    
  }
  
  public function T_A() {
    
    $args = func_get_args();
    
    return $this->add_translation( 'attr', $args );
    
  }
  
  public function T_A01n(
    $zero_message,
    $one_message,
    $other_message,
    $number,
    $context = CONTEXT_GLOBAL
  ) {
    
    return $this->markup(
      A01n( $zero_message, $one_message, $other_message, $number, $context )
    );
    
  }

  public function T_NBSP() {
    
    $args = func_get_args();
    
    return $this->add_translation( 'nbsp', $args );
    
  }
  
  public function T_T() {
    
    $args = func_get_args();
    
    return $this->add_translation( 'text', $args );
    
  }
  
  public function T_T01n(
    $zero_message,
    $one_message,
    $other_message,
    $number,
    $context = CONTEXT_GLOBAL
  ) {
    
    return $this->text(
      T01n( $zero_message, $one_message, $other_message, $number, $context )
    );
    
  }
  
  public function T_S() {
    
    $args = func_get_args();
    
    return $this->add_translation( 'safe', $args );
    
  }
  
  public function T_S01n(
    $zero_message,
    $one_message,
    $other_message,
    $number,
    $context = CONTEXT_GLOBAL
  ) {
    
    return $this->text(
      S01n( $zero_message, $one_message, $other_message, $number, $context )
    );
    
  }
  
  public function add_goto(
    $url,
    $inner_html_message = null,
    $title_message = null,
    $class = null,
    $element_attrs = null
  ) {

    dal()->url->whitelist( $url );
    
    return $this->new_link(
      '/meta-refresh.php',
      $inner_html_message,
      $title_message,
      $class,
      array( 'goto' => $url ),
      $element_attrs
    );
    
  }
    
  public function add_link(
    $input,
    $title_message = null,
    $class = null,
    $query = null,
    $element_attrs = null
  ) {
    
    $args = func_get_args();

    if ( is_array( $input ) ) {
      
      $inner_html_message = read_a( $input, 'inner_html' );
      $title_message = read_a( $input, 'title' );
      $class = read_a( $input, 'class' );
      $query = read_a( $input, 'query' );
      $element_attrs = read_a( $input, 'element_attrs' );

      array_shift( $args );
      
    } 
    else {
      
      $inner_html_message = $input;
      
      array_shift( $args );
      array_shift( $args );
      array_shift( $args );
      array_shift( $args );
      array_shift( $args );

    }
    
    $inner_html = 'link';
    
    if ( $inner_html_message ) {

      $inner_html_args = array_merge( array( $inner_html_message ), $args );

      $inner_html = intl()->format( 'html', $inner_html_args, CONTEXT_LINK );
      
    }
    
    $title = null;
    
    if ( $title_message ) {

      $title_args = array_merge( array( $title_message ), $args );

      $title = intl()->format( 'attr', $title_args, CONTEXT_LINK );
      
    }
    
    $link = get_link( $inner_html, $title, $class, $query );
    
    $anchor = $link->to_anchor();
    
    if ( $element_attrs ) {

      $class = read_a( $element_attrs, 'class' );
      
      if ( $class ) { unset( $element_attrs[ 'class' ] ); }
      
      $element_attrs = flatten( $element_attrs );
      
      $anchor->attr_args( $element_attrs );
      
      if ( $class ) { $anchor->add_class( $class ); }
      
    }
    
    return $this->add( $anchor );
    
  }
  
  public function new_link(
    $input, /* $path or array spec */
    $inner_html_message = null,
    $title_message = null,
    $class = null,
    $query = null,
    $element_attrs = null
  ) {
    
    $args = func_get_args();

    if ( is_array( $input ) ) {
      
      $path = read_a( $input, 'path' );
      $inner_html_message = read_a( $input, 'inner_html' );
      $title_message = read_a( $input, 'title' );
      $class = read_a( $input, 'class' );
      $query = read_a( $input, 'query' );
      $element_attrs = read_a( $input, 'element_attrs' );

      array_shift( $args );
      
    } 
    else {
      
      $path = $input;
      
      array_shift( $args );
      array_shift( $args );
      array_shift( $args );
      array_shift( $args );
      array_shift( $args );
      array_shift( $args );

    }
    
    $inner_html = 'link';
    
    if ( $inner_html_message ) {

      $inner_html_args = array_merge( array( $inner_html_message ), $args );

      $inner_html = intl()->format( 'html', $inner_html_args, CONTEXT_LINK );
      
    }
    
    $title = null;
    
    if ( $title_message ) {

      $title_args = array_merge( array( $title_message ), $args );

      $title = intl()->format( 'attr', $title_args, CONTEXT_LINK );
      
    }
    
    $link = new_link( $path, $inner_html, $title, $class, $query );
    
    $anchor = $link->to_anchor();
    
    if ( $element_attrs ) {

      $class = read_a( $element_attrs, 'class' );
      
      if ( $class ) { unset( $element_attrs[ 'class' ] ); }
      
      $element_attrs = flatten( $element_attrs );
      
      $anchor->attr_args( $element_attrs );
      
      if ( $class ) { $anchor->add_class( $class ); }
      
    }
    
    return $this->add( $anchor );
    
  }
  
  public function add_class( $class ) {

    if ( ! is_array( $class ) ) {

      $class = preg_split( "/\\s+/", trim( $class ) );
      
    }
    
    $result = array();

    $current = read_a( $this->attributes, 'class' );
    
    if ( $current ) {
      
      $current = preg_split( "/\\s+/", trim( $current ) );
      
      foreach ( $current as $name ) {
        
        if ( strlen( $name ) ) { $result[] = $name; }
        
      }
    }
    
    foreach ( $class as $name ) {
      
      if ( strlen( $name ) ) { $result[] = $name; }
      
    }
    
    $result = array_unique( $result );
    
    $result = implode( ' ', $result );
    
    $this->validate_attr( 'class', $result );

    $this->attributes[ 'class' ] = $result;

    return $this;
    
  }
  
  public function get( &$result ) {
    
    $result = $this;
    
    return $this;
    
  }
  
  public function mark() {
    
    $root = $this->root();
    
    $root->mark = $root->tail;
    
    return $this;
    
  }
  
  public function restore() {

    $root = $this->root();
    
    $root->tail = $root->mark;
    
    return $this;
    
  }
  
  public function find( $test, $element = null, $result = null ) {
        
    if ( $element === null ) {

      $this->mark();
     
      $result = GrcHtmlComposite::Create();
      
      $this->find( $test, $this->root(), $result );
      
      return $result;
      
    }
    
    if ( $test( $element ) ) {
      
      $result->add( $element );
      
    }

    foreach ( $element->children as $child ) {
      
      $this->find( $test, $child, $result );
      
    }

    return $result;
    
  }
  
  public function find_id( $id ) {

    return $this->find(
      function( $element ) use ( $id ) {
        return read_a( $element->attributes, 'id' ) === $id;
      }
    );

  }
  
  public function find_class( $class ) {

    return $this->find(
      function( $element ) use ( $class ) {
        $array = read_a( $element->attributes, 'class' );
        if ( ! is_array( $array ) ) {
          $array = explode( ' ', $class );
        }
        return in_array(
          $class,
          $array
        );
      }
    );
        
  }
  
  public function find_tag( $tag ) {

    return $this->find(
      function( $element ) use ( $tag ) { return $element->tag === $tag; }
    );
        
  }
  
  public function attr_args( &$args ) {

    global $html5_spec;
    
    $argc = count( $args );
    
    if ( $argc === 1 ) {

      $default = read_a( $html5_spec, $this->tag, 'default' );
      
      if ( $default === null ) {
  
        $this->markup( $args[ 0 ] );
        
      }
      else {
      
        $this->validate_attr( $default, $args[ 0 ] );
        
        $this->attributes[ $default ] = $args[ 0 ];
      
      }
    } 
    else {

      for ( $i = 0; $i < $argc; $i += 2 ) {
      
        $value = $args[ $i + 1 ];
        
        if ( $value === null ) { continue; }
        
        $name = $args[ $i ];
        
        $this->validate_attr( $name, $value );
                
        $this->attributes[ $name ] = $value;
          
      }
    }
    
    return $this;
    
  }
  
  public function get_version( $default = HTML_VERSION ) {
    
    static $valid = array( '4' => true, 'X' => true, '5' => true );
    
    $override = read_a( $_GET, 'html' );
    
    if ( array_key_exists( $override, $valid ) ) {
      
      return $override;
      
    }
    
    if ( ! array_key_exists( $default, $valid ) ) {

      throw Error(
        'Unsuppored HTML version "%default%".',
        'default', $default
      );
      
    }
    
    return $default;
    
  }
  
  public function get_pretty( $default = HTML_PRETTY ) {
    
    $override = read_a( $_GET, 'pretty' );
    
    if ( $override === null ) { return $default; }
    
    return $override ? true : false;
    
  }
  
  public function get_renderer( $version, $pretty ) {

    switch ( $version ) {
      
      case '4' : return GrcHtmlView4::Create( $pretty );
      case 'X' : return GrcHtmlViewX::Create( $pretty );
      case '5' : return GrcHtmlView5::Create( $pretty );
        
    }
    
    throw Error(
      'Unsupported HTML version "%version%".',
      'version', $version
    );
    
  }
  
  public function to_html(
    $default_version = HTML_VERSION,
    $default_pretty = HTML_PRETTY
  ) {

    $version = $this->get_version( $default_version );
    $pretty = $this->get_pretty( $default_pretty );
    
    $renderer = $this->get_renderer( $version, $pretty );
    
    return $renderer->to_html( $this );

  }
  
  public function render(
    $default_version = HTML_VERSION,
    $default_pretty = HTML_PRETTY
  ) {
    
    $version = $this->get_version( $default_version );
    $pretty = $this->get_pretty( $default_pretty );
    
    $renderer = $this->get_renderer( $version, $pretty );
    
    $renderer->render( $this );
    
  }
  
  public function hidden(
    $name,
    $value
  ) {
    
    return $this->input( 'type', 'hidden', 'name', $name, 'value', $value );
    
  }
  
  public function input_field(
    $heading,
    $id,
    $name,
    &$data,
    $placeholder = null,
    $tooltip = null,
    $auto_complete = 'on',
    $events = null,
    $class = null,
    $th_width = null,
    $td_width = null,
    $disabled = false
  ) {

    return $this->input_template(
      'text',
      $heading,
      $id,
      $name,
      $data,
      $placeholder,
      $tooltip,
      $auto_complete,
      $events,
      $class,
      $th_width,
      $td_width,
      $disabled
    );
    
  }
  
  public function password_field(
    $heading,
    $id,
    $name,
    &$data,
    $placeholder = null,
    $tooltip = null,
    $auto_complete = 'on',
    $events = null,
    $class = null,
    $th_width = null,
    $td_width = null
  ) {

    return $this->input_template(
      'password',
      $heading,
      $id,
      $name,
      $data,
      $placeholder,
      $tooltip,
      $auto_complete,
      $events,
      $class,
      $th_width,
      $td_width
    );
    
  } 

  public function submit_field(
    $heading,
    $id,
    $placeholder,
    $tooltip = null,
    $th_width = null,
    $td_width = null
  ) {

    return $this->input_template(
      'submit',
      $heading,
      $id,
      null,
      $data,
      $placeholder,
      $tooltip,
      null,
      null,
      null,
      null,
      $th_width,
      $td_width
    );
    
  } 

  public function select_field(
    $heading,
    $id,
    $name,
    &$map,
    &$data,
    $tooltip = null,
    $events = null,
    $class = null,
    $th_width = null,
    $td_width = null,
    &$th = null,
    &$td = null,
    $prepend = null
  ) {

    global $error;
    
    $error_message = read_a( $error, $name );

    $selected = $data[ $name ];
    
    $th_style = 'vertical-align:middle;text-align:right;';
    $td_style = 'text-align: left;';
    
    if ( $th_width ) {
      
      $th_style .= 'width:' . $th_width;
      
    }
    
    if ( $td_width ) {
      
      $td_style .= 'width:' . $td_width;
      
    }
    
    $tail = $this->
      tr()->
        th(
          'style', $th_style,
          'title', A( $tooltip )
        )->
          get( $th )->
          label( 'for', $id )->
            T_TH( $heading )->
            markup( ':' )->
          label_end()->
        th_end()->
        td(
          'style', $td_style,
          'title', A( $tooltip )
        )->
          get( $td )->
          test( $prepend )->add( $prepend )->test_end()->
          select(
            'id', $id,
            'name', $name,
            'class', $class,
            'data-selected', serialize( $selected )
          )->
            get( $select )->
          select_end()->
        td_end()->
        test( $error_message )->
          td( 'class', 'error' )->
            text( $error_message )->
          td_end()->
        test_end()->
      tr_end();

    if ( $events ) {
      
      foreach ( $events as $event => $dispatch ) {
        
        $select->$event = $dispatch;
        
      }
    }
    
    foreach ( $map as $key => $val ) {
      
      //var_dump( array( $key, $selected ) ); die;
      
      $select->
        option(
          'value', $key,
          'selected', ( $key == $selected )
        )->
          markup( $val )->
        option_end();
      
    }
    
    return $tail;
    
  }
  
  private function input_template(
    $type,
    $heading,
    $id,
    $name,
    &$data,
    $placeholder = null,
    $tooltip = null,
    $auto_complete = 'on',
    $events = null,
    $class = null,
    $th_width = null,
    $td_width = null,
    $disabled = false
  ) {
    
    global $error;
    
    $error_message = read_a( $error, $name );

    if ( $name === null ) {
      
      $value = $placeholder;
      
    }
    else {
    
      $value = read_a( $data, $name );
  
    }
    
    $th_style = 'vertical-align:middle;text-align:right;';
    $td_style = 'text-align:left;';
    
    if ( $th_width ) {
      
      $th_style .= 'width:' . $th_width;
      
    }
    
    if ( $td_width ) {
      
      $td_style .= 'width:' . $td_width;
      
    }
    
    $tail = $this->
      tr()->
        th(
          'style', $th_style,
          'title', A( $tooltip )
        )->
          label( 'for', $id )->
            T_TH( $heading )->
            markup( ':' )->
          label_end()->
        th_end()->
        td(
          'style', $td_style,
          'title', A( $tooltip )
        )->
          input(
            'type', $type,
            'id', $id,
            'name', $name,
            'value', $value,
            'placeholder', $placeholder,
            'autocomplete', $auto_complete,
            'class', $class,
            'disabled', $disabled
          )->
          get( $td )->
        td_end()->
        test( $error_message )->
          td( 'class', 'error' )->
            text( $error_message )->
          td_end()->
        test_end()->
      tr_end();
    
    if ( $events ) {
      
      foreach ( $events as $event => $dispatch ) {
        
        $input = $td->children[ 0 ];
    
        $input->$event = $dispatch;
        
      }
    }
    
    return $tail;
    
  }
  
  private function validate_attr( $name, $value ) {
    
    $tag = $this->tag;

    if ( preg_match( '/^[^a-z][^a-z0-9\-_]*$/', $name ) ) {

      throw Error(
        'Attribute name "%name%" on element "%tag%" contains invalid ' .
          'characters.',
        'name', $name,
        'tag', $tag
      );
      
    }
    
    if ( strpos( $name, 'data-' ) === 0 ) { return; }
    
    global $html5_spec;
    
    if ( ! read_a( $html5_spec, $tag, 'attr', $name ) ) {
      
      throw Error(
        'Invalid attribute "%name%" on element "%tag%".',
        'name', $name,
        'tag', $tag
      );
      
    }
    
    if ( is_bool( $value ) ) { return; }
    
    if ( preg_match( '/<>\'\"/', $value ) ) {

      throw Error(
        'Attribute value "%value%" for "%name%" on element "%tag%" ' .
          'contains invalid characters.',
        'value', $value,
        'name', $name,
        'tag', $tag
      );
      
    }
  }
  
  private function add_translation(
    $type,
    &$args,
    $context = CONTEXT_GLOBAL
  ) {

    $nbsp = false;
    
    if ( $type === 'nbsp' ) {
      
      $type = 'attr';
      $nbsp = true;
      
    }
    
    $context = ( $context === CONTEXT_GLOBAL ) ?
      $this->get_context() :
      $context;
    
    $content = intl()->format( $type, $args, $context );

    if ( $type === 'text' || $type === 'safe' ) {

      return $this->text( $content );
      
    }
    
    if ( $nbsp ) {
      
      return $this->markup( nbsp( $content ) );
      
    }
    
    return $this->markup( $content );
    
  }
}
