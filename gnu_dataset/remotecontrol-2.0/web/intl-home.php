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

require_once __DIR__ . '/../src/include.php';

verify( ! user()->deleted );

html( intl()->get_lang() );

render_head( 'Language Administration Dashboard' );

render_page();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;

  //$tail->add( get_intl_menu() );
  
  $active_count = dal()->intl->active_count();
  $inactive_count = dal()->intl->inactive_count();
  $language_count = $active_count + $inactive_count;
  
  $context_count = dal()->intl->context_count();

  $html_count = dal()->intl->html_message_count();
  $attr_count = dal()->intl->attr_message_count();
  $text_count = dal()->intl->text_message_count();
  $safe_count = dal()->intl->safe_message_count();
  $message_count = $html_count + $attr_count + $text_count + $safe_count;
  
  $translation_count = dal()->intl->translation_count();
  
  $invalid_count = dal()->intl->invalid_count();
  $invalid_reported_count = dal()->intl->invalid_reported_count();
  
  $missing_count = dal()->intl->missing_count();
  $missing_reported_count = dal()->intl->missing_reported_count();
    
  $help_link = new_link(
    '/intl-help.php',
    H( 'Help' ),
    A( 'Comprehensive help is available.' )
  );
  
  if ( is_translator() ) {
    
    $translator_table = new GrcTableView(
      'tran_list',
      array(
        'langtag' => array(
          'heading' => A( 'Language', CONTEXT_TABLE_HEADING ),
          'type' => STRING_COLUMN
        ),
      )
    );
    
    //$translator_data = dal()->intl->translator_report( get_username() );
    
  }
  
  $tail->
    p()->
      T_H( 'Welcome to the language administration dashboard. ' )->
      T_H( 'From here you can manage the internationalization subsystem ' .
        'of GNUrc. ' )->
      T_H( 'The internationalization subsystem provides support ' .
        'for translation of content within the GNUrc web interface. ' )->
      T_H( 'To navigate the language administration system please ' .
        'use the Language Admin Menu in the top left of your screen. ' )->
      add( $help_link->to_anchor() )->
      T_H( ' is available from the administration menu.' )->
    p_end()->
    test( is_administrator() )->
      p()->
        span(
          H01n(
            'You have no active languages ',
            'You have one active language ',
            'You have %number% active languages ',
            $active_count,
            CONTEXT_INTL
          ) .
          H01n(
            'and no inactive languages ',
            'and one inactive language ',
            'and %number% inactive languages ',
            $inactive_count,
            CONTEXT_INTL
          ) .
          H01n(
            'for a total of zero languages. ',
            'for a total of one language. ',
            'for a total of %number% languages. ',
            $language_count,
            CONTEXT_INTL
          )
        )->
          T_H( 'You can ', CONTEXT_INTL )->
          add(
            new_link(
              '/intl-language-list.php',
              H( 'edit registered languages', CONTEXT_INTL )
            )->to_anchor()
          )->
          T_H( ' or ', CONTEXT_INTL )->
          add(
            new_link(
              '/intl-language-add.php',
              H( 'add a language', CONTEXT_INTL )
            )->to_anchor()
          )->
          T_H( '.', CONTEXT_INTL )->
        span_end()->
      p_end()->
      p()->
        span(
          H01n(
            'You have no HTML messages, ',
            'You have one HTML message, ',
            'You have %number% HTML messages, ',
            $html_count,
            CONTEXT_INTL
          ) .
          H01n(
            'no attribute messages, ',
            'one attribute message, ',
            '%number% attribute messages, ',
            $attr_count,
            CONTEXT_INTL
          ) .
          H01n(
            'no text messages, ',
            'one text message, ',
            '%number% text messages, ',
            $text_count,
            CONTEXT_INTL
          ) .
          H01n(
            'and no safe messages ',
            'and one safe message ',
            'and %number% safe messages ',
            $safe_count,
            CONTEXT_INTL
          ) .
          H01n(
            'for a total of zero messages. ',
            'for a total of one message. ',
            'for a total of %number% messages. ',
            $message_count,
            CONTEXT_INTL
          )
        )->
          T_H( 'You can ', CONTEXT_INTL )->
          add(
            new_link(
              '/intl-message-list.php',
              H( 'browse registered messages', CONTEXT_INTL )
            )->to_anchor()
          )->
          T_H( '. ', CONTEXT_INTL )->
        span_end()->
        span(
          H01n(
            'There are no registered message contexts. ',
            'There is one registered message context. ',
            'There are %number% registered message contexts. ',
            $context_count,
            CONTEXT_INTL
          )
        )->
          test( $context_count !== 0 )->
            T_H( 'You can ', CONTEXT_INTL )->
            add(
              new_link(
                '/intl-context-list.php',
                H( 'browse registered message contexts', CONTEXT_INTL )
              )->to_anchor()
            )->
            T_H( '. ', CONTEXT_INTL )->
          test_end()->
        span_end()->
      p_end()->    
      p()->
        span(
          H01n(
            'You have no translations.',
            'You have one translation.',
            'You have %number% translations.',
            $translation_count,
            CONTEXT_INTL
          )
        )->
        span_end()->
      p_end()->
      p()->
        span(
          H01n(
            'There are no invalid langtags',
            'There is one invalid langtag',
            'There are %number% invalid langtags',
            $invalid_count,
            CONTEXT_INTL
          ) .
          H01n(
            '.',
            ' reported once.',
            ' reported %number% times.',
            $invalid_reported_count,
            CONTEXT_INTL
          )
        )->
        span_end()->
      p_end()->
      p()->
        span(
          H01n(
            'There are no missing langtags ',
            'There is one missing langtag ',
            'There are %number% missing langtags ',
            $missing_count,
            CONTEXT_INTL
          ) .
          H01n(
            'reported zero times.',
            'reported once.',
            'reported %number% times.',
            $missing_reported_count,
            CONTEXT_INTL
          )
        )->
        span_end()->
      p_end()->
    test_end()->
    test( false /*is_translator()*/ )->
      h2()->T_H( 'Our system detected that you are a registered translator' )->h2_end()->
      p()->
      p_end()->
    test_end();

}
