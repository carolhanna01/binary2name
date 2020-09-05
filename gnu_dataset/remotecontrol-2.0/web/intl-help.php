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

define( 'CHECK_USER', false );

require_once __DIR__ . '/../src/include.php';

verify( ! user()->deleted );

html( intl()->get_lang() );

render_head( 'Language Administration Help' );

html()->add_context( CONTEXT_INTL_HELP );

render_page();

html()->pop_context();

render_foot();

html()->render();

function render_page() {

  $tail = html()->tail;
  
  //$tail->add( get_intl_menu() );
  
  $tail->
    h2( 'id', 'welcome' )->
      T_Hn( 'Welcome' )->
    h2_end()->
    p()->
      T_H(
        'This is the help file for the GNUrc language administration system. '
      )->
      T_H(
        'The language administration system provides management and ' .
        'development support for the GNUrc internationalization subsystem, ' .
        'also known as the translations subsystem. '
      )->
      T_H(
        'The internationalization subsystem is used to manage translations ' .
        'of content within the GNUrc web interface. '
      )->
      T_H(
        'The software provides support for messages, contexts, content ' .
        'types, parameters, languages, translations and inheritance. '
      )->
      T_H(
        'These concepts are explained below.'
      )->
    p_end()->
    h2( 'id', 'toc' )->
      T_Hn( 'Table of Contents' )->
    h2_end()->
    ul()->
      li()->
        a( 'href', '#welcome' )->T_Hn( 'Welcome' )->a_end()->
      li_end()->
      li()->
        a( 'href', '#toc' )->T_Hn( 'Table of Contents' )->a_end()->
      li_end()->
      li()->
        a( 'href', '#workflow' )->T_Hn( 'Workflow' )->a_end()->
      li_end()->
      li()->
        a( 'href', '#messages' )->T_Hn( 'Messages' )->a_end()->
        ul()->
          li()->
            a( 'href', '#contexts' )->T_Hn( 'Message Contexts' )->a_end()->
          li_end()->
          li()->
            a( 'href', '#types' )->T_Hn( 'Content Types' )->a_end()->
            ul()->
              li()->
                a( 'href', '#html' )->T_Hn( 'HTML Content Type' )->a_end()->
              li_end()->
              li()->
                a( 'href', '#attr' )->T_Hn( 'HTML Attribute Content Type' )->a_end()->
              li_end()->
              li()->
                a( 'href', '#text' )->T_Hn( 'Plain Text Content Type' )->a_end()->
              li_end()->
              li()->
                a( 'href', '#safe' )->T_Hn( 'Safe Text Content Type' )->a_end()->
              li_end()->
            ul_end()->
          li_end()->
          li()->
            a( 'href', '#parameters' )->T_Hn( 'Message Parameters' )->a_end()->
            ul()->
              li()->
                a( 'href', '#01n' )->T_Hn( 'The 01n Idiom' )->a_end()->
              li_end()->
            ul_end()->
          li_end()->
        ul_end()->
      li_end()->
      li()->
        a( 'href', '#languages' )->T_Hn( 'Languages' )->a_end()->
        ul()->
          li()->
            a( 'href', '#langtags' )->T_Hn( 'Langtags' )->a_end()->
          li_end()->
          li()->
            a( 'href', '#fallbacks' )->T_Hn( 'Fallbacks' )->a_end()->
          li_end()->
          li()->
            a( 'href', '#defaults' )->
              T_Hn( 'English Language Defaults and Translations' )->
            a_end()->
          li_end()->
        ul_end()->
      li_end()->
      li()->
        a( 'href', '#translations' )->T_Hn( 'Translations' )->a_end()->
        ul()->
          li()->
            a( 'href', '#inheritance' )->T_Hn( 'Inheritance' )->a_end()->
          li_end()->
        ul_end()->
      li_end()->
    ul_end()->
    h2( 'id', 'workflow' )->
      T_Hn( 'Workflow' )->
    h2_end()->
    p()->
      T_H(
        'The intended use of the internationalization subsystem is to ' .
        'support development of the GNUrc software. '
      )->
      T_H(
        'The system can be used by the development team to develop support ' .
        'within the GNUrc software for various languges. '
      )->
      T_H(
        'It is envisioned that the language administration system will not ' .
        'be needed beyond development, as by that time translation for ' .
        'supported languages will have been completed. '
      )->
      T_H(
        'So language administration is a development-time activity.'
      )->
    p_end()->
    p()->
      T_H(
        'The languge administration system can be used at development ' .
        'time to configure support for various languages. '
      )->
      T_H(
        'The GNUrc translators will use the language administration ' .
        'system to register translated content in the GNUrc database.'
      )->
    p_end()->
    p()->
      T_H(
        'Once translation for a supported language is complete an "export" ' .
        'process is run and the GNUrc software is updated with support for ' .
        'the new language. '
      )->
      T_H(
        'After export the software includes the translated content in the ' .
        'source-code itself and, thus, the database is no longer required ' .
        'for language support purposes.'
      )->
    p_end()->
    p()->
      T_H(
        'It is hoped that the GNUrc development team will be able to ' .
        'solicit translation services from volunteers. '
      )->
      T_H(
        'However, we want to leave open the possibility of paid translation ' .
        'support. '
      )->
      T_H(
        'Paid translators are typically compensated on a per-word basis. ' .
        'Thus, it becomes important to record how many words a translator ' .
        'has translated. '
      )->
      T_H(
        'To this end a translation accounting service is provided within ' .
        'the language administration system.'
      )->
    p_end()->
    h2( 'id', 'messages' )->
      T_Hn( 'Messages' )->
    h2_end()->
    p()->
      T_H(
        'At the heart of the internationalizatoin subsystem are "messages". '
      )->
      T_H(
        'Messages are English language content available for translation. '
      )->
      T_H(
        'Each message in the system can be associated with one or more ' .
        'contexts and one or more content types. '
      )->
      T_H(
        'Message contexts and and content types are explained below. '
      )->
      T_H(
        'Messages can also contain parameters. '
      )->
      T_H(
        'Parameters are placeholders within a message which are ' .
        'automatically replaced with variable content before presentation. '
      )->
      T_H(
        'The message parameter syntax is explained below. '
      )->
      T_H(
        'You can '
      )->
      new_link(
        '/intl-language-list.php',
        'report on all messages',
        'View a report on all messages registered in GNUrc.'
      )->
      T_H(
        ' within the internationalization subsystem. '
      )->
      T_H(
        'Messages are defined by the software developers in the GNUrc ' .
        'source code. '
      )->
      T_H(
        'It is not possible to add messages via the administration tools.'
      )->
    p_end()->
    h3( 'id', 'contexts' )->
      T_Hn( 'Message Contexts' )->
    h3_end()->
    p()->
      T_H(
        'Message contexts are a label that indicates the context of a ' .
        'message. '
      )->
      T_H(
        'The message context exists to provide support for message content ' .
        'which may vary according to context. '
      )->
      T_H(
        'For example, the English language word "Japanese" can be used to ' .
        'indicate either the Japanese language or the Japanese nationality, ' .
        'depending on context. '
      )->
      T_H(
        'Because English language translations might need to vary based on ' .
        'usage context we provide support for it within our ' .
        'internationalizatoin subsystem. '
      )->
      T_H(
        'There is a default message context and it is called, ' .
        'unsurprisingly, "default". '
      )->
      T_H(
        'You can '
      )->
      new_link(
        '/intl-context-list.php',
        'report on all message contexts in use',
        'View a report on all message contexts registered in GNUrc.'
      )->
      T_H(
        ' within the internationalization subsystem.'
      )->
    p_end()->
    h3( 'id', 'types' )->
      T_Hn( 'Content Types' )->
    h3_end()->
    p()->
      T_H(
        'English language transations might need to be available in ' .
        'different formats for different purposes. '
      )->
      T_H(
        'GNUrc provides support for three content types.'
      )->
    p_end()->
    h4( 'id', 'html' )->
      T_Hn( 'HTML Content Type' )->
    h4_end()->
    p()->
      T_H(
        'The HTML Content Type is the content type of most messages. '
      )->
      T_H(
        'Content in HTML format can be used to add markup to web content ' .
        'served from GNUrc. '
      )->
      T_H(
        'By supporting HTML content in our translations we make it possible ' .
        'for translators to include '
      )->
      add_goto(
        'http://www.w3.org/TR/REC-html40/index/elements.html',
        'HTML elements',
        'Read the HTML 4.0 Element specification.'
      )->
      T_H(
        ' and/or '
      )->
      add_goto(
        'http://www.w3.org/TR/REC-html40/sgml/entities.html',
        'HTML entities',
        'Read the HTML 4.0 Entities specification.'
      )->
      T_H(
        ' in their translations, as may be appropriate. '
      )->
      T_H(
        'This means translators can include HTML markup, such as '
      )->
      T_T(
        '<b>for bold</b>'
      )->
      T_H(
        ', within their translations.'
      )->
    p_end()->
    h4( 'id', 'attr' )->
      T_Hn( 'HTML Attribute Content Type' )->
    h4_end()->
    p()->
      T_H(
        'The second most commony used content type is the HTML Attribute ' .
        'format. '
      )->
      T_H(
        'HTML attribute content differs from HTML content in that it ' .
        'provides support for HTML entities, but not HTML elements. '
      )->
      T_H(
        'For example HTML content allows users to include markup elements ' .
        'such as '
      )->
      T_T(
        '<b>for bold</b>'
      )->
      T_H(
        '. '
      )->
      T_H(
        'Such elements cannot be used in the HTML Attribute format. '
      )->
      T_H(
        'There is, however, support for HTML entities. '
      )->
      T_H(
        'HTML entities are a format for representing special characters ' .
        'within HTML documents. '
      )->
      T_H(
        'For example the HTML entity '
      )->
      markup( '<code>&amp;copy;</code>' )->
      T_H(
        ' can be used to include the copyright symbol (&copy;) within ' .
        'an HTML document. '
      )->
      T_H(
        'HTML attribute content can be used in more contexts than ' .
        'HTML content, most notabily within HTML element attributes, which ' .
        "is why we've called it the " . '"attribute" format.'
      )->
    p_end()->
    h4( 'id', 'text' )->
      T_Hn( 'Plain Text Content Type' )->
    h4_end()->
    p()->
      T_H(
        'The second last content type is the plain text format. '
      )->
      T_H(
        'The plain text format allows for specification of content ' .
        'that cannot and does not support any form of HTML content. '
      )->
      T_H(
        'When used translations and messages in plain text format ' .
        'will be converted to HTML format without markup suppport ' .
        'when necessary.'
      )->
    p_end()->
    h4( 'id', 'safe' )->
      T_Hn( 'Safe Text Content Type' )->
    h4_end()->
    p()->
      T_H(
        'The last and least commonly used content type is the safe text ' .
        'format. '
      )->
      T_H(
        'The safe text format allows for specification of content ' .
        'that can safely be used in HTML documents without conversion ' .
        'to HTML. '
      )->
      T_H(
        'This means that safe text content cannot and must not include ' .
        'the characters: <code>&lt; &gt; &apos; &quot; &amp;</code>'
      )->
    p_end()->
    h3( 'id', 'parameters' )->
      T_Hn( 'Message Paramters' )->
    h3_end()->
    p()->
      T_H(
        'Message content can include parameter tags delimited ' .
        'by percent signs (%). '
      )->
      T_H(
        'For example, a number might be represented within a ' .
        'message as %number%. '
      )->
      T_H(
        'Translations retain the %number% parameter within ' .
        'translated content and substitutions occur before content presentation. '
      )->
    p_end()->
    p()->
      T_H(
        'For example, a message might be defined as: '
      )->
      T_H(
        '"Your name is %name%." '
      )->
      T_H(
        'This message might then be translated into Japanese as: '
      )->
      //markup( '"あなたの名前は%name%です。" '
      markup( '"あなたの名前は%name%です" '
      )->
      T_H(
        'Note that the parameter is retained in the translated content. '
      )->
      T_H(
        'This allows for parameter substitution. '
      )->
      T_H(
        'So before message translations are rendered the paramters ' .
        'are replaced with relevant values, which can vary by context.'
      )->
    p_end()->
    h4( 'id', '01n' )->
      T_Hn( 'The 01n Idiom' )->
    h4_end()->
    p()->
      T_H(
        'One idiom of particular interest within the translation system ' .
        'is the 01n pattern. '
      )->
      T_H(
        'This pattern supports translation for numbers where the number ' .
        'is 0, 1 or n. '
      )->
      T_H(
        'When a variable is zero (0) the "zero" message is used. '
      )->
      T_H(
        'For example the "zero" message might be "You have zero items." '
      )->
      T_H(
        'When a variable is one (1) the "one" message is used. '
      )->
      T_H(
        'For example the "one" message might be "You have one item." '
      )->
      T_H(
        'And when a variable is any number other than zero or one the ' .
        '"nth" message is used. '
      )->
      T_H(
        'For example the "nth" message might be "You have %number% items." '
      )->
      T_H(
        'The 01n idiom allows for correct and appropriate pluralization.'
      )->
    p_end()->
    h2( 'id', 'languages' )->
      T_Hn( 'Languages' )->
    h2_end()->
    p()->
      T_H(
        'GNUrc administrators can add support for particular ' .
        'languages by adding them into the system. '
      )->
      T_H(
        'Over 8,000 languages, and their variants and regionalizations, ' .
        'are supported. '
      )->
      T_H(
        'You can '
      )->
      new_link(
        '/intl-language-add.php',
        'add a language'
      )->
      T_H(
        ' or '
      )->
      new_link(
        '/intl-language-list.php',
        'edit registered languages',
        'Click to report on registered languages.'
      )->
      T_H(
        '.', CONTEXT_PUNCTUATION )->
    p_end()->
    h3( 'id', 'langtags' )->
      T_Hn( 'Langtags' )->
    h3_end()->
    p()->
      T_H(
        'Within the translation subsystem languages are represented ' .
        'by what we call "langtags", short for "language tags". '
      )->
      T_H(
        'The langtag format is defined in a standard known as '
      )->
      add_goto(
        'http://tools.ietf.org/html/rfc5646',
        'RFC5646: Tags for Identifying Languages'
      )->
      T_H(
        '. ', CONTEXT_PUNCTUATION
      )->
      T_H(
        'This format is the standard format that is used by web ' .
        'browsers to indicate languages that content is desired in. '
      )->
      T_H(
        'Using this format means we can parse HTTP requests ' .
        'and redirect the user to a link that supports their language (' .
        'assuming we have support for at least one of their nominated ' .
        'languages).'
      )->
    p_end()->
    h3( 'id', 'fallbacks' )->
      T_Hn( 'Fallbacks' )->
    h3_end()->
    p()->
      T_H(
        'When a language is registered the administrator must ' .
        'nominate a fallback language for the new language. '
      )->
      T_H(
        'When translations are unavailable for a given language, ' .
        'or if the translation nominates inheritance (see below), ' .
        'the fallback language is used to provide translations. '
      )->
      T_H(
        'All languages specify a fallback language. '
      )->
      T_H(
        'The default language "en" nominates itself as its ' .
        'fallback language. '
      )->
      T_H(
        'Other languages cannot nominate themselves as fallback ' .
        'languages.'
      )->
    p_end()->
    p()->
      T_H(
        'So, for example, the en-AU language can fallback to ' .
        'en-GB, while en-GB falls back to en. '
      )->
      T_H(
        'Then, if an en-AU translation is requested, but is ' .
        'unavailable, the en-GB translation will be queried. '
      )->
      T_H(
        'In turn, if an en-GB translation is unavailable ' .
        'the en translation will be used.'
      )->
    p_end()->
    h3( 'id', 'defaults' )->
      T_Hn( 'English Language Defaults and Translations' )->
    h3_end()->
    p()->
      T_H(
        'Message content is defined for "en", which is International ' .
        'English. '
      )->
      T_H(
        'For our purposes International English is United States ' .
        'English. '
      )->
      T_H(
        'So United States English is fully supported by default ' .
        'because that is the language message content is defined in. '
      )->
      T_H(
        'It is interesting to note, however, that translations for ' .
        '"en" and "en-US" remain available. '
      )->
      T_H(
        'By default the "en-US" language inherits the "en" language, ' .
        'thus, generally, the "en" language content is referenced for "en-US" ' .
        'translations. '
      )->
      T_H(
        'However, it remains possible to provide "en-US" translations ' .
        'that differ from their "en" specifications. '
      )->
      T_H(
        'This might be useful in the unlikely case that a specific ' .
        'US translation could be defined soley for use in the "en-US" language ' .
        'where the "en" language offers a less US-centric phraseology. '
      )->
      T_H(
        'Hey, could happen. '
      )->
    p_end()->
    p()->
      T_H(
        'A more interesting possibility is translations of "en" content. '
      )->
      T_H(
        'Content is defined as "en", so what use could there be in ' .
        'providing support for "en" translations? '
      )->
      T_H(
        'The answer is: fixing spelling errors. '
      )->
      T_H(
        'If the software developers make a mistake when inputting ' .
        'the default International English in the program an administrator ' .
        'can resolve the problem by defining an "en" translation. '
      )->
      T_H(
        'Neat hack! :)'
      )->
    p_end()->
    h2( 'id', 'translations' )->
      T_Hn( 'Translations' )->
    h2_end()->
    p()->
      T_H(
        'Translations are conversions of messages into particular ' .
        'languages. '
      )->
      T_H(
        'You can '
      )->
      new_link(
        '/intl-translation-list.php',
        'managed translations',
        'View all translations registered in GNUrc.'
      )->
      T_H(
        ' within the internationalization subsystem. '
      )->
    p_end()->
    h3( 'id', 'inheritance' )->
      T_Hn( 'Inheritance' )->
    h3_end()->
    p()->
      T_H(
        'When translations are being created the translator ' .
        'has the option of inheriting its translation. '
      )->
      T_H(
        'This means that when a request for this translation ' .
        'is received the registered fallback language is consulted for ' .
        'the translation. '
      )->
      T_H(
        'So, if, for example, the International English message is ' .
        '"Hello, %name%.", then the en-GB and en-AU languages can inherit ' .
        'their translation as the English is the same in each region.'
      )->
    p_end()->
    p()->
      T_H(
        'Inheritance is the default process when translations ' .
        'for particular languages have not been registered.'
      )->
    p_end()->
    p()->
      T_H( "And that's everything we have to tell you, so far."
      )->
    p_end();
    
  
}
