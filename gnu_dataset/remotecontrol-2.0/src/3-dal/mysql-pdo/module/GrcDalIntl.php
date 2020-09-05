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

class GrcDalIntl extends GrcDalModule {
  
  public static function Create( $dal ) {
    
    return new self( $dal );
    
  }

  //public function intl_order_context( $context, $seq ) {
  public function order_context( $context, $seq ) {
    
    $sql = "replace into v2_intl_context values ( :context, :seq )";
    
    $args = array(
      'context' => $context,
      'seq' => $seq
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );
    
    $this->finish( $stmt );    
    
  }
  
  //public function intl_get_active_count() {
  public function active_count() {

    return $this->table_count( 'v2_intl_language', array( 'active' => 1 ) );
    
  }
  
  //public function intl_get_inactive_count() {
  public function inactive_count() {

    return $this->table_count( 'v2_intl_language', array( 'active' => 0 ) );
    
  }

  //public function intl_get_context_count() {
  public function context_count() {
    
    $sql = "
select
  count(distinct context) as `count`
from
  v2_intl_message
";
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );

    return $this->fetch_int( $stmt, 'count' );

  }
  
  //public function intl_get_html_message_count() {
  public function html_message_count() {

    return $this->table_count( 'v2_intl_message', array( 'type' => 'html' ) );
    
  }
  
  //public function intl_get_attr_message_count() {
  public function attr_message_count() {

    return $this->table_count( 'v2_intl_message', array( 'type' => 'attr' ) );
    
  }
  
  //public function intl_get_text_message_count() {
  public function text_message_count() {

    return $this->table_count( 'v2_intl_message', array( 'type' => 'text' ) );
    
  }
  
  //public function intl_get_safe_message_count() {
  public function safe_message_count() {

    return $this->table_count( 'v2_intl_message', array( 'type' => 'safe' ) );
    
  }
  
  //public function intl_get_translation_count() {
  public function translation_count() {

    return $this->table_count( 'v2_intl_translation', array(), array( 'langtag' => 'en' ) );
    
  }
  
  //public function intl_get_invalid_count() {
  public function invalid_count() {

    return $this->table_count( 'v2_intl_langtag_invalid' );
    
  }
  
  //public function intl_get_missing_count() {
  public function missing_count() {

    return $this->table_count( 'v2_intl_langtag_missing' );
    
  }
  
  //private function intl_get_reported_count( $type ) {
  private function get_reported_count( $type ) {
    
    $sql = "select sum(`count`) as `total` from v2_intl_langtag_$type";
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    return $this->fetch_int( $stmt, 'total' );
    
  }
  
  //public function intl_get_invalid_reported_count() {
  public function invalid_reported_count() {
    
    return $this->get_reported_count( 'invalid' );
    
  }
  
  //public function intl_get_missing_reported_count() {
  public function missing_reported_count() {
    
    return $this->get_reported_count( 'missing' );
    
  }
  
  //public function intl_register_message( $type, $context, $hash, $message ) {
  public function register_message( $type, $context, $hash, $content ) {
        
    validator()->string( $type, 'Type', 4, 4 );
    validator()->string( $context, 'Context', 1, 64 );
    validator()->md5( $hash, 'Hash' );
    validator()->string( $content, 'Content' );
    
    switch ( $type ) {
      
      case 'html' :

        validator()->html( $content );
        
        break;
      
      case 'attr' :

        validator()->attr( $content );
        
        break;
      
      case 'text' :

        validator()->text( $content );
        
        break;
      
      case 'safe' :

        validator()->safe( $content );
        
        break;
      
      default :
        
        $this->error( $this->error_invalid_message_type, 'type', $type );
      
    }

    $sql = "
select
  count(*) as `count`
from
  v2_intl_message
where
  hash = :hash
and
  type = :type
and
  context = :context
";

    $args = array(
      'hash' => $hash,
      'type' => $type,
      'context' => $context,
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );
    
    if ( $count > 0 ) { return; }
        
    $sql = "
insert into v2_intl_message (
  `type`,
  `context`,
  `hash`,
  `content`,
  words
)
values (
  :type,
  :context,
  :hash,
  :content,
  :words
)";
    
    $args = array(
      'type' => $type,
      'context' => $context,
      'hash' => $hash,
      'content' => $content,
      'words' => count_words( $content )
    );
    
    $stmt = $this->prepare( $sql );

    try {

      $this->execute( $stmt, $sql, $args );
      
    }
    catch ( Exception $ex ) {

      // hey, we tried.
      
    }
    
    $this->finish( $stmt );
    
  }

  //public function intl_next_translation( $langtag ) {
  public function next_translation( $langtag ) {

    $sql = "
SELECT
  m1.hash
FROM
  v2_intl_message m1
where m1.hash not in (
  select
    m2.`hash`
  from
    v2_intl_message m2
  inner join
    v2_intl_translation t
  on
    m2.hash = t.hash
  where
    t.langtag = :langtag
)
order by
  m1.seq
limit 1
";
    
    $args = array( 'langtag' => $langtag );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );

    return $this->fetch_string( $stmt, 'hash' );
        
  }

  //public function intl_untranslated_count_for_language( $langtag ) {
  public function untranslated_count_for_language( $langtag ) {
    
    $args = array(
      'langtag' => $langtag
    );
    
    $translated = $this->table_count( 'v2_intl_translation', $args );

    $available = $this->table_count( 'v2_intl_message' );

    $result = $available - $translated;
    
    if ( $result < 0 ) { $result = 0; }
    
    return $result;
    
  }
  
  //public function intl_untranslated_count_for_message( $langtag, $hash ) {
  public function untranslated_count_for_message( $langtag, $hash ) {

    $args = array(
      'langtag' => $langtag,
      'hash' => $hash
    );
    
    $translated = $this->table_count( 'v2_intl_translation', $args );

    $args = array(
      'hash' => $hash
    );
    
    $available = $this->table_count( 'v2_intl_message', $args );

    $result = $available - $translated;
    
    if ( $result < 0 ) { $result = 0; }
    
    return $result;
    
  }
  
  //public function intl_register_translator( $username ) {
  public function register_translator( $username ) {
    
    validator()->safe_name( $username, 'Username', 1, 20 );
    
    $sql = "replace into v2_intl_translator values ( :username )";
    
    $args = array(
      'username' => $username,
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );
    
    $this->finish( $stmt );    
    
  }
    
  //public function intl_register_translation(
  public function register_translation(
    $langtag,
    $type,
    $context,
    $hash,
    $text
  ) {
    
    validator()->string( $langtag, 'Langtag', 2, 64 );
    validator()->string( $type, 'Type', 4, 4 );
    validator()->string( $context, 'Context', 1, 64 );
    validator()->md5( $hash, 'Hash' );
    validator()->string( $text, 'Text' );
    
    switch ( $type ) {
      
      case 'html' :

        validator()->html( $text );
        
        break;
      
      case 'attr' :

        validator()->attr( $text );
        
        break;
      
      case 'text' :

        validator()->text( $text );
        
        break;
      
      case 'safe' :

        validator()->safe( $text );
        
        break;
      
      default :
        
        throw Error(
          'Invalid message type "%type%".',
          'type', $type
        );
      
    }

    $sql = "
select
  count(*) as `count`
from
  v2_intl_translation
where
  hash = :hash
and
  langtag = :langtag
and
  type = :type
and
  context = :context
";

    $args = array(
      'hash' => $hash,
      'langtag' => $langtag,
      'type' => $type,
      'context' => $context,
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );
    
    if ( $count === 0 ) {

      $this->insert_translation(
        $langtag,
        $type,
        $context,
        $hash,
        $text,
        user()->username
      );
      
    }
    else {
      
      $this->update_translation(
        $langtag,
        $type,
        $context,
        $hash,
        $text,
        user()->username
      );
      
    }
  }
  
  //private function intl_insert_translation(
  private function insert_translation(
    $langtag,
    $type,
    $context,
    $hash,
    $text,
    $translator
  ) {
        
    $sql = "
insert into v2_intl_translation (
  langtag,
  `type`,
  `context`,
  `hash`,
  `text`,
  translator
)
values (
  :langtag,
  :type,
  :context,
  :hash,
  :text,
  :translator
)";
    
    $args = array(
      'langtag' => $langtag,
      'type' => $type,
      'context' => $context,
      'hash' => $hash,
      'text' => $text,
      'translator' => $translator,
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );
    
    $this->finish( $stmt );    
    
  }
  
  private function update_translation(
    $langtag,
    $type,
    $context,
    $hash,
    $text,
    $translator
  ) {
        
    $sql = "
update
  v2_intl_translation
set
  `text` = :text
and
  translator = :translator
where
  langtag = :langtag
and
  type = :type
and
  context = :context
and
  hash = :hash
";
    
    $args = array(
      'langtag' => $langtag,
      'type' => $type,
      'context' => $context,
      'hash' => $hash,
      'text' => $text,
      'translator' => $translator
    );
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );
    
    $this->finish( $stmt );    
    
  }
  
  private function log_langtag_status( $langtag, $type ) {
              
    $sql = "select count(*) as `count` from v2_intl_langtag_$type where langtag = :langtag";
    
    $stmt = $this->prepare( $sql );

    $args = array( 'langtag' => $langtag );
    
    $this->execute( $stmt, $sql, $args );

    $count = $this->fetch_int( $stmt, 'count' );
    
    if ( $count === 0 ) {
      
      $sql = "insert into v2_intl_langtag_$type values ( :langtag, 1 )";
      
    }
    else {
      
      $sql = "update v2_intl_langtag_$type set `count` = `count` + 1 where langtag = :langtag";
      
    }
    
    $stmt = $this->prepare( $sql );

    $this->execute( $stmt, $sql, $args );
    
    $this->finish( $stmt );

    return $count + 1;
    
  }
  
  public function log_langtag_invalid( $langtag ) {
    
    return $this->log_langtag_status( $langtag, 'invalid' );
    
  }

  public function log_langtag_missing( $langtag ) {
    
    return $this->log_langtag_status( $langtag, 'missing' );
    
  }
  
  private function &get_langtag_status(
    $type,
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "
select
  t.langtag as langtag,
  t.`count` as `count`
from
  v2_intl_langtag_$type t
left join
  v2_intl_language l
on
  lower( t.langtag ) = lower( l.langtag )
where
  l.langtag is null
";

    $start = 0;
    
    if ( $page ) {

      $count = $this->sql_count( $sql );

      $page->set_count( $count );
      
      $start = $page->start;

    }

    if ( $sort ) {

      $sql .= $sort->to_sql();

    }

    if ( $page ) {

      $sql .= $page->to_sql();

    }
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_all( $stmt, $table, $start );
    
    return $table;
    
  }

  public function &get_langtag_total( $type ) {

    $sql = "
select
  count( distinct langtag ) as langtag,
  sum( `count` ) as `count`
from
  v2_intl_langtag_$type
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function &get_langtag_invalid( &$count = -1, $page = null, $sort = null ) {
    
    return $this->get_langtag_status( 'invalid', $count, $page, $sort );
    
  }

  public function &get_langtag_missing( &$count = -1, $page = null, $sort = null ) {
    
    return $this->get_langtag_status( 'missing', $count, $page, $sort );
    
  }
  
  public function &get_langtag_invalid_total() {
    
    return $this->get_langtag_total( 'invalid' );
    
  }

  public function &get_langtag_missing_total() {
    
    return $this->get_langtag_total( 'missing' );
    
  }
  
  public function &message_report(
    &$search,
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "
select
  group_concat( distinct m.`type`  order by m.`type` separator ', ' ) as `type`,
  group_concat( distinct m.context  order by m.`context` separator ', ' ) as context,
  m.`hash`,
  m.`content`,
  min( m.seq ) as seq,
  max( m.words ) as words,
  group_concat( distinct l.langtag ) as `translate`,
  group_concat( distinct t.langtag ) as `edit`
from
  v2_intl_message m
left join
  v2_intl_language l
on
  -- 'en' <> l.langtag
  1 = 1
left join
  v2_intl_translation t
on
  l.langtag = t.langtag
and
--  m.type = t.type
-- and
--  m.context = t.context
-- and
  m.hash = t.hash
group by
  m.`hash`,
  m.`content`
";
    
    $this->apply_search( $sql, $search, 'having' );
    
    //echo "<pre>"; echo $sql; exit;
    
    $start = 0;
    
    if ( $page ) {

      $count = $this->sql_count( $sql );

      $page->set_count( $count );
      
      $start = $page->start;

    }

    if ( $sort ) {

      $sql .= $sort->to_sql();

    }

    if ( $page ) {

      $sql .= $page->to_sql();

    }
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_all( $stmt, $table, $start );
    
    return $table;
    
  }

  public function &message_report_total() {

    $sql = "
select
  count( distinct `type` ) as `type`,
  count( distinct context ) as context,
  count( distinct `hash` ) as `hash`,
  concat( min( length( `content` ) ), ' to ', max( length( `content` ) ) ) as `content`,
  count( * ) as seq,
  sum( words ) as words,
  null as translate,
  null as edit,
  null as `index`
from
  v2_intl_message
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }

  public function &messages_for_hash( $hash ) {

    $sql = "
select
  *
from
  v2_intl_message
where
  hash = :hash
";

    $args = array( 'hash' => $hash );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
    
    $this->fetch_all( $stmt, $table );
    
    return $table;
    
  }

  public function translation_exists( $langtag, $type, $context, $hash ) {
    
    $sql = '
select
  count(*) as `count`
from
  v2_intl_translation
where
  langtag = :langtag
and
  type = :type
and
  context = :context
and
  hash = :hash
';
 
    $args = array(
      'langtag' => $langtag,
      'type' => $type,
      'context' => $context,
      'hash' => $hash,
    );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
    
    return 0 !== $this->fetch_int( $stmt, 'count' );
    
  }
  
  public function &message_context( $type, $context, $seq ) {

    $sql = "
select
  *
from
  v2_intl_message
where
  seq between :seq_start and :seq_end
order by
  seq
";
/*
  type = :type
and
  context = :context
and
*/

    $args = array(
      //'type' => $type,
      //'context' => $context,
      'seq_start' => $seq - 2,
      'seq_end' => $seq + 2
    );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
    
    $this->fetch_all( $stmt, $table );
    
    return $table;
    
  }
  
  public function &get_contexts(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "
select
  context,
  count( * ) as `count`,
  group_concat( distinct `type` order by `type` separator ', ' ) as types,
  sum( words ) as words
from
  v2_intl_message
group by
  context
";
    
    $start = 0;

    if ( $page ) {

      $count = $this->sql_count( $sql );

      $page->set_count( $count );
      
      $start = $page->start;

    }

    if ( $sort ) {

      $sql .= $sort->to_sql();

    }

    if ( $page ) {

      $sql .= $page->to_sql();

    }
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_all( $stmt, $table, $start );
    
    return $table;
    
  }
  
  public function &get_contexts_total() {

    $sql = "
select
  count( distinct context ) as context,
  count( * ) as `count`,
  count( distinct `type` ) as types,
  sum( words ) as words
from
  v2_intl_message
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function &get_language_map(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "select * from v2_intl_language";

    $start = 0;
    
    if ( $page ) {

      $count = $this->sql_count( $sql );

      $page->set_count( $count );
      
      $start = $page->start;

    }

    if ( $sort ) {

      $sql .= $sort->to_sql();

    }

    if ( $page ) {

      $sql .= $page->to_sql();

    }
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_map( $stmt, $map, 'langtag', $start );
    
    return $map;
    
  }  
  
  public function &get_languages_total() {
    
    $sql = "
select
  count( distinct langtag ) as langtag,
  count( distinct fallback ) as fallback,
  count( distinct english_name ) as english_name,
  count( distinct local_name ) as local_name,
  count( distinct active ) as active
from
  v2_intl_language
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function &get_language( $langtag ) {

    $sql = "select * from v2_intl_language where langtag = :langtag";
    
    $args = array(
      'langtag' => $langtag
    );

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );

    $this->try_fetch_row( $stmt, $result );
    
    return $result;
    
  }

  public function add_language(
    $langtag,
    $fallback,
    $english_name,
    $local_name
  ) {

    validator()->string( $langtag, 'Langtag', 2, 64 );
    validator()->string( $fallback, 'Fallback', 2, 64 );
    validator()->string( $english_name, 'English Name', 2, 64 );
    validator()->string( $local_name, 'Local Name', 2, 64 );
    
    $sql = "
insert into
  v2_intl_language
values (
  :langtag,
  :fallback,
  :english_name,
  :local_name,
  0
)";
    
    $stmt = $this->prepare( $sql );
    
    $args = array(
      'langtag' => $langtag,
      'fallback' => $fallback,
      'english_name' => $english_name,
      'local_name' => $local_name
    );
    
    $this->execute( $stmt, $sql, $args );

    $this->finish( $stmt );
    
  }
  
  public function save_language(
    $langtag,
    $fallback,
    $english_name,
    $local_name,
    $active
  ) {

    validator()->string( $langtag, 'Langtag', 2, 64 );
    validator()->string( $fallback, 'Fallback', 2, 64 );
    validator()->string( $english_name, 'English Name', 2, 64 );
    validator()->string( $local_name, 'Local Name', 2, 64 );
    validator()->dbool( $active, 'Active' );
    
    $sql = "
update
  v2_intl_language
set
  fallback = :fallback,
  english_name = :english_name,
  local_name = :local_name,
  active = :active
where
  langtag = :langtag
";
    
    $stmt = $this->prepare( $sql );
    
    $args = array(
      'langtag' => $langtag,
      'fallback' => $fallback,
      'english_name' => $english_name,
      'local_name' => $local_name,
      'active' => $active ? 1 : 0
    );
    
    $this->execute( $stmt, $sql, $args );

    $this->finish( $stmt );
    
  }
  
  public function &get_translations(
    &$count = -1,
    $page = null,
    $sort = null
  ) {

    $sql = "
select
  t.langtag as langtag,
  t.type as type,
  t.context as context,
  t.hash as hash,
  t.`text` as `text`,
  m.content as content,
  m.seq as seq,
  m.words as words
from
  v2_intl_translation t
inner join
  v2_intl_message m
on
  t.type = m.type
and
  t.context = m.context
and
  t.hash = m.hash
";

    $start = 0;
    
    if ( $page ) {

      $count = $this->sql_count( $sql );

      $page->set_count( $count );
      
      $start = $page->start;

    }

    if ( $sort ) {

      $sql .= $sort->to_sql();

    }

    if ( $page ) {

      $sql .= $page->to_sql();

    }
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_all( $stmt, $table, $start );
    
    return $table;
    
  }  
  
  public function &get_translations_for_message( $langtag, $hash ) {

    $sql = "select * from v2_intl_translation where langtag = :langtag and hash = :hash";

    $args = array( 'langtag' => $langtag, 'hash' => $hash );
    
    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql, $args );
    
    $this->fetch_all( $stmt, $table );
    
    return $table;
    
  }
  
  public function &get_translations_total() {
    
    $sql = "
select
  count( distinct t.langtag ) as langtag,
  count( distinct t.type ) as type,
  count( distinct t.context ) as context,
  count( distinct t.hash ) as hash,
  concat( min( length( t.`text` ) ), ' to ', max( length( t.`text` ) ) ) as `text`,
  concat( min( length( m.content ) ), ' to ', max( length( m.content ) ) ) as `content`,
  count( distinct m.seq ) as seq,
  sum( m.words ) as words
from
  v2_intl_translation t
inner join
  v2_intl_message m
on
  t.type = m.type
and
  t.context = m.context
and
  t.hash = m.hash
";

    $stmt = $this->prepare( $sql );
    
    $this->execute( $stmt, $sql );
    
    $this->fetch_row( $stmt, $row );
    
    return $row;
    
  }
  
  public function translate( $langtag, $type, $context, $hash ) {
    
    $sql = '
select
  `text`
from
  v2_intl_translation
where
  `langtag` = :langtag
and
  `type` = :type
and
  context = :context
and
  `hash` = :hash
';
    
    $stmt = $this->prepare( $sql );
    
    $args = array(
      'langtag' => $langtag,
      'type' => $type,
      'context' => $context,
      'hash' => $hash,
    );
    
    $this->execute( $stmt, $sql, $args );

    return $this->fetch_field( $stmt, 'text' );
    
  }
}
