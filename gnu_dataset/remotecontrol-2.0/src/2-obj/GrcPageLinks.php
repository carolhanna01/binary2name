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

class GrcPageLinks {

  public $page;
  public $current_page;
  public $total_pages;
  public $first_page;
  public $previous_page;
  public $next_page;
  public $last_page;
  public $page_list;
  public $size_decrease;
  public $size_increase;
  public $size_10;
  public $size_20;
  public $size_30;
  public $size_50;
  public $size_100;
  public $size_200;
  public $size_300;
  public $size_500;
  public $size_1000;
  public $size_all;
  public $default_page;

  public function __construct( $data_page, $name = null ) {

    if ( $name === null || $name === "" ) {
      $page_name = "page";
      $size_name = "size";
      $sort_name = 'sort';
    }
    else {
      $page_name = $name . "_page";
      $size_name = $name . "_size";
      $sort_name = $name . '_sort';
    }

    $page = $data_page->page;
    $size = $data_page->size;
    $max_page = $data_page->max_page();

    $page_no = get_link( $page, "Page " . $page, 'active' );
    $page_no->set_page( $name, 1, $size );

    $page_of = get_link( $max_page, "Page " . $max_page );
    $page_of->set_page( $name, $max_page, $size );
    if ( $page === $max_page ) {
      $page_of->add_class( "active" );
    }

    $page_f = get_link( '&#8656;', "First page" );
    $page_f->set_page( $name, 1, $size );

    $prev_page = $page;
    if ( $page > 1 ) {
      $prev_page--;
    }

    $page_p = get_link( '&#8592;', "Previous page" );
    $page_p->set_page( $name, $prev_page, $size );

    $next_page = $page + 1;
    if ( $next_page > $max_page ) {
      $next_page = $max_page;
    }

    $page_n = get_link( '&#8594;', "Next page" );
    $page_n->set_page( $name, $next_page, $size );

    $page_l = get_link( '&#8658;', "Last page" );
    $page_l->set_page( $name, 0, $size );

    $page_link = array();

    for ( $i = $page - 3, $il = $page + 4; $i < $il; $i++ ) {

      if ( $i < 1 ) {
        continue;
      }
      if ( $i > $max_page ) {
        break;
      }

      $link = get_link( $i, "Page " . $i );
      $link->set_page( $name, $i, $size );

      if ( $page === $i ) {
        $link->add_class( "active" );
      }

      $page_link[] = $link;

    }

    $decrease_size = $size - 10;
    if ( $decrease_size <= 0 ) {
      $decrease_size = 10;
    }
    $size_decrease = get_link( "-10", "Show 10 less items per page" );
    $size_decrease->set_page( $name, 1, $decrease_size );

    $size_increase = get_link( "+10", "Show an additional 10 items per page" );
    $size_increase->set_page( $name, 1, $size + 10 );

    $size_10 = get_link( "10", "Show 10 items per page" );
    $size_10->set_page( $name, 1, 10 );
    if ( $size === 10 ) {
      $size_10->add_class( "active" );
    }

    $size_20 = get_link( "20", "Show 20 items per page" );
    $size_20->set_page( $name, 1, 20 );
    if ( $size === 20 ) {
      $size_20->add_class( "active" );
    }

    $size_30 = get_link( "30", "Show 30 items per page" );
    $size_30->set_page( $name, 1, 30 );
    if ( $size === 30 ) {
      $size_30->add_class( "active" );
    }

    $size_50 = get_link( "50", "Show 50 items per page" );
    $size_50->set_page( $name, 1, 50 );
    if ( $size === 50 ) {
      $size_50->add_class( "active" );
    }
    
    $size_100 = get_link( "100", "Show 100 items per page" );
    $size_100->set_page( $name, 1, 100 );
    if ( $size === 100 ) {
      $size_100->add_class( "active" );
    }

    $size_200 = get_link( "200", "Show 200 items per page" );
    $size_200->set_page( $name, 1, 200 );
    if ( $size === 200 ) {
      $size_200->add_class( "active" );
    }

    $size_300 = get_link( "300", "Show 300 items per page" );
    $size_300->set_page( $name, 1, 300 );
    if ( $size === 300 ) {
      $size_300->add_class( "active" );
    }

    $size_500 = get_link( "500", "Show 500 items per page" );
    $size_500->set_page( $name, 1, 500 );
    if ( $size === 500 ) {
      $size_500->add_class( "active" );
    }

    $size_1000 = get_link( "1000", "Show 1000 items per page" );
    $size_1000->set_page( $name, 1, 1000 );
    if ( $size === 1000 ) {
      $size_1000->add_class( "active" );
    }

    $size_all = get_link( "All", "Show all items on a single page" );
    $size_all->set_page( $name, 1, 0 );
    if ( $size === 0 ) {
      $size_all->add_class( "active" );
    }

    $default_link = get_link( 'Default', 'Clear pagination and sort options and use default settings' );
    $default_link->clear( $page_name );
    $default_link->clear( $size_name );
    $default_link->clear( $sort_name );

    $this->page = $data_page;
    $this->current_page = $page_no;
    $this->total_pages = $page_of;
    $this->first_page = $page_f;
    $this->previous_page = $page_p;
    $this->next_page = $page_n;
    $this->last_page = $page_l;
    $this->page_list = $page_link;
    $this->size_decrease = $size_decrease;
    $this->size_increase = $size_increase;
    $this->size_10 = $size_10;
    $this->size_20 = $size_20;
    $this->size_30 = $size_30;
    $this->size_50 = $size_50;
    $this->size_100 = $size_100;
    $this->size_200 = $size_200;
    $this->size_300 = $size_300;
    $this->size_500 = $size_500;
    $this->size_1000 = $size_1000;
    $this->size_all = $size_all;
    $this->default_page = $default_link;

  }

  public static function Create( $data_page, $name = null ) {
    
    $class = get_called_class();
    
    return new $class( $data_page, $name );
    
  }
  
  public function render( $tail ) {

    $tail = $tail->div( 'class', 'nav' );
    
    $this->first_page->render( $tail );
    
    $tail->markup( ' ' );
    
    $this->previous_page->render( $tail );

    $tail->markup( ' ' );
    
    $this->next_page->render( $tail );

    $tail->markup( ' ' );
    
    $this->last_page->render( $tail );
    
    $tail->T_H( ' Total: %total%;', 'total', format_int( $this->page->count ) );
    
    $tail->T_H( ' Page ' );
    
    $this->current_page->render( $tail );
    
    $tail->T_H( ' of ' );

    $this->total_pages->render( $tail );
    
    $tail->T_H( ': ' );

    $is_first = true;

    foreach ( $this->page_list as $link ) {

      if ( $is_first ) {
        
        $is_first = false;
        
      }
      else {
       
        $tail->T_T( ', ', CONTEXT_PUNCTUATION );
       
      }

      $link->render( $tail );

    }

    $tail->T_H( ';' );
    
    $tail->T_H( ' Page size: ' );
    
    $this->size_decrease->render( $tail );
    
    $tail->T_H( ', ' );
    
    $this->size_increase->render( $tail );

    $tail->T_H( ', ' );
    
    $this->size_10->render( $tail );

    $tail->T_H( ', ' );
    
    $this->size_20->render( $tail );
    
    $tail->T_H( ', ' );
    
    $this->size_30->render( $tail );
    
    $tail->T_H( ', ' );
    
    $this->size_50->render( $tail );
    
    $tail->T_H( ', ' );
    
    $this->size_100->render( $tail );
    
    $tail->T_H( ', ' );
    
    /*
    $this->size_200->render( $tail );
    
    $tail->T_H( ', ' );
    
    $this->size_300->render( $tail );
    
    $tail->T_H( ', ' );
    
    $this->size_500->render( $tail );
    
    $tail->T_H( ', ' );
    
    $this->size_1000->render( $tail );
    
    $tail->T_H( ', ' );
    */
    
    $this->size_all->render( $tail );
    
    $tail->T_H( ', ' );
    
    $this->default_page->render( $tail );

    $tail->div_end();

  }
}
