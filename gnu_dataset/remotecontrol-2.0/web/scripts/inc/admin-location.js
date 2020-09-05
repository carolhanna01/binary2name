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

function edit_location( id ) {
  
  grc_edit( 'location', id, [ 'name', 'timezone' ] );

}

function cancel_edit_location( id ) {
  
  grc_cancel_edit( 'location', id, [ 'name', 'timezone' ] );

}

function update_location( id ) {
  
  grc_update(
    'location',
    id,
    [ 'name', 'timezone' ],
    reload_on_success
  );

}

function delete_location( id ) {
  
  grc_delete_feature( 'location', id );
  
}

function add_location() {
  
  grc_add(
    'location',
    [ 'name', 'timezone' ],
    reload_on_success
  );
  
}
