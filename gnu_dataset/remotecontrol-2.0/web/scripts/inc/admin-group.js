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

function edit_group( id ) {
  
  grc_edit( 'group', id, [ 'name', 'description' ] );

}

function cancel_edit_group( id ) {
  
  grc_cancel_edit( 'group', id, [ 'name', 'description' ] );

}

function update_group( id ) {
  
  grc_update(
    'group',
    id,
    [ 'name', 'description' ],
    reload_on_success
  );

}

function delete_group( id ) {
  
  grc_delete_feature( 'group', id );
  
}

function add_group() {
  
  grc_add(
    'group',
    [ 'name', 'description' ],
    reload_on_success
  );
  
}
