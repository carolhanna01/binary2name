<?php

//        # Copyright (C) 2012-2013 GNU remotecontrol authors.
//        #
//        # This program is free software: you can redistribute it and/or modify
//        # it under the terms of the GNU Affero General Public License as
//        # published by the Free Software Foundation, either version 3 of the
//        # License, or (at your option) any later version.
//        #
//        # This program is distributed in the hope that it will be useful,
//        # but WITHOUT ANY WARRANTY; without even the implied warranty of
//        # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//        # GNU Affero General Public License for more details.
//        #
//        # You should have received a copy of the GNU Affero General Public License
//        # along with this program.  If not, see <http://www.gnu.org/licenses/>.

    require_once(__DIR__.'/../lib/Global.php');
    
    $langCode = $_POST['LangCode'];
    $user = $_SESSION['User'];
    
    $dataAccess = new MySqlDataAccess(Common::ConnectionString);
    $dataAccess->Command->CommandType = CommandType::StoredProcedure;
    $dataAccess->Command->CommandText = 'CALL sp_User_Insert_Update('.$user->ID.', \''.$user->UserName.'\', '.$user->MaxTherms.', \''.$langCode.'\')';
    
    $dataAccess->Command->ExecuteNonQuery();
     
    $dataAccess->Cleanup();
    
    unset($_SESSION['User']);

?>
