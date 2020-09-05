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

	$action = intval($_POST['Action']);

	@ $userid = $_POST['UserId'];
	@ $username = $_POST['UserName'];
	@ $maxnumtherms = $_POST['MaxNumTherms'];
	@ $langCode = $_POST['LangCode'];
        
        $languageObj = null;
	
        if ($action != 3)
        {
            $languageObj = new Language($langCode, true);
        }

	$dataAccess = new MySqlDataAccess(Common::ConnectionString);
	$dataAccess->Command->CommandType = CommandType::StoredProcedure;

	switch($action)
	{
		case 1:

			$dataAccess->Command->CommandText = "CALL sp_User_Insert_Update(null, '$username', $maxnumtherms, '$langCode')";
			
			$userid = intval($dataAccess->Command->ExecuteScalar());

			if($userid)
			{
				printf('%s;%s;%s;%s;%s', $userid, $username, number_format($maxnumtherms), $langCode, $languageObj->Description);
			}
			else
			{
				$error = $dataAccess->Command->Connection->error;

				printf('ERR:The following error occurred while accessing the database: %s', $error);
			}

			break;

		case 2:

			$dataAccess->Command->CommandText = "CALL sp_User_Insert_Update($userid, '$username', $maxnumtherms, '$langCode')";

			if($dataAccess->Command->ExecuteNonQuery())
			{
				printf('%s;%s;%s;%s;%s', $userid, $username, number_format($maxnumtherms), $langCode, $languageObj->Description);
			}
			else
			{
				$error = $dataAccess->Command->Connection->error;

				printf('ERR:The following error occurred while accessing the database: %s', $error);
			}

			break;

		case 3:

			$dataAccess->Command->CommandText = "CALL sp_User_Delete($userid)";

			if($dataAccess->Command->ExecuteNonQuery())
			{
				print $userid;
			}
			elseif ($dataAccess->Command->Connection->errno == 1146) 
			{
			        printf('ERR:It is not possible to remove a user with themorstats associated');
			} 
			else 
			{
			        $error = $dataAccess->Command->Connection->error;

				printf('ERR:The following error occurred while accessing the database: %s', $error);
			}

			break;
	}

	$dataAccess->Cleanup();

?>
