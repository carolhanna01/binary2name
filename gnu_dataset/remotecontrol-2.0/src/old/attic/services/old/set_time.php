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
    
    $currentuser = $_SESSION['User'];
    $thermColl = $_SESSION['Thermostats'];
    
    foreach($_POST as $key => $value)
    {
        $thermColl->FindThermostatByID(intval($value))->Selected = true;
    }
    $DateTime = new DateTime();
    $TimeZoneOffset = 0.0;
    $targetTimeStamp = 0;   
    
    $result = '';
    foreach($thermColl as $therm)
    {
        if($therm->Selected)
        {
            $response = '';
            
            $TimeZoneOffset = $therm->TimeZoneOffset->Offset;
            $targetTimeStamp = 0;
            
            if($DateTime !== null && $TimeZoneOffset !== null)
            {
echo ("1");
            $localTimeStamp = strtotime($DateTime->format(DateTime::ISO8601));
                $targetOffsetInSeconds = $TimeZoneOffset * 3600;
                $diffBetweenLocalAndTarget = intval(date('Z', $localTimeStamp)) - $targetOffsetInSeconds;
                $targetTimeStamp = $localTimeStamp + $diffBetweenLocalAndTarget + $targetOffsetInSeconds;
                //$targetDateTimeString = date('n/j/Y H:i T', $targetTimeStamp);
            }
            
            if($targetTimeStamp > 0)
            {
echo "2";
            $targetObject = $therm->ObjectArray->FindObjectByOID('OID2.5.1');
                
                $targetObject->NewValue = $targetTimeStamp;
                
                $therm->ObjectsToUpdate = new IPThermostatObjectCollection(false);
                $therm->ObjectsToUpdate->append($targetObject);
                
                $response = $therm->Execute();
            }
            else
            {
            echo("3");
                $response = 'NT';
            }            

            if($response == 'NT')
            {
            echo("4");
                $result .= 'NoTransaction-'.$therm->ID.';';
            }
            else if(!$response || strpos($response, 'FAILED') !== false || strpos($response, 'OID') === false)
            {
            echo("5");
                $result .= 'TRANSERR:'.$response.'-'.$therm->ID.';';
            }
            else if($response && $response != 'NT' && strpos($response, 'FAILED') === false)
            {
            echo("6");
                $result .= 'Success-'.$therm->ID.';';

                $oids = array();

                parse_str($response, $oids);

                $translog = new TransactionLog();

                foreach($oids as $oid => $value)
                {
                    $obj = $therm->ObjectArray->FindObjectByOID(str_replace('_', '.', $oid));

                    $translog->AddRecord(new TransactionLogRecord($therm, $obj));
                }

                $translog->Save($currentuser->ID);

                foreach($oids as $oid => $value)
                {
                    $obj = $therm->ObjectArray->FindObjectByOID(str_replace('_', '.', $oid));

                    $obj->CurrentValue = $obj->NewValue;
                    $obj->NewValue = '';
                }
            }
        }
    }

    print rtrim($result, ';');

?>
