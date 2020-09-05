/**
 *
 * @licstart  The following is the entire license notice for the
 *  JavaScript code in this page.
 *
 * Copyright (C) 2012-2013 GNU remotecontrol authors
 *
 *
 * The JavaScript code in this page is free software: you can
 * redistribute it and/or modify it under the terms of the GNU
 * General Public License (GNU GPL) as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option)
 * any later version.  The code is distributed WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU GPL for more details.
 *
 * As additional permission under GNU GPL version 3 section 7, you
 * may distribute non-source (e.g., minimized or compacted) forms of
 * that code without the copy of the GNU GPL normally required by
 * section 4, provided you include this license notice and a URL
 * through which recipients can access the Corresponding Source.
 *
 * @licend  The above is the entire license notice
 * for the JavaScript code in this page.
 *
 */

var prevThermostat = null;
var currViewedThermId = null;

function changeLanguage(langCode)
{
    ajax.doPost('services/change_language.php', 'LangCode=' + langCode, 'loadimage', handleChangeLanguageResponse);
}

function refreshThermostats()
{
	ajax.doPost('services/refresh_thermostats.php', '', 'loadimage', handleRefreshThermostatsResponse);
}

function populateForm(id, viewsettingsclicked)
{
  
	//alert('id = ' + id);
	if(viewsettingsclicked)
	{
		currViewedThermId = null;
	}
	else if (id > -1)
	{
		currViewedThermId = id;
	}

        if (id > -1)
        {
            ajax.doPost('services/get_thermostat_values.php', 'ThermID=' + id, 'loadimage', handlePopulateResponse);
        }
}

function toggleSelectThermostat(checkbox)
{
	var row = document.getElementById('~' + checkbox.id.substr(7) + '~');

	if(checkbox.checked)
	{
		row.className = 'selected';
	}
	else
	{
		reapplyAlternatingRowStyle(document.getElementById('thermostats'));

		reapplySelectedRows();
	}
}

function toggleSelectAllThermostats(checkbox)
{
	var thermostatsTable = document.getElementById('thermostats');
	for(var i = 1; i < thermostatsTable.rows.length-1; i++)
	{
          if(thermostatsTable.rows[i].cells[0].firstChild.type == 'checkbox')
		{
			thermostatsTable.rows[i].cells[0].firstChild.checked = checkbox.checked;

			toggleSelectThermostat(thermostatsTable.rows[i].cells[0].firstChild);
		}
	}

	if(!checkbox.checked)
	{
		reapplyAlternatingRowStyle(thermostatsTable);
	}
}

function reapplyAlternatingRowStyle(table)
{
	for(var i = 1; i < table.rows.length; i++)
	{
		if(table.rows[i].id != 'addrow' && table.rows[i].className != 'alarm' && table.rows[i].className != 'filterreminder')
		{
			if(i % 2 == 0)
			{
				table.rows[i].className = 'alt';
			}
			else
			{
				table.rows[i].className = '';
			}
		}
	}
}

function reapplySelectedRows()
{
	var thermostatsTable = document.getElementById('thermostats');

	for(var i = 1; i < thermostatsTable.rows.length; i++)
	{
		if((thermostatsTable.rows[i].id != 'addrow') && thermostatsTable.rows[i].cells[0].firstChild.checked)
		{
			thermostatsTable.rows[i].className = 'selected';
		}
	}
}

function renumberThermostats()
{
	var thermostatTable = document.getElementById('thermostats');

	for(var i = 1; i < thermostatTable.rows.length; i++)
	{
		if(thermostatTable.rows[i].id != 'addrow')
		{
			thermostatTable.rows[i].cells[2].innerHTML = '<span>' + i.toString() + '.&nbsp;</span>';
		}
	}
}

function setHVACModeDDLBackground(ddl)
{
	switch(ddl.value)
	{
		case '2':

			ddl.className = 'fieldvalue heat';

			break;

		case '3':

			ddl.className = 'fieldvalue cool';

			break;

		default:

			ddl.className = 'fieldvalue';

			break;
	}
}

function setDefaultClassDDLBackground(ddl)
{
	switch(ddl.value)
	{
		case '1':

			ddl.className = 'occupied';

			break;

		case '2':

			ddl.className = 'unoccupied';

			break;

		case '3':

			ddl.className = 'other';

			break;
	}
}

function fillDateTimeDateDDL(dayOfMonth)
{
	var monthDDL = document.getElementById('datetime_month');
	var yearDDL = document.getElementById('datetime_year');

	var postData = '';

	if(dayOfMonth != null)
	{
		postData = 'month=' + monthDDL.value + '&year=' + yearDDL.value + '&day=' + dayOfMonth;
	}
	else
	{
		postData = 'month=' + monthDDL.value + '&year=' + yearDDL.value;
	}

	ajax.doPost('services/calculate_days_of_month.php', postData, 'loadimage', handleCalcDaysOfMonthResponse);
}

function addThermostat()
{
	var name = document.getElementById('Name_0_Input').value;
	var description = document.getElementById('Description_0_Input').value;
	var host = document.getElementById('Host_0_Input').value;
	var port = document.getElementById('Port_0_Input').value;
    var timeZoneOffsetId = document.getElementById('TimeZoneOffset_0_Input').value;
	var groupId = document.getElementById('Group_0_Input').value;

	if(name == '' || host == '' || port == '' || timeZoneOffsetId == '' || groupId == '')
	{
            handleError(getMessage('GRCHM$MSG1'));

            return;
	}

	document.getElementById('Name_0_Input').value = '';
	document.getElementById('Description_0_Input').value = '';
	document.getElementById('Host_0_Input').value = '';
	document.getElementById('Port_0_Input').value = '';
    document.getElementById('TimeZoneOffset_0_Input').value = '';
	document.getElementById('Group_0_Input').value = '';

	var postData = 'Action=1&Name=' + name + '&Description=' + description + '&Host=' + host + '&Port=' + port + '&TimeZoneOffsetID=' + timeZoneOffsetId + '&GroupID=' + groupId;

	ajax.doPost('services/process_thermostat_db.php', postData, 'loadimage', handleAddResponse);
}

function editThermostat(id)
{
	document.getElementById('Name_' + id + '_Span').className = 'edit';
	document.getElementById('Description_' + id + '_Span').className = 'edit';
	document.getElementById('Host_' + id + '_Span').className = 'edit';
	document.getElementById('Port_' + id + '_Span').className = 'edit portinput';
    document.getElementById('TimeZoneOffset_' + id + '_Span').className = 'edit';
	document.getElementById('Group_' + id + '_Span').className = 'edit';

	document.getElementById('Name_' + id + '_Input').className = 'edit';
	document.getElementById('Description_' + id + '_Input').className = 'edit';
	document.getElementById('Host_' + id + '_Input').className = 'edit';
	document.getElementById('Port_' + id + '_Input').className = 'edit portinput';
    document.getElementById('TimeZoneOffset_' + id + '_Input').className = 'edit';
	document.getElementById('Group_' + id + '_Input').className = 'edit';

	document.getElementById('Edit_' + id).style.display = 'none';
	document.getElementById('Update_' + id).style.display = 'inline';
	document.getElementById('Cancel_' + id).style.display = 'inline';
}

function cancelEditThermostat(id)
{
	document.getElementById('Name_' + id + '_Span').className = 'view';
	document.getElementById('Description_' + id + '_Span').className = 'view';
	document.getElementById('Host_' + id + '_Span').className = 'view';
	document.getElementById('Port_' + id + '_Span').className = 'view';
    document.getElementById('TimeZoneOffset_' + id + '_Span').className = 'view';
	document.getElementById('Group_' + id + '_Span').className = 'view';

	document.getElementById('Name_' + id + '_Input').className = 'view';
	document.getElementById('Description_' + id + '_Input').className = 'view';
	document.getElementById('Host_' + id + '_Input').className = 'view';
	document.getElementById('Port_' + id + '_Input').className = 'view';
    document.getElementById('TimeZoneOffset_' + id + '_Input').className = 'view';
	document.getElementById('Group_' + id + '_Input').className = 'view';

	document.getElementById('Edit_' + id).style.display = 'inline';
	document.getElementById('Update_' + id).style.display = 'none';
	document.getElementById('Cancel_' + id).style.display = 'none';
}

function updateThermostat(id)
{
	var name = document.getElementById('Name_' + id + '_Input').value;
	var description = document.getElementById('Description_' + id + '_Input').value;
	var host = document.getElementById('Host_' + id + '_Input').value;
	var port = document.getElementById('Port_' + id + '_Input').value;
    var timeZoneOffsetId = document.getElementById('TimeZoneOffset_' + id + '_Input').value;
	var groupId = document.getElementById('Group_' + id + '_Input').value;

	if(name == '' || host == '' || port == '' || timeZoneOffsetId == '' || groupId == '')
	{
    alert( 'Error' );
    
    return;
	}

	document.getElementById('Name_' + id + '_Span').className = 'view';
	document.getElementById('Name_' + id + '_Input').className = 'view';

	document.getElementById('Description_' + id + '_Span').className = 'view';
	document.getElementById('Description_' + id + '_Input').className = 'view';

	document.getElementById('Host_' + id + '_Span').className = 'view';
	document.getElementById('Host_' + id + '_Input').className = 'view';

	document.getElementById('Port_' + id + '_Span').className = 'view';
	document.getElementById('Port_' + id + '_Input').className = 'view';

    document.getElementById('TimeZoneOffset_' + id + '_Span').className = 'view';
	document.getElementById('TimeZoneOffset_' + id + '_Input').className = 'view';

	document.getElementById('Group_' + id + '_Span').className = 'view';
	document.getElementById('Group_' + id + '_Input').className = 'view';

	document.getElementById('Edit_' + id).style.display = 'inline';
	document.getElementById('Update_' + id).style.display = 'none';
	document.getElementById('Cancel_' + id).style.display = 'none';

	var postData = 'Action=2&ThermostatId=' + id + '&Name=' + name + '&Description=' + description + '&Host=' + host + '&Port=' + port + '&TimeZoneOffsetID=' + timeZoneOffsetId + '&GroupID=' + groupId;

	ajax.doPost('services/process_thermostat_db.php', postData, 'loadimage', handleUpdateResponse);
}

function deleteThermostat(id)
{
	if(!confirm(getMessage('GRCHM$MSG6')))
	{
            return false;
	}

	var postData = 'Action=3&ThermostatId=' + id;

	ajax.doPost('services/process_thermostat_db.php', postData, 'loadimage', handleDeleteResponse);
}
/*
function getOffset( el ) {
    var _x = 0;
    var _y = 0;
    while( el && !isNaN( el.offsetLeft ) && !isNaN( el.offsetTop ) ) {
        _x += el.offsetLeft - el.scrollLeft;
        _y += el.offsetTop - el.scrollTop;
        el = el.offsetParent;
    }
    //return { top: _y, left: _x };
    return [ _x, _y ];
}
*/

function toggleAuthInfo(show, id, sourceLink)
{
	var authInfoDiv = document.getElementById('authinfo');
	var usernameLabel = document.getElementById('UserNameLabel');
	var usernameInput = document.getElementById('UserName_ThermID_Input') || document.getElementById('UserName_' + id + '_Input');
	var passwordLabel = document.getElementById('PasswordLabel');
	var passwordInput = document.getElementById('Password_ThermID_Input') || document.getElementById('Password_' + id + '_Input');
	var saveAuthInfoButton = document.getElementById('AuthInfoSaveButton');
	var cancelAuthInfoButton = document.getElementById('AuthInfoCancelButton');

	if(show)
	{

    /* this jquery code can't be used...
    var elem = $(sourceLink).parent();
    var auth = $('#authinfo');
    var pos = elem.position();
    var sourceLinkPos = [
      pos.left - auth.outerWidth() - 1,
      pos.top
    ];
    */
   
   var rect = sourceLink.parentNode.getBoundingClientRect();
   //for ( var i in rect ) { alert( i + ': ' + rect[ i ] ); }
   var sourceLinkPos = [
     rect.x - 262,
     rect.y - 230
   ];
   
		usernameInput.id = usernameInput.id.replace('ThermID', id);
		passwordInput.id = passwordInput.id.replace('ThermID', id);
		saveAuthInfoButton.setAttribute('onclick', saveAuthInfoButton.getAttribute('onclick').replace('ThermID', id));
		cancelAuthInfoButton.setAttribute('onclick', cancelAuthInfoButton.getAttribute('onclick').replace('ThermID', id));

		authInfoDiv.style.left = sourceLinkPos[0].toString() + 'px';
		authInfoDiv.style.top = sourceLinkPos[1].toString() + 'px';

		authInfoDiv.style.display = 'block';
	}
	else
	{
		usernameInput.id = usernameInput.id.replace(id, 'ThermID');
		passwordInput.id = passwordInput.id.replace(id, 'ThermID');
		saveAuthInfoButton.setAttribute('onclick', saveAuthInfoButton.getAttribute('onclick').replace(id, 'ThermID'));
		cancelAuthInfoButton.setAttribute('onclick', cancelAuthInfoButton.getAttribute('onclick').replace(id, 'ThermID'));

		authInfoDiv.style.display = 'none';
	}

	usernameLabel.setAttribute('for', usernameInput.id);
	passwordLabel.setAttribute('for', passwordInput.id);
}

function getAuthInfo(id)
{
	var postData = 'Action=2&ThermostatId=' + id;

	ajax.doPost('services/process_thermostat_authinfo.php', postData, 'loadimage', handleAuthInfoGetResponse);
}

function updateAuthInfo(id)
{
	var username = document.getElementById('UserName_' + id + '_Input').value;
	var password = document.getElementById('Password_' + id + '_Input').value;

	if(password == '')
	{
		handleError(getMessage('GRCHM$MSG2'));

		return;
	}

	var postData = 'Action=1&ThermostatId=' + id + '&Username=' + username + '&Password=' + password;

	ajax.doPost('services/process_thermostat_authinfo.php', postData, 'loadimage', handleAuthInfoUpdateResponse);
}

function buildQueryString()
{
	var form = document.getElementById("tstat");
	var params = '';

	var hour = 0;
	var minute = 0;
	var timesuffix = '';

	for(var i = 0; i < form.elements.length; i++)
	{
		if(form.elements[i].name.indexOf('timesuffix') == -1 && form.elements[i].name != 'OID2.5.1' && form.elements[i].name != '')
		{
			if(i == 0)
			{
				params += form.elements[i].name + '=' + form.elements[i].value;
			}
			else
			{
				params += '&' + form.elements[i].name + '=' + form.elements[i].value;
			}
		}
		else if(form.elements[i].name == 'OID2.5.1')
		{
//			if(document.getElementById('setdatetime').checked)
//			{
				//var targetDate = new Date();
				//				var year = parseInt(document.getElementById('datetime_year').value);
				//				var month = parseInt(document.getElementById('datetime_month').value);
				//				var date = parseInt(document.getElementById('datetime_date').value);
				//				var hr = parseInt(document.getElementById('datetime_hour').value);
				//				var mins = parseInt(document.getElementById('datetime_minutes').value);
//				var year = document.getElementById('datetime_year').value;
//				var month = document.getElementById('datetime_month').value;
//				var date = document.getElementById('datetime_date').value;
//				var hr = document.getElementById('datetime_hour').value;
//				var mins = document.getElementById('datetime_minutes').value;

//				if(document.getElementById('datetime_PM').checked)
//				{
//					if(parseInt(hr) != 12)
//					{
//						var pmhour = parseInt(hr) + 12;
//						hr = pmhour.toString();
//					}
//				}
//				else if(document.getElementById('datetime_AM').checked)
//				{
//					if(parseInt(hr) == 12)
//					{
//						hr = '00';
//					}
//				}

//				var dateString = month + '/' + date + '/' + year + ' ' + hr + ':' + mins;
//
//				var timeZoneOffset = parseFloat(document.getElementById('datetime_timezone').value);

//				if(document.getElementById('setusdst').checked)
//				{
//					timeZoneOffset++;
//				}

//				if(i == 0)
//				{
//					params += 'DateTime=' + dateString + '&TimeZoneOffset=' + timeZoneOffset.toString();
//				}
//				else
//				{
//					params += '&DateTime=' + dateString + '&TimeZoneOffset=' + timeZoneOffset.toString();
//				}
//			}
		}
		else
		{
			if(form.elements[i].id.indexOf('setbackPeriodHour') != -1)
			{
				hour = parseInt(form.elements[i].value);
			}

			if(form.elements[i].id.indexOf('setbackPeriodMinute') != -1)
			{
				minute = parseInt(form.elements[i].value);
			}

			if(form.elements[i].name.indexOf('timesuffix') != -1 && form.elements[i].checked)
			{
				timesuffix = form.elements[i].value;

				if(i == 0)
				{
					params += form.elements[i].offsetParent.id + '=' + calculateSetbackPeriodValue(hour, minute, timesuffix);
				}
				else
				{
					params += '&' + form.elements[i].offsetParent.id + '=' + calculateSetbackPeriodValue(hour, minute, timesuffix);
				}
			}
		}
	}

	params += getSelectedThermostats();

	//alert('params = ' + params);

	return params;
}

function calculateSetbackPeriodValue(hour, minute, timesuffix)
{
	switch(timesuffix)
	{
		case 'AM':

			if(hour == 12)
			{
				return minute;
			}
			else
			{
				return (hour * 60 + minute);
			}

			break;

		case 'PM':

			if(hour == 12)
			{
				return (hour * 60 + minute);
			}
			else
			{
				return ((hour + 12) * 60 + minute);
			}

			break;

		default:

			return 0;

			break;
	}
}

function getSelectedThermostats()
{
	var thermostattable = document.getElementById('thermostats');
	var thermparams = '';

	for(var i = 1; i < (thermostattable.rows.length); i++)
	{
		if((thermostattable.rows[i].id != 'addrow') && (thermostattable.rows[i].className.indexOf('selected') != -1))
		{
			thermparams += '&T' + i + '=' + thermostattable.rows[i].id.replace(/~/g, '');
		}
	}

	return thermparams;
}

function getFormField(name)
{
	var form = document.getElementById('tstat');

	for(var i = 0; i < form.elements.length; i++)
	{
    //alert( form.elements[ i ].name + '=' + name );
		if(form.elements[i].name == name)
		{
			return form.elements[i];
		}
	}
  
  //alert( "Couldn't find '" + name + "'." );
  alert( "Not found: '" + name + "'." );
  

	return null;
}

function getTZODDLItems(selectedOffsetValue)
{
    ajax.async = false;
    ajax.doPost('services/construct_tzo_ddl_items.php', 'SelectedOffsetValue=' + selectedOffsetValue, 'loadimage', handleGetTZODDLItemsResponse);
    ajax.async = true;

    return document.getElementById('tzoDDLItems').value;
}

function getGroupDDLItems(selectedGroupValue)
{
    ajax.async = false;
    ajax.doPost('services/construct_group_ddl_items.php', 'SelectedGroupValue=' + selectedGroupValue, 'loadimage', handleGetGroupDDLItemsResponse);
    ajax.async = true;

    return document.getElementById('groupDDLItems').value;
}

function setTime()
{
    if(getSelectedThermostats() == '')
    {
        handleError(getMessage('GRCHM$MSG3'));
    }
    else
    {
        ajax.doPost('services/set_time.php', getSelectedThermostats(), 'loadimage', handleSetTimeResponse);
    }
}

function submitForm()
{
        //buildQueryString();
	if(getSelectedThermostats() == '')
	{
		handleError(getMessage('GRCHM$MSG3'));
	}
	else
	{
		ajax.doPost('services/send_request.php', buildQueryString(), 'loadimage', handleSubmitResponse);
	}

}

function validateThermsChecked()
{

}

function handleChangeLanguageResponse(resp)
{
    if(resp.substr(0,4) == 'ERR:')
    {
        handleError(resp);
    }
    else
    {
        window.location.reload();
    }
}

function handleRefreshThermostatsResponse(resp)
{

        if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var therms = resp.split('^');
		for(var i = 0; i < therms.length; i++)
		{

			var therminfo = therms[i].split('$');
			var targetRow = document.getElementById('~' + therminfo[0] + '~');

			//alert('therminfo = ' + therminfo);

			targetRow.cells[3].innerHTML = therminfo[1];
			targetRow.cells[4].innerHTML = therminfo[2];
			targetRow.cells[5].innerHTML = therminfo[3];

//			if(therminfo[5] == 3 || therminfo[6] == 3 || therminfo[7] == 3 || therminfo[8] == 2)
//			{
//				var title = '';
//
//				if(therminfo[5] == 3)
//				{
//					title += 'Low Temperature Alert! ';
//				}
//
//				if(therminfo[6] == 3)
//				{
//					title += 'High Temperature Alert! ';
//				}
//
//				if(therminfo[7] == 3)
//				{
//					title += 'High Humidity Alert! ';
//				}
//
//				if(therminfo[8] == 2)
//				{
//					title += 'Filter Change Required';
//				}
//
//				if(therminfo[5] == 3 || therminfo[6] == 3 || therminfo[7] == 3)
//				{
//					targetRow.className = 'alarm';
//				}
//				else
//				{
//					targetRow.className = 'filterreminder';
//				}
//
//				targetRow.title = title;
//			}
		}
	}

	populateForm(currViewedThermId, false);
}

function handlePopulateResponse(resp)
{
  //alert( 'Populate Response: ' + resp );
  
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var params = resp.split(';');

		//alert('resp = ' + resp);

		for(var i = 1; i < params.length; i++)
		{
			var obj = params[i].split('=');

			if(obj[0].substr(0, 10) == 'OID4.4.1.3')
			{
				var periodStartValue = obj[1];
				var oidlastthree = obj[0].substr(obj[0].length - 3);
        alert( 'last 3: ' + oidlastthree );
        alert( 'obj[0]: ' + obj[ 0 ] );
				var periodStartHour = document.getElementById('setbackPeriodHour' + oidlastthree);
				var periodStartMinute = document.getElementById('setbackPeriodMinute' + oidlastthree);
				var periodStartAM = document.getElementById('setbackPeriodTimeSuffixAM' + oidlastthree);
				var periodStartPM = document.getElementById('setbackPeriodTimeSuffixPM' + oidlastthree);

				var hours = Math.floor(parseInt(periodStartValue) / 60);
				var minutes = parseInt(periodStartValue) % 60;

				if(hours > 12)
				{
					periodStartHour.value = hours - 12;
				}
				else
				{
					periodStartHour.value = hours;
				}

				if(hours >= 12)
				{
					periodStartAM.checked = false;
					periodStartPM.checked = true;
				}
				else
				{
					periodStartAM.checked = true;
					periodStartPM.checked = false;
				}

				if(minutes < 10)
				{
					periodStartMinute.value = '0' + minutes;
				}
				else
				{
					periodStartMinute.value = minutes;
				}
			}
			else if(obj[0] == 'OID2.5.1')
			{
				//alert('OID2.5.1 = ' + (obj[1] * 1000));

//				var currLocalDate = new Date();
//				var currThermDate = new Date((parseInt(obj[1]) + (currLocalDate.getTimezoneOffset() * 60)) * 1000);
//
//				var month = currThermDate.getMonth() + 1;
//				var year = currThermDate.getFullYear();
//
//				document.getElementById('datetime_month').value = month.toString();
//				document.getElementById('datetime_year').value = year.toString();
//
//				fillDateTimeDateDDL(currThermDate.getDate().toString());
//
//				var hour = currThermDate.getHours();
//				var mins = currThermDate.getMinutes();

//				document.getElementById('datetime_AM').checked = false;
//				document.getElementById('datetime_PM').checked = false;

//				if(hour > 12)
//				{
//					hour -= 12;
//
//					if(hour == 12)
//					{
//						document.getElementById('datetime_AM').checked = true;
//					}
//					else
//					{
//						document.getElementById('datetime_PM').checked = true;
//					}
//				}
//				else if(hour == 12)
//				{
//					document.getElementById('datetime_PM').checked = true;
//				}
//				else
//				{
//					document.getElementById('datetime_AM').checked = true;
//				}

//				if(hour < 10)
//				{
//					hour = '0' + hour.toString();
//				}
//
//				if(mins < 10)
//				{
//					mins = '0' + mins.toString();
//				}
//
//				document.getElementById('datetime_hour').value = hour;
//				document.getElementById('datetime_minutes').value = mins;
			}
			else
			{
				//				if(obj[0].substr(0, 3) != 'OID')
				//				{
				//					alert('obj[0] = ' + obj[0]);
				//				}

				getFormField(obj[0]).value = obj[1];

				if(obj[0].substr(3, 5) == '4.1.1')
				{
					switch(obj[1])
					{
						case '2':

							getFormField(obj[0]).className = 'fieldvalue heat';

							break;

						case '3':

							getFormField(obj[0]).className = 'fieldvalue cool';

							break;

						default:

							getFormField(obj[0]).className = 'fieldvalue';

							break;
					}
				}

				if(obj[0].substr(3, 7) == '4.4.3.2')
				{
					switch(obj[1])
					{
						case '1':

							getFormField(obj[0]).className = 'occupied';

							break;

						case '2':

							getFormField(obj[0]).className = 'unoccupied';

							break;

						case '3':

							getFormField(obj[0]).className = 'other';

							break;
					}
				}
			}
		}

		setAveraging('L', document.getElementById('localsensorstate').value);
		setAveraging('R1', document.getElementById('remotesensor1state').value);
		setAveraging('R2', document.getElementById('remotesensor2state').value);

		//		alert('currViewedThermId = ' + currViewedThermId);
		//		alert('prevThermostat = ' + prevThermostat);

		if(currViewedThermId)
		{
			document.getElementById('View_' + currViewedThermId).style.display = 'none';
			document.getElementById('CurrView_' + currViewedThermId).style.display = 'inline';

			prevThermostat = currViewedThermId;
		}
		else
		{
			document.getElementById('View_' + params[0]).style.display = 'none';
			document.getElementById('CurrView_' + params[0]).style.display = 'inline';

			if(prevThermostat)
			{
				document.getElementById('View_' + prevThermostat).style.display = 'inline';
				document.getElementById('CurrView_' + prevThermostat).style.display = 'none';
			}

			prevThermostat = params[0];
		}
	}
}

function handleSetTimeResponse(resp)
{
    if(resp.substr(0,4) == 'ERR:')
    {
        handleError(resp);
    }
    else
    {
        handleSubmitResponse(resp);
    }
}

function handleSubmitResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var thermResponses = resp.split(';');

		for(var i = 0; i < thermResponses.length; i++)
		{
			var messageAndID = thermResponses[i].split('-');

			document.getElementById('selectalltherms').checked = false;

			toggleSelectAllThermostats(document.getElementById('selectalltherms'));

			var notransInd = document.getElementById('NoTrans_' + messageAndID[1]);
			var failureInd = document.getElementById('Failure_' + messageAndID[1]);
			var successInd = document.getElementById('Success_' + messageAndID[1]);

			notransInd.style.display = 'none';
			failureInd.style.display = 'none';
			successInd.style.display = 'none';

			if(messageAndID[0] == 'NoTransaction')
			{
				notransInd.title = getMessage('GRCHM$TTP2');
				notransInd.style.display = 'inline';
			}
			else if(messageAndID[0].substr(0, 9) == 'TRANSERR:')
			{
				failureInd.title =  getMessage('GRCHM$TTP3').replace('{0}', messageAndID[0]);
				failureInd.style.display = 'inline';
			}
			else
			{
				successInd.title = getMessage('GRCHM$TTP4');
				successInd.style.display = 'inline';
			}
		}

		refreshThermostats();
	}
}

function handleAuthInfoGetResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var authinfo = resp.split(';');

		document.getElementById('UserName_ThermID_Input').value = authinfo[1];
		document.getElementById('Password_ThermID_Input').value = authinfo[2];

		toggleAuthInfo(true, authinfo[0], document.getElementById('ChangeAuthInfo_' + authinfo[0]));
	}
}

function handleAuthInfoUpdateResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		//window.location.reload();
		var setAuthInfoLink = document.getElementById('SetAuthInfo_' + resp);

		if(setAuthInfoLink)
		{
			setAuthInfoLink.id = 'ChangeAuthInfo_' + resp;
			setAuthInfoLink.className = 'change';
			setAuthInfoLink.setAttribute('onclick', 'getAuthInfo(' + resp + ', this);');
			setAuthInfoLink.innerHTML = 'Change';
		}

		toggleAuthInfo(false, resp);

		refreshThermostats();
	}
}

function handleCalcDaysOfMonthResponse(resp)
{
	var numDays = 0;
	var dateToSelect = '';

	if(resp.indexOf(':') != -1)
	{
		var values = resp.split(':');

		numDays = parseInt(values[0]);
		dateToSelect = values[1];
	}
	else
	{
		numDays = parseInt(resp);
	}

	var dateDDL = document.getElementById('datetime_date');

	dateDDL.options.length = 0;

	for(var i = 1; i <= numDays; i++)
	{
		var option = new Option();
		option.value = i.toString();
		option.text = i.toString();

		dateDDL.options[i-1] = option;
	}

	if(dateToSelect != '')
	{
		dateDDL.value = dateToSelect;
	}
}

function handleAddResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var params = resp.split(';');

		var thermostatId = params[0];
		var name = params[1];
		var description = params[2];
		var host = params[3];
		var port = params[4];
        var timeZoneOffsetId = params[5];
        var timeZoneOffsetDesc = params[6];
		var groupId = params[7];
		var groupName = params[8];
		var maxtherms = params[9];

        var timeZoneOffsetDDLItems = getTZODDLItems(timeZoneOffsetId);

		var groupDDLItems = getGroupDDLItems(groupId);

		var thermostatsTable = document.getElementById('thermostats');
		var targetRowIndex = thermostatsTable.rows.length - 1;

		thermostatsTable.insertRow(targetRowIndex);

		var row = thermostatsTable.rows[targetRowIndex];
		row.id = '~' + thermostatId + '~';

		if(thermostatsTable.rows[row.rowIndex - 1].className == '')
		{
			row.className = 'alt';
		}

		row.insertCell(0);
		row.insertCell(1);
		row.insertCell(2);
		row.insertCell(3);
		row.insertCell(4);
		row.insertCell(5);
		row.insertCell(6);
		row.insertCell(7);
		row.insertCell(8);
		row.insertCell(9);
		row.insertCell(10);
		row.insertCell(11);
		row.insertCell(12);
		row.insertCell(13);
		row.insertCell(14);

		var cell1 = row.cells[0];
		var cell2 = row.cells[1];
		var cell3 = row.cells[2];
		var cell4 = row.cells[3];
		var cell5 = row.cells[4];
		var cell6 = row.cells[5];
		var cell7 = row.cells[6];
		var cell8 = row.cells[7];
		var cell9 = row.cells[8];
		var cell10 = row.cells[9];
		var cell11 = row.cells[10];
		var cell12 = row.cells[11];
		var cell13 = row.cells[12];
		var cell14 = row.cells[13];
		var cell15 = row.cells[14];

		cell1.innerHTML = '<input type="checkbox" id="Select_' + thermostatId + '" onclick="toggleSelectThermostat(this);" />';

		cell2.innerHTML = '<a href="javascript:void(0);" id="Edit_' + thermostatId + '" onclick="editThermostat(' + thermostatId + ');" class="edit">Edit</a>';
		cell2.innerHTML += '<a href="javascript:void(0);" id="Update_' + thermostatId + '" onclick="updateThermostat(' + thermostatId + ');" class="update">Update</a>';
		cell2.innerHTML += '&nbsp;<a href="javascript:void(0);" id="Cancel_' + thermostatId + '" onclick="cancelEditThermostat(' + thermostatId + ');" class="cancel">Cancel</a>';

		cell3.innerHTML = '<span>' + targetRowIndex + '.&nbsp;</span>';

		cell7.innerHTML = '<span id="Group_' + thermostatId + '_Span" class="view">' + groupName + '</span>';

		cell7.innerHTML += '<select id="Group_' + thermostatId + '_Input" class="view">' + groupDDLItems + '</select>';

		cell8.innerHTML = '<span id="Name_' + thermostatId + '_Span" class="view">' + name + '</span><input type="text" id="Name_' + thermostatId + '_Input" value="' + name + '" class="view" />';

		cell9.innerHTML = '<span id="Description_' + thermostatId + '_Span" class="view">' + description + '</span><input type="text" id="Description_' + thermostatId + '_Input" value="' + description + '" class="view" />';

		cell10.innerHTML = '<span id="Host_' + thermostatId + '_Span" class="view">' + host + '</span><input type="text" id="Host_' + thermostatId + '_Input" value="' + host + '" class="view" />';

		cell11.innerHTML = '<span id="Port_' + thermostatId + '_Span" class="view">' + port + '</span><input type="text" id="Port_' + thermostatId + '_Input" value="' + port + '" class="view" />';

        cell12.innerHTML = '<span id="TimeZoneOffset_' + thermostatId + '_Span" class="view">' + timeZoneOffsetDesc + '</span>';

        cell12.innerHTML += '<select id="TimeZoneOffset_' + thermostatId + '_Input" class="view">' + timeZoneOffsetDDLItems + '</select>';

		cell13.innerHTML = '<a href="javascript:void(0);" id="SetAuthInfo_' + thermostatId + '" onclick="toggleAuthInfo(true, ' + thermostatId + ', this);" class="set">Set</a>';

		cell14.innerHTML = '<a href="javascript:void(0);" id="View_' + thermostatId + '" onclick="populateForm(' + thermostatId + ');" class="viewsettings">View&nbsp;Settings</a><span id="CurrView_' + thermostatId + '" class="currentlyviewing">Currently&nbsp;Viewing</span>';

		cell15.innerHTML = '<a href="javascript:void(0);" id="Delete_' + thermostatId + '" onclick="deleteThermostat(' + thermostatId + ');" class="delete">Delete</a>';

		if((thermostatsTable.rows.length - 2) == maxtherms)
		{
			thermostatsTable.deleteRow(thermostatsTable.rows.length - 1);
		}
	}
}

function handleUpdateResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var params = resp.split(';');

		var thermostatId = params[0];
		var name = params[1];
		var description = params[2];
		var host = params[3];
		var port = params[4];
        var timeZoneOffsetId = params[5];
        var timeZoneOffsetDesc = params[6];
		var groupId = params[7];
		var groupName = params[8];

		document.getElementById('Name_' + thermostatId + '_Span').innerHTML = name;
		document.getElementById('Name_' + thermostatId + '_Input').value = name;

		document.getElementById('Description_' + thermostatId + '_Span').innerHTML = description;
		document.getElementById('Description_' + thermostatId + '_Input').value = description;

		document.getElementById('Host_' + thermostatId + '_Span').innerHTML = host;
		document.getElementById('Host_' + thermostatId + '_Input').value = host;

		document.getElementById('Port_' + thermostatId + '_Span').innerHTML = port;
		document.getElementById('Port_' + thermostatId + '_Input').value = port;

        document.getElementById('TimeZoneOffset_' + thermostatId + '_Span').innerHTML = timeZoneOffsetDesc;
		document.getElementById('TimeZoneOffset_' + thermostatId + '_Input').value = timeZoneOffsetId;

		document.getElementById('Group_' + thermostatId + '_Span').innerHTML = groupName;
		document.getElementById('Group_' + thermostatId + '_Input').value = groupId;
	}
}

function handleDeleteResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var thermostatsTable = document.getElementById('thermostats');

		thermostatsTable.deleteRow(document.getElementById('~' + resp + '~').rowIndex);

		var lastRow = thermostatsTable.rows[thermostatsTable.rows.length - 1];

        var timeZoneOffsetDDLItems = getTZODDLItems();

		var groupDDLItems = getGroupDDLItems();

		if(lastRow.id != 'addrow')
		{
			thermostatsTable.insertRow(thermostatsTable.rows.length);

			var addrow = thermostatsTable.rows[thermostatsTable.rows.length - 1];
			addrow.insertCell(0);
			addrow.insertCell(1);
			addrow.insertCell(2);
			addrow.insertCell(3);
			addrow.insertCell(4);
			addrow.insertCell(5);
            addrow.insertCell(6);
			addrow.insertCell(7);

			var cell1 = addrow.cells[0];
			var cell2 = addrow.cells[1];
			var cell3 = addrow.cells[2];
			var cell4 = addrow.cells[3];
			var cell5 = addrow.cells[4];
			var cell6 = addrow.cells[5];
            var cell7 = addrow.cells[6];
			var cell8 = addrow.cells[7];

			cell1.id = 'addlinkcell';
			cell1.colSpan = '5';
			cell1.innerHTML = '<a href="javascript:void(0);" onclick="addThermostat();" class="add">Add</a>';

			cell2.innerHTML = '<select id="Group_0_Input">' + groupDDLItems + '</select>';

			cell3.innerHTML = '<input type="text" id="Name_0_Input" />';

			cell4.innerHTML = '<input type="text" id="Description_0_Input" />';

			cell5.innerHTML = '<input type="text" id="Host_0_Input" />';

			cell6.innerHTML = '<input type="text" id="Port_0_Input" class="portinput" />';

            cell7.innerHTML = '<select id="TimeZoneOffset_0_Input">' + timeZoneOffsetDDLItems + '</select>';

			cell8.colSpan = '2';
			cell8.innerHTML = '&nbsp;'
		}

		reapplyAlternatingRowStyle(thermostatsTable);

		renumberThermostats();
	}
}

function handleGetTZODDLItemsResponse(resp)
{
    if(resp.substr(0,4) == 'ERR:')
    {
        handleError(resp);
    }
    else
    {
        document.getElementById('tzoDDLItems').value = resp;
    }
}

function handleGetGroupDDLItemsResponse(resp)
{
    if(resp.substr(0,4) == 'ERR:')
    {
        handleError(resp);
    }
    else
    {
        document.getElementById('groupDDLItems').value = resp;
    }
}

function clearError()
{
	var messageLabel = document.getElementById('message');
	messageLabel.className = '';
	messageLabel.innerHTML = '';
}

function setAveraging(sensor, state)
{
	var sensorAveragingDDL = null;

	switch(sensor)
	{
		case 'L':

			sensorAveragingDDL = document.getElementById('localsensoraverage');

			break;

		case 'R1':

			sensorAveragingDDL = document.getElementById('remotesensor1average');

			break;

		case 'R2':

			sensorAveragingDDL = document.getElementById('remotesensor2average');

			break;
	}

	if(state == '1')
	{
		sensorAveragingDDL.value = '1';
		sensorAveragingDDL.disabled = 1;
	}
	else
	{
		sensorAveragingDDL.disabled = 0;
	}
}

function validateForm()
{
	if(!validateSetbackTemps())
	{
		return false;
	}

	if(!validatePeriodStartTimes())
	{
		return false;
	}

	clearError();

	return true;
}

function validateSetbackTemps()
{
	if(document.getElementById('hvacmode').value == '4')
	{
		var setbackheat = parseInt(document.getElementById('setbackheat').value) / 10;
		var setbackcool = parseInt(document.getElementById('setbackcool').value) / 10;

		if(Math.abs(setbackheat - setbackcool) <= 2)
		{
			handleError(getMessage('GRCHM$MSG4'));

			return false;
		}
	}

	return true;
}

function validatePeriodStartTimes()
{
	for(var i = 1; i <= 3; i++)
	{
		var prevminutes = 0;

		for(var j = 1; j <= 4; j++)
		{
			var setbackPeriodHour = document.getElementById('setbackPeriodHour' + i + '.' + j);
			var setbackPeriodMinute = document.getElementById('setbackPeriodMinute' + i + '.' + j);
			var setbackPeriodTimeSuffixAM = document.getElementById('setbackPeriodTimeSuffixAM' + i + '.' + j);
			var setbackPeriodTimeSuffixPM = document.getElementById('setbackPeriodTimeSuffixPM' + i + '.' + j);

			var hour = parseInt(setbackPeriodHour.value);
			var minute = parseInt(setbackPeriodMinute.value);
			var timesuffix = '';

			if(setbackPeriodTimeSuffixAM.checked)
			{
				timesuffix = 'AM';
			}

			if(setbackPeriodTimeSuffixPM.checked)
			{
				timesuffix = 'PM';
			}

			var temptotalminutes = calculateSetbackPeriodValue(hour, minute, timesuffix);

			if(j > 1)
			{
				if(temptotalminutes <= prevminutes)
				{
					handleError(getMessage('GRCHM$MSG5'));

					return false;
				}
			}

			prevminutes = temptotalminutes;
		}
	}

	return true;
}
