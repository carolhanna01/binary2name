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

function addUser()
{
	var userName = document.getElementById('UserName_0_Input').value;
	var maxNumTherms = document.getElementById('MaxNumTherms_0_Input').value;
	var langCode = document.getElementById('LangCode_0_Input').value;

	if(userName == '' || maxNumTherms == '' || langCode == '')
	{
            handleError(getMessage('GRCAD$HDR1$MSG1'));

            return;
	}

	var postData = 'Action=1&UserName=' + userName + '&MaxNumTherms=' + maxNumTherms + '&LangCode=' + langCode;

	ajax.doPost('services/process_user_db.php', postData, 'loadimage', handleAddUserResponse);

	document.getElementById('UserName_0_Input').value = '';
	document.getElementById('MaxNumTherms_0_Input').value = '';
    document.getElementById('LangCode_0_Input').value = '';
}

function editUser(id)
{
	document.getElementById('UserName_' + id + '_Span').className = 'edit';
	document.getElementById('MaxNumTherms_' + id + '_Span').className = 'edit';
	document.getElementById('LangCode_' + id + '_Span').className = 'edit';

	document.getElementById('UserName_' + id + '_Input').className = 'edit';
	document.getElementById('MaxNumTherms_' + id + '_Input').className = 'edit number';
    document.getElementById('LangCode_' + id + '_Input').className = 'edit';

	document.getElementById('Users_Edit_' + id).style.display = 'none';
	document.getElementById('Users_Update_' + id).style.display = 'inline';
	document.getElementById('Users_Cancel_' + id).style.display = 'inline';

}

function updateUser(id)
{
	var userName = document.getElementById('UserName_' + id + '_Input').value;
	var maxNumTherms = document.getElementById('MaxNumTherms_' + id + '_Input').value;
	var langCode = document.getElementById('LangCode_' + id + '_Input').value;

	if(userName == '' || maxNumTherms == '' || langCode == '')
	{
            handleError(getMessage('GRCAD$HDR1$MSG1'));

            return;
	}

	var postData = 'Action=2&UserId=' + id + '&UserName=' + userName + '&MaxNumTherms=' + maxNumTherms + '&LangCode=' + langCode;

	ajax.doPost('services/process_user_db.php', postData, 'loadimage', handleUpdateUserResponse);

	document.getElementById('UserName_' + id + '_Span').className = 'view';
	document.getElementById('UserName_' + id + '_Input').className = 'view';

	document.getElementById('MaxNumTherms_' + id + '_Span').className = 'view';
	document.getElementById('MaxNumTherms_' + id + '_Input').className = 'view';

	document.getElementById('LangCode_' + id + '_Span').className = 'view';
	document.getElementById('LangCode_' + id + '_Input').className = 'view';

	document.getElementById('Users_Edit_' + id).style.display = 'inline';
	document.getElementById('Users_Update_' + id).style.display = 'none';
	document.getElementById('Users_Cancel_' + id).style.display = 'none';
}

function deleteUser(id)
{
	var postData = 'Action=3&UserId=' + id;

	ajax.doPost('services/process_user_db.php', postData, 'loadimage', handleDeleteUserResponse);
}

function cancelEditUser(id)
{
	document.getElementById('UserName_' + id + '_Span').className = 'view';
	document.getElementById('MaxNumTherms_' + id + '_Span').className = 'view';
	document.getElementById('LangCode_' + id + '_Span').className = 'view';

	document.getElementById('UserName_' + id + '_Input').className = 'view';
	document.getElementById('MaxNumTherms_' + id + '_Input').className = 'view';
    document.getElementById('LangCode_' + id + '_Input').className = 'view';

	document.getElementById('Users_Edit_' + id).style.display = 'inline';
	document.getElementById('Users_Update_' + id).style.display = 'none';
	document.getElementById('Users_Cancel_' + id).style.display = 'none';
}

function addGroup()
{
	var name = document.getElementById('Name_0_Input').value;
	var description = document.getElementById('Description_0_Input').value;

	if(name == '')
	{
            handleError(getMessage('GRCAD$HDR2$MSG1'));

            return;
	}

	var postData = 'Action=1&Name=' + name + '&Description=' + description;

	ajax.doPost('services/process_group_db.php', postData, 'loadimage', handleAddGroupResponse);

	document.getElementById('Name_0_Input').value = '';
	document.getElementById('Description_0_Input').value = '';
}

function editGroup(id)
{
	document.getElementById('Name_' + id + '_Span').className = 'edit';
	document.getElementById('Description_' + id + '_Span').className = 'edit';

	document.getElementById('Name_' + id + '_Input').className = 'edit';
	document.getElementById('Description_' + id + '_Input').className = 'edit number';

	document.getElementById('Groups_Edit_' + id).style.display = 'none';
	document.getElementById('Groups_Update_' + id).style.display = 'inline';
	document.getElementById('Groups_Cancel_' + id).style.display = 'inline';

}

function updateGroup(id)
{
	var name = document.getElementById('Name_' + id + '_Input').value;
	var description = document.getElementById('Description_' + id + '_Input').value;

	if(name == '')
	{
            handleError(getMessage('GRCAD$HDR2$MSG1'));

            return;
	}

	var postData = 'Action=2&GroupId=' + id + '&Name=' + name + '&Description=' + description;

	ajax.doPost('services/process_group_db.php', postData, 'loadimage', handleUpdateGroupResponse);

	document.getElementById('Name_' + id + '_Span').className = 'view';
	document.getElementById('Name_' + id + '_Input').className = 'view';

	document.getElementById('Description_' + id + '_Span').className = 'view';
	document.getElementById('Description_' + id + '_Input').className = 'view';

	document.getElementById('Groups_Edit_' + id).style.display = 'inline';
	document.getElementById('Groups_Update_' + id).style.display = 'none';
	document.getElementById('Groups_Cancel_' + id).style.display = 'none';
}

function deleteGroup(id)
{
	var postData = 'Action=3&GroupId=' + id;

	ajax.doPost('services/process_group_db.php', postData, 'loadimage', handleDeleteGroupResponse);
}

function cancelEditGroup(id)
{
	document.getElementById('Name_' + id + '_Span').className = 'view';
	document.getElementById('Description_' + id + '_Span').className = 'view';

	document.getElementById('Name_' + id + '_Input').className = 'view';
	document.getElementById('Description_' + id + '_Input').className = 'view';

	document.getElementById('Groups_Edit_' + id).style.display = 'inline';
	document.getElementById('Groups_Update_' + id).style.display = 'none';
	document.getElementById('Groups_Cancel_' + id).style.display = 'none';
}

function getLangDDLItems(selectedLangValue)
{
    ajax.async = false;
    ajax.doPost('services/construct_lang_ddl_items.php', 'SelectedLangValue=' + selectedLangValue, 'loadimage', handleGetLangDDLItemsResponse);
    ajax.async = true;

    return document.getElementById('langDDLItems').value;
}

function refreshTotals()
{
    ajax.doPost('services/get_admin_totals.php', '', 'loadimage', handleRefreshTotals);
}

function handleAddUserResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var params = resp.split(';');

		var userId = params[0];
		var userName = params[1];
		var maxNumTherms = params[2];
		var langCode = params[3];
                var langDesc = params[4];

                var languageDDLItems = getLangDDLItems(langCode);

		var profileAdminTable = document.getElementById('profileadmin');
		var targetRowIndex = profileAdminTable.rows.length - 1;

		profileAdminTable.insertRow(targetRowIndex);

		var row = profileAdminTable.rows[targetRowIndex];
		row.id = '~' + userId + '~';

		if(profileAdminTable.rows[row.rowIndex - 1].className == '')
		{
			row.className = 'alt';
		}

		row.insertCell(0);
		row.insertCell(1);
		row.insertCell(2);
		row.insertCell(3);
                row.insertCell(4);

		var cell1 = row.cells[0];
		var cell2 = row.cells[1];
		var cell3 = row.cells[2];
		var cell4 = row.cells[3];
                var cell5 = row.cells[4];

		cell1.innerHTML = '<a href="javascript:void(0);" id="Users_Edit_' + userId + '" onclick="editUser(' + userId + ');" class="edit">Edit</a>';
		cell1.innerHTML += '<a href="javascript:void(0);" id="Users_Update_' + userId + '" onclick="updateUser(' + userId + ');" class="update">Update</a>';
		cell1.innerHTML += '&nbsp;<a href="javascript:void(0);" id="Users_Cancel_' + userId + '" onclick="cancelEditUser(' + userId + ');" class="cancel">Cancel</a>';

		cell2.innerHTML = '<span id="UserName_' + userId + '_Span" class="view">' + userName + '</span><input type="text" id="UserName_' + userId + '_Input" value="' + userName + '" class="view" />';

		cell3.innerHTML = '<span id="MaxNumTherms_' + userId + '_Span" class="view">' + maxNumTherms + '</span><input type="text" id="MaxNumTherms_' + userId + '_Input" value="' + maxNumTherms + '" class="view" />';

		cell4.innerHTML = '<span id="LangCode_' + userId + '_Span" class="view">' + langDesc + '</span>';

        cell4.innerHTML += '<select id="LangCode_' + userId + '_Input" class="view">' + languageDDLItems + '</select>';

		cell5.innerHTML = '<a href="javascript:void(0);" id="Users_Delete_' + userId + '" onclick="deleteUser(' + userId + ');" class="delete">Delete</a>';

        refreshTotals();
	}
}

function handleUpdateUserResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
               var params = resp.split(';');
		var userId = params[0];
		var userName = params[1];
		var maxNumTherms = params[2];
		var langCode = params[3];
                var langDesc = params[4];

		document.getElementById('UserName_' + userId + '_Span').innerHTML = userName;
		document.getElementById('UserName_' + userId + '_Input').value = userName;

		document.getElementById('MaxNumTherms_' + userId + '_Span').innerHTML = maxNumTherms;
		document.getElementById('MaxNumTherms_' + userId + '_Input').value = maxNumTherms;

		document.getElementById('LangCode_' + userId + '_Span').innerHTML = langDesc;
		document.getElementById('LangCode_' + userId + '_Input').value = langCode;

        refreshTotals();
	}
}

function handleDeleteUserResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var profileAdminTable = document.getElementById('profileadmin');

		profileAdminTable.deleteRow(document.getElementById('~' + resp + '~').rowIndex);

		var className = null;

		for(var i = 2; i < profileAdminTable.rows.length - 1; i++)
		{
			className = (className) ? '' : 'alt';
		    profileAdminTable.rows[i].className = className;
		}

        refreshTotals();
	}
}

function handleAddGroupResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var params = resp.split(';');

		var groupId = params[0];
		var name = params[1];
		var description = params[2];

		var groupsAdminTable = document.getElementById('groupsadmin');
		var targetRowIndex = groupsAdminTable.rows.length - 1;

		groupsAdminTable.insertRow(targetRowIndex);

		var row = groupsAdminTable.rows[targetRowIndex];
		row.id = '~' + groupId + '~';

		if(groupsAdminTable.rows[row.rowIndex - 1].className == '')
		{
			row.className = 'alt';
		}

		row.insertCell(0);
		row.insertCell(1);
		row.insertCell(2);
		row.insertCell(3);

		var cell1 = row.cells[0];
		var cell2 = row.cells[1];
		var cell3 = row.cells[2];
		var cell4 = row.cells[3];

		cell1.innerHTML = '<a href="javascript:void(0);" id="Groups_Edit_' + groupId + '" onclick="editGroup(' + groupId + ');" class="edit">Edit</a>';
		cell1.innerHTML += '<a href="javascript:void(0);" id="Groups_Update_' + groupId + '" onclick="updateGroup(' + groupId + ');" class="update">Update</a>';
		cell1.innerHTML += '&nbsp;<a href="javascript:void(0);" id="Groups_Cancel_' + groupId + '" onclick="cancelEditGroup(' + groupId + ');" class="cancel">Cancel</a>';

		cell2.innerHTML = '<span id="Name_' + groupId + '_Span" class="view">' + name + '</span><input type="text" id="Name_' + groupId + '_Input" value="' + name + '" class="view" />';

		cell3.innerHTML = '<span id="Description_' + groupId + '_Span" class="view">' + description + '</span><input type="text" id="Description_' + groupId + '_Input" value="' + description + '" class="view" />';

		cell4.innerHTML = '<a href="javascript:void(0);" id="Groups_Delete_' + groupId + '" onclick="deleteGroup(' + groupId + ');" class="delete">Delete</a>';

		refreshTotals();
	}
}

function handleUpdateGroupResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
        var params = resp.split(';');
		var groupId = params[0];
		var name = params[1];
		var description = params[2];

		document.getElementById('Name_' + groupId + '_Span').innerHTML = name;
		document.getElementById('Name_' + groupId + '_Input').value = name;

		document.getElementById('Description_' + groupId + '_Span').innerHTML = description;
		document.getElementById('Description_' + groupId + '_Input').value = description;
	}
}

function handleDeleteGroupResponse(resp)
{
	if(resp.substr(0,4) == 'ERR:')
	{
		handleError(resp);
	}
	else
	{
		var groupsAdminTable = document.getElementById('groupsadmin');

		groupsAdminTable.deleteRow(document.getElementById('~' + resp + '~').rowIndex);

		var className = null;

		for(var i = 2; i < groupsAdminTable.rows.length - 1; i++)
		{
			className = (className) ? '' : 'alt';
		    groupsAdminTable.rows[i].className = className;
		}

		refreshTotals();
	}
}

function handleGetLangDDLItemsResponse(resp)
{
    if(resp.substr(0,4) == 'ERR:')
    {
        handleError(resp);
    }
    else
    {
        document.getElementById('langDDLItems').value = resp;
    }
}

function handleRefreshTotals(resp)
{
    if(resp.substr(0,4) == 'ERR:')
    {
        handleError(resp);
    }
    else
    {
        var params = resp.split(';');
        var totalUserCount = params[0];
        var totalThermCount = params[1];
		var totalGroupCount = params[2];

        document.getElementById('totalusercount').innerHTML = totalUserCount.toString();
        document.getElementById('totalthermcount').innerHTML = totalThermCount.toString();
		document.getElementById('totalgroupcount').innerHTML = totalGroupCount.toString();
    }
}
