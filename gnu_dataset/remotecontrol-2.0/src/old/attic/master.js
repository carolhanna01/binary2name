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

function getPosition(obj)
{
	var left = 0;
	var top = 0;

	do
	{
		left += obj.offsetLeft;
		top += obj.offsetTop;
	}
	while(obj == obj.offsetParent);

	return [left, top];
}

function RestrictToInteger(textbox)
{
	textbox.value = textbox.value.replace(/[^\d]/g,"");
}

function RestrictToDomainChars(textbox)
{
	textbox.value = textbox.value.replace(/[^a-zA-Z0-9.\/\-]/g,"");
}

function getMessage(resCode)
{   
    ajax.async = false;
    ajax.doPost('services/translate_resource.php', 'ResourceCode=' + resCode, 'loadimage', handleGetMessageResponse);
    ajax.async = true;
    
    return document.getElementById("messageHidden").value;
}

function handleGetMessageResponse(resp)
{
    if(resp.substr(0,4) == 'ERR:')
    {
        handleError(resp);
    }
    else
    {
        document.getElementById("messageHidden").value = resp;
    }
}

function handleError(resp)
{
    var messageLabel = document.getElementById('message');
    messageLabel.className = 'error';
    messageLabel.innerHTML = resp;
}
