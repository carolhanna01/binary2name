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

function Ajax()
{
    this.req = null;
    this.url = null;
    this.method = 'GET';
    this.async = true;
    this.status = null;
    this.statusText = '';
    this.statusImgId = '';
    this.postData = null;
    this.readyState = null;
    this.responseText = null;
    this.responseXML = null;
    this.handleResp = null;
    this.responseFormat = 'text';
    this.mimeType = null;
    
    this.init = function(){
    
        if(!this.req)
        {
            try
            {
                this.req = new XMLHttpRequest();
            }
            catch(e)
            {
                try
                {
                    this.req = new ActiveXObject('MSXML2.XMLHTTP');
                }
                catch(e)
                {
                    try
                    {
                        this.req = new ActiveXObject('Microsoft.XMLHTTP');
                    }
                    catch(e)
                    {
                        return false;
                    }
                }
            }
        }
    
        return this.req;
        
    };
    
    this.doReq = function(){
        
        if(!this.init())
        {
            alert(getMessage('GRC$MSG1'));
            return;
        }

        //alert( this.method );
        //alert( this.url );
        
	this.req.open(this.method, this.url, this.async);
        
        if(this.method == 'POST')
        {
          this.req.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
        }
        
        if(this.mimeType)
        {
            try
            {
                this.req.overrideMimeType(this.mimeType);
            }
            catch(e)
            {
                // couldn't override MIME type -- IE6 or Opera?
            }
        }
        
        var self = this;
        
        this.req.onreadystatechange = function(){

            var resp = null;
        
            if(self.req.readyState == 4)
            {
              //alert( 'statusImgId: ' + self.statusImgId );
              
		document.getElementById(self.statusImgId).style.display = 'none';
                
                switch(self.responseFormat)
                {
                    case 'text':
                    
                        resp = self.req.responseText;
                        
                        break;
                        
                    case 'xml':
                    
                        resp = self.req.responseXML;
                    
                        break;
                        
                    case 'object':
                    
                        resp = self.req;
                    
                        break;
                }
                
                if(self.req.status >= 200 && self.req.status <= 299)
                {
                    self.handleResp(resp);
                }
                else
                {
                  //alert( 'error' );
                  
                    self.handleErr(resp);
                }
            }
            else
            {
                    document.getElementById(self.statusImgId).style.display = 'block';
            }
        
        };
        
        this.req.send(this.postData);
        
    };
    
    this.setMimeType = function(mimeType){
    
        this.mimeType = mimeType;
    
    };
    
    this.handleErr = function(){
    
      //alert( 'error' );
      
      return;
      
        var errorWin;
        
        try
        {
            //errorWin = window.open('','errorWin');
            errorWin.document.body.innerHTML = this.responseText;
        }
        catch(e)
        {
            var msg = getMessage('GRC$MSG2');
            msg = msg.replace('{0}', this.req.status);
            msg = msg.replace('{1}', this.req.statusText);            
            
            alert(msg);
        }
    
    };
    
    this.setHandlerErr = function(funcRef){
    
        this.handleErr = funcRef;
    
    };
    
    this.setHandlerBoth = function(funcRef){
    
        this.handleResp = funcRef;
        this.handleErr = funcRef;
    
    };
    
    this.abort = function(){
    
        if(this.req)
        {
            this.req.onreadystatechange = function(){};
            this.req.abort();
            this.req = null;
        }
    
    };
    
    this.doGet = function(url, loadingImgId, hand, format){
    
        this.url = url;
        this.handleResp = hand;
	//this.handleErr = handerr;
        this.responseFormat = format || 'text';
	this.statusImgId = loadingImgId;
        this.doReq();
    
    };
    
    this.doPost = function(url, postData, loadingImgId, hand, format){
    
    //alert( url );
    
      this.url = url;
      this.handleResp = hand;			 
      this.responseFormat = format || 'text';
      this.method = 'POST';
      this.postData = postData;
      this.statusImgId = loadingImgId;      
      this.doReq();
    
    };
}
