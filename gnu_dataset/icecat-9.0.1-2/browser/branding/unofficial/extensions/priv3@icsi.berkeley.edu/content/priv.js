
const socialPlugins = ["google", "facebook", "twitter", "linkedin"];
const aggregators = ["addthis"];

const googleHosts = ["google", "gstatic", "google-analytics", "youtube"];
const facebookHosts = ["facebook", "fbcdn"];
const twitterHosts = ["twitter", "twimg", "tweetmeme"];
const linkedinHosts = ["linkedin"];

const tags = ["img", "iframe"];

var disAllowedHosts = [];
var statusCookies = [];

function extStatus() {
	var p = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch);
	return p.getBoolPref("extension.priv.status");
}

var urlHelpers = {

	initDisAllowedHosts : function() {
		for(var i in socialPlugins) {
			var elem = eval(socialPlugins[i] + "Hosts");
			for(var j in elem){
				disAllowedHosts.push(elem[j]);
			}
		}
	},

	getHostname : function(url) {
		var re = new RegExp('^(?:f|ht)tp(?:s)?\://([^/]+)', 'im');
		var match = url.match(re);
		if(match)
			return match[1].toString();
		return null;
	},

	isAggregator : function(host) {
		for(var j in aggregators){
			var re = new RegExp(aggregators[j]);
			if(host.match(re))
				return true;
		}
		return false;
	}, 

	getSocialPlugin : function(hostname) {
		if(!hostname)
			return null;
		for(var i in socialPlugins) {
			var elem = eval(socialPlugins[i] + "Hosts");
			for(var j in elem){
				var re = new RegExp(elem[j]);
				if(hostname.match(re))
					return socialPlugins[i];
			}
		}
		return null;
	},

	HostAllowed : function(host){
		for(var i in disAllowedHosts) {
			var re = new RegExp(disAllowedHosts[i] + "\.");
			if(host.match(re))
				return false;
		}
		return true;
	},

	getElement : function(doc, url, elems) {
		for(var i in elems) {
			var e = doc.getElementsByTagName(elems[i]);
			for(var j = 0; j < e.length; j++) {
				if(e[j] && (e[j].src == url))
					return e[j];
			}
		}
		return null;
	},

	getElements : function(doc, url, elems) {
		var elm = [];
		for(var i in elems) {
			var e = doc.getElementsByTagName(elems[i]);
			for(var j = 0; j < e.length; j++) {
				if(e[j] && (e[j].src == url))
					elm.push(e[j]);
			}
		}
		return elm;
	}
};

var replayClick = function(evt, doc, browser) {
	var e = doc.createEvent("MouseEvents");
	e.initMouseEvent("click", true, true, doc.defaultView, 0, 0, 0, 0, 0, false, false, false, false, evt.button, null);
	var target = doc.elementFromPoint(evt.clientX, evt.clientY);
	if(extStatus()) {
		var linkNode = clickHelpers.getNodeIfTargetIsHyperLink(target);
		if(linkNode) {
			browser.clickedURLs.push([doc.location.hostname, linkNode.href]);
		}
	}
	target.dispatchEvent(e);
};

var reload = function(elems, plugin, browser) {
	var count = 0;
	for(var i = 0; i < elems.length; i++){
		var e = elems[i];

		switch(e.tagName) {
			case "IFRAME":
				if(e && e.contentDocument) {
					e.contentDocument.location.reload(true);
					browser.reloadedItems.push([plugin, e]);
					count++;
				}
				break;
			case "SCRIPT":
				var parent = e.parentNode;
				var script = e.ownerDocument.createElement("script");
				script.src = e.src;
				script.async = false;
				if(parent != null) {
					parent.insertBefore(script, e);
					parent.removeChild(e);
				} else {
					e.ownerDocument.body.appendChild(e);
				}
				browser.reloadedItems.push([plugin, e]);
				count++;
				break;
			default:
				break;
		}
	}

	var item = document.getElementById("intercept-status");
	item.setAttribute("label", "Reloaded : " + count);
	item.style.color = "#333333";

	if(setIntervalID && setInterceptStatus) {
		// clear the status bar
		window.clearInterval(setIntervalID);
		setIntervalID = null;

		// reset the status bar
		window.setTimeout(function(){
			setIntervalID = window.setInterval(setInterceptStatus, 20);
		}, 10000);
	}
};

var interceptClick = function(event, browser) {
	var url = null;
	var target = event.originalTarget;
	var doc = target.ownerDocument;
	var linkNode = clickHelpers.getNodeIfTargetIsHyperLink(target);
	if(linkNode && linkNode.href)
		url = linkNode.href;
	else
		url = doc.location.href;
	var plugin = urlHelpers.getSocialPlugin(urlHelpers.getHostname(url));
	if(plugin == null)
		plugin = urlHelpers.getSocialPlugin(urlHelpers.getHostname(doc.location.href));
	// if plugin is still null, then it means that link is not an iframe, it may
	// be a span embedded in the parent document with some script as in linkedin
	if(plugin && browser.social[plugin].userInteraction == false){
		if(doc.location != browser.contentDocument.location && 
				(!urlHelpers.HostAllowed(doc.location.host) || doc.pluginElement)) {
			event.stopPropagation();
			event.preventDefault();
			browser.social[plugin].userInteraction = true;
			var iframes = urlHelpers.getElements(browser.contentDocument, doc.location, ["iframe"]);
			var iframe = null;
			for(var i = 0; i < iframes.length; i++) {
				if(iframes[i].contentDocument == doc) {
					iframe = iframes[i];
					break;
				}
			}
			if(iframe != null) {
				if(extStatus()) {
					iframe.addEventListener("load", function(){ 
						replayClick(event, iframe.contentDocument, browser); 
					}, false);
					// reload only if left click
					if(event.button == 0) {
						reload(browser.social[plugin].savedElems, plugin, browser);
					}
				} else {
					replayClick(event, iframe.contentDocument, browser);
				}
			} else {
			}
		}
	} else {
	}
};

var clickHelpers = {

	getNodeIfTargetIsHyperLink : function(node) {
		var el = node;
		while ((el != null) && el.tagName != "BODY") {
			if(el.tagName == "A")
				return el;
			el = el.parentNode;
		}
		return null;
	},

	// this checks if the user actually clicked the link
	isClickedURL : function(loc, url) {
		//alert("hostname = " + urlHelpers.getHostname(url));
		var b = listener.getBrowserFromURL(loc);
		var links = b ? b.clickedURLs : [];
		for(var i = 0; i < links.length; i++) {
			if(urlHelpers.getHostname(url) == links[i][0] || url == links[i][1])
				return true;
		}
		return false;
	},

	// this is for Yahoo's implementation of share,
	// which opens up a new window on user click
	checkIfHTTPIsForNavigation : function(browser, requestURL) {
		var parentWindow = browser.contentWindow.opener;
		if(parentWindow) {
			var parentDocument = parentWindow.document;
			if(this.isClickedURL(parentWindow.top.location, requestURL))
				return true;
		}
		return false;
	}
}

var cookieHelpers = {

	setCookieAccess : function(url, bool) {
		var permission = bool ? Components.interfaces.nsICookiePermission.ACCESS_ALLOW : Components.interfaces.nsICookiePermission.ACCESS_DENY;
		var uri = Components.classes["@mozilla.org/network/standard-url;1"].createInstance(Components.interfaces.nsIURI);
		uri.spec = url;
		var cookiePermissionService = Components.classes["@mozilla.org/cookie/permission;1"].getService(Components.interfaces.nsICookiePermission);
		cookiePermissionService.setAccess(uri, permission);
	},

	denyCookies : function(evt, browser) {
		if(!extStatus())
			return;
		var url = evt.target.src;
		var plugin = (url == null || url == "") ? null : urlHelpers.getSocialPlugin(urlHelpers.getHostname(url));
		if(plugin == null)
			return;
		var doc = browser.contentDocument;
		if(browser.social[plugin].userInteraction == false) {
			var ownerDoc = evt.target.ownerDocument;		
			if((ownerDoc.location == doc.location) && !urlHelpers.HostAllowed(url)){
				this.setCookieAccess(doc.location.href, false);
			}
		} else {
		}
	},

	allowCookies : function(evt, browser) {
		if(!extStatus())
			return; 
		var url = evt.target.src;
		var plugin = (url == null || url == "") ? null : urlHelpers.getSocialPlugin(urlHelpers.getHostname(url));
		if(plugin == null)
			return;	
		var doc = browser.contentDocument;
		if(browser.social[plugin].userInteraction == false) {
			var ownerDoc = evt.target.ownerDocument;
			if((ownerDoc.location == doc.location) && !urlHelpers.HostAllowed(url)){
				this.setCookieAccess(doc.location.href, true);
				browser.social[plugin].savedElems.push(evt.target);
			}
		} else {
		}
	}
}

var listener = {
	observe : function(aSubject, aTopic, aData) {
		if(typeof Components == "undefined") {
			return;
		}
		// status is true when the extension is enabled
		if(!extStatus()) {
			return;
		}
		var httpChannel = aSubject.QueryInterface(Components.interfaces.nsIHttpChannel);
		// get the browser that fired the http-on-modify request
		var browser = this.getBrowserFromChannel(aSubject);
		var host = httpChannel.getRequestHeader("Host");
		var requestURL = aSubject.URI.spec;
		var plugin = urlHelpers.getSocialPlugin(urlHelpers.getHostname(requestURL));
		var addressBarURLHost = null;

		if(browser != null && plugin != null) {
			if(aSubject.loadFlags & Components.interfaces.nsIChannel.LOAD_INITIAL_DOCUMENT_URI) {
				addressBarURLHost = urlHelpers.getHostname(requestURL);
			}
			// do nothing if user is visiting FB, or
			// if a new window opens up and navigates to FB which happens via a click on a hyperlink
			// this is a hack for addThis, need to check if other aggregators work the same way
			if(((browser.contentDocument.location.hostname == "" || 
				urlHelpers.isAggregator(browser.contentDocument.location.hostname)) && plugin) || 
				(!urlHelpers.HostAllowed(browser.contentDocument.location.hostname) || 
					(addressBarURLHost && !urlHelpers.HostAllowed(addressBarURLHost)) ||
					clickHelpers.checkIfHTTPIsForNavigation(browser, requestURL) && plugin)) {
				browser.social[plugin].userInteraction = true;
				return;
			}
			if(!urlHelpers.HostAllowed(host) && plugin && (browser.social[plugin].userInteraction == false)) {
				if(aTopic == "http-on-modify-request"){
					var doc = browser.contentDocument;
					if(!browser.scriptFlag) {
						browser.addEventListener("click", function(evt){ interceptClick(evt, browser); }, true);
						doc.addEventListener("beforescriptexecute", function(evt){ cookieHelpers.denyCookies(evt, browser); }, true);
				  		doc.addEventListener("afterscriptexecute", function(evt){ cookieHelpers.allowCookies(evt, browser); }, true);
			  			browser.scriptFlag = true;
			  		}
					httpChannel.setRequestHeader("Cookie", null, false);
					browser.blockedItems.push([plugin, requestURL]);

	   	    		var elem = urlHelpers.getElement(doc, requestURL, tags);
		  			if(elem != null){
		   				// request is from the main document
		   				browser.social[plugin].savedElems.push(elem);
		   			} else {
		   				// request is an XHR from within an iframe
		   				if(this.isXHR(aSubject)) {
		   					return;
		   				}
		   				// request is from an element within an iframe
		   				// Yahoo's implementation of FB's like and share
		   				var loc = this.getDocument(aSubject).location;
		   				var e = urlHelpers.getElements(doc, loc, ["iframe"]);
		  				for(var k = 0; k < e.length; k++) {
		  			 		if(e[k].contentDocument.pluginElement == undefined){
			  					var found = false;
			  					var item = browser.social[plugin].savedElems;
			  					e[k].contentDocument.pluginElement = false;
			  					for(var j = 0; j < item.length; j++) {
			  						if(e[k].src == item[j].src && e[k] == item[j]) {
				  						found = true;
				  						break;
				  					}
				  				}
			  					if(!found){
				  					browser.social[plugin].savedElems.push(e[k]);
		  							e[k].contentDocument.pluginElement = true;
			  					} else {
			  						break;
			  					}
			  				}
		  				}
		   			}
				} else {
		  		} 
			}
		} else {
			if(aTopic == "http-on-modify-request") {
		  		if(checkLoginStatusFlag && plugin != null) {
					var ck = httpChannel.getRequestHeader("Cookie");
					statusCookies.push([host, ck]);
			  	}
			}
		}
	},
	
	getRequestLoadContext: function(aChannel) {
		try {
			if(aChannel && aChannel.notificationCallbacks) {
				return aChannel.notificationCallbacks.getInterface(Components.interfaces.nsILoadContext);
			}
		} catch (e) {
		}
		try {
			if(aChannel && aChannel.loadGroup && aChannel.loadGroup.notificationCallbacks) {
				return aChannel.loadGroup.notificationCallbacks.getInterface(Components.interfaces.nsILoadContext);
			}
		} catch (e) {
		}
		return null;
	},
	
	getWindowForRequest: function(aChannel) {
		var loadContext = this.getRequestLoadContext(aChannel);
		try {
			if(loadContext)
				return loadContext.associatedWindow;
		} catch (e) {
		}
		return null;
	},
	
	getDocument: function (aChannel) {
		try {
			var notificationCallbacks = aChannel.notificationCallbacks ? aChannel.notificationCallbacks : aChannel.loadGroup.notificationCallbacks;
			if (!notificationCallbacks)
		  		return null;
			var domWin = notificationCallbacks.getInterface(Components.interfaces.nsIDOMWindow);
			return domWin.document;
		}
		catch (e) {
			return null;
		}
	},
	
	getBrowserFromChannel: function (aChannel) {
		try {
			var notificationCallbacks = aChannel.notificationCallbacks ? aChannel.notificationCallbacks : aChannel.loadGroup.notificationCallbacks;
			if (!notificationCallbacks){
				return null;
		  	}
			var domWin = notificationCallbacks.getInterface(Components.interfaces.nsIDOMWindow);
			return gBrowser.getBrowserForDocument(domWin.top.document);
		}
		catch (e) {
			if(this.isXHR(aChannel)){
				var win = this.getWindowForRequest(aChannel);
				if(win && win.top)
					return gBrowser.getBrowserForDocument(win.top.document);
			}
			return null;
		}
	},
	
	getBrowserFromURL: function(url) {
		var wm = Components.classes["@mozilla.org/appshell/window-mediator;1"].getService(Components.interfaces.nsIWindowMediator);
		var browserEnumerator = wm.getEnumerator("navigator:browser");

		while (browserEnumerator.hasMoreElements()) {
			var browserWin = browserEnumerator.getNext();
			var tabbrowser = browserWin.gBrowser;

		    var numTabs = tabbrowser.browsers.length;
		    for (var index = 0; index < numTabs; index++) {
		    	var currentBrowser = tabbrowser.getBrowserAtIndex(index);
				if (url == currentBrowser.currentURI.spec) {
			        return currentBrowser;
				}
			}
		}
		return null;
	}, 
	
	onPageUnload: function (evt){
		var target = evt.originalTarget.defaultView;
		if(target != target.top)
			return;
		var b = this.getBrowserFromURL(target.location);
		initHelpers.initBrowser(b);
	},
	
	// thanks to Firebug
	isXHR: function(aChannel) {
		try {
			var notificationCallbacks = aChannel.notificationCallbacks ? aChannel.notificationCallbacks : aChannel.loadGroup.notificationCallbacks;
			if (!notificationCallbacks)
				return false;
			var xhrRequest = notificationCallbacks ? notificationCallbacks.getInterface(Components.interfaces.nsIXMLHttpRequest) : null;
			return (xhrRequest != null);
		}
		catch (exc) {
		}
		return false;
	},
	
	QueryInterface : function(aIID) {
		if (aIID.equals(Components.interfaces.nsISupports) ||
        		aIID.equals(Components.interfaces.nsIObserver))
      		return this;
    	throw Components.results.NS_NOINTERFACE;
	}
};

var initHelpers = {

	initBrowser : function(b) {
		if(b == null || typeof b == "undefined")
			return;
		b.scriptFlag = false;
		b.clickedURLs = [];
		b.social = [];
		for(var i in socialPlugins) {
			b.social[socialPlugins[i]] = {};
		}
		for(var j in b.social) {
			b.social[j].userInteraction = false;
			b.social[j].savedElems = [];
		}
	
		b.blockedItems = [];
		b.reloadedItems = [];
	},

	initTab : function(event) {
		var b = gBrowser.getBrowserForTab(event.target);
		this.initBrowser(b);
	}
}

var initPriv = function(evt) {

	// init the first tab
	initHelpers.initBrowser(gBrowser.selectedBrowser);

	var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
  	observerService.addObserver(listener, "http-on-modify-request", false);
  	
  	// init the subsequent tabs
	gBrowser.tabContainer.addEventListener("TabOpen", initHelpers.initTab, false);
	gBrowser.tabContainer.addEventListener("TabClose", initHelpers.initTab, false);
  	
  	urlHelpers.initDisAllowedHosts();
}

window.addEventListener('load', initPriv, false);
window.addEventListener('pagehide', function(event){
	if (event.originalTarget instanceof HTMLDocument) {
		listener.onPageUnload(event);
	}
}, false);

