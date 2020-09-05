
var EXTENSION_VERSION = "";
var RESET_EXTENSION_VERSION = "-1";
const EXTENSION_ID = "{e8f509f0-b677-11de-8a39-0800200c9a99}";
var pref = Components.classes["@mozilla.org/preferences-service;1"].getService(Components.interfaces.nsIPrefBranch);

var	userLoggedInStatus = null;
var checkLoginStatusFlag = false;


var loginHelpers = {

	sendXHR : function(url) {
		var req = Components.classes["@mozilla.org/xmlextras/xmlhttprequest;1"].createInstance(Components.interfaces.nsIXMLHttpRequest);
		req.onprogress = null;
		req.onload = null;
		req.onerror = null;
		req.open("GET", url, true);
		req.send(null);
	},

	isInSession : function(ck, host) {
		var cookieArray = ck.split(';');
		for(var i = 0; i < cookieArray.length; i++) {
			var cArray = cookieArray[i].split('=');
			cname = cArray[0].trim();
			cValue = cArray[1].trim();
			if(host == "www.facebook.com" && cname == "c_user") {
				userLoggedInStatus.push(["facebook", true]);
				return true;
			} else if(host == "www.google.com" && cname == "SID") {
				userLoggedInStatus.push(["google", true]);
				return true;
			} else if(host == "www.twitter.com" && cname == "twid") {
				userLoggedInStatus.push(["twitter", true]);
				return true;
			} else if(host == "www.linkedin.com" && cname == "leo_auth_token") {
				if(cValue.substring(1,4).trim() == "LIM") {
					userLoggedInStatus.push(["linkedin", true]);
					return true;
				}
			}
		}
		return false;
	},

	displayLoginStatus : function(statusArray) {
		var gStatus = false, fStatus = false, tStatus = false, lStatus = false;
		var str = "";
		for(var i = 0; i < statusArray.length; i++) {
			var elem = statusArray[i];
			if(elem[1]) {
				switch(elem[0]) {
					case 'google':
						if(!gStatus) {
							gStatus = true;
							str += ", Google";
						}
						break;
					case 'facebook':
						if(!fStatus) {
							fStatus = true;
							str += ", Facebook";
						}
						break;
					case 'twitter':
						if(!tStatus) {
							tStatus = true;
							str += ", Twitter";
						}
						break;
					case 'linkedin':
						if(!lStatus) {
							lStatus = true;
							str += ", LinkedIn";
						}
						break;
				}
			}
		}
		return str.slice(1);
	},

	getLoginStatus : function() {
		checkLoginStatusFlag = true;
		this.sendXHR("http://www.google.com");
		this.sendXHR("http://www.facebook.com");
		this.sendXHR("https://www.facebook.com");
		this.sendXHR("http://www.twitter.com");
		this.sendXHR("http://www.linkedin.com");
		checkLoginStatusFlag = false;
		userLoggedInStatus = [];
		for(var i = 0; i < statusCookies.length; i++) {
			var sck = statusCookies[i];
			this.isInSession(sck[1], sck[0]);
		}
		var str = this.displayLoginStatus(userLoggedInStatus);
		userLoggedInStatus = [];
		statusCookies = [];
		return str;
	}
}

var toggleHelpers = {

	getCurrentStatus : function() {
		return pref.getBoolPref("extension.priv.status");
	},

	setCurrentStatus : function(val){
		pref.setBoolPref("extension.priv.status", val);
	},

	getPanelMessageStatus : function() {
		return pref.getBoolPref("extension.priv.msgstatus");
	},

	setPanelMessageStatus : function(val){
		pref.setBoolPref("extension.priv.msgstatus", val);
	},

	toggleMessagePanel : function() {
		var parent = document.getElementById("status-bar");
		var entry = null, item = null;
		if (this.getPanelMessageStatus()) {
			// remove status bar message
			entry = document.getElementById("intercept-status");
			parent.removeChild(entry);
		
			item = document.getElementById("msgstatus");
			item.setAttribute("checked", "true");
		
			this.setPanelMessageStatus(false);
		} else {
			// show status bar message
			entry = document.createElement("statusbarpanel");
			entry.setAttribute("id", "intercept-status");
			entry.setAttribute("label", "");
			//var popup = document.getElementById("intercept");
			//entry.setAttribute("popup", popup.id);
			entry.setAttribute("popup", "intercept");

			var before = document.getElementById("last");
			parent.insertBefore(entry, before);
		
			item = document.getElementById("msgstatus");
			item.setAttribute("checked", "false");
		
			this.setPanelMessageStatus(true);
		}
	},

	toggleCurrentStatus : function() {
		if (this.getCurrentStatus()) {
			item = document.getElementById("my-icon");
			item.setAttribute("image", "chrome://priv/content/icons/off.png");

			item = document.getElementById("disable");
			item.setAttribute("checked", "true");

			this.setCurrentStatus(false);
		} else {
			item = document.getElementById("my-icon");
			item.setAttribute("image", "chrome://priv/content/icons/on.png");

			item = document.getElementById("disable");
			item.setAttribute("checked", "false");

			this.setCurrentStatus(true);
		}
	}
}

var menuHelpers = {

	createReloadedItemsMenu : function(items) {
		// remove all existing login entries in the menu
		var p = document.getElementById("intercept");
		var el = p.firstChild;
		while(el != null) {
			var next = el.nextSibling;
			if(el.getAttribute("id").slice(0,6) == "status") {
				p.removeChild(el);
			}
			el = next;
		}
		items.sort();
		var fb = false;
		var gg = false;
		var tw = false;
		var ln = false;
		var entry = null, parent = document.getElementById("intercept");
		for(var i = 0; i < items.length; i++) {
			switch(items[i][0]) {
				case "facebook":
					if(fb)
						break;
					fb = true;
					entry = document.createElement("menuitem");
					entry.setAttribute("label", "Facebook");
					entry.setAttribute("id", "status");
					parent.appendChild(entry);
					// add a separator
					entry = document.createElement("menuseparator");
					entry.setAttribute("id", "status");
					parent.appendChild(entry);
					break;
				case "google":
					if(gg)
						break;
					gg = true;
					entry = document.createElement("menuitem");
					entry.setAttribute("label", "Google");
					entry.setAttribute("id", "status");
					parent.appendChild(entry);
					// add a separator
					entry = document.createElement("menuseparator");
					entry.setAttribute("id", "status");
					parent.appendChild(entry);
					break;
				case "twitter":
					if(tw)
						break;
					tw = true;
					entry = document.createElement("menuitem");
					entry.setAttribute("label", "Twitter");
					entry.setAttribute("id", "status");
					parent.appendChild(entry);
					// add a separator
					entry = document.createElement("menuseparator");
					entry.setAttribute("id", "status");
					parent.appendChild(entry);
					break;
				case "linkedin":
					if(ln)
						break;
					ln = true;
					entry = document.createElement("menuitem");
					entry.setAttribute("label", "LinkedIn");
					entry.setAttribute("id", "status");
					parent.appendChild(entry);
					// add a separator
					entry = document.createElement("menuseparator");
					entry.setAttribute("id", "status");
					parent.appendChild(entry);
					break;
				default:
					break;
			}
			entry = document.createElement("menuitem");
			entry.setAttribute("label", items[i][1].src);
			entry.setAttribute("id", "status");
			parent.appendChild(entry);
		}
	},

	createBlockedItemsMenu : function(items) {
		// remove all existing login entries in the menu
		var p = document.getElementById("intercept");
		var el = p.firstChild;
		while(el != null) {
			var next = el.nextSibling;
			if(el.getAttribute("id").slice(0,6) == "status") {
				p.removeChild(el);
			}
			el = next;
		}

		var entry = null, parent = document.getElementById("intercept");
		if(items.fb.length) {
			entry = document.createElement("menuitem");
			entry.setAttribute("label", "Facebook : " + items.fb.length);
			entry.setAttribute("id", "status_fb");
			parent.appendChild(entry);
		}
		if(items.gg.length) {
			entry = document.createElement("menuitem");
			entry.setAttribute("label", "Google : " + items.gg.length);
			entry.setAttribute("id", "status_gg");
			parent.appendChild(entry);
		}
		if(items.ln.length) {
			entry = document.createElement("menuitem");
			entry.setAttribute("label", "LinkedIn : " + items.ln.length);
			entry.setAttribute("id", "status_ln");
			parent.appendChild(entry);
		}
		if(items.tw.length) {
			entry = document.createElement("menuitem");
			entry.setAttribute("label", "Twitter : " + items.tw.length);
			entry.setAttribute("id", "status_fb");
			parent.appendChild(entry);
		}
	},

	populateBlockedItemsList : function(elems) {
		var str = {
			fb : [],
			gg : [],
			tw : [],
			ln : []
		};
		for(var i = 0; i < elems.length; i++) {
			var plugin = elems[i][0];
			var req = elems[i][1]
			switch(plugin) {
				case "facebook":
					str.fb.push(req);
					break;
				case "google":
					str.gg.push(req);
					break;
				case "twitter":
					str.tw.push(req);
					break;
				case "linkedin":
					str.ln.push(req);
					break;
			}
		}
		return str;
	}
}

var Privlistener = {
	observe: function(subject, topic, data) {
		switch (topic) {
			case 'sessionstore-windows-restored':
			window.gBrowser.selectedTab = window.gBrowser.addTab("http://priv3.icsi.berkeley.edu");
			Priv.unregister();
			break;
		}
	}
}

var Priv = {

	getVersion : function() {
		var version = document.getElementById("version");
		version.label += buildInfo;
	},

	setExtID: function(addon) {
		EXTENSION_VERSION = addon.version;
	},
	
	initializeOverlay : function() {
		var finished = 0;
		try {
			finished = 1 * pref.getCharPref("extension.priv.install");
		} catch(e) {
			finished = -1;
		}
		if (finished != 1 * EXTENSION_VERSION) {
			pref.setCharPref("extension.priv.install", EXTENSION_VERSION);
		}
		return finished;
	},

	onLoad: function() {
		toggleHelpers.setCurrentStatus(true);
		toggleHelpers.setPanelMessageStatus(true);
		var ver = this.initializeOverlay();
		if(ver < 1 * EXTENSION_VERSION) {
			this.register();
		}
		this.getVersion();
	},
	
	status: function(){
		return toggleHelpers.getCurrentStatus();
	},
	
	register: function() {
		var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
		observerService.addObserver(Privlistener, "sessionstore-windows-restored", false);
	},
    
	unregister: function(){
		var observerService = Components.classes["@mozilla.org/observer-service;1"].getService(Components.interfaces.nsIObserverService);
		observerService.removeObserver(Privlistener, "sessionstore-windows-restored");
	},

	onMenuItemCommand: function(e, type) {
		var item = null;
		switch(type) {
			case 'toggle':
				toggleHelpers.toggleCurrentStatus();
				break;

			case 'bugreport':
				var loc = gBrowser.contentDocument.location;
				var xulRuntime = Components.classes["@mozilla.org/xre/app-info;1"].getService(Components.interfaces.nsIXULRuntime);
				var appInfo = Components.classes["@mozilla.org/xre/app-info;1"].getService(Components.interfaces.nsIXULAppInfo);;
				var platform = appInfo.vendor + " " + appInfo.name + " " + appInfo.version + " / " + xulRuntime.OS;
				gBrowser.loadURI('mailto:priv3-bug@icsi.berkeley.edu?subject=Bug report: Build #' + buildInfo + ', Browser/OS [' + platform + '], URL [' + loc + ']');
				break;
				
			case 'about':
				window.open("chrome://priv/content/about.xul", "About", "chrome,centerscreen");
				break;
				
			case 'status':
				toggleHelpers.toggleMessagePanel();
				break;
				
			case 'intercept':
				var url = gBrowser.contentDocument.location;
				var b = listener.getBrowserFromURL(url);
				var str = null;
				if(setIntervalID != null) {
					str = menuHelpers.populateBlockedItemsList(b.blockedItems);
					menuHelpers.createBlockedItemsMenu(str);
				} else {
					menuHelpers.createReloadedItemsMenu(b.reloadedItems);
				}
				break;
			
			default:
				break;
		}
	},
	
	createMenuEntry: function(site) {
		var parent = document.getElementById("clipmenu");
		var node = document.getElementById("ver");
		var menuEntry = this.createMenuItem("Logged into " + site);
		parent.insertBefore(menuEntry, node);
	},
	
	createMenuItem: function (aLabel) {
		var item = document.createElement("menuitem");
		item.setAttribute("label", aLabel);
		item.setAttribute("id", "login_" + aLabel);
		return item;
	},

	onPopUpShowing: function(e) {
		// remove all existing login entries in the menu
		var p = document.getElementById("clipmenu");
		var el = p.firstChild;
		while(el != p.lastChild) {
			var next = el.nextSibling;
			if(el.getAttribute("id").slice(0,5) == "login") {
				p.removeChild(el);
			}
			el = next;
		}
		// get the existing login status
		var str = loginHelpers.getLoginStatus();
		var sites = str.split(',');
		sites.sort();
		for(var i = 0; i < sites.length; i++) {
			var site = sites[i].trim();
			if(site == "") {
				var node = document.getElementById("abt");
				if(node)
					p.removeChild(node);
			} else {
				// create and add context menu for site
				this.createMenuEntry(site);
			}
		}
	},

	onIconClick: function(e) {
		// toggle status on middle button click
		if (e.button == 1) {
			toggleHelpers.toggleCurrentStatus();
		}
	}
};

var uninstallListener = {
	onUninstalling: function(addon) {
		if (addon.id == EXTENSION_ID) {
			pref.setCharPref("extension.priv.install", RESET_EXTENSION_VERSION);
		}
	}
}

try {
	Components.utils.import("resource://gre/modules/AddonManager.jsm");
	AddonManager.addAddonListener(uninstallListener);
} catch (ex) {}

AddonManager.getAddonByID(EXTENSION_ID, Priv.setExtID);

window.addEventListener("load", function(e) { Priv.onLoad(e); }, false);
