var ContentLoader = {
 READY_STATE_UNITIALIZED:0,
 READY_STATE_LOADING:1,
 READY_STATE_LOADED:2,
 READY_STATE_INTERACTIVE:3,
 READY_STATE_COMPLETE:4,

 load:function(url,onload,method,params,onerror) {
  this.url=url;
  this.req=null;
  this.onload=onload;
  this.onerror=(onerror) ? onerror : this.defaultError;
  if (!method || method == 'GET') {
   this.loadXMLDoc(url);
  }
  else if (method == 'POST') {
   this.sendForm(url,params);
  }
 },

 loadXMLDoc:function(url) {
  if (window.XMLHttpRequest) {
   this.req = new XMLHttpRequest();
  } else if (window.ActiveXObject) {
   this.req = new ActiveXObject("Microsoft.XMLHTTP");
  }
  if (this.req) {
   try {
    var loader=this;
    this.req.onreadystatechange=function() {
     loader.onReadyState.call(loader);
    };
    this.req.open('GET',url,true);
    this.req.send(null);
   } catch(err) {
    this.onerror.call(this);
   }
  }
 },
 
 sendForm:function(url, parameters) {
  if (window.XMLHttpRequest) {
   this.req = new XMLHttpRequest();
  } else if (window.ActiveXObject) {
   this.req = new ActiveXObject("Microsoft.XMLHTTP");
  }
  if (this.req) {
   try {
    var loader=this;
    this.req.onreadystatechange=function() {
     loader.onReadyState.call(loader);
    };
    this.req.open('POST',url,true);
    this.req.setRequestHeader("Content-type", "application/x-www-form-urlencoded");
    this.req.setRequestHeader("Content-length", parameters.length);
    this.req.setRequestHeader("Connection", "close");
    this.req.setRequestHeader("smdc_data",parameters);
    this.req.send('');
   } catch(err) {
    //alert(err.toString());
    this.onerror.call(this);
   }
  }
 },

 onReadyState:function() {
  var req=this.req;
  var ready=req.readyState;
  if (ready==this.READY_STATE_COMPLETE) {
   try {
    var httpStatus=req.status;
    if (httpStatus==200 || httpStatus===0) {
     this.onload.call(this);
    } else {
     this.onerror.call(this);
    }
   }
   catch (err) {
    //alert(err.toString());
   }
  }
 },

 defaultError:function() {
//  alert('Error:'+this.reqReadyState + '; '+this.req.status + '; '+this.req.getAllResponseHeaders());
 }
};

function reload() {
	this.location = location;
}
