function showmenu(element){
   var DivRef = document.getElementById(element);
   var IfrRef = document.getElementById(element+'i');
   if (navigator.userAgent.indexOf("MSIE 6.0") != -1) {
	DivRef.className = "popupie6";
   }
   DivRef.style.visibility = "visible";
   DivRef.style.display = "block";
   IfrRef.width = DivRef.offsetWidth;
   IfrRef.height = DivRef.offsetHeight;
   IfrRef.style.top = DivRef.style.top;
   IfrRef.style.left = DivRef.style.left;
   IfrRef.style.zIndex = 10000;
   DivRef.style.zIndex = 10005;
   if (navigator.userAgent.indexOf("MSIE 6.0") != -1) {
   	IfrRef.style.display = "block";
   }
}

function hide(element){
   document.getElementById(element).style.visibility = "hidden";
   document.getElementById(element).style.display = "none";
   document.getElementById(element+'i').style.display = "none";
}
