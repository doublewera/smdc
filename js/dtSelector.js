/* <table border="0" width="100%">

insertDtSelector(id_where, "", "_begin_select")
<tr><td>
<select name="xdy0" id="day_begin_select"><option value="0"></option></select>
<select name="xmn0" id="month_begin_select" onchange="javascript:populate_begin()"><option value="0"></option></select>
<select name="xyr0" id="year_begin_select" onchange="javascript:populate_begin()"><option value="0"></option></select>
<select name="xhr0" id="hour_begin_select"><option value="0"></option></select>:
<select name="xmi0" id="minute_begin_select"><option value="0"></option></select>
</td></tr>

insertDtSelector(id_where, "", "_end_select")
<tr><td>
<select name="xdy1" id="day_end_select"><option value="0"></option></select>
<select name="xmn1" id="month_end_select" onchange="javascript:populate_end()"><option value="0"></option></select>
<select name="xyr1" id="year_end_select" onchange="javascript:populate_end()"><option value="0"></option></select>
<select name="xhr1" id="hour_end_select"><option value="0"></option></select>:
<select name="xmi1" id="minute_end_select"><option value="0"></option></select>
</td></tr> */

function insertDtSelector(id_where, params) {
  document.getElementById(id_where).appendChild(
    createDtSelector(params)
  );
}

var nm_month =  ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
var len_month = [31,29,31,30,31,30,31,31,30,31,30,31];

function createDtSelector(params) {
// params = {"defaultdt", "uniquePrefix", "uniqueSuffix", "maxyear", "minyear", "dtvalues" : "year,month,day,hour,minute,second"}
  var dtdiv = document.createElement("div");
  var dtvars = params["dtvalues"].split(",");
  for (var i in dtvars) {
    var dtvar = document.createElement("select");
    dtvar.setAttribute("id", dtvars[i] + params["uniqueSuffix"]);
    dtdiv.appendChild(dtvar);
  }
  return dtdiv;
}

// year_, month_ and day_ begin/end must be defined elsewhere

// Read a page's GET URL variables and return them as an associative array.
function getUrlVars()
{
    var vars = [], hash;
    var hashes = window.location.href.slice(window.location.href.indexOf('?') + 1).split('&');

    for(var i = 0; i < hashes.length; i++)
    {
        hash = hashes[i].split('=');
        vars.push(hash[0]);
        vars[hash[0]] = hash[1];
    }

    return vars;
}

function padout(number) { return (number < 10) ? '0' + number : number; }

function inittt(c) {

 if(!Array.indexOf){
	    Array.prototype.indexOf = function(obj){
	        for(var i=0; i<this.length; i++){
	            if(this[i]==obj){
	                return i;
	            }
	        }
	        return -1;
	    }
 }

 var c = (c == null) ? '' : c;
 document.forms.request_data.elements[c+'month_begin_select'].options.length = 0;
 document.forms.request_data.elements[c+'month_end_select'].options.length = 0;
 document.forms.request_data.elements[c+'day_begin_select'].options.length = 0;
 document.forms.request_data.elements[c+'day_end_select'].options.length = 0;
 document.forms.request_data.elements[c+'hour_begin_select'].options.length = 0;
 document.forms.request_data.elements[c+'hour_end_select'].options.length = 0;
 document.forms.request_data.elements[c+'year_begin_select'].options.length = 0;
 document.forms.request_data.elements[c+'year_end_select'].options.length = 0;

 for(i=0; i<=year_end-year_begin; i++) {
	var o = new Option(year_begin+i,year_begin+i);
  o.id = year_begin+i;
	o.name = year_begin + i;
  document.forms.request_data.elements[c+'year_begin_select'].options[i] = o;
	var o = new Option(year_begin+i,year_begin+i);
  o.id = year_begin+i;
	o.name = year_begin + i;
  document.forms.request_data.elements[c+'year_end_select'].options[i] = o;
 }
 for(i=0;i<nm_month.length; i++) {
	var o = new Option(nm_month[i], i);
  o.id = padout(i + 1);
  o.name = padout(i + 1);
  document.forms.request_data.elements[c+'month_begin_select'].options[i] = o;
	var o = new Option(nm_month[i], i);
  o.id = padout(i + 1);
  o.name = padout(i + 1);
  document.forms.request_data.elements[c+'month_end_select'].options[i] = o;
 }
 for(i=0;i<24; i++) {
	var o = new Option(i, i);
  o.id = padout(i);
  o.name = padout(i);
  document.forms.request_data.elements[c+'hour_begin_select'].options[i] = o;
	var o = new Option(i, i);
  o.id = padout(i);
  o.name = padout(i);
  document.forms.request_data.elements[c+'hour_end_select'].options[i] = o;
 }
 for(i=0;i<60; i++) {
	var o = new Option(i, i);
  o.id = padout(i);
  o.name = padout(i);
  document.forms.request_data.elements[c+'minute_begin_select'].options[i] = o;
	var o = new Option(i, i);
  o.id = padout(i);
  o.name = padout(i);
  document.forms.request_data.elements[c+'minute_end_select'].options[i] = o;
 }
 if ((year_end) && (year_begin)) {
  document.forms.request_data.elements[c+'year_begin_select'].options[year_end - year_begin].selected = true;
  document.forms.request_data.elements[c+'year_end_select'].options[year_end - year_begin].selected = true;
 }
 if (month_end) {
  document.forms.request_data.elements[c+'month_begin_select'].options[month_end].selected = true;
  document.forms.request_data.elements[c+'month_end_select'].options[month_end].selected = true;
 }
 populate_begin(c);
 populate_end(c);
 document.forms.request_data.elements[c+'day_begin_select'].options[day_end].selected = true;
 document.forms.request_data.elements[c+'day_end_select'].options[day_end].selected = true;
 document.forms.request_data.elements[c+'hour_begin_select'].options[hour_end - 1].selected = true;
 document.forms.request_data.elements[c+'hour_end_select'].options[hour_end].selected = true;

	var vars = getUrlVars();
  var frm = document.forms.request_data;

	if (vars['yr0']) {
		frm.elements[c+'year_begin_select'].options.namedItem(vars["yr0"]).selected = true;
		frm.elements[c+'year_end_select'].options.namedItem(vars["yr1"]).selected = true;
		frm.elements[c+'month_begin_select'].options.namedItem(vars["mn0"]).selected = true;
		frm.elements[c+'month_end_select'].options.namedItem(vars["mn1"]).selected = true;
		frm.elements[c+'day_begin_select'].options.namedItem(vars["dy0"]).selected = true;
		frm.elements[c+'day_end_select'].options.namedItem(vars["dy1"]).selected = true;
		frm.elements[c+'hour_begin_select'].options.namedItem(vars["hr0"]).selected = true;
		frm.elements[c+'hour_end_select'].options.namedItem(vars["hr1"]).selected = true;
	}

 if (window.apevtest) {
  document.getElementById("year_end_select").options[year_end - year_begin].selected = true;
  document.getElementById("month_end_select").options[month_end].selected = true;
 }
 if (window.apevtest) {
  document.getElementById("day_end_select").options[day_end].selected = true;
 }

	if (typeof createSmallMap == "function") {
    createSmallMap();
  }
  if (typeof createMap == "function") {
    createMap();
  } 
}

function populate_begin(c) {
 var c = (c == null) ? '' : c;
 var selectedDay = 0;
 if (document.forms.request_data.elements[c+'day_begin_select'].options.length) {
  selectedDay = document.forms.request_data.elements[c+'day_begin_select'].selectedIndex;
  document.forms.request_data.elements[c+'day_begin_select'].options.length = 0;
 }
 var c_year, c_month;
 for (i=0; i<document.forms.request_data.elements[c+'month_begin_select'].options.length; i++) {
  if (document.forms.request_data.elements[c+'month_begin_select'].options[i].selected) {
   c_month = i;
  }
 }
 for (i=0; i<document.forms.request_data.elements[c+'year_begin_select'].options.length; i++) {
  if (document.forms.request_data.elements[c+'year_begin_select'].options[i].selected) {
   c_year = document.forms.request_data.elements[c+'year_begin_select'].options[i];
  }
 }
 for (i=0; i<len_month[c_month]; i++) {
   if (!(i==28 && c_month==1 && (c_year.value%4!=0 || (c_year.value%100==0 &&  c_year.value%400!=0)) )) {
		var o = new Option(i + 1, i + 1);
  	 o.id = padout(i + 1);
	  o.name = padout(i + 1);
    document.forms.request_data.elements[c+'day_begin_select'].options[i] = o ; //new Option(i+1,i+1);
   }
  }
 if (document.forms.request_data.elements[c+'day_begin_select'].options.length > selectedDay) {
  document.forms.request_data.elements[c+'day_begin_select'].selectedIndex = selectedDay;
 }
 else {
  document.forms.request_data.elements[c+'day_begin_select'].selectedIndex = document.forms.request_data.elements[c+'day_begin_select'].options.length - 1;
 }
 if (document.forms.request_data.elements[c+'day_end_select'].options.length == 0 || document.forms.request_data.elements[c+'month_end_select'].options.length == 0 || document.forms.request_data.elements[c+'year_end_select'].options.length == 0)
	return;
 if (document.forms.request_data.elements[c+'year_begin_select'].selectedIndex > document.forms.request_data.elements[c+'year_end_select'].selectedIndex)
 	document.forms.request_data.elements[c+'year_end_select'].selectedIndex = document.forms.request_data.elements[c+'year_begin_select'].selectedIndex;	
 if ((document.forms.request_data.elements[c+'month_begin_select'].selectedIndex > document.forms.request_data.elements[c+'month_end_select'].selectedIndex) 
    && (document.forms.request_data.elements[c+'year_end_select'].selectedIndex == document.forms.request_data.elements[c+'year_begin_select'].selectedIndex)) {
 	document.forms.request_data.elements[c+'month_end_select'].selectedIndex = document.forms.request_data.elements[c+'month_begin_select'].selectedIndex;
 }
}

function populate_end(c) {
 var c = (c == null) ? '' : c;
// document.forms.request_data.elements[c+'day_end_select'].options.length = 0;

 var selectedDay = 0;
 if (document.forms.request_data.elements[c+'day_end_select'].options.length) {
  selectedDay = document.forms.request_data.elements[c+'day_end_select'].selectedIndex;
  document.forms.request_data.elements[c+'day_end_select'].options.length = 0;
 }

 var c_year, c_month;
 for (i=0; i<document.forms.request_data.elements[c+'month_end_select'].options.length; i++) {
  if (document.forms.request_data.elements[c+'month_end_select'].options[i].selected) {
   c_month = i;
  }
 }
 for (i=0; i<document.forms.request_data.elements[c+'year_end_select'].options.length; i++) {
  if (document.forms.request_data.elements[c+'year_end_select'].options[i].selected) {
   c_year = document.forms.request_data.elements[c+'year_end_select'].options[i];
  }
 }
 for (i=0; i<len_month[c_month]; i++) {
   if (!(i==28 && c_month==1 && (c_year.value%4!=0 || (c_year.value%100==0 &&  c_year.value%400!=0)) )) {
		var o = new Option(i + 1, i + 1);
  	 o.id = padout(i + 1);
	  o.name = padout(i + 1);
    document.forms.request_data.elements[c+'day_end_select'].options[i] = o;
   }
  }

 if (document.forms.request_data.elements[c+'day_end_select'].options.length > selectedDay) {
  document.forms.request_data.elements[c+'day_end_select'].selectedIndex = selectedDay;
 }
 else {
  document.forms.request_data.elements[c+'day_end_select'].selectedIndex = document.forms.request_data.elements[c+'day_end_select'].options.length - 1;
 }
}
