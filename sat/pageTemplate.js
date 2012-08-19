// js/jquery-1.3.js REQUIRED
// js/development-bundle/ui/jquery.ui.datepicker.js REQUIRED

function retrieve(noradid) {
    //var msg = {"src": appid, "dst": "satellitedb"};
    var channels = [];
    var coords = [];
    $("input:checked").each(function() {
        if (this.id.substr(0, 4) == "chan") {
            channels.push(this.id.substr(4, this.id.length));
        }
        else if(this.id.substr(0, 5) == "coord") {
            coords.push(this.id.substr(5, this.id.length));
        }
    });
    var dateFrom = $("#dtintFrom").datepicker("getDate");
    var dateTill = $("#dtintTill").datepicker("getDate");
    dateFrom = dateFrom.getFullYear() + "-" + padout(dateFrom.getMonth() + 1) + "-" + padout(dateFrom.getDate());
    dateTill = dateTill.getFullYear() + "-" + padout(dateTill.getMonth() + 1) + "-" + padout(dateTill.getDate());
    var hhFrom = $('#hhFrom').val();
    var mmFrom = $('#mmFrom').val();
    var ssFrom = $('#ssFrom').val();
    var hhTill = $('#hhTill').val();
    var mmTill = $('#mmTill').val();
    var ssTill = $('#ssTill').val();
    var timeFrom = hhFrom + ":" + mmFrom + ":" + ssFrom;
    var timeTill = hhTill + ":" + mmTill + ":" + ssTill;

    var params = "satellites=" + noradid;
    params += "&instruments=" + selectedInstrument;
    params += "&dtFrom=" + dateFrom + " " + timeFrom;
    params += "&dtTill=" + dateTill + " " + timeTill;
    params += "&channels="  + channels;
    params += "&coords=" + coords;

    // msg["channels"] = channels;
    // msg["coords"] = coords;
    // msg["dt_record"] = [[dateFrom + " " + timeFrom, dateTill + " " + timeTill]];
    // alert(msg);
    // alert(params);
    $.get(
        "sat/db.py/getData?" + params,
        function(data) {
            document.getElementById("responsedata").innerHTML = data;
        }
    );
}

var sections = new Array('Overview','Data','Charts');
var selectedInstrument = "";

function fillPages(id_where, noradid) {
    var wheretoput = document.getElementById(id_where);
    wheretoput.innerHTML = "<h3 class='separator' align='center' id='SAT_HEADER'> </h3><div id='linkstbl' />";
    $.get(
        "sat/db.py/info?id=" + noradid + "&silos=name",
        function(data) {document.getElementById("SAT_HEADER").innerHTML = data;}
    );

    var linkstbl = document.getElementById("linkstbl");
    for (var i in sections) {
        var idprefix = sections[i];
        if (idprefix != "Data") {
            lbrct = document.createTextNode("[");
            ahref = document.createElement("a");
            ahref.setAttribute("id", idprefix + "_caller");
            ahref.setAttribute("href", "javascript:switchdiv('" + sections[i] + "')");
            ahref.innerHTML = sections[i];
            rbrct = document.createTextNode("]");
            linkstbl.appendChild(lbrct);
            linkstbl.appendChild(ahref);
            linkstbl.appendChild(rbrct);
        }

        var divpage = document.createElement("div");
        divpage.setAttribute("id", sections[i]);
        wheretoput.appendChild(divpage);
    }
    $("#Overview").load("sat/nid/" + noradid + ".html");

    var tablecode = '<table id="dataTable"><tr><td valign="top" id="requestform" style="width:23em"> \
        <form id="request_data"> \
        <table border="0" id="dtint" width="100%"> \
        <tr><td colspan="3" class="separator"><i>Select <b>Date and Time</b> below and click </i>\
        <input value="retrieve" type="button" onclick="javascript:retrieve(' + noradid + ');" /> \
        </td></tr> \
        <tr><th align="right"><label for="dtintFrom">From</label></th> \
            <td><input id="dtintFrom" name="dtintFrom" type="text" style="border:none"/></td> \
            <td><select id="hhFrom" name="hhFrom" style="background: transparent;border:none;"> </select> \
             <b>:</b> <select id="mmFrom" name="mmFrom" style="background: transparent;border:none"> </select> \
             <span style="display:none"> \
              <b>:</b> <select id="ssFrom" name="ssFrom" style="background: transparent;border:none"> </select> \
             </span> \
            </td> \
        </tr> \
        <tr><th align="right"><label for="dtintTill">Till</label></th> \
            <td><input id="dtintTill" name="dtintTill" type="text" style="border:none"/></td> \
            <td><select id="hhTill" name="hhTill" style="background: transparent;border:none"> </select> \
             <b>:</b> <select id="mmTill" name="mmTill" style="background: transparent;border:none"> </select></td> \
             <span style="display:none"> \
              <b>:</b> <select id="ssTill" name="ssTill" style="background: transparent;border:none"> </select> \
             </span> \
            </td> \
        </tr> \
        </table> \
        </form> \
        </td><td valign="top" id="responsedata">&nbsp;</td></tr></table>'

    document.getElementById("Data").innerHTML= tablecode;
    var form = document.getElementById("request_data");
    $.get(
        "sat/db.py/info?id=" + noradid + "&silos=firstdata,lastdata",
        function(data) {
            var datetimeArray = parseDtArray(data);
            var dtftarr = ['From', 'Till'];
            var dtnow = new Date();
            if (datetimeArray[1]) {
                dtnow = datetimeArray[1];
            }
            for (var dti in dtftarr) {
                var dtid = 'dtint' + dtftarr[dti];
                $("#" + dtid).datepicker({
                    changeMonth: true,
                    changeYear: true,
                    duration: 'fast',
                    dateFormat: 'M, d, yy',
                    minDate: datetimeArray[0],
                    maxDate: dtnow,
                    firstDay: 1
                });
            }
            $("#dtintFrom").datepicker("setDate", dtnow);
            $("#dtintTill").datepicker("setDate", dtnow);
            populatetime();
            document.getElementById("hhTill").options[1].selected = true;
        }
    );
    $.get("sat/db.py/form?satellites=" + noradid,
        function(data) {
            var newdiv = document.createElement('div');
            newdiv.innerHTML = data;
            form.appendChild(newdiv);
            var instrlist = document.getElementById("instruments" + noradid);
            var firstInstr = 0;
            for (var i in instrlist.childNodes) {
                if (instrlist.childNodes[i].childNodes != undefined) {
                var inname = instrlist.childNodes[i].childNodes[0].id;
                sections.push(inname);
                lbrct = document.createTextNode("[");
                ahref = document.createElement("a"); 
                ahref.setAttribute("id", inname + "_caller");
                ahref.setAttribute("href", "javascript:switchsubdiv('Data','" + inname + "')");
                ahref.innerHTML = inname;
                rbrct = document.createTextNode("]");
                var linkstbl = document.getElementById("linkstbl");
                linkstbl.appendChild(lbrct);
                linkstbl.appendChild(ahref);
                linkstbl.appendChild(rbrct);
                }
            }
            selectedInstrument = instrlist.childNodes[0].childNodes[0].id;
            switchsubdiv('Data', selectedInstrument);
        }
    );
}

function padout(number) { return (number < 10) ? '0' + number : number; }

function populatetime() {
    var FT = ["From", "Till"];
    for (var k in FT) {
      for (var i=0; i<24; i++) {
        var o = new Option(padout(i), padout(i));
        o.id = padout(i);
        o.name = padout(i);
        document.getElementById('hh' + FT[k]).options[i] = o;
      }
      $('#hh' + FT[k])[0].selected = true;
    }
    var mmss = ["mm", "ss"];
    for (var i=0; i<60; i++) {
     for (var j in mmss) {
      for (var k in FT) {
       var o = new Option(padout(i), padout(i));
       o.id = padout(i);
       o.name = padout(i);
       document.getElementById(mmss[j] + FT[k]).options[i] = o;
      }
     }
    }
}

function parseDtArray(data) {
    var dtstrarr = data.split(",");
    var dtArr = [];
    for (i in dtstrarr) {
        if (dtstrarr[i] === "None") {
            dtArr[i] = null;
        } else {
            var ymdhms = dtstrarr[i].split(" ");
            var ymd = ymdhms[0].split("-");
            var MM = parseInt(ymd[1]);
            if (MM == 0) MM = parseInt(ymd[1][1]);
            var DD = parseInt(ymd[2]);
            if (DD == 0) DD = parseInt(ymd[2][1]);

            var hms = ymdhms[1].split(":");
            var hh = parseInt(hms[0]);
            if (hh == 0) hh = parseInt(hms[0][1]);
            var mm = parseInt(hms[1]);
            if (mm == 0) mm = parseInt(hms[1][1]);
            var ss = parseInt(hms[2]);
            if (ss == 0) ss = parseInt(hms[2][1]);
            var dt = new Date(parseInt(ymd[0]), MM-1, DD, hh, mm, ss);
            dtArr[i] = dt;
        }
    }
    return dtArr;
}
