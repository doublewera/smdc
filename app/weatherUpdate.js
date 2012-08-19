var refreshSeconds = 15;
var refreshMinutes = 0;

function refreshData() {
  var currTime = new Date();
  var currMinute = currTime.getMinutes();
  if (!updated) {
    updated = 1;
    //window.location.reload();
    $.get('http://'+document.domain+'/smdc/app/rss.py/tables', 
       function(data) {
          document.getElementById('realtimetables').innerHTML = data;
          refreshData();
        }
    );
  }
  if (currMinute == 59) {
    updated = 0;
  }
  moveBlueString();
  setTimeout(refreshData, refreshSeconds*1000);
}

function moveBlueString() {
  var currTime = new Date();
  var currMinute = currTime.getMinutes();
  if (currMinute > 0) {
    currMinute -= 1;
  }
  else {
    currMinute = 59;
  }
  var prevStr = document.getElementById("td1_" + currMinute.toString());
	if (prevStr != null) { // not for main page
	  prevStr.style.backgroundColor="#FFFFFF";
	  if (currMinute < 59) {
	    currMinute += 1;
	  }
	  else {
	    currMinute = 0;
	  }
	  var currStr = document.getElementById("td1_" + currMinute.toString());
	  currStr.style.backgroundColor="#BDD1FF";
      document.getElementById("currentRssMove").innerHTML = "Rss = " + currStr.cells[1].innerHTML;
      document.getElementById("currentPswMove").innerHTML = "Psw = " + currStr.cells[2].innerHTML;
      document.getElementById("currentBzMove").innerHTML = "Bz = " + currStr.cells[5].innerHTML;
    updateRssValue(currStr.cells[1].innerHTML);
	  moveYellowLine(
        currStr.cells[0].innerHTML.split(":")[0],
        currStr.cells[0].innerHTML.split(":")[1],
        currStr.cells[0].innerHTML
      );
	}
}

function updateRssValue(val) {
	var rssValue = document.getElementById("RssCalc");
	rssValue.innerHTML = val;
}

//function updateRssTables() {
//  document.getElementById('realtimetables').innerHTML = loader.req.responseText;
//  refreshData();
//}

/* Vertical time mark at the magnetosphere monitor pictures */
function moveYellowLine(hours, minutes, caption) {
  var currTime = new Date();
  var halfstrlen = 15;
  var begintime = 55;
  var endtime = 310;

  document.getElementById("currUT").innerHTML = caption; //hours + ":" + minutes;
  document.getElementById("yellowLineHourlyComment").innerHTML = caption + " UT<sub>ACE</sub>"; //hours + ":" + minutes;
  minutes = parseInt(minutes);
  /* for reloading pictures, because they are old */
  var ct = (endtime - begintime) * minutes /60 + begintime;
  document.getElementById("yellowLineHourly").style.left = ct.toString() + "px";
  if (caption == "--:--") {
    document.getElementById("yellowLineHourly").style.left = "-1000px";
  }

  /* for keeping the note "current ACE time" at the beginning of the graphic until yellow line reach the middle of it */
//  if(minutes > refreshMinutes) {
    ct -= halfstrlen;
//  }
  var movingLineLeft = ct.toString() + "px";
  if (caption == "--:--") {
    movingLineLeft = "-1000px";
  }
  document.getElementById("yellowLineHourlyComment").style.left = movingLineLeft;
  document.getElementById("currentRssMove").style.left = movingLineLeft;
  document.getElementById("currentPswMove").style.left = movingLineLeft;
  document.getElementById("currentBzMove").style.left = movingLineLeft;

/*  var currTime = new Date();
  var currMinute = currTime.getMinutes();
  var currHour = currTime.getHours();*/
//  ct = (endtime - begintime) * (currTime.getHours() + minutes / 60.0)/24 + begintime;
//  document.getElementById("yellowLineDaily").style.left = ct.toString() + "px";
}
