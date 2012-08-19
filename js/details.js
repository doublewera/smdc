// sections must be defined in the appropriate server-side script code

function switchdiv(id){
	hidealldivs();
	showdiv(id);
	currDiv = id;
	/* KOSTYL */
	/* if (typeof createSmallMap == "function") {
		createSmallMap();
	}
	if (typeof createMap == "function") {
		createMap();
	} */
    if (id === "Data") { // special for satellites!
        if (selectedInstrument && (selectedInstrument != "")) {
            switchsubdiv(id, selectedInstrument);
        }
    }
}

function switchsubdiv(id0, id1){
	hidealldivs();
	showdiv(id0);
	showdiv(id1);
	currDiv = id0;
	currSubdiv = id1;
}

function hidealldivs(){
	for (var i=0;i<sections.length;i++){
		hidediv(sections[i]);
	}		  
}

function switchdiv2(id, array){
	hidealldivs2(array);
	showdiv(id);
}

function hidealldivs2(array){
	for (var i=0;i<array.length;i++){
		hidediv(array[i]);
	}		  
}

function hidediv(id) {
	if (document.getElementById) { 
		document.getElementById(id).style.display = 'none';
		if (document.getElementById(id+'_caller')) {
			document.getElementById(id+'_caller').style.color = '#0000ff';
		}
	}
}


function showdiv(id) {
	if (document.getElementById(id)) { 
		document.getElementById(id).style.display = 'block';
		if (document.getElementById(id+'_caller')) {
			document.getElementById(id+'_caller').style.color = '#ff0000';
		}
	}
}
