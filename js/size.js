function getSize(dimension) {
  var w = 0, h = 0;
  if( typeof( window.innerWidth ) == 'number' ) {
    //Non-IE
    w = window.innerWidth;
    h = window.innerHeight;
  } else if( document.documentElement && ( document.documentElement.clientWidth || document.documentElement.clientHeight ) ) {
    //IE 6+ in 'standards compliant mode'
    w = document.documentElement.clientWidth;
    h = document.documentElement.clientHeight;
  } else if( document.body && ( document.body.clientWidth || document.body.clientHeight ) ) {
    //IE 4 compatible
    w = document.body.clientWidth;
    h = document.body.clientHeight;
  }
  if (dimension) {
   return w;
  }
  else {
    return h;
  }
}

function docHeight() {
	return getSize(0);
}

function elmHeight(element) {
	return document.getElementById(element).offsetHeight;
}

function totalWidth() {
	frameWidth = 0;
if (self.innerWidth) {
	frameWidth = self.innerWidth;
}
else if (document.documentElement && document.documentElement.clientWidth) {
	frameWidth = document.documentElement.clientWidth;
}
else if (document.body) {
	frameWidth = document.body.clientWidth;
}
 return frameWidth - 256;
}
