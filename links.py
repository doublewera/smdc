import re

DOCROOT = "/home/www/smdc/"

def links():
    str = ""
    f = open(DOCROOT+'meta/directory.conf','r')
    x = f.readline()
    csect = 'default'
    sections = {}
    while (x !=''):
        p = re.compile('\[[\w\s]+\]')
        if p.match(x):
            csect = x[1:-2]
        p = re.compile('".+":"[\w\S]+"')
        if p.match(x):
            sections.setdefault(csect,[]).append(x)
        x = f.readline()
    f.close()
    allLinks = 'Search: <input type="text" id="searchbox" /><br/>'
    if len(sections)!=0:
        keys = sections.keys()
        for k in keys:
            sections[k].sort()
        str+="""<script src="js/details.js" type="text/javascript"> </script>
                <script src="js/jquery-1.3.js" type="text/javascript"> </script>
                <script type="text/javascript">

        //<!-- 

        $(document).ready(function () {
	    jQuery.expr[':'].Contains = function(a,i,m){
	      return (a.textContent || a.innerText || "").toUpperCase().indexOf(m[3].toUpperCase())>=0;
	    };
            $("#searchbox").keyup(function() {
                text = document.getElementById("searchbox").value;
                allRoot = document.getElementById("all");
                $("#all a:Contains("+ text +")").css("display", "inline");
                $("#all a:not(:Contains("+ text +"))").css("display", "none");
                $("#all a:Contains("+ text +") + br").css("display", "inline");
                $("#all a:not(:Contains("+ text +")) + br").css("display", "none");
            });
        });

        var sections=new Array("""
        str+="'all',"
        for x in sections.keys():
            if x!=sections.keys()[0]:
                str+=','
            str+="'"+re.sub('\s','_',x)+"'"
        str+=");\n //--> \n</script>"    
        str+='</td></tr><tr><td colspan="2"><table><tr><td width="15%" valign="top">'
        str+='[<a id="all_caller" href="javascript:switchdiv(\'all\')">All links</a>]'
        for x in sections.keys():
            str+=' [<a id="'+re.sub('\s','_',x)+'_caller" href="javascript:switchdiv(\''+re.sub('\s','_',x)+'\')">'+re.sub('\s',' ',x)+'</a>]'
        str+='<hr />\n'
        for x in sections.keys():
            str+='\n<div id="'+re.sub('\s','_',x)+'" style="display:none">'
            str+='<div class="separator"><h3>'+x+'</h3></div>'
            allLinks+='<br/><div class="separator"><b>%s</b></div>'%x
            for z in sections[x]:
                p = re.compile('".+":')
                m = p.match(z)
                text = m.group()[1:-2]
                text = re.sub('&','&amp;',text)
                p = re.compile(':"[\w\S]+"')
                m = p.search(z)
                href = m.group()[2:-1]
                str+="\n<a href=\""+href+"\">"+text+"</a><br />"
                allLinks += "\n<a href=\""+href+"\">"+text+"</a><br />"
            str+='</div>'
        str+="</td></tr>\n</table>"
        str+='<div id="all" style="display:none"><div class="separator"><h3>All links</h3></div>%s</div>' % allLinks
    str += """<hr />
    <p>This page also contains links to manuals and user guides for software routinely used in SINP.
	More links will be added as the need arises.</p>
    <h3>FORTRAN</h3>
    <ul>
    <li><a href="http://www.star.le.ac.uk/~cgp/prof77.html">Professional Programmer's Guide to Fortran77</a></li>
    <li><a href="http://www.idris.fr/data/cours/lang/fortran/f90/F77.html">Fortran 77 for beginners</a></li>
    <li><a href="/doc/Fortran90UsersGuide.pdf">Fortran90 user's guide</a></li>
    <li><a href="/doc/UserGuide.pdf">Pathscale compiler user guide</a></li>
    </ul>
    """
    return str


