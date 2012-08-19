#!/usr/bin/python3

from mod_python import apache
from os import system

def index(req):
    thepath = "/home/www/smdc/app/paraboloid/"
    f = open("%s/in/inf" % thepath, "w");
    f.write(" ".join(map(lambda n : req.form.get(n, ''), "ut,iy,mo,id,ro,v,bz,dst,al,x,y,z".split(","))))
    f.close()
    system("%s/msph" % thepath)
    result = ""
    if req.form.get('graph', '') == "E":
        gnuplotfile = """   
            set nokey
            set title  'Distribution of Bmean along Sun-Earth line'
            set terminal gif
            set output '%s/out/figures/%i.gif'
            set title  'DISTRIBUTION OF Bmean ALONG SUN-EARTH LINE '
            set xlabel 'Distance, Earths radii'
            set ylabel 'Bmean, nT'
            plot       'in/model' with lines lt -1
            quit\n"""
        i = 69
        f = open("%s/out/m.plt" % thepath, "w")
        f.write(gnuplotfile % (thepath, i))
        f.close()
        system("/usr/bin/gnuplot %s/in/m.gnu" % (thepath))
        f = open('%s/out/figures/%i.gif' % (thepath, i))
        result = f.read()
        f.close()
        #system("rm -f %s/out/figures/*" % thepath);
    else:
        req.content_type = "text/html"
        result = """<table class="simpletable">
            <tr>
            <td>X,Y,Z</td>
            <td width=150>%s</td>
            <td width=150>%s</td>
            <td width=150>%s</td>
            </tr>
            <tr>
            <td>Bx,By,Bz</td>  
            <td width=150>%s</td>
            <td width=150>%s</td>
            <td width=150>%s</td>
            </tr>   
            </table>"""
        f = open("%s/out/outf" % thepath)
        result = result % tuple([req.form.get('x', " "),
                                 req.form.get('y', ' '),
                                 req.form.get('z', '')] + f.read().strip().split())
        f.close()
    return result #"<p>" + str(req.form.keys()) + " " + req.form.get("xuuu2", '') + "</p>"
