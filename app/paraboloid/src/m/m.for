      PROGRAM PARAB_MODEL
      DIMENSION x(3), bm(3)
      open(1, file='in/inf')
      open(2, file='out/outf')      
      open(3, file='in/model')
      read (1,*) ut,iy,mo,id,ro,v,bz,dst,al,x(1),x(2),x(3)
      call a99_l(ut,iy,mo,id,ro,v,bz,dst,al,x,bm)
      write (2,*) bm
      do 10 i=1,51
      xx = 1.0 + 0.2*(i-1)
      x(1) = xx
      call a99_l(ut,iy,mo,id,ro,v,bz,dst,al,x,bm)
      sred = sqrt(bm(1)**2 + bm(2)**2 + bm(3)**2)
      write (3,77) xx, sred
   10 continue
   77 format (f7.2,3x, f7.2)
      stop
      end
