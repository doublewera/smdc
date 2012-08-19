      PROGRAM LINE_O
      DIMENSION x(3), bm(3), bimf(3), bb(7,3)
c      call pstatus(1.,1.,1.,1.,1.,1.,1.) 
      data sl ,   h                                                             
     *     /150., -0.25/    
      open (20,file='in/inf')
      open (21,file='in/infile')
      kpr=0
c      bimf(1)=3.
c      bimf(2)=0.
c      bimf(3)=-10.9
c      ut=9
c      iy=1998
c      mo=9
c      id=25
c      ro=6.1
c      s=400.
c      dst=-20.
c      al=-471.
      x(1)=8. 
      x(2)=0.01                                                                 
      x(3)=0.  
      
      read (20,*) ut,iy,mo,id
      read (21,*) ro,s,dst,al,bimf
                                                 
      call a2000f(ut,iy,mo,id,ro,s,bimf,dst,al,x,bm,bb)                         
      call a2000f_lineXZ(ut,iy,mo,id,ro,s,bimf,dst,al,x,sl,h,kpr)
 10   continue
 100  format(1x,3f12.3)
      stop
      end
      
      subroutine a2000f_lineXZ(ut,iy,mo,id,ro,s,bimf,dst,al,x0,sl,h,kpr)
*-----------------------------------------------------------------
*  Calculation of magnetic field lines in GSM coordinates in XZ plane
*  at time moment UT (Universal Time) on
*  year=iy;
*  day=id in month=mo.
*  ro, s(V), bimf - are solar wind density and velocity, and IMF.
*  dst - value of Dst index;
*  AL  - value of al index. 
*  bm=0 when point x(3) is outside the magnetosphere
*                    sl, h  - the maximum length of the magnetic field line     
*                             and step along the field line, Re;                
*                    kpr -    provides the control of the field line printing   
*                             (1 - enabled, 0 - disabled).                      
* NOTE: The resulting field lines is writing in file output.dat                  
*       in the working directory separated by blank line.                                               
* WARNING: Because of the paraboloid coordinates singularity, avoid             
*          the magnetic field calculations at the Ox axis and magnetic          
*          field line calculations on the y=0 plane. 
*          R1>X>-50                           
*                                                                               
* Written by V.Kalegaev                                                         
*-------------------------------------------------------------
       COMMON/A/Y(3),F(5),V(3),U(3),YF(3)
       COMMON/BSM/x0sm(3)
c      COMMON/SM/SSCP,SSP,simf,smd,ssd,ssr,smr,sbt
       COMMON/T2/PI,R1,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,bd,bd0
       DIMENSION x0(3), bimf(3), bm(3), par(10), bdd(7,3)
c       OPEN (1,file="in/open")
       OPEN (23,FILE="out/closed")
       
       call fstatus(1.,0.,1.,1.,1.,1.,1.,1.,1.,0.)  
c                   SSCP,SSP,simf,smd,ssd,ssr,smr,sbt,ss1,ss2

       
       call submod(ut,iy,mo,id,ro,s,bimf,dst,al,par)


       r1=par(6)
       IF(x0(1)+0.5/R1*(x0(2)**2+x0(3)**2).GT.R1) then
       do i=1,3
       bm(i)=0.
       do j=1,7
       bdd(j,i)=0.
       end do
       end do
       return
       end if
      
*  Step1 -        from Ionosphere
c       write (1,*)
c       print *, 'Step1'
       do 20 ii=1,24
       amlt=0.0+5.*ii
       do jj=1,8
       alat=(72.+2.*(jj))
       call Pi_line(amlt,alat,par,sl,h,kpr)
       rasst=Y(1)+0.5/r1*(Y(2)**2+Y(3)**2)
c       write (*,*) rasst, R1
c       if (Y(1)+0.5/R1*(Y(2)*Y(2)+Y(3)*Y(3)).ge.R1) then
c       write (22,*) x0sm
c       go to 20 
c       else
c       continue
c       end if
c       if (Y(1)+0.5/R1*(Y(2)*Y(2)+Y(3)*Y(3)).lt.R1) then
       if ((Y(1)**2+Y(2)**2+Y(3)**2).lt.1.) then
       write (23,*) x0sm
       else
       continue
       end if
       end do
   20  end do
       
126   format(2x,3f6.2,2x,4f10.1)
      return
      END

