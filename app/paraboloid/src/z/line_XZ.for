      PROGRAM LINES
      DIMENSION x(3), bm(3), bimf(3), bb(7,3)
c      call pstatus(1.,1.,1.,1.,1.,1.,1.) 
      data sl ,   h                                                             
     *     /150., -0.25/   
      x(1)=8.
      x(2)=0.01
      x(3)=0.
      
      open (22,FILE='in/inf')
      open (23,FILE='in/infile')
      open (1,FILE='out/lineXYZ')
                                                                 
      read (22,*) ut, iy, mo, id                                                 
      read (23,*) ro,s,dst, al, bimf                                          
     
      call a2000f(ut,iy,mo,id,ro,s,bimf,dst,al,x,bm,bb)                         
      call a2000f_lineXZ(ut,iy,mo,id,ro,s,bimf,dst,al,x,sl,h,1)
 10   continue
      stop
      end
      subroutine a2000f_lineXZ(ut,iy,mo,id,ro,s,bimf,dst,al,x0,sl,h,kpr)
*-----------------------------------------------------------------
*  Calculation of magnetic field lines in GSM coordinates in XZ plane
*  at time moment UT (Universal Time) on
*  year=iy;
*  day=id in month=mo.
*  ro, S, bimf - are solar wind density and velocity, and IMF.
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
c      COMMON/SM/SSCP,SSP,simf,smd,ssd,ssr,smr,sbt
      COMMON/T2/PI,R1,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,bd,bd0
      DIMENSION x0(3), bimf(3), bm(3), par(10), bdd(7,3)
      rad=PI/180.
      call fstatus(1.,0.,1.,1.,1.,1.,1.,1.,1.,0.)  
c                   SSCP,SSP,simf,smd,ssd,ssr,smr,sbt,ss1,ss2

      call submod(ut,iy,mo,id,ro,s,bimf,dst,al,par)
c      open (1, file='out/output.dat')
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
      
      do ii=1,14
      amlt=0.01
      alat=64.+6.*(ii-1)
      call Pi_line(amlt,alat,par,sl,h,kpr)
      write (1,*)
      end do
   
      h=0.25
      do ii=1,10
      amlt=0.01
      alat=-54.-6.*(ii-1)
      call Pi_line(amlt,alat,par,sl,h,kpr)
      write (1,*)
      end do
      amlt=0.01
      alat=106.
      call Pi_line(amlt,alat,par,sl,h,kpr)
      write (1,*)
      alat=-108.
      call Pi_line(amlt,alat,par,sl,h,kpr)
      write (1,*)
      
      return
      END
