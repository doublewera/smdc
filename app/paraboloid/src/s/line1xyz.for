      subroutine s_line(UT,ID,im,iyear,x0,par,sl,h,ipr,kint,kext)
*****************************************************************
*  Calculation of the magnetic field line in the magnetosphere of Saturn.
*  Ver.3 (28.09.2005)
*
*  INPUT PARAMETERS: x0(3) is a point where the magnetic field is being
*                          calculated, in GSM coordinates, Re;
*                    par(1) - geomagnetic dipole tilt angle, degrees;	     *
*                    par(2) - magnetic field at MD outer edge, nT;	     *
*                    par(3) - magnetic field at TC inner edge, nT;           *
*                    par(4) - distance to the external edge of MD;           *
*                    par(5) - distance to the inner edge of MD;	             *
*                    par(6) - magnetopause stand-off distance, Re;	     *
*                    par(7) - distance to the inner edge of geotail 	     *
*                             current sheet;				     *
*                    par(8) - the total current of Region 1 FAC, MA;	     *
*                    par(9-11) -IMF penetrated components in GSM coord., nT. *
*                    sl, h  - the maximum length of the magnetic field line 
*                             and step along the field line, Re;
*                    ipr -    provides the control of the field line printing 
*                             (1 - enabled, 0 - disabled).
*    kint=-1,0,+1 - +3 /int. field: dipole with psi=0.; =0.; int. of order kint/
*    kext=0,+1/external field: NO; YES/
* NOTE: The resulting field line is writing in file output.dat 
*       in the working directory.
* WARNING: Because of the paraboloid coordinates singularity, avoid
*          the magnetic field calculations at the Ox axis and magnetic 
*          field line calculations on the y=0 plane.
*
* Written by V.Kalegaev
*****************************************************************
      COMMON/TFAC/STM,CTM,BFAC0,BFAC1,TETAM,AJ0
      COMMON/TK/B1(3),B2(3),B1A(3),B1R(3),B2A(3),B2R(3),
     *BA(3),AB,B(3),BIMF(3),BDISKX(3),bint(3),BDISK1X(3)
      COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD

      DIMENSION X0(3), FF(3), bm(3), par(11)

      psi=par(1)  
      bdc11=par(2)  !magnetic field at MD outer edge
      bt=par(3)    ! magnetic field at TC inner edge
      rd1=par(4)   ! distance to the external edge of MD
      rd2=par(5)   ! distance to the inner edge of MD
      R1=par(6)    ! magnetopause stand-off distance
      R2=par(7)    ! distance to the inner edge of geotail current sheet
      aj0=par(8)    ! maximum of Region 1 FAC intensity
      do 2 i=1,3
2     bimf(i)=par(i+8) ! IMF components
          RKM=6.
          TETAM=20.
      if (kint.eq.-1) then					    
c      psi=0.				      
c      bd=2.12321e4
      bd=2.11600e4
      tpsi=psi*pi/180.					    
      spsi=sin(tpsi)						    
      cpsi=cos(tpsi)
      goto 4
      end if	

      call strans (UT,ID,im,iyear,tpsi,BD)				      
c        print *, 'line', id
      psi=tpsi
      tpsi=tpsi*pi/180.					    
      spsi=sin(tpsi)						    
      cpsi=cos(tpsi)						    

4       CALL MAS2D
        call line(UT,ID,im,iyear,X0,sl,h,ipr,kint,kext)
c      print *, tpsi, spsi,cpsi
      return
      END
C
      SUBROUTINE line(UT,ID,im,iyear,X0,sl,h,iprint,kint,kext)
********************************************************************
* Program calculating a magnetic field line or drift trajectory.
*
* INPUT PARAMETERS: X0 is the starting point of the magnetic field line, 
*                   in GSM coordinates, Re; 
*                   sl,h are the maximum length of field line and step 
*                        along the field line in Earth's radii; 
*                   iprint  provides the control of the field line printing 
*                        (1 - enabled, 0 - disabled).
*    kint=-1,0,+1 - +3 /int. field: dipole with psi=0.; =0.; int. of order kint/
*    kext=0,+1/external field: NO; YES/
* NOTE: The resulting field line or drift trajectory is writing in file
*       output.dat in the working directory.
* WARNING: Because of the paraboloid coordinates singularity, avoid
*          the magnetic field calculations at the Ox axis and magnetic 
*          field line calculations on the y=0 plane.
*
* Written by I. Alexeev.
************************************************************************
      COMMON/TK/B1(3),B2(3),B1A(3),B1R(3),B2A(3),B2R(3),
     *BA(3),AB,B(3),BIMF(3),BDISKX(3),bint(3)
      COMMON/SM/SSCP,SSP,simf,smd,ssd,ssr,smr,sbt,ss1,ss2
      COMMON/AA/BM,ZN,HN,ON
     *,CP,V7
      COMMON/COR1/AL,BE,SQ,PQ,QA,BEMIN
      COMMON/COR2/CFI,SFI                                                       
      COMMON/COR3/R,CT,ST
      COMMON/A/Y(3),F(5),V(3),U(3),YF(3)
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      COMMON/T21/BD1,R0,RKM,BK1,BKA,Bkb,BKC
      COMMON/TIN/TIN
      COMMON/MI/M
      DIMENSION FF(3),tism(3)
      DIMENSION X0(3),yc(3)
      COMMON/TRAN/g2gsm(3,3)
      
      iprintf=1
      F(3)=H
      F(4)=H
      F(1)=H/2.
      F(2)=H/2.
      F(5)=H/2.
      kj=int(abs(sl/h))+1
      TIN=0.
      RMIN=1.+H/8.

      OPEN (11, FILE='out/debug')

      DO 7 I=1,3
      Y(I)=X0(I)
      U(I)=Y(I)
 7    CONTINUE
      call field_t(UT,ID,im,iyear,Y,v,kint,kext)
      SP=CP

      if (iprint.eq.1) then
c      write (11,123) Y,AB,B
      write (11,*) Y

c      write (11,321)
      end if

      P=H/2.
      J=0
      JP=0
 27   CONTINUE
      J=J+1
      JP=JP+1
      DO 29 I=1,3
      U(I)=Y(I)
      YF(I)=U(I)
 29   CONTINUE
      call field_t(UT,ID,im,iyear,U,v,kint,kext)
      IF(AB-BM)11,11,12
 12   CALL MIROR
      J=J+1
      JP=JP+1
 11   CALL RK(1)
      DO 30 M=2,4
      call field_t(UT,ID,im,iyear,U,v,kint,kext)
      CALL RK(M)
 30   CONTINUE
      call field_t(UT,ID,im,iyear,Y,v,kint,kext)
      if (iprint.eq.1) then
      if (JP.eq.10) then 
      write (11,*) Y
          JP=0
	  else
	  continue
	  end if
	 else
	 continue
	 end if
c      if (y(3).lt.0.) goto 32

      TIN=TIN+P*(CP+SP)
      SP=CP
c      if (y(1).lt.-40.) goto 32
      IF(Y(1)+0.5/R1*(Y(2)**2+Y(3)**2).GT.R1) GOTO 32
      IF(SQRT(Y(1)**2+Y(2)**2+Y(3)**2).LT.RMIN)GOTO 33
      IF (J.LE.KJ) GO TO 27
33    CONTINUE
*-----------------------------------------------------------------------        
*                      Reflection from the ionosphere                           
*-----------------------------------------------------------------------        
      IF (r.lt.RMIN) then                                                         
      call ion_tr(tism,tilat,timlt)                                             
      y(2)=tism(2)                                                              
      y(1)=tism(1)*CPSI-tism(3)*SPSI                                            
      y(3)=tism(1)*SPSI+tism(3)*CPSI                                            
      call field_t(UT,ID,im,iyear,U,v,kint,kext)                                       
                                                                                
      if (iprint.eq.1) write (11,*) Y
c      ,AB                                      
      call PERE2(y,yc,g2gsm,-1)                                                 
                                                                                                      
      CALL MIROR                                                                
         do 4 i=1,3                                                             
 4       y(i)=yf(i)                                                             
      call field_t(UT,ID,im,iyear,y,v,kint,kext)                                       
                                                                                
      end if                                                                    
                                                                   

 32   CONTINUE
      write (11,*)
321   FORMAT(/,(5X,1HX,7X,1HY,7X,1HZ,3X,1H|,8X,2HAB,
     *3x,1H|,9X,2HBX,10X,2HBY,10X,2HBZ))
123   FORMAT(3F12.3,1X,1H|,F14.3,1x,1H|,2x,3F14.3,1X)
 124  FORMAT(3F12.3,1X)
111   FORMAT(6F10.3,1X,F12.3, '----ionosphere')

      return
      END
C
C                                                                               
      SUBROUTINE MIROR                                                          
      COMMON/A/Y(3),F(5),V(3),U(3),YF(3)                                        
      COMMON/TIN/TIN                                                            
      COMMON/MI/M                                                               
      COMMON/AA/BM,ZN,HN,ON,CP,V7                                               
      ZN=-ZN                                                                    
      TIN=0.0                                                                   
      RETURN                                                                    
      END                                                                       
c
      subroutine ion_tr(rysm,rlat,rmlt)                                         
************************************************************************        
*     Calculation of the ionospheric footpoint of magnetic field line,          
*     RYsm(3), SM coordinates, Re;                                              
*     RLAT - latitude, RFI - longitude (degrees); RMLT - MLT (hours).           
************************************************************************        
      COMMON/COR3/R,CT,ST                                                       
      COMMON/A/Y(3),F(5),V(3),U(3),YF(3)                                        
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD         
      dimension rysm(3)                                                         
      RQQ=R*SQRT(R)                                                             
      ROX=(Y(1)*CPSI+Y(3)*SPSI)/RQQ                                             
      rysm(1)=ROX                                                               
      ROY=Y(2)/RQQ                                                              
      rysm(2)=ROY                                                               
      roz=y(3)                                                                  
      ROZ=SQRT(1.-ROX*ROX-ROY*ROY)*roz/abs(roz)                                 
      rysm(3)=ROZ                                                               
      rlat=90-ACOS(ROZ)*180./PI                                                 
      rfi=ASIN(roy)*180./PI                                                     
      IF (ROX.LT.0.) rfi=180.-rfi                                               
      IF (roy.LT.0..and.rox.gt.0.) rfi=360.+rfi                                 
      rmlt=12.+rfi/15.                                                          
      IF (RMLT.GT.24.) RMLT=RMLT-24.                                            
      return                                                                    
      end                                                                       
