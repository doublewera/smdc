      subroutine s_field(UT,ID,im,iyear,x0,par,bm,bcm,kint,kext)
*********************************************************************
*  Calculation of the magnetic field in the magnetosphere of Saturn.
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
*
*    kint=-1,0,+1 - +3 /int. field: dipole with psi=0.; =0.; int. of order kint/
*    kext=0,+1/external field: NO; YES/
*                                                  			     *
*  OUTPUT PARAMETERS - magnetic field components at the point x0(3)
*                      in cronian SM/sph coordinates, nT:
*         bm(i)     -  total magnetic field (i=1,3);
*         bcm(1,i)  -  internal field
*         bcm(2,i)  -  CF magnetic field
*         bcm(3,i)  -  tail magnetic field
*         bcm(4,i)  -  magnetodisc field
*         bcm(5,i)  -  magnetodisc screening field
*         bcm(6,i)  -  BFAC
*         bcm(7,i)  -  IMF
*
* WARNING: Because of the paraboloid coordinates singularity, avoid
*          the magnetic field calculations at the Ox axis.
*****************************************************************
      COMMON/TFAC/STM,CTM,BFAC0,BFAC1,TETAM,AJ0
      COMMON/TK/B1(3),B2(3),B1A(3),B1R(3),B2A(3),B2R(3),
     *BA(3),AB,B(3),BIMF(3),BDISKX(3),bint(3),BDISK1X(3)
      COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD

      DIMENSION X0(3), FF(3), bm(3), par(11),bcm(7,3)

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
      IF(x0(1)+0.5/R1*(x0(2)**2+x0(3)**2).le.R1) GOTO 32       
         do i=1,3
         bm(i)=bimf(i)                     ! total magnetic field
         bcm(1,i)=0.                              ! internal field
         bcm(2,i)=0.                                ! CF magnetic field
         bcm(3,i)=0.                                ! tail magnetic field
         bcm(4,i)=0.                            ! magnetodisc field
         bcm(5,i)=0.                           ! magnetodisc screening field
         bcm(7,i)=bimf(i)                              ! IMF
         bcm(6,i)=0.                             ! BFAC
         end do
         return
                          

32      if (kint.eq.-1) then					    
c       psi=0.				      
c      bd=2.12321e4
      bd=2.11600e4
      tpsi=psi*pi/180.					    
      spsi=sin(tpsi)						    
      cpsi=cos(tpsi)
      goto 4
      end if	

      call strans (UT,ID,im,iyear,tpsi,BD) !--- defines BD, psi
      psi=tpsi				      
      tpsi=tpsi*pi/180.					    
      spsi=sin(tpsi)						    
      cpsi=cos(tpsi)	
4           CALL MAS2D

            call field_t(UT,ID,im,iyear,X0,FF,kint,kext)
c            print *, 'fie',   psi,tpsi,x0,B 

         do 1 i=1,3
         bm(i)=b(i)        ! total magnetic field
         bcm(1,i)=bint(i)                              ! internal field
         bcm(2,i)=b1(i)                                ! CF magnetic field
         bcm(3,i)=b2(i)                                ! tail magnetic field
         bcm(4,i)=bDISKX(i)                            ! magnetodisc field
         bcm(5,i)=bDISK1X(i)                           ! magnetodisc screening field
         bcm(7,i)=bimf(i)                              ! IMF
         bcm(6,i)=b(i)-b1(i)-b2(i)-bDISKX(i)-bimf(i)-bint(i)   ! BFAC
	 if (aj0.eq.0.) bcm(6,i)=0.

1         continue     



      return
      END
C
      subroutine field_t(UT,ID,im,iyear,X0,FF,kint,kext)
*   Calculation of the total magnetic field: external(FIELD(X0,FF))
*  Ver.3 (28.09.2005)
*    plus internal (satint_cart(UT,ID,im,iyear,x0,bo6,kgarm)) field 
*    kint=-1,0,+1 - +3 /int. field: dipole with psi=0.; Bint=0.; expansion of order kint/
*    kext=0,+1/external field: NO; YES/
*
      COMMON/TK/B1(3),B2(3),B1A(3),B1R(3),B2A(3),B2R(3),
     *BA(3),AB,B(3),BIMF(3),BDISKX(3),bint(3),BDISK1X(3)
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      DIMENSION bo6(3), bo61(3), ff(3),x0(3),pw(3)
      rad=180./3.1415926
      if (kint) 9,10,11

10    bint(1)=0.
      bint(2)=0.
      bint(3)=0.
      goto 12

9     call bdipc(psi,x0,pw)
      bint(1)=pw(1)
      bint(2)=pw(2)
      bint(3)=pw(3)
      goto 12

11    kgarm=kint
      call satint_cart(UT,ID,im,iyear,x0,bo6,kgarm)
      bint(1)=bo6(1)
      bint(2)=bo6(2)
      bint(3)=bo6(3)


12    if (kext) 29,30,31
      
31    call field(X0,FF)                                                
                                                                                
      do i=1,3                                                             
      b(i)=b(i) + bint(i)       ! total magnetic field                       
      end do                                                              
      call bmodu(b,ab)
      do i=1,3                                                             
      ff(i)=b(i)/ab                            
      end do 
      return
      
30    do i=1,3                                                             
      b(i)=bint(i)              ! total magnetic field                       
      end do                                                              
      call bmodu(b,ab)
      do i=1,3                                                             
      ff(i)=b(i)/ab                            
      end do 
      return

29    continue                                                      
     
      return
      end
c=======================================================
c       INTERNAL FIELD
C=======================================================

      SUBROUTINE satint_cart(UT,ID,im,iyear,x,b,nm)
C===================================================================
c
C  CALCULATES COMPONENTS OF MAIN Kronian FIELD IN KSM/sph
C  planetocentric COORD SYSTEM
C
C------INPUT PARAMETERS:
C  NM - MAXIMAL ORDER OF HARMONICS TAKEN INTO ACCOUNT (NOT MORE THAN 3)
C  x - KRONIAN SM/sph COORDINATES OF THE POINT (IN UNITS Rs=58232 KM)
C----- OUTPUT PARAMETERS:
C  B - KSM COMPONENTS OF MAIN GEOMAGN.FIELD IN nT
C=====================================================================
      REAL x(3),b(3),xc(3),xs(3),s2car(3,3),bc(3),bs(3),bs1(3)
      COMMON/TRAN/g2gsm(3,3)

      rad=180./3.1415926

      call STRANS (UT,ID,im,iyear,psi,BD)
      call PERE2(x,xc,g2gsm,-1)
      ax=xc(1)
      ay=xc(2)
      az=xc(3)
      call SPHCAR(R,TETA,PHI,aX,aY,aZ,-1)
      xs(1)=r
      xs(2)=teta
      xs(3)=phi
      call satint(NM,xs,bs)
      call sphtocar(teta,phi,s2car)
      call PERE2(bs,bc,s2car,1)
      call PERE2(bc,b,g2gsm,1)
      return
      end
c
      SUBROUTINE satint(NM,xs,bs)
C===================================================================
c
C  CALCULATES COMPONENTS OF MAIN Kronian FIELD IN SPHERICAL
C  planetocentric COORD SYSTEM
C
C------INPUT PARAMETERS:
C  NM - MAXIMAL ORDER OF HARMONICS TAKEN INTO ACCOUNT (NOT MORE THAN 3)
C  xs - SPHERICAL COORDINATES OF THE POINT (R IN UNITS RJ=58232 KM,
C  COLATITUDE T AND LONGITUDE F IN RADIANS)
C----- OUTPUT PARAMETERS:
C  Bs - SPHERICAL COMPONENTS OF MAIN GEOMAGN.FIELD IN nT
C
C           Revised version of the geopack/IGRF program by
C                           NIKOLAI A. TSYGANENKO
C
        IMPLICIT NONE
C=====================================================================

      REAL A(11),B(11),G(66),H(66),REC(66),xs(3),bs(3)

        REAL R,T,F,BR,BT,BF,DT,F2,F1,S,P,AA,PP,D,BBR,BBF,U,CF,SF,
     1       C,W,X,Y,Z,Q,BI,P2,D2,AN,E,HH,BBT,QQ,XK,DP,PM

      LOGICAL BK,BM

        INTEGER IY,NM,MA,IPR,IYR,KNM,N,N2,M,MNN,MN,K,MM
c
c   G and H are in Gausses referenced to Saturn III (1965) coord.
c
      DATA   G/0., 21232., 23., 1563., -132., 5.,
     *         2821., -209., 282., -156.,
     *         0.,0.,0.,0.,0.,
     * 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     * 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     * 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
      DATA   H/0., 0., 60., 0., 51.,-112.,
     *         0., 1365., -80., 192.,
     *         0.,0.,0.,0.,0.,
     * 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     * 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,
     * 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0./
c
      DATA MA,IYR,IPR,IY/0,0,0,2000/
      r=xs(1)
      t=xs(2)
      f=xs(3)
      IF(MA.NE.1) GOTO 10
      IF(IY.NE.IYR) GOTO 30
      GOTO 130
10    MA=1
      KNM=15
C
      DO 20 N=1,11
         N2=2*N-1
         N2=N2*(N2-2)
         DO 20 M=1,N
            MN=N*(N-1)/2+M
20    REC(MN)=FLOAT((N-M)*(N+M-2))/FLOAT(N2)
C
C
C
C    GET HERE WHEN COEFFICIENTS FOR APPROPRIATE IGRF MODEL HAVE BEEN ASSIGNED
C
30    IYR=IY
300   S=1.
      DO 120 N=2,11
         MN=N*(N-1)/2+1
         S=S*FLOAT(2*N-3)/FLOAT(N-1)
         G(MN)=G(MN)*S
         H(MN)=H(MN)*S
         P=S
         DO 120 M=2,N
            AA=1.
            IF (M.EQ.2) AA=2.
            P=P*SQRT(AA*FLOAT(N-M+1)/FLOAT(N+M-2))
            MNN=MN+M-1
            G(MNN)=G(MNN)*P
120         H(MNN)=H(MNN)*P
C
130   IF(KNM.EQ.NM) GO TO 140
      KNM=NM
      K=KNM+1
140   PP=1./R
      P=PP
      DO 150 N=1,K
         P=P*PP
         A(N)=P
150      B(N)=P*N
      P=1.
      D=0.
      BBR=0.
      BBT=0.
      BBF=0.
      U=T
      CF=COS(F)
      SF=SIN(F)
      C=COS(U)
      S=SIN(U)
      BK=(S.LT.1.E-5)
      DO 200 M=1,K
         BM=(M.EQ.1)
         IF(BM) GOTO 160
         MM=M-1
         W=X
         X=W*CF+Y*SF
         Y=Y*CF-W*SF
         GOTO 170
160      X=0.
         Y=1.
170      Q=P
         Z=D
         BI=0.
         P2=0.
         D2=0.
         DO 190 N=M,K
            AN=A(N)
            MN=N*(N-1)/2+M
            E=G(MN)
            HH=H(MN)
            W=E*Y+HH*X
            BBR=BBR+B(N)*W*Q
            BBT=BBT-AN*W*Z
            IF(BM) GOTO 180
            QQ=Q
            IF(BK) QQ=Z
            BI=BI+AN*(E*X-HH*Y)*QQ
180         XK=REC(MN)
            DP=C*Z-S*Q-XK*D2
            PM=C*Q-XK*P2
            D2=Z
            P2=Q
            Z=DP
190        Q=PM
         D=S*D+C*P
         P=S*P
         IF(BM) GOTO 200
         BI=BI*MM
         BBF=BBF+BI
200   CONTINUE
C
      BR=BBR
      BT=BBT
      IF(BK) GOTO 210
      BF=BBF/S
      GOTO 220
210   IF(C.LT.0.) BBF=-BBF
      BF=BBF
220   CONTINUE
      bs(1)=br
      bs(2)=bt
      bs(3)=bf
      RETURN
      END
C

      SUBROUTINE STRANS (UT,ID,im,iyear,tpsi,BD)
***************************************************************
*   Calculation of the Saturn's geomagnetic dipole tilt angle.
*   and matrices of transition from the geographic coordinates.
*   (Cartesian) to the GSM-coordinates (G2GSM(3,3) in the     .
*   COMMON BLOCK /TRAN/).                                     .
*                _            _                               .
*                Xgsm = G2GSM*Xgeogr                          .
* (by Franz,Harpet "Heliospheric coord.systems",PSS,50,217,2002)                                                           .
*
*    UT     is universal time;                                .
*    WW      is Saturn's prime meridian in System III
*            (determined by Rep. of IAU/IAG WG on cartograph.
*             coordinates, 2000);   .
*    D_ASC  is Saturn's north pole right ascession in GEI2000;
*    D_DEC  is Saturn's north pole declination in GEI2000;
*    t_rot  is Saturn's length of day (hrs)
*
*    ALPHA1 is Kronian dipole latitude (the angle between the  .
*              Saturn's axis and the dipole moment);         .
*    PHINP  is Kronian dipole longitude;                       .
*    ALPHA2 is obliquity to Saturn's orbit (the angle between.
*              Saturn's axis and the normal to orbit plane); .
*    PHIM   is the angle between the midnight meridian plane  .
*              and meridional plane containing Kronian dipole);.
*    PHISE  is the angle between the Earth-Saturn line and   .
*           the projection of the Saturn's axis on the       .
*           ecliptic plane;
*    B1     is western longitude of the noon meridian;        .
*    B2     is the angle between the noon geogr. meridian     .
*           and the Y=0 plane in the Kronian SM-coordinates.  .
*
*    PSI    is the tilt angle of the Kronian dipole, degrees;  .
*    BD     is Kronian MF at equator, nT.                      .
***************************************************************
      COMMON/TRAN/g2gsm(3,3)
      dimension gauss(3),xe(3)
      real*4 lonecl,latecl

      data gauss/21232.,23.,60./
      data w0,w1,a0,a1,d0,d1 /38.90, 810.793902, 40.589,-0.036,
     *83.537,-0.004/
      data aec0,aec1/79.53005,-5.03134e-2/
      data t_rot,alpha20/10.656,26.73/
      call day2000(iyear,im,id,ut,d2000,t2000)
      call satu_ecl(iyear,im,id,ut,lonecl,latecl,xe)
      rad=180./3.1415926
      twopi=2.*3.1415926

      ALPHA2= alpha20/rad
      W=mod(W0+W1*D2000,360.)
      if (w.lt.0.) w=w+360.
      D_ASC=A0+A1*T2000
      D_DEC=D0+D1*T2000
      D_long=Aec0+Aec1*T2000  !ecliptical longitude of Kronian dipole

      G1=GAUSS(2)
      H1=GAUSS(3)
      PD=G1**2+H1**2
      G10=GAUSS(1)
      BD=SQRT(G10*G10+PD)
      ALPHA1= ATAN(1.*SQRT(PD)/G10)
      PHINP= twopi/2.+ ATAN(H1/G1)

cccccc      PHIM= UT*360./t_rot/rad+phinp
      ww=w+D_ASC+90.

      PHIM= mod((ww+phinp*rad-lonecl),360.)/rad
           if (phim.lt.0.) phim=phim+twopi
      if (phim.ge.twopi) phim=phim-twopi
      PHISE= mod((180.-lonecl+d_long),360.)/rad
      if (PHISE.lt.0.) PHISE=PHISE+twopi
      B1=mod((ww-180.-lonecl),360.)/rad
      if (b1.lt.0.) b1=b1+twopi

      SB= SIN(ALPHA2)*COS(PHISE)
      CB= SQRT(1-SB*SB)
      SB1=SIN(B1)
      CB1=COS(B1)
      SPSI= -SB*COS(ALPHA1) + CB*SIN(ALPHA1)*COS(PHIM)
      CPSI=  SQRT(1-SPSI*SPSI)
      psi=asin(spsi)*rad
ccc      print *, ww-360., lonecl
ccc      print *, psi,alpha1*rad, phinp*rad

      CB2=(COS(ALPHA1)+SB*SPSI)/CPSI/CB
      SB2=SQRT(abs(1-CB2*CB2))
      IF(PHIM.LE.0..OR.PHIM.GE.3.1415926) SB2=-SB2
        tpsi=psi
      g2gsm(1,1)=cb1*cb
      g2gsm(1,2)=-sb1*cb
      g2gsm(1,3)=sb
      g2gsm(2,1)=sb1*cb2-cb1*sb*sb2
      g2gsm(2,2)=cb1*cb2+sb1*sb*sb2
      g2gsm(2,3)=cb*sb2
      g2gsm(3,1)=-sb1*sb2-cb1*sb*cb2
      g2gsm(3,2)=-cb1*sb2+sb1*sb*cb2
      g2gsm(3,3)=cb*cb2
         RETURN
         END
c
      subroutine satu_ecl(iy,im,id,ut,lonecl,latecl,xe)

*    Calculation of the Saturns ecliptical longitude and latitude
*    (lonecl,latecl) as well as position in the ecliptical Cartesian
*    coordinates (xe(3)) at the time moment
*    iy,im,id,ut (year, month, day and UT)
*       Special version for heliocentric coordinates only
*    (by  P.Schlyter "How to compute planetary ositions"
*	http://www.stjarnhimlen.se/ http://home.tiscali.se/pausch
*    PARAMETERS:

*The primary orbital elements:
*
*    N = longitude of the ascending node
*    i = inclination to the ecliptic (plane of the Earth's orbit)
*    w = argument of perihelion
*    a = semi-major axis, or mean distance from Sun
*    e = eccentricity (0=circle, 0-1=ellipse, 1=parabola)
*    M = mean anomaly (0 at perihelion; increases uniformly with time)
*
*Related orbital elements:
*
*    w1 = N + w   = longitude of perihelion
*    L  = M + w1  = mean longitude
*    q  = a*(1-e) = perihelion distance
*    Q  = a*(1+e) = aphelion distance (Ql)
*    P  = a ^ 1.5 = orbital period (years if a is in AU, astronomical units)
*    T  = Epoch_of_M - (M(deg)/360) / P  = time of perihelion
*    v  = true anomaly (angle between position and perihelion)
*    E  = eccentric anomaly (ea)

      dimension xe(3),xeq(3)
      real*4 lonsun,lonecl,latecl
      real *4 N,i,w,a,e,M,L,w1,q,Ql, p,t,v,ea
      rad=180./3.1415926

*   day No in J2000 epoch
      iid= 367*iy - 7 * (iy + (im+9)/12 ) / 4 + 275*im/9 + id - 730530
*   data in fraction of days in J2000 epoch
      d = 1.*iid + UT/24.0

*   Earth eqliptic obliquity
      ecl = 23.4393 - 3.563E-7 * d

*   Saturn's orbital elements


      N = 113.6634 + 2.38980E-5 * d
      i = 2.4886 - 1.081E-7 * d
      w = 339.3939 + 2.97661E-5 * d
      a = 9.55475  
      e = 0.055546 - 9.499E-9 * d
      M = 316.9670 + 0.0334442282 * d

      M=mod(M,360.)
      if (M.ge.360.) M=M-360.
      if (M.lt.0.) M=M+360.
      Ea = M + e*rad* sin(M/rad) * ( 1.0 + e * cos(M/rad))

*   Saturn's distance r and its true anomaly v
      xv = a*(cos(Ea/rad) - e)
      yv = a*sqrt(1.0 - e*e) * sin(Ea/rad)

      v = atan2(yv, xv)
      r = sqrt( xv*xv + yv*yv )
c      print *, M, ea,xv,yv,v,r

*   Saturn's ecliptic coordinates
      an=n/rad
      aw=w/rad
      ai=i/rad
      vaw=mod(v*rad+w,360.)
      if (vaw.ge.360.) vaw=vaw-360.
      if (vaw.lt.0.) vaw=vaw+360.
      vaw=vaw/rad
      xh = r * ( cos(aN) * cos(vaw) - sin(aN) * sin(vaw) * cos(ai) )
      yh = r * ( sin(aN) * cos(vaw) + cos(aN) * sin(vaw) * cos(ai) )
      zh = r * ( sin(vaw) * sin(ai) )

*   Saturn's ecliptic heliocentr. longitude and latitude
      lonecl = atan2( yh, xh )*rad
      latecl = atan2( zh, sqrt(xh*xh+yh*yh) )*rad
      if (lonecl.lt.0.) lonecl=lonecl+360.
      if (latecl.lt.0.) latecl1=latecl+360.

c      slon_corr= 3.82394E-5 * ( 365.2422 * ( 1965. - 2000.0 ) - d )
c      print *, slon_corr

      xh = r * cos(lonecl/rad) * cos(latecl1/rad)
      yh = r * sin(lonecl/rad) * cos(latecl1/rad)
      zh = r               * sin(latecl1/rad)
c      print *, 'Saturn ecl. coord', xh,yh,zh


*   Saturn's ecliptic heliocentric position
      xe(1)=xh
      xe(2)=yh
      xe(3)=zh
      return
      end
*
      subroutine day2000(iy,im,id,ut,d2000,t2000)
      real*8 d,sjd
*   day No in J2000 epoch
      iid= 367*iy - 7 * (iy + (im+9)/12 ) / 4 + 275*im/9 + id - 730530
*   data in fraction of days in J2000 epoch
      d2000 = 1.d0*iid + UT/24.0-1.5 ! j2000 epoch day No
      sjd2000=d2000+2451545.0d0      ! julian day
      t2000=d2000/36525.0d0          ! j2000 epoch day No in centuries
      RETURN
      end
c
      subroutine sphtocar(t,f,s2car)
****************************************************************
*     Calculation of the transition matrix from spherical coordinates to
*     cartesian ones:
*                  VectCAR=(S2CAR)*VectSPH
****************************************************************
      dimension s2car(3,3)

      St=SIN(T)
      Ct=COS(T)
      SFi=SIN(f)
      CFi=COS(f)

      s2car(3,1)=CT
      s2car(3,2)=-ST
      s2car(3,3)=0.
      s2car(2,1)=ST*SFI
      s2car(2,2)=CT*SFI
      s2car(2,3)=CFI
      s2car(1,1)=ST*CFI
      s2car(1,2)=CT*CFI
      s2car(1,3)=-SFI

      RETURN
      END
c
c
      SUBROUTINE bdipg(xs,bs)
****************************************************************************
* Program calculating the dipole magnetic field in geographic coordinates.  *
*									   *
* INPUT PARAMETERS:  xs(3) is geographic coordinates of the point where
*                    the magnetic field is being calculated, Rj.	          *
* OUTPUT PARAMETERS: Bs(3) is the magnetic field in GEO coordinates, nT.    *
****************************************************************************
	dimension xs(3),bs(3)
c_Saturn    data g0,g1,h1/424202., -65929., 24116./
c_Earth      data g0,g1,h1/-29682., -1789.,  5318./
      data g0,g1,h1/21232.,23.,60./
      r=xs(1)
      t=xs(2)
      f=xs(3)
      U=T
      CF=COS(F)
      SF=SIN(F)
      C=COS(U)
      S=SIN(U)
	br=2*(g0*c+(g1*cf+h1*sf)*s)/r/r/r
	bt=(g0*s-(g1*cf+h1*sf)*c)/r/r/r
	bf=(g1*sf-h1*cf)/r/r/r
c	print *,'dips',r,t,f,cf,sf,s,c,g0,g1,h1,br,bt,bf
      bs(1)=br
      bs(2)=bt
      bs(3)=bf
      RETURN
      END
C
      SUBROUTINE BDIPC(psi,x,B)
****************************************************************************
* Program calculating the dipole magnetic field in Cartesian coordinates.  *
*									   *
* INPUT PARAMETERS:  x(3) is GSM coordinates of the point where the magnetic *
*                    field is being calculated, Rj;	                   *
*                    PSI  is  Saturn's dipole tilt angle, rads. 	           *
* OUTPUT PARAMETERS: B(3) is the magnetic field in GSM coordinates, nT.    *
****************************************************************************
      dimension x(3),b(3)
*   BM   is Saturn's dipole moment, nT*Rj^3.
c      bm=4.299716e5
*   BM   is Saturn's dipole moment, nT*Rs^3.
      bm=2.12321e4
c_Earth      bm=30500.87
      rad=180./3.1415926
      tpsi=psi/rad
      Spsi=SIN(tpsi)
      Cpsi=COS(tpsi)
      x1=x(1)
      x2=x(2)
      x3=x(3)
      r2=(x1*x1+x2*x2+x3*x3)
      r=sqrt(r2)
      r5=r2*r2*r
      p=x3*cpsi-x1*spsi
      br=bm/r5
      b(1)=-Br*(-r*r*spsi-3.*x1*p)
      b(2)= Br*(3.*x2*p)
      b(3)=-Br*(r*r*cpsi-3.*x3*p)
      RETURN
      END
C
      SUBROUTINE PERE2(A,B,T,K)
**********************************************************
*     Transition A into B vectors by T (K>0)
*                       B=T*A
*     or T^{-1} matrices (K<=0)
*                       B=T^{-1}*A
**********************************************************
      DIMENSION A(3),B(3),T(3,3)
      if (k) 1,1,2
2     b(1)=t(1,1)*a(1)+t(1,2)*a(2)+t(1,3)*a(3)
      b(2)=t(2,1)*a(1)+t(2,2)*a(2)+t(2,3)*a(3)
      b(3)=t(3,1)*a(1)+t(3,2)*a(2)+t(3,3)*a(3)
      return
1     b(1)=t(1,1)*a(1)+t(2,1)*a(2)+t(3,1)*a(3)
      b(2)=t(1,2)*a(1)+t(2,2)*a(2)+t(3,2)*a(3)
      b(3)=t(1,3)*a(1)+t(2,3)*a(2)+t(3,3)*a(3)
      return
      end
C
      SUBROUTINE SMtoGSM(psi,SM2GSM)
****************************************************************
*     Calculation of the transition matrix from SM coordinates to
*     GSM ones:
*                  VectGSM=(SM2GSM)*VectSM
****************************************************************
      DIMENSION SM2GSM(3,3)
      Spsi=SIN(psi)
      Cpsi=COS(psi)
      SM2GSM(1,1)=CPSI
      SM2GSM(1,2)=0.
      SM2GSM(1,3)=-SPSI
      SM2GSM(2,1)=0.
      SM2GSM(2,2)=1.
      SM2GSM(2,3)=0.
      SM2GSM(3,1)=SPSI
      SM2GSM(3,2)=0.
      SM2GSM(3,3)=CPSI
      return
      END
C
c-----------------------------------------------------------------------
c
      SUBROUTINE SPHCAR(R,TETA,PHI,X,Y,Z,J)
C
C   CONVERTS SPHERICAL COORDS INTO CARTESIAN ONES AND VICA VERSA
C    (TETA AND PHI IN RADIANS).
C
C                  J&gt0            J&lt0
C-----INPUT:   J,R,TETA,PHI     J,X,Y,Z
C----OUTPUT:      X,Y,Z        R,TETA,PHI
C
C
C                   AUTHOR: NIKOLAI A. TSYGANENKO
C                           INSTITUTE OF PHYSICS
C                           ST.-PETERSBURG STATE UNIVERSITY
C                           STARY PETERGOF 198904
C                           ST.-PETERSBURG
C                           RUSSIA
C
        IMPLICIT NONE

        REAL R,TETA,PHI,X,Y,Z,SQ

        INTEGER J


      IF(J.GT.0) GOTO 3
      SQ=X**2+Y**2
      R=SQRT(SQ+Z**2)
      IF (SQ.NE.0.) GOTO 2
      PHI=0.
      IF (Z.LT.0.) GOTO 1
      TETA=0.
      RETURN
  1   TETA=3.141592654
      RETURN
  2   SQ=SQRT(SQ)
      PHI=ATAN2(Y,X)
      TETA=ATAN2(SQ,Z)
      IF (PHI.LT.0.) PHI=PHI+6.28318531
      RETURN
  3   SQ=R*SIN(TETA)
      X=SQ*COS(PHI)
      Y=SQ*SIN(PHI)
      Z=R*COS(TETA)
      RETURN
      END

      SUBROUTINE bmodu(x,xx)
      dimension x(3)
      xx=sqrt(x(1)*x(1)+x(2)*x(2)+x(3)*x(3))
      return
      end

      SUBROUTINE doyday(idoy,iyear,iday,imon)                                   
*  Transforms idoy, day of the year (iyear), into iday, day of the month
*   (imonth).
      dimension mond(12), m(12)                                                  
      data m/31,28,31,30,31,30,31,31,30,31,30,31/                               
      do 2 k=1,12                                                               
      mond(k)=m(k)                                                              
      if (mod(iyear,100).eq.0.and.mod(iyear,400).ne.0) goto 2                   
                                                                                
      IF (MOD(IYear,4).EQ.0) mond(2)=29                                         
 2    continue                                                                  
                                                                                
      kk=idoy                                                                   
      do j=1,12                                                                 
      iday=kk                                                                   
      imon=j                                                                    
      kk=kk-mond(j)                                                             
      if (kk.le.0)goto 1                                                        
      end do                                                                    
1     continue                                                                  
      return                                                                    
      end                                                                       



c=======================================================
c       EXTERNAL FIELD
C=======================================================

                              SUBROUTINE MAS2D
      REAL L,L0
      COMMON/COR1/AL,BE,SQ,PQ,QA,BEMIN
      COMMON/T3/L(6,5),L0(5)
      COMMON/T1/A1(12) 
      COMMON/T1D/A1DISK(12)
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      COMMON/T21/BD1,R0,RKM,BK1,BKA,BKB,BKC
      COMMON/TFAC/STM,CTM,BFAC0,BFAC1,TETAM,AJ0
      COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP

      COMMON/S1/ CB(6,5),CB2(6,5),
     *CD(6,5),CB3(6,5),CD2(6,5),
     *CD3(6,5)
      COMMON/S2/ CF0(5),CF1(5),CF2(5),CF3(5),CF4(5)
      COMMON/S2D/ CF0D(5),CF1D(5),CF2D(5),CF3D(5),CF4D(5)
      COMMON/S5/CI0(9),CI1(9)
      DIMENSION CF00(5),CF01(5)
     *,CB0(6,5),CD0(6,5)
     *,D1(12)
        DATA                      CB0/
     *7.493663380000,3.119532240E-1,1.706022280E-2,1.03359770E-3,
     *6.633343340E-5,4.421213830E-6,5.777707910E-3,2.103707660E-4,
     *7.538502750E-6,3.011267810E-7,1.313975350E-8,6.13862642E-10,
     *1.867244830E-5,5.965184960E-7,1.712466940E-8,5.43331737E-10,
     *1.89762765E-11,7.17571687E-13,8.545308210E-8,2.571561030E-9,
     *6.45145558E-11,1.75563377E-12,5.24527385E-14,1.70163718E-15,
     *4.43194331E-10,1.26958726E-11,2.88578820E-13,6.99784044E-15,
     *1.85385885E-16,0.
     */,

     *                             CD0/
     *5.40613258E-5,1.17552607E-3,2.01041928E-2,0.31415060400,
     *4.67288038000,67.3370338000,2.50590102E-3,0.202158093,
     *7.58793123,216.779913,5328.09218,118727.936,
     *1.72159326E-1,21.3628656,1157.19802,45465.0611,
     *1482947.1,42673478.6,14.734532, 2354.2227,
     *161804.316,7916120.89,316102914.,10974469500.,
     *1367.45791,254342.53,20562943.2,1179904930.,
     *54901114700.,2205064660000./

      DATA D1/0.64972264,0.21646207,
     +0.043429128,-0.000846358,-0.004917225,-0.002224403,0.94028094,
     +0.4649891,0.12928167,-0.014765534,-0.016942754,-0.022559739 /

      DATA CF00/-94630.534652,-10798057.06,-652953011.
     +92,-30158380850.,-1198163932400.
     */
      DATA
     *CF01/-1318.693367,-190097.98824,-96033
     +82.4047,-369794441.86,-12502247266.
     */
c               SPSI=0.
c	       CPSI=1.0
	        AL01=SQRT(2.4)
                 AL1=SQRT(3.)
		 AL0=SQRT(1.+2.*R2/R1)
                 Z0=R1*2.*SPSI*CPSI/(3.+SPSI**2)

C*****************ЗАДАЕМ КОНСТАНТЫ ДЛЯ BDISK*******
        CALL MASDISK
C*****************ЗАДАЕМ КОНСТАНТЫ ДЛЯ BRING И BFAC*******
ccc         WRITE (1,872) BD
c        CALL MASRING
         BDDISK=BDC1/2*(1-EKSI)  !?
	        CALL MASFAC
C*****************ВЫЧИСЛЯЕМ ЭФФЕКТИВНЫЙ ДИПОЛЬНЫЙ МОМЕНТ *******
        B0= BD+BD1*BKA
	 BD1=B0
ccc       WRITE (1,830)
ccc          WRITE (1,710) BD1,BKA,BKB,BKC
ccc          WRITE (1,830)
ccc          WRITE (1,870) B0
ccc	  WRITE (1,830)
ccc          WRITE (1,871) BDISKDIP,BDDISK
C*****************ВЫЧИСЛЯЕМ ЭФФЕКТИВНЫЙ ДИПОЛЬНЫЙ МОМЕНТ *******
         
      P=BD1/R1/R1
            DO 20 I=1,6
            P=P/R1
            A1(I)=CPSI*D1(I)*P
            A1(I+6)=P*SPSI*D1(I+6)
 20        CONTINUE


      
       P=BD1/(-31.200)/R1/R1/R1
      
                  DO 1 N=1,5
      CF0(N)=CF00(N)*SPSI*P
      CF1(N)=CF01(N)*CPSI*P
      CF2(N)=CF1(N)*L(1,N)
      CF3(N)=CF2(N)*L(1,N)
      CF4(N)=CF0(N)*L0(N)
    1              CONTINUE

C*****************ВЫЧИСЛЯЕМ ЭФФЕКТИВНЫЙ ДИПОЛЬНЫЙ МОМЕНТ ДИСКA*******
         
         P=BDISKDIP/R1/R1
            DO 120 I=1,6
            P=P/R1
            A1DISK(I)=CPSI*D1(I)*P
            A1DISK(I+6)=P*SPSI*D1(I+6)
 120        CONTINUE


      
       P=BDISKDIP/(-31.200)/R1/R1/R1
      
                  DO 11 N=1,5
      CF0D(N)=CF00(N)*SPSI*P
      CF1D(N)=CF01(N)*CPSI*P
      CF2D(N)=CF1D(N)*L(1,N)
      CF3D(N)=CF2D(N)*L(1,N)
      CF4D(N)=CF0D(N)*L0(N)
 11              CONTINUE
ccc                    WRITE  (11,795) CF0D,CF1D,CF2D,CF4D
C*******************************************************
C     ПЕРЕСЧЕТ коэфициентов разложения поля токового
C     слоя. Константы выбраны так, чтобы на переднем
C     краю токового слоя поле было равно BT. Ток в слое
C     зависит от alfa как AL0/AL.
C*******************************************************

                      P=BT/40.*AL0/AL01
              DO 2 K=1,6
                M=2*K-1
                    DO 2 N=1,5
                      ZZ=L(K,N)*AL01
                      ZA=L(K,N)*AL0
                      CALL BESK(M,ZZ,SK ,DSK )
                      CALL BESM(M,ZZ,SI ,DSI )
                      CALL BESK(M,ZA,SKA,DSKA)
                      CALL BESM(M,ZA,SIA,DSIA)
                      SIII=ZA-ZZ
                      SIA=SIA/SI*EXP(SIII)
                      CB(K,N)=P*CB0(K,N)*SKA/SK
                      CB2(K,N)=L(K,N)*CB(K,N)
                      CB3(K,N)=L(K,N)*CB2(K,N)
                      CD(K,N)=P*CD0(K,N)*SIA
                      CD2(K,N)=L(K,N)*CD(K,N)
                      CD3(K,N)=L(K,N)*CD2(K,N)
    2          CONTINUE

C*******************************************************
C  Присвоение численных значений константам
C    AL1, C0, E5, BETA0, BEMIN
C*******************************************************
      C0= 61.96773354*BT/40.*AL0/AL01
      E5=10.**7
      BETA0=1.0+0.2*(PSI/30.)**2
      BEMIN=.00001
C***************************************************************
  795 format (2X,5HCF=  ,5E12.3)
  796 format (9e8.2)
  797 format (6e12.3/)

C*****************************************************************
C*******************************************************
C
C
C*******************************************************

 710     FORMAT
     *        (1H#,3X,'! BD1=',F10.2,2X,'! BKA=',E10.4,
     *         2X,'! BKB=',E10.4,2X,'! BKC=',E10.4,2X,'!')

 830     FORMAT
     *        (1H#,7X,10H----------,
     *55H-------------------------------------------------------)
 870    FORMAT(1H#,10X,'B0=',F15.2)
 871    FORMAT(1H#,10X,'BDISKDIP= ',F15.2,10X,'BDDISK= ',F15.2)
 872    FORMAT(1H#,10X,'BD= ',F15.2)
 
                                       RETURN
                                       END




                                    SUBROUTINE MASFAC
C*************************************************************
C ВЫЧИСЛЕНИЕ  констант для поля продольных токов
C TETAM - полярный угол конуса, по которому текут продольные токи.
C AJ0 - полный продольный ток в одном полушарии
C STM,CTM - sin(TETAM),  cos(TETAM), BFACP0=0,000098*AJ0/STM ,
C BFACP1=BFACP0*(1-CTM).
C
C*************************************************************

      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      COMMON/TFAC/STM,CTM,BFAC0,BFAC1,TETAM,AJ0

        U= TETAM*PI/180.
        STM=SIN(U)
        CTM=COS(U)
        BFAC0=0.000098*AJ0/STM
        BFAC1=BFAC0*(1-CTM)

                                           RETURN
                                           END


                                    SUBROUTINE MASDISK
C*************************************************************
C ВЫЧИСЛЕНИЕ  констант для поля  токового диска 
C BDC1 радиальное поле при R=RD1, RD1 внешний край диска
C RD2 - внутренний край диска
C EKSI=RD2/RD1 - отношение размеров диска
C
C*************************************************************

  
       COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP
        EKSI=RD2/RD1
        BDC1=BDC11
        BDISKDIP=BDC1/2*(1-EKSI)*RD1*RD1*RD1        
                                   RETURN
                                           END


                                            BLOCK DATA
      REAL L,L0
      COMMON/T3/L(6,5),L0(5)
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      COMMON/S1/ CB(6,5),CB2(6,5),
     *CD(6,5),CB3(6,5),CD2(6,5),
     *CD3(6,5)
      COMMON/S2/ CF0(5),CF1(5),CF2(5),CF3(5),CF4(5)
      COMMON/S5/CI0(9),CI1(9)
      COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP

      DATA
     *L0/3.83170597,7.01558667,10.17346814,13.3236919,16.47063005/,
     *L/1.84118390,4.2011889412,6.4156163752,8.5778364889,10.711433969,
     +12.826491226,5.3314427000,8.0152365984,10.519860874,12.932386237,
     *15.286737667,17.600266557,8.5363163000,11.345924311,13.987188630,
     *16.529365884,19.004593538,21.430854238,11.706005000,14.585848286,
     *17.312842488,19.941853366,22.501398726,25.008518704,14.863588700,
     *17.788747866,20.575514521,23.268052926,25.891277276,28.460857279
     */


      DATA
     *R1/100./,BD/430000./,PI/3.1415  /,BT/3.6/,BDC11/3.54/
      DATA CI0/
     *+0.003923767,-0.016476329,+0.026355372,
     *-0.020577063,+0.009162808,-0.001575649,
     *+0.002253187,+0.013285917,+0.398942280/
      DATA CI1/
     *-0.004200587,+0.017876535,-0.028953121,
     *0.022929673,-0.010315550,+0.001638014,
     *-0.003620183,-0.039880242,+0.398942280/
                                            END




C       ФАЙЛ СОДЕРЖИТ SPEED;MIRROR;
C               rk;pere;pris;com;





C                          ФАЙЛ СОДЕРЖИТ
C                 rk;pere;pris;com;

                           SUBROUTINE RK(M)
      COMMON/A/Y(3),F(5),V(3),U(3),YF(3)
      DO 26 I=1,3
      Y(I)=Y(I)+F(M+1)*V(I)/3.
      U(I)=YF(I)+F(M)*V(I)
   26 CONTINUE
                            RETURN
                            END

                            SUBROUTINE PERE(UF,VV,DET)
      DIMENSION UF(3),VV(3),DET(3,3)
      DO 1 I=1,3
      P=0.
      DO 2 J=1,3
      P=P+DET(I,J)*UF(J)
    2 CONTINUE
      VV(I)=P
    1 CONTINUE
3     FORMAT(5H DET=,9E12.5)
4     FORMAT(9H 3UF,3VV=,6E12.5)
                             RETURN
                             END

                             SUBROUTINE PRIS(UF,VV)
      COMMON /COR1/AL,BE,SQ,PQ,QA,BEMIN
      COMMON /COR2/CFI,SFI
      DIMENSION UF(3,3),VV(3,3)
      UF(1,1)=-AL/SQ
      UF(1,2)=BE/SQ
      UF(1,3)=0.
      Z=SFI/SQ
      UF(2,1)=BE*Z
      UF(2,2)=AL*Z
      UF(2,3)=CFI
      Z=CFI/SQ
      UF(3,1)=BE*Z
      UF(3,2)=AL*Z
      UF(3,3)=-SFI
      DO 1 I=1,3
      DO 1 J=1,3
      VV(I,J)=UF(J,I)
    1 CONTINUE
2     FORMAT(4H VV=,9E12.5)
                                         RETURN
                                         END


C                       ФАЙЛ СОДЕРЖИТ
C                  FIELD;BDIP;BRING; BEG;flyd;dery4d;
C                 ПЕРЕДЕЛАНО ".Ю!)(#





                SUBROUTINE FIELD (UF,FF)
C                 ПЕРЕДЕЛАНО 21.09.2005 
      COMMON/TK/B1(3),B2(3),B1A(3),B1R(3),B2A(3),B2R(3),
     *BA(3),AB,B(3),BIMF(3),BDISKX(3),bint(3),BDISK1X(3)
      COMMON/COR1/AL,BE,SQ,PQ,QA,BEMIN
      COMMON/COR2/CFI,SFI
      COMMON/COR3/R,CT,ST
      COMMON/COR4/CTE,STE,CFIE,SFIE
      COMMON/COR5/XSM,YSM,ZSM 
      COMMON/COR6/XM,YM,ZM 
      COMMON/TFAC/STM,CTM,BFAC0,BFAC1,TETAM,AJ0
      COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      DIMENSION UF(3),FF(3),V1(3),V2(3),V3(3),B11X(3),B11RE(3),bm(3),
     *bdiskd(3),bdx(3) 
     
      DIMENSION UZ(3,3),ZU(3,3),ZMU(3,3),A2X(3,3),X2A(3,3),bcm(7,3),
     *adx(3,3),xda(3,3)
 
      X=UF(1)
      YX=UF(2)
      Z=UF(3)
      XSM=X
      YSM=YX
      ZSM=Z
      T=SQRT(YX*YX+Z*Z)
      R=SQRT(X*X+T*T)

C*********************************************************
C     Вычисление солнечно-магнитных координат
C*********************************************************
                           XM= X*CPSI+Z*SPSI
                           YM= YX
                           ZM=-X*SPSI+Z*CPSI
C*************************************************************
C Вычисление CTE,STE,CFIE,SFIE - крониамагнитных сферических
C координат.
C     TETA - полярный угол, FIE - долгота от местного магнитного 
C     полдня
C*************************************************************
                           RMN=SQRT(YM*YM+XM*XM)
                      IF (R.EQ.0.) THEN
		           CTE=1.
			   STE=0.
			          ELSE 
		           CTE=ZM/R
                           STE=RMN/R
		                 END IF	   
                      IF (RMN.GT.0) GOTO 111
                           CFIE=1.0
                           SFIE=0.
                           GOTO 112
111                   CONTINUE
                           SFIE=YM/RMN
                           CFIE=XM/RMN
112                   CONTINUE
C*********************************************************
C     Вычисление матрицы перевода от сферических солнечно-
C     магнитных координат к декартовым.
C*********************************************************
      ZMU(1,1)=STE*CFIE
      ZMU(1,2)=CTE*CFIE
      ZMU(1,3)=-SFIE
      ZMU(2,1)=STE*SFIE
      ZMU(2,2)=CTE*SFIE
      ZMU(2,3)=CFIE
      ZMU(3,1)=CTE
      ZMU(3,2)=-STE
      ZMU(3,3)=0.
                   IF (R.EQ.0.)   THEN
		           CT=1.
			   ST=0.
			          ELSE 
		           CT=X/R
                           ST=T/R
		                   END IF
      RX=R/R1
      X1=X/R1-0.5
      RY=RX**2-X1-0.25
      RY=SQRT(ABS(RY))
      BE=RY+X1
      BE=SQRT(ABS(BE))
      AL=RY-X1
      AL=SQRT(ABS(AL))
      PQ=AL*BE
      T=R1*PQ
      QA=AL*AL+BE*BE
      SQ=SQRT(QA)
      IF(T.GT.0) GOTO 1100
      CFI=0.
      SFI=1.
      GOTO 1200
1100  CONTINUE
      SFI=YX/T
      CFI=Z/T
1200   CONTINUE

C*********************************************************
C     Вычисление поля токового диска в солнечно-магнитосферные координаты
C*********************************************************
      CALL BDISK(BDISKX)
C****************************M*********************************
 
C*********************************************************
C     Вычисление поля продольных токов
C*********************************************************
      CALL BFAC(B11RE)
C*********************************************************
C     Перевод в декартовы солнечно-магнитные координаты
C*********************************************************
      CALL PERE(B11RE,V2,ZMU)
C*********************************************************
C     Перевод в солнечно-магнитосферные координаты
C*********************************************************
                           B11X(1)=V2(1)*CPSI-V2(3)*SPSI
                           B11X(2)=V2(2)
                           B11X(3)=V2(1)*SPSI+V2(3)*CPSI

C*********************************************************
C*     Вычисление поля токового слоя хвоста
C*      переход к координатам токового слояб сдвинутым на Z0
C**********************************************************
      Z=Z+Z0                                                            
      T=SQRT(YX*YX+Z*Z)
      R=SQRT(X*X+T*T)
      RX=R/R1
      X1=X/R1-0.5
      RY=RX**2-X1-0.25
      RY=SQRT(ABS(RY))
      BE=RY+X1
      BE=SQRT(ABS(BE))
      AL=RY-X1
      AL=SQRT(ABS(AL))
      PQ=AL*BE
      T=R1*PQ
      QA=AL*AL+BE*BE
      SQ=SQRT(QA)
      IF(T.GT.0) GOTO 2100
      CFI=0.
      SFI=1.
      GOTO 2200
2100  CONTINUE
      SFI=YX/T
      CFI=Z/T
2200   CONTINUE
      CALL DERY4D(B2A)
      CALL PRIS(A2X,X2A)
      CALL PERE(B2A,B2,A2X)
C*********************************************************
C      Возвращение к дипольным параболоидальным координатам
C      
C*********************************************************

      Z=Z-Z0                                                            
      T=SQRT(YX*YX+Z*Z)
      R=SQRT(X*X+T*T)
                  IF (R.EQ.0.)   THEN
		           CT=1.
			   ST=0.
			          ELSE 
		           CT=X/R
                           ST=T/R
		                   END IF
      RX=R/R1
      X1=X/R1-0.5
      RY=RX**2-X1-0.25
      RY=SQRT(ABS(RY))
      BE=RY+X1
      BE=SQRT(ABS(BE))
      AL=RY-X1
      AL=SQRT(ABS(AL))
      PQ=AL*BE
      T=R1*PQ
      QA=AL*AL+BE*BE
      SQ=SQRT(QA)
          IF(T.GT.0) GOTO 3100
           CFI=0.
            SFI=1.
           GOTO 3200
3100      CONTINUE
          SFI=YX/T
          CFI=Z/T
3200      CONTINUE
      ZU(1,1)=CT
      ZU(1,2)=-ST
      ZU(1,3)=0.
      ZU(2,1)=ST*SFI
      ZU(2,2)=CT*SFI
      ZU(2,3)=CFI
      ZU(3,1)=ST*CFI
      ZU(3,2)=CT*CFI
      ZU(3,3)=-SFI
      DO 7 I=1,3
      DO 7 J=1,3
      UZ(I,J)=ZU(J,I)
 7    CONTINUE

C*********************************************************
C     Вычисление поля  диполя и диполя токового диска
C    в солнечно-магнитосферных координатах
C*********************************************************
      call BDIPC(psi,UF,V1)
C***************************************************************************
C Program calculating the dipole magnetic field in Cartesian coordinates.  *
C									   *
C INPUT PARAMETERS:  UF(3) is GSM coordinates of the point where the	   *
C		      magneticfield is being calculated, Rj;		   *
C		     PSI  is  Saturn's dipole tilt angle, rads. 	   *
C OUTPUT PARAMETERS: V1(3) is the magnetic field in GSM coordinates, nT.   *
C***************************************************************************
       pd=B0/BDISKDIP 

      do 82 I=1,3
      bdx(I)=V1(I) 
      bint(I)=bdx(I)
      bdiskd(I)=bdx(I)/pd
82    continue      

C*********************************************************
C     Вычисление поля экранировки диполя 
C    в солнечно-магнитосферных координатах
C*********************************************************
      IF(AL1-AL)4,4,5
    4 CALL FLYD(V1)
      CALL PRIS(ADX,XDA)
       CALL PERE(V1,V2,ADX)
C***********************
      do 81 I=1,3
      B1(I)=V2(I)-bdx(I)
81    continue      
C*********************************************************
C     Вычисление поля экранировки токового диска 
C    в солнечно-магнитосферных координатах
C*********************************************************
       CALL FLYDISK(V1)
      CALL PERE(V1,V2,ADX)
C*************  ВЫЧИТАНИЕ ДИПОЛЬНОГО ЧЛЕНА ПОЛЯ ДИСКА **********
      do 80 I=1,3
      BDISK1X(I)=V2(I)-bdiskd(I)
80    continue      
       GOTO 6
    5 CALL BEG(V1)
      CALL PERE(V1,B1,ZU)
C*********************************************************
C     Вычисление поля экранировки токового диска 
C    в солнечно-магнитосферных координатах
C*********************************************************
      CALL BEGDISK(V1)
      CALL PERE(V1,BDISK1X,ZU)
    6 CONTINUE

C**** ДОБАВЛЕНИЕ  BCFDIP, ММП, BFAC, и BDISK1X  в сол-магн. коор****
C**** ДОБАВЛЕНИЕ  BTAIL и BDISK в сол-магнитосферных координатах****
C**********************************************
      DO 9 I=1,3
      B(I)=BIMF(I)+B1(I)+B2(I)+B11X(I)+BDISKX(I)+BDISK1X(I)
    9 CONTINUE
      AB=SQRT(B(1)*B(1)+B(2)*B(2)+B(3)*B(3))
      DO 10 I=1,3
      FF(I)=B(I)/AB
   10 CONTINUE
      CALL PERE(B,BA,X2A)

C        WRITE (12,523) B1A,Al,BE
 523          FORMAT
     * (3X,' B1A=',F15.6,5X,'B1BE=',F15.6,5X,' B1FI=',F15.6,/
     *  3X,' al=',F9.6,' BE=',F9.6,/)
             RETURN
      END


                          SUBROUTINE BDIP (P,BD1)
C    Переделано
      COMMON/COR2/CFI,SFI
      COMMON/COR3/R,CT,ST
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      DIMENSION P(3)


      RR=R*R
      T=BD1/R/RR
      TC=T*CPSI
      TS=T*SPSI
      CPF=TC*CFI
       P(1)= 2*(CPF*ST-TS*CT)
       P(2)= -CPF*CT-TS*ST
       P(3)= TC*SFI

                              RETURN
                              END




                          SUBROUTINE BDISK (BDSM)
C*************************************************************
C Вычисление  поля  токового диска,  который расположен в эква
C ториальной плоскости, начиная с RD2 и кончая RD1 (R1 в данной
C версии). BDC1 - величина радиального поля токового диска на
C расстоянии RD1=100Rj. В текущей версии BDC1 равно 3.6 нТ, при этом  
C магнитный поток через полушарие равен 2pi*(100 Rj)**2 *3.6 Вб
C или 1.14 ТВб (10**12 Вб). Полный ток в диске равен JTD1 =
C  60 Rj А/м (Rj/RD2)(1-EKSI), где EKSI=RD2/RD1. При RD1=100Rj и EKSI
C =0.3 JTD1 = 98 МА. RD2 - это расстояние до ближайшего к Юпитеру
C краю ткового диска.
C
C
C  R,CTE,STE,CFIE,SFIE - йовимагнитные сферические
C координаты,  R - расстояние от центра Юпитера в радиусах Юпитера,
C CTE и STE - cos(teta) и sin(teta), где teta - коширота, CFIE
C и SFIE  - cos(fie) и sin(fie), где fie - долгота от полдня.
C  PB(I) - результат работы программы - магнитное поле токового
C  диска в сферических йовомагнитных координатах в нТ. BDCr, BDCtetha,
C и BDCfi
C AL1 - определяет переход к вычислению поля в приближении далекого хвоста
C магнитосферы, когда вычисляется сумма поля диполя и поля токов экранировки.
C
C
C
C*************************************************************

     
      COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP
      COMMON /COR1/AL,BE,SQ,PQ,QA,BEMIN
      COMMON/COR2/CFI,SFI
      COMMON/COR3/R,CT,ST
      COMMON/COR4/CTE,STE,CFIE,SFIE 
      COMMON/COR5/XgSM,YgSM,ZgSM 
      COMMON/COR6/XSM,YSM,ZSM 
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0
     *,BD
      DIMENSION BDSM(3),PA(3),BDISKX(3),PB(3),VD1(3),B1D(3),UZ(3,3)
     *,ZU(3,3),ZMU(3,3),ADX(3,3),XDA(3,3),BDSM1(3),sm2gsm(3,3)


*************************************************************
C Вычисление CTE,STE,CFIE,SFIE - йовимагнитных сферических
C координат.
C     TETA - полярный угол, FIE - долгота от местного магнитного 
C     полдня
C*************************************************************
                           RMN=SQRT(YSM*YSM+XSM*XSM)
                           CTE=ZSM/R
                           STE=RMN/R
                      IF (RMN.GT.0) GOTO 111
                           CFIE=1.0
                           SFIE=0.
                           GOTO 112
111                   CONTINUE
                           SFIE=YSM/RMN
                           CFIE=XSM/RMN
112                   CONTINUE
C*********************************************************
C     Вычисление матрицы перевода от сферических солнечно-
C     магнитных координат к декартовым.
C*********************************************************
      ZMU(1,1)=STE*CFIE
      ZMU(1,2)=CTE*CFIE
      ZMU(1,3)=-SFIE
      ZMU(2,1)=STE*SFIE
      ZMU(2,2)=CTE*SFIE
      ZMU(2,3)=CFIE
      ZMU(3,1)=CTE
      ZMU(3,2)=-STE
      ZMU(3,3)=0.


C****************************M*********************************
      T=RD1/R
      X=RD2/R
      X2=X*X
      T2=T*T
      T3=T2*T

C*************************************************************
C      Вычисление начальных значений  вспомогательных переменных
C     X1K=T**2I, X2K=X**2I, Y1K=T**(-2I), Y2K=X**(-2I)
C*************************************************************
      X1K=1.
      X2K=1.
      Y1K=1.
      Y2K=1.
    
C*************************************************************
C E= P1(CT), S= P2(CT), E1= dP1(CT)/dTETHA, S1= dP2(CT)/dTETHA
C
C
C*************************************************************
      
      E=CTE
      S=(3.*CTE*CTE-1.)/2.
      E1=-STE
      S1=-3.*CTE*STE
      U1=-CTE
      U2=U1
      U3=-CTE/2.
      U4=U3
      V1=STE/2.
      V2=V1
      V3=V1
      V4=V1
      G=-0.5
C      US=3*U3
C*************************************************************
C      Вычисление полиномов Лежандра  по реккурентным
C       формулам  
C 
C     W=P(2I+1), S=P(2I), E=P(2I-1), Q=P(2I+2)
C*************************************************************
    
      DO 19 I=1,20
      I2=I+I
      P=I2/(I2+1.)   
      W=(1.+P)*CTE*S-P*E
      P2=1.-1./(I2+2.)
      Q=(1.+P2)*CTE*W-P2*S
      E=W
      S=Q
      
C*************************************************************
C      Вычисление коэффициентов разложения и вспомогательных переменных
C      по реккурентным формулам Fk=(1-2K)/(2K+2) Fk-1 
C      F=Fk, G=Fk-1, T=RD1/R, X=RD2/R
C*************************************************************
     
       F=G*(2.-3.*P2)
       G=F

C*************************************************************
C      Вычисление гленов разложения и вспомогательных переменных
C     H= Fk*P(2I+1), H1=(2I+2)*Fk*P(2I+1), H2=(2I+1)*Fk*P(2I+1)
C     X1K=T**2I, X2K=X**2I, Y1K=T**(-2I), Y2K=X**(-2I)
C*************************************************************

      HP=W*F
      H1=HP*(I2+2.)
      H2=H1-HP
C      H3=H1+H2
C*************************************************************      
C      Вычисление производных полиномов Лежандра по реккурентным
C       формулам         
C 
C     W1=P'(2I+1), S1=P'(2I), E1=P'(2I-1), Q1=P'(2I+2)
C*************************************************************
      
      P1=1.+1./I2   
      W1=(1.+P1)*CTE*S1-P1*E1
      P3=2.-P
      Q1=(1.+P3)*CTE*W1-P3*S1
      E1=W1
      S1=Q1
      
      HP1=W1*F
      
C*************************************************************
C      Вычисление сумм для широтхной компоненты поля токового диска
C*************************************************************
       IF (R.GT.RD1) GOTO 24
       IF (R.GT.RD2) GOTO 23
C*************************************************************
C Вычисление  сумм для поля  токового диска, при R < RD2 
C*************************************************************
      Y1K=Y1K/T2
      Y2K=Y2K/X2
      U3=U3+H2*Y1K
      U4=U4+H2*Y2K
      V3=V3+HP1*Y1K
      V4=V4+HP1*Y2K
       GOTO 25
C*************************************************************
C Вычисление сумм для поля  токового диска, при RD1 >R > RD2  
C*************************************************************

 23      CONTINUE

       X2K=X2K*X2
       Y1K=Y1K/T2
      U2=U2+H1*X2K
      U3=U3+H2*Y1K
      V2=V2+HP1*X2K
      V3=V3+HP1*Y1K
       GOTO 25
C*************************************************************
C Вычисление  сумм для  поля  токового диска, при R >RD1 
C*************************************************************

 24      CONTINUE
      X1K=X1K*T2
      X2K=X2K*X2
      U1=U1+H1*X1K
      U2=U2+H1*X2K
      V1=V1+HP1*X1K
      V2=V2+HP1*X2K
 
 25      CONTINUE
 19   CONTINUE    

C*************************************************************
C Вычисление  поля  токового диска, по известным суммам 
C*************************************************************
 
      IF (R.GT.RD1) GOTO 4
      IF (R.GT.RD2) GOTO 3

C*************************************************************
C Вычисление  поля  токового диска, при R < RD2 
C*************************************************************
 
       EKSI2=EKSI*EKSI
       PB(1)=BDC1*(U3-U4/EKSI2) 
       PB(2)=BDC1*(V3-V4/EKSI2)
       PB(3)= 0.
C       WRITE (3,830)
C       WRITE (3,503) PB(1),PB(2),PB(3),R,CTE,STE,CFIE,SFIE
        GOTO 11
c                              RETURN
C*************************************************************
C Вычисление  поля  токового диска, при RD1 >R > RD2  
C*************************************************************

3      CONTINUE
       P=T3*EKSI
       PB(1)=BDC1*(T2*SIGN(1.,CTE)+U3+P*U2) 
       PB(2)=BDC1*(V3 -P*V2)
       PB(3)= 0.
       P11=SIGN(1.,CTE)
C       WRITE (3,831)
C       WRITE (3,502) PB(1),PB(2),PB(3),R,CTE,STE,CFIE,P11
         GOTO 11
c	                        RETURN
C*************************************************************
C Вычисление  поля  токового диска, при R >RD1 
C*************************************************************

4      CONTINUE
       P=BDC1*T3
       PB(1)=P*(EKSI*U2-U1) 
       PB(2)=P*(-EKSI*V2+V1)
       PB(3)= 0.                     
C       WRITE (3,832)
C       WRITE (3,502) PB(1),PB(2),PB(3),R,CTE,STE,CFIE,SFIE
         GOTO 11
c	                      RETURN
11      continue			      
C*********************************************************
C     Перевод в декартовы солнечно-магнитные координаты
C*********************************************************
      CALL PERE(PB,BDSM1,ZMU)

C*********************************************************
C     Перевод в декартовы солнечно-магнитосферные координаты
C*********************************************************
      rad=180./3.1415926                                                        
      tpsi=psi/rad                                                              
      call smtogsm(tpsi,sm2gsm)                                                 
      call pere2(bdsm1,bdsm,sm2gsm,1)                                             
 
C       WRITE (3,830)
C       WRITE (3,503) PB(1),PB(2),PB(3),R,CTE,STE,CFIE,SFIE

830     FORMAT
     *        (7X,10H-- R < RD2,
     *55H-------------------------------------------------------)
831     FORMAT
     *        (7X,18H-- RD2 < R < RD1-, 
     *45H---------------------------------------------)
832     FORMAT
     *        (7X,10H-- RD1 < R, 
     *55H-------------------------------------------------------)

 502          FORMAT
     * (3X,' BDR=',F15.6,5X,' BDT=',F15.6,5X,' BDFI=',F15.6,/
     *  3X,' R=',F15.6,' CTE=',F9.6,' STE=',F9.6,' CFIE=',F9.6,
     *' P11=',F9.6,/)

 503          FORMAT
     * (3X,' BDR=',F15.6,5X,' BDT=',F15.6,5X,' BDFI=',F15.6,/
     *  3X,' R=',F15.6,' CTE=',F9.6,' STE=',F9.6,' CFIE=',F9.6,
     *' SFIE=',F9.6,/)


833     FORMAT
     *        (7X,10H-- AL1 < AL,
     *55H-------------------------------------------------------)

 504          FORMAT
     * (3X,' BDA=',F15.6,5X,' BDB=',F15.6,5X,'BDSMX=',F15.6,/
     *  3X,'BDSMY=',F15.6,'BDSMZ=',F9.6,' XSM=',F9.6,' YSM=',F9.6,
     *' ZSM=',F9.6,/)
                                 RETURN
                              END


                    SUBROUTINE BEGDISK (UF)
C    Переделано
      COMMON/T21/BD1,R0,RKM,BK1,BKA,BKB,BKC
      COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP
      COMMON /COR2/CFI,SFI
      COMMON/COR3/R,CT,ST
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      COMMON/T1D/A1DISK(12)


      DIMENSION UF(3),UFR(3)
      DIMENSION EL(7),EL1(7),EL2(7)

      EL(1)=1.
      EL1(1)=1.
      EL(2)=CT
      EL1(2)=3.*CT
      EL2(1)=0.
      EL2(2)=3.
      EL2(3)=15.*CT
      DO 19 I=2,6
      P=1.-1./I
      EL(I+1)=(1.+P)*CT*EL(I)-P*EL(I-1)
19    EL1(I+1)=(3.-P)*CT*EL1(I)-(2.-P)*EL1(I-1)
      DO 20 I=3,6
      P=1.+3./(I-1)
      EL2(I+1)=(1.+P)*CT*EL2(I)-P*EL2(I-1)
   20 CONTINUE
      T=A1DISK(6)
      U3=6.*T*EL1(7)
      V=EL1(6)
      U2=T*V
      U1=6.*U2
      T=A1DISK(12)
      V1=6.*T*EL(7)
      V2=T*V
      I=5
21    CONTINUE
      T=A1DISK(I)
      U3=U3*R+I*T*EL1(I+1)
      U1=U1*R+I*T*EL1(I)
      U2=U2*R+T*EL1(I)
      T=A1DISK(I+6)
      V1=V1*R+I*T*EL(I+1)
      V2=V2*R+T*EL1(I)
      I=I-1
      IF(I.GT.0)GOTO 21
      UF(1)=-ST*CFI*U1+V1
      UF(2)=CFI*(CT*(U1+U2)-U3)-V2*ST
      UF(3)=SFI*U2
      RETURN
      END


                                       SUBROUTINE FLYDISK (P)

C***************************************************
C     Исправлено и добавлен обход нуля
C***************************************************
      COMMON/TDISK/RD1,RD2,EKSI,BDC1,BDC11,BDISKDIP
      COMMON/S2D/ CF0D(5),CF1D(5),CF2D(5),CF3D(5),CF4D(5)
      REAL L,L0
      COMMON /T3/ L(6,5),L0(5)
      COMMON /COR1/AL,BE,SQ,PQ,QA,BEMIN
      COMMON /COR2/CFI,SFI
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      DIMENSION P(3)

                              IF (BE.GT.BEMIN) GO TO 10

C***************************************************
C     ОБХОД ДЛЯ BE РАВНОГО НУЛЮ.
C***************************************************
      Y3=0.
           DO 1 N=1,5
           Z=L(1,N)
           CALL BESK(1,Z*AL,V,DV)
           Y3=Y3+CF1D(N)*Z*V/2
1          CONTINUE
      P(1)=0.
      P(2)=0.
      P(3)=Y3*SFI/AL

                                                    RETURN
C***************************************************
10                               CONTINUE
      Y1=0.
      Y2=0.
      Y3=0.
      Y6=0.
      Y7=0.
           DO 2 N=1,5
           Z=L(1,N)
           CALL BESS(1,Z*BE,U,DU)
           CALL BESK(1,Z*AL,V,DV)
           X=CF2D(N)
           Y1=Y1+X*U*DV
           Y2=Y2+X*DU*V
           Y3=Y3+CF1D(N)*U*V
           Z=L0(N)
           X=Z*BE
           Z=Z*AL
           DU=CF0D(N)
           DV=CF4D(N)
           U=BESK0(Z)
           Z=BESK1(Z)
           V=BESJ0(X)
           X=BESJ1(X)
           Y6=Y6+DU*Z*V
           Y7=Y7+DU*U*X
  2        CONTINUE
      P(1)=(-Y1*CFI+Y6)/SQ
      P(2)=(-Y2*CFI+Y7)/SQ
      P(3)=Y3*SFI/PQ

C        WRITE (11,52)
C	WRITE (11,51) P,AL,BE,CFI
 52   FORMAT('****************FLYDISK(P)********************')
 51   FORMAT(2X,3F9.4,3X,3F9.4)
                                                 RETURN
                                                END



                          SUBROUTINE BFAC (P)
C*************************************************************
C Вычисление  поля продольных токов,  которые текут по поверх-
C ности конуса. R,CTE,STE,CFIE,SFIE - геомагнитные сферические
C координаты,  R - расстояние от центра Земли в радиусах Земли,
C CTE и STE - cos(teta) и sin(teta), где teta - коширота, CFIE
C и SFIE  - cos(fie) и sin(fie), где fie - долгота от полдня.
C BFACP0=0,000098*AJ0/STM, где AJ0 - полный продольный ток
C в одном полушарии и BFACP1=BFACP0*(1-CTM).
C*************************************************************


      COMMON/COR3/R,CT,ST
      COMMON/COR4/CTE,STE,CFIE,SFIE
      COMMON/TFAC/STM,CTM,BFAC0,BFAC1,TETAM,AJ0
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      DIMENSION P(3)

      IF (R.GT.1.AND.R.LT.R1) GOTO 3
       P(1)= 0.
       P(2)= 0.
       P(3)= 0.
                              RETURN
3      CONTINUE

      U=BFAC0/R
      U1=BFAC1/R
      IF (STE.GT.STM)        GOTO 2
      IF (CTE.GT.0)          GOTO 1
C***************************************************
C             ЮЖНАЯ ПОЛЯРНАЯ ШАПКА
C***************************************************
       U3=U/(1-CTE)
       P(1)= 0.
       P(2)= U3*CFIE
       P(3)= U3*SFIE

                              RETURN
C***************************************************
C            СЕВЕРНАЯ ПОЛЯРНАЯ ШАПКА
C***************************************************
1      CONTINUE
       U3=U/(1+CTE)
       P(1)= 0.
       P(2)= U3*CFIE
       P(3)= -U3*SFIE

                              RETURN
C***************************************************
C             СЕРДЦЕВИНА МАГНИТОСФЕРЫ
C***************************************************
2      CONTINUE
       U3=U1/STE/STE
       P(1)= 0.
       P(2)= U3*CFIE
       P(3)= U3*SFIE*CTE
                              RETURN
                              END

                    SUBROUTINE BEG (UF)
C    Переделано

      COMMON /COR2/CFI,SFI
      COMMON/COR3/R,CT,ST
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      COMMON/T21/BD1,R0,RKM,BK1,BKA,BKB,BKC
      COMMON/T1/A1(12)


      DIMENSION UF(3)
      DIMENSION EL(7),EL1(7),EL2(7)
c      print *,BD1,R0,RKM,BK1,BKA,BKB,BKC

      EL(1)=1.
      EL1(1)=1.
      EL(2)=CT
      EL1(2)=3.*CT
      EL2(1)=0.
      EL2(2)=3.
      EL2(3)=15.*CT
      DO 19 I=2,6
      P=1.-1./I
      EL(I+1)=(1.+P)*CT*EL(I)-P*EL(I-1)
19    EL1(I+1)=(3.-P)*CT*EL1(I)-(2.-P)*EL1(I-1)
      DO 20 I=3,6
      P=1.+3./(I-1)
      EL2(I+1)=(1.+P)*CT*EL2(I)-P*EL2(I-1)
   20 CONTINUE
      T=A1(6)
      U3=6.*T*EL1(7)
      V=EL1(6)
      U2=T*V
      U1=6.*U2
      T=A1(12)
      V1=6.*T*EL(7)
      V2=T*V
      I=5
21    CONTINUE
      T=A1(I)
      U3=U3*R+I*T*EL1(I+1)
      U1=U1*R+I*T*EL1(I)
      U2=U2*R+T*EL1(I)
      T=A1(I+6)
      V1=V1*R+I*T*EL(I+1)
      V2=V2*R+T*EL1(I)
      I=I-1
      IF(I.GT.0)GOTO 21
c Chapman-Ferraro currents MF:
      UF(1)=-ST*CFI*U1+V1                                                     
      UF(2)=CFI*(CT*(U1+U2)-U3)-V2*ST                                         
      UF(3)=SFI*U2
C       print *, 'beg   ',uf                                                            
C      WRITE (11,52)
C      WRITE (11,51) uf,u1,u2,u3
 52   FORMAT('*******************BEG(UF,VV)********************')
 51   FORMAT(2X,3F9.4,3X,3F9.4)
      RETURN
      END


                                       SUBROUTINE FLYD (P)

C***************************************************
C     Исправлено и добавлен обход нуля
C***************************************************
      COMMON/S2/ CF0(5),CF1(5),CF2(5),CF3(5),CF4(5)
      REAL L,L0
      COMMON /T3/ L(6,5),L0(5)
      COMMON /COR1/AL,BE,SQ,PQ,QA,BEMIN
      COMMON /COR2/CFI,SFI
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      COMMON/T21/BD1,R0,RKM,BK1,BKA,BKB,BKC
      DIMENSION P(3)
                              IF (BE.GT.BEMIN) GO TO 10

C***************************************************
C     ОБХОД ДЛЯ BE РАВНОГО НУЛЮ.
C***************************************************
      Y3=0.
           DO 1 N=1,5
           Z=L(1,N)
           CALL BESK(1,Z*AL,V,DV)
           Y3=Y3+CF1(N)*Z*V/2
1          CONTINUE
      P(1)=0.
      P(2)=0.
      P(3)=Y3*SFI/AL

                                                    RETURN
C***************************************************
10                               CONTINUE
      Y1=0.
      Y2=0.
      Y3=0.
      Y6=0.
      Y7=0.
           DO 2 N=1,5
           Z=L(1,N)
           CALL BESS(1,Z*BE,U,DU)
           CALL BESK(1,Z*AL,V,DV)
           X=CF2(N)
           Y1=Y1+X*U*DV
           Y2=Y2+X*DU*V
           Y3=Y3+CF1(N)*U*V
           X=CF3(N)
           Z=L0(N)
           X=Z*BE
           Z=Z*AL
           DU=CF0(N)
           DV=CF4(N)
           U=BESK0(Z)
           Z=BESK1(Z)
           V=BESJ0(X)
           X=BESJ1(X)
           Y6=Y6+DU*Z*V
           Y7=Y7+DU*U*X
    2      CONTINUE
      P(1)=(-Y1*CFI+Y6)/SQ
      P(2)=(-Y2*CFI+Y7)/SQ
      P(3)=Y3*SFI/PQ
                                                RETURN
                                                END


                                      SUBROUTINE DERY4D (P)
C************************************************************
C    Переделано, включены обходы для AL и BE близких к 0
C      24.06.94
C************************************************************

      REAL L,L0
      COMMON /COR1/AL,BE,SQ,PQ,QA,BEMIN
      COMMON /COR2/CFI,SFI
      COMMON/T2/PI,R1,R2,BETA0,AL0,C0,E5,AL1,BT,CPSI,SPSI,PSI,Z0,B0,BD
      COMMON/T3/ L(6,5),L0(5)
      COMMON/S1/CB(6,5),CB2(6,5),
     +CD(6,5),CB3(6,5),CD2(6,5),CD3(6,5)
      DIMENSION P(3),U(6,5),DU(6,5)
C************************************************************
C Обход для BE<BEMIN
C************************************************************
                                       IF(BE-BEMIN)1,1,2
1     CONTINUE
              DO 3  K=1,6
              M=2*K-1
                DO 3  N=1,5
                CALL BESK(M,AL*L(K,N),Z,DZ)
                U(K,N)=Z*E5
                DU(K,N)=DZ*E5
3             CONTINUE
      R=+1.
      V3=0.
      BE1=BE/BETA0
      CVI=CFI
      SVI=-SFI
      SVS=2.*SFI*CFI
      CVS=2.*CFI**2-1.
               DO 4  K=1,6
               Y3=0.
               CMFI=CVI*CVS-SVI*SVS
               SMFI=SVI*CVS+CVI*SVS
               CVI=CMFI
               SVI=SMFI
                    DO 5  N=1,5
                    Z=L(K,N)/2
                    X1=CD(K,N)
                    Y3=Y3+X1*Z*U(K,N)
5                   CONTINUE
               X3=SMFI*R
               V3=V3+Y3*X3
               R=-R
4              CONTINUE
      P(1)=0.
      P(2)=0.
      P(3)=V3/AL
                                            RETURN
2                                         CONTINUE
                                     IF(AL-AL0) 6,6,7
C************************************************************
C     ВАРИАНТ РАССЧЕТА ДЛЯ AL<AL0
C************************************************************
6     CONTINUE
C************************************************************
C Обход для AL<BEMIN
C************************************************************
                                     IF(AL-BEMIN) 8,8,9
   8  CONTINUE
               DO 10 K=1,6
                   DO 10 N=1,5
                   U(K,N)=L(K,N)/2
  10               CONTINUE
      W=0.
      R=+1.
      V3=0.
      BE1=BE/BETA0
      CVI=CFI
      SVI=-SFI
      SVS=2.*SFI*CFI
      CVS=2.*CFI**2-1.
            DO 11 K=1,6
            M=2*K-1
            Y3=0.
            CMFI=CVI*CVS-SVI*SVS
            SMFI=SVI*CVS+CVI*SVS
            CVI=CMFI
            SVI=SMFI
                DO 12 N=1,5
                X=L(K,N)*BE1
                CALL BESS(M,X,Z,DZ)
                X1=CB(K,N)
                Y3=Y3+X1*Z*U(K,N)
  12            CONTINUE
            X3=SMFI*R
            V3=V3+Y3*X3
            R=-R
  11        CONTINUE
      P(1)=0.
      P(2)=0.
      P(3)=V3/BE1
                                            RETURN
  9   CONTINUE
C************************************************************
           DO 13 K=1,6
           M=2*K-1
              DO 13 N=1,5
              X =AL *L(K,N)
              Y=X -16.118095651
              E4=EXP(Y)
              CALL BESM(M,X,Z,DZ)
              U(K,N)=Z*E4
              DU(K,N)=DZ*E4
  13          CONTINUE
      W=0.
      R=+1.
      V2=0.
      V1=0.
      V3=0.
      BE1=BE/BETA0
      CVI=CFI
      SVI=-SFI
      SVS=2.*SFI*CFI
      CVS=2.*CFI**2-1.
           DO 14 K=1,6
           M=2*K-1
           Y1=0.
           Y2=0.
           Y3=0.
           CMFI=CVI*CVS-SVI*SVS
           SMFI=SVI*CVS+CVI*SVS
           CVI=CMFI
           SVI=SMFI
                DO 15  N=1,5
                X=L(K,N)*BE1
                CALL BESS(M,X,Z,DZ)
                X1=CB(K,N)
                X2=CB2(K,N)
                Y1=Y1+X2*Z*DU(K,N)
                Y2=Y2+X2*DZ*U(K,N)
                Y3=Y3+X1*Z*U(K,N)
 15             CONTINUE
           X1=CMFI/M*R
           X2=CMFI*M*R
           X3=SMFI*R
           V1=V1+Y1*X1
           V2=V2+Y2*X1
           V3=V3+Y3*X3
           R=-R
 14        CONTINUE
      V1=V1*E5
      V2=V2*E5
      V3=V3*E5
      P(1)=(-V1-W*AL)/SQ
      P(2)=-V2/SQ
      P(3)=V3/PQ
                                            RETURN

C************************************************************
C     ВАРИАНТ РАССЧЕТА ДЛЯ AL>AL0
C************************************************************
7     CONTINUE
          DO 16 K=1,6
          M=2*K-1
             DO 16 N=1,5
             X=AL*L(K,N)
             CALL BESK(M,X,Z,DZ)
             U(K,N)=Z*E5
             DU(K,N)=DZ*E5
16           CONTINUE
      W=+SIGN(1.,CFI)*C0/AL**2
      IF (CFI.EQ.0.) W=0.
      R=+1.
      V2=0.
      V1=0.
      V3=0.
      BE1=BE/BETA0
      CVI=CFI
      SVI=-SFI
      SVS=2.*SFI*CFI
      CVS=2.*CFI**2-1.
         DO 17 K=1,6
         M=2*K-1
         Y1=0.
         Y2=0.
         Y3=0.
         CMFI=CVI*CVS-SVI*SVS
         SMFI=SVI*CVS+CVI*SVS
         CVI=CMFI
         SVI=SMFI
             DO 18 N=1,5
             X=L(K,N)*BE1
             CALL BESS(M,X,Z,DZ)
             X1=CD(K,N)
             X2=CD2(K,N)
             Y1=Y1+X2*Z*DU(K,N)
             Y2=Y2+X2*DZ*U(K,N)
             Y3=Y3+X1*Z*U(K,N)
 18          CONTINUE
         X1=CMFI/M*R
         X2=CMFI*M*R
         X3=SMFI*R
         V1=V1+Y1*X1
         V2=V2+Y2*X1
         V3=V3+Y3*X3
         R=-R
 17      CONTINUE
      P(1)=(-V1-W*AL)/SQ
      P(2)=-V2/SQ
      P(3)=V3/PQ
                                            RETURN
                                            END





                              SUBROUTINE BESK(M,X,V,DV)
      U=BESK0(X)
      V=BESK1(X)
      L=M-1
      IF(L)3,4,3
3     DO 2 K=1,L
      S=2.*K*V/X+U
      U=V
2     V=S
4     DV=-(M*V/X+U)
                                          RETURN
                                          END
                                     SUBROUTINE BESM(M,X,V,DV)
      COMMON/S5/CI0(9),CI1(9)
      IF(X.GT.3.75)GO TO 5
      XA=-X
      IF(XA-174.673)50,50,51
 51   PRINT 52,X
52    FORMAT(18H GRAND EXP-BESM,X=,E12.5)
      X=-174.670
50    CONTINUE
      E=EXP(-X)
      V=BSI(M,X)*E
      DV=0.5*E*(BSI(M-1,X)+BSI(M+1,X))
      RETURN
5     CONTINUE
      IF(X)90,91,91
90    PRINT 92,X
92    FORMAT(19H NEGATIVE X-BESM,X=,E12.5)
   91 X=ABS(X)
      U=UG(CI0(1),X)/SQRT(X)
      V=UG(CI1(1),X)/SQRT(X)
      L=M-1
      IF(L)3,4,3
3     DO 2 K=1,L
      S=U-2.*K*V/X
      U=V
2     V=S
4     DV=U-M*V/X
      RETURN
                                        END
                                    SUBROUTINE BESS(M,X,V,DV)
      IF(X.GT.3.75)GO TO 5
      V=BSJ(M,X)
      DV=0.5*(BSJ(M-1,X)-BSJ(M+1,X))
                                          RETURN
5     U=BESJ0(X)
      V=BESJ1(X)
      L=M-1
      IF(L)3,4,3
3     DO 2 K=1,L
      S=2*K*V/X-U
      U=V
2      V=S
4     DV=U-M*V/X
                                          RETURN
                                      END
                                        FUNCTION BSJ(N,X)
      SUM=1.
      P=1.
      Z=-X*X/4.
      DO 2 K=1,7
      P=P*Z/K/(K+N)
2     SUM=SUM+P
      IF(N.LE.0)GO TO 3
      DO 1 K=1,N
1     SUM=SUM/K
  3   CONTINUE
7     FORMAT(16H EXP NEGATIVE,N=,I3,2HX=,E12.5)
      IF(X)4,5,4
5     CONTINUE
      BSJ=0.
      PRINT 7,N,X
      GO TO 6
4     CONTINUE
      BSJ=SUM*(X/2.)**N
6                                    RETURN
                                      END
                                     FUNCTION BSI(N,X)
      SUM=1.
      P=1.
      Z=X*X/4.
      DO 2 K=1,7
      P=P*Z/K/(K+N)
2     SUM=SUM+P
      IF(N.LE.0)GO TO 3
      DO 1 K=1,N
1     SUM=SUM/K
  3   CONTINUE
7     FORMAT(16H EXP NEGATIVE,N=,I3,2HX=,E12.5)
      IF(X)4,5,4
5     CONTINUE
      PRINT 7,N,X
      BSI=0.
      GO TO 6
4     CONTINUE
      BSI=SUM*(X/2.)**N
6                                          RETURN
                                            END
                                        REAL FUNCTION UG(V,X)
      DIMENSION V(9)
      UG=V(1)
      DO 7 I=2,9
      UG=UG*(3.75/X)+V(I)
7     CONTINUE
                                        RETURN
                                             END
                                           FUNCTION BESJY(X)
C
      LOGICAL L
C
                                  ENTRY BESJ0(X)
C
      L=.TRUE.
      V=ABS(X)
      IF(V .GE. 8.0) GO TO 4
    8 F=0.0625*X**2-2.0
      A =           - 0.00000 00000 000008
      B = F * A     + 0.00000 00000 000413
      A = F * B - A - 0.00000 00000 019438
      B = F * A - B + 0.00000 00000 784870
      A = F * B - A - 0.00000 00026 792535
      B = F * A - B + 0.00000 00760 816359
      A = F * B - A - 0.00000 17619 469078
      B = F * A - B + 0.00003 24603 288210
      A = F * B - A - 0.00046 06261 662063
      B = F * A - B + 0.00481 91800 694676
      A = F * B - A - 0.03489 37694 114089
      B = F * A - B + 0.15806 71023 320973
      A = F * B - A - 0.37009 49938 726498
      B = F * A - B + 0.26517 86132 033368
      A = F * B - A - 0.00872 34423 528522
      A = F * A - B + 0.31545 59429 497802
      BESJY=0.5*(A-B)
      IF(L)                                      RETURN
C
      A =           + 0.00000 00000 000016
      B = F * A     - 0.00000 00000 000875
      A = F * B - A + 0.00000 00000 040263
      B = F * A - B - 0.00000 00001 583755
      A = F * B - A + 0.00000 00052 487948
      B = F * A - B - 0.00000 01440 723327
      A = F * B - A + 0.00000 32065 325377
      B = F * A - B - 0.00005 63207 914106
      A = F * B - A + 0.00075 31135 932578
      B = F * A - B - 0.00728 79624 795521
      A = F * B - A + 0.04719 66895 957634
      B = F * A - B - 0.17730 20127 811436
      A = F * B - A + 0.26156 73462 550466
      B = F * A - B + 0.17903 43140 771827
      A = F * B - A - 0.27447 43055 297453
      A = F * A - B - 0.06629 22264 065699
      BESJY=0.636619772367581*ALOG(X)*BESJY+0.5*(A-B)
                                        RETURN
C
    4 F=256.0/X**2-2.0
      B =           + 0.00000 00000 000007
      A = F * B     - 0.00000 00000 000051
      B = F * A - B + 0.00000 00000 000433
      A = F * B - A - 0.00000 00000 004305
      B = F * A - B + 0.00000 00000 051683
      A = F * B - A - 0.00000 00000 786409
      B = F * A - B + 0.00000 00016 306465
      A = F * B - A - 0.00000 00517 059454
      B = F * A - B + 0.00000 30751 847875
      A = F * B - A - 0.00053 65220 468132
      A = F * A - B + 1.99892 06986 950373
      P=A-B
      B =           - 0.00000 00000 000006
      A = F * B     + 0.00000 00000 000043
      B = F * A - B - 0.00000 00000 000334
      A = F * B - A + 0.00000 00000 003006
      B = F * A - B - 0.00000 00000 032067
      A = F * B - A + 0.00000 00000 422012
      B = F * A - B - 0.00000 00007 271916
      A = F * B - A + 0.00000 00179 724572
      B = F * A - B - 0.00000 07414 498411
      A = F * B - A + 0.00006 83851 994261
      A = F * A - B - 0.03111 17092 106740
      Q=8.0*(A-B)/V
      F=V-0.785398163397448
      A=COS(F)
      B=SIN(F)
      F=0.398942280401432/SQRT(V)
      IF(L) GO TO 6
      BESJY=F*(Q*A+P*B)
      RETURN
    6 BESJY=F*(P*A-Q*B)
                                        RETURN
C
      ENTRY BESJ1(X)
C
      L=.TRUE.
      V=ABS(X)
      IF(V .GE. 8.0) GO TO 5
    3 F=0.0625*X**2-2.0
      B =           + 0.00000 00000 000114
      A = F * B     - 0.00000 00000 005777
      B = F * A - B + 0.00000 00000 252812
      A = F * B - A - 0.00000 00009 424213
      B = F * A - B + 0.00000 00294 970701
      A = F * B - A - 0.00000 07617 587805
      B = F * A - B + 0.00001 58870 192399
      A = F * B - A - 0.00026 04443 893486
      B = F * A - B + 0.00324 02701 826839
      A = F * B - A - 0.02917 55248 061542
      B = F * A - B + 0.17770 91172 397283
      A = F * B - A - 0.66144 39341 345433
      B = F * A - B + 1.28799 40988 576776
      A = F * B - A - 1.19180 11605 412169
      A = F * A - B + 1.29671 75412 105298
      BESJY=0.0625*(A-B)*X
      IF(L)                                RETURN
C
      B =           - 0.00000 00000 000244
      A = F * B     + 0.00000 00000 012114
      B = F * A - B - 0.00000 00000 517212
      A = F * B - A + 0.00000 00018 754703
      B = F * A - B - 0.00000 00568 844004
      A = F * B - A + 0.00000 14166 243645
      B = F * A - B - 0.00002 83046 401495
      A = F * B - A + 0.00044 04786 298671
      B = F * A - B - 0.00513 16411 610611
      A = F * B - A + 0.04231 91803 533369
      B = F * A - B - 0.22662 49915 567549
      A = F * B - A + 0.67561 57807 721877
      B = F * A - B - 0.76729 63628 866459
      A = F * B - A - 0.12869 73843 813500
      A = F * A - B + 0.04060 82117 718685
      BESJY=0.636619772367581*ALOG(X)*BESJY-0.636619772367581/X
     1     +0.0625*(A-B)*X
                                    RETURN
C
    5 F=256.0/X**2-2.0
      B =           - 0.00000 00000 000007
      A = F * B     + 0.00000 00000 000055
      B = F * A - B - 0.00000 00000 000468
      A = F * B - A + 0.00000 00000 004699
      B = F * A - B - 0.00000 00000 057049
      A = F * B - A + 0.00000 00000 881690
      B = F * A - B - 0.00000 00018 718907
      A = F * B - A + 0.00000 00617 763396
      B = F * A - B - 0.00000 39872 843005
      A = F * B - A + 0.00089 89898 330859
      A = F * A - B + 2.00180 60817 200274
      P=A-B
      B =           + 0.00000 00000 000007
      A = F * B     - 0.00000 00000 000046
      B = F * A - B + 0.00000 00000 000360
      A = F * B - A - 0.00000 00000 003264
      B = F * A - B + 0.00000 00000 035152
      A = F * B - A - 0.00000 00000 468636
      B = F * A - B + 0.00000 00008 229193
      A = F * B - A - 0.00000 00209 597814
      B = F * A - B + 0.00000 09138 615258
      A = F * B - A - 0.00009 62772 354916
      A = F * A - B + 0.09355 55741 390707
      Q=8.0*(A-B)/V
      F=V-2.356194490192345
      A=COS(F)
      B=SIN(F)
      F=0.398942280401432/SQRT(V)
      IF(L) GO TO 7
      BESJY=F*(Q*A+P*B)
                                         RETURN
    7 BESJY=F*(P*A-Q*B)
      IF(X .LT. 0.0) BESJY=-BESJY
                                    RETURN
C
                                  ENTRY BESY0(X)
C
      IF(X .LE. 0.0) GO TO 9
      L=.FALSE.
      V=X
      IF(V .GE. 8.0) GO TO 4
      GO TO 8
C
                                    ENTRY BESY1(X)
C
      IF(X .LE. 0.0) GO TO 9
      L=.FALSE.
      V=X
      IF(V .GE. 8.0) GO TO 5
      GO TO 3
C
    9 BESJY=0.
      PRINT 100,X
                                        RETURN
100   FORMAT(1X,32HBESJY...NON-POSITIVE ARGUMENT X=,E12.5)
C
                                            END
                                      FUNCTION BESIK(X)
      LOGICAL L,E
C
                                       ENTRY EBESI0(X)
C
      E=.TRUE.
      GO TO 1
      ENTRY BESI0(X)
      E=.FALSE.
    1 L=.TRUE.
      V=ABS(X)
      IF(V .GE. 8.0) GO TO 4
    8 F=0.0625*X**2-2.0
      A =               0.00000 00000 00002
      B = F * A     +   0.00000 00000 00120
      A = F * B - A +   0.00000 00000 06097
      B = F * A - B +   0.00000 00002 68828
      A = F * B - A +   0.00000 00101 69727
      B = F * A - B +   0.00000 03260 91051
      A = F * B - A +   0.00000 87383 15497
      B = F * A - B +   0.00019 24693 59688
      A = F * B - A +   0.00341 63317 66012
      B = F * A - B +   0.04771 87487 98174
      A = F * B - A +   0.50949 33654 39983
      B = F * A - B +   4.01167 37601 79349
      A = F * B - A +  22.27481 92424 62231
      B = F * A - B +  82.48903 27440 24100
      A = F * B - A + 190.49432 01727 42844
      A = F * A - B + 255.46687 96243 62167
      BESIK=0.5*(A-B)
      IF(L .AND. E) BESIK=EXP(-V)*BESIK
      IF(L)                                RETURN
      A =           +   0.00000 00000 00003
      B = F * A     +   0.00000 00000 00159
      A = F * B - A +   0.00000 00000 07658
      B = F * A - B +   0.00000 00003 18588
      A = F * B - A +   0.00000 00112 81211
      B = F * A - B +   0.00000 03351 95256
      A = F * B - A +   0.00000 82160 25940
      B = F * A - B +   0.00016 27083 79043
      A = F * B - A +   0.00253 63081 88086
      B = F * A - B +   0.03008 07224 20512
      A = F * B - A +   0.25908 44324 34900
      B = F * A - B +   1.51153 56760 29228
      A = F * B - A +   5.28363 28668 73920
      B = F * A - B +   8.00536 88687 00334
      A = F * B - A -   4.56343 35864 48395
      A = F * A - B -  21.05766 01774 02440
      BESIK=-ALOG(0.125*X)*BESIK+0.5*(A-B)
      IF(E) BESIK=EXP(X)*BESIK
                                    RETURN
    4 F=32.0/V-2.0
      B =           - 0.00000 00000 00001
      A = F * B     - 0.00000 00000 00001
      B = F * A - B + 0.00000 00000 00004
      A = F * B - A + 0.00000 00000 00010
      B = F * A - B - 0.00000 00000 00024
      A = F * B - A - 0.00000 00000 00104
      B = F * A - B + 0.00000 00000 00039
      A = F * B - A + 0.00000 00000 00966
      B = F * A - B + 0.00000 00000 01800
      A = F * B - A - 0.00000 00000 04497
      B = F * A - B - 0.00000 00000 33127
      A = F * B - A - 0.00000 00000 78957
      B = F * A - B + 0.00000 00000 29802
      A = F * B - A + 0.00000 00012 38425
      B = F * A - B + 0.00000 00085 13091
      A = F * B - A + 0.00000 00568 16966
      B = F * A - B + 0.00000 05135 87727
      A = F * B - A + 0.00000 72475 91100
      B = F * A - B + 0.00017 27006 30778
      A = F * B - A + 0.00844 51226 24921
      A = F * A - B + 2.01655 84109 17480
      BESIK=0.199471140200717*(A-B)/SQRT(V)
      IF(E)                                      RETURN
      BESIK=EXP(V)*BESIK
                                         RETURN
                                         ENTRY EBESI1(X)
      E=.TRUE.
      GO TO 2
                                    ENTRY BESI1(X)
      E=.FALSE.
    2 L=.TRUE.
      V=ABS(X)
      IF(V .GE. 8.0) GO TO 3
    7 F=0.0625*X**2-2.0
      A =           +   0.00000 00000 00001
      B = F * A     +   0.00000 00000 00031
      A = F * B - A +   0.00000 00000 01679
      B = F * A - B +   0.00000 00000 79291
      A = F * B - A +   0.00000 00032 27617
      B = F * A - B +   0.00000 01119 46285
      A = F * B - A +   0.00000 32641 38122
      B = F * A - B +   0.00007 87567 85754
      A = F * B - A +   0.00154 30190 15627
      B = F * A - B +   0.02399 30791 47841
      A = F * B - A +   0.28785 55118 04672
      B = F * A - B +   2.57145 99063 47755
      A = F * B - A +  16.33455 05525 22066
      B = F * A - B +  69.39591 76337 34448
      A = F * B - A + 181.31261 60405 70265
      A = F * A - B + 259.89023 78064 77292
      BESIK=0.0625*(A-B)*X
      IF(L .AND. E) BESIK=EXP(-V)*BESIK
      IF(L)                                    RETURN
      A =           +   0.00000 00000 00001
      B = F * A     +   0.00000 00000 00042
      A = F * B - A +   0.00000 00000 02163
      B = F * A - B +   0.00000 00000 96660
      A = F * B - A +   0.00000 00036 96783
      B = F * A - B +   0.00000 01193 67971
      A = F * B - A +   0.00000 32025 10692
      B = F * A - B +   0.00007 00106 27855
      A = F * B - A +   0.00121 70569 94516
      B = F * A - B +   0.01630 00492 89816
      A = F * B - A +   0.16107 43016 56148
      B = F * A - B +   1.10146 19930 04852
      A = F * B - A +   4.66638 70268 62842
      B = F * A - B +   9.36161 78313 95389
      A = F * B - A -   1.83923 92242 86199
      A = F * A - B -  26.68809 54808 62668
      BESIK=ALOG(0.125*X)*BESIK+1.0/X-0.0625*(A-B)*X
      IF(E) BESIK=EXP(X)*BESIK
                                    RETURN
    3 F=32.0/V-2.0
      B =           + 0.00000 00000 00001
      A = F * B     + 0.00000 00000 00001
      B = F * A - B - 0.00000 00000 00005
      A = F * B - A - 0.00000 00000 00010
      B = F * A - B + 0.00000 00000 00026
      A = F * B - A + 0.00000 00000 00107
      B = F * A - B - 0.00000 00000 00053
      A = F * B - A - 0.00000 00000 01024
      B = F * A - B - 0.00000 00000 01804
      A = F * B - A + 0.00000 00000 05103
      B = F * A - B + 0.00000 00000 35408
      A = F * B - A + 0.00000 00000 81531
      B = F * A - B - 0.00000 00000 47563
      A = F * B - A - 0.00000 00014 01141
      B = F * A - B - 0.00000 00096 13873
      A = F * B - A - 0.00000 00659 61142
      B = F * A - B - 0.00000 06297 24239
      A = F * B - A - 0.00000 97321 46728
      B = F * A - B - 0.00027 72053 60764
      A = F * B - A - 0.02446 74429 63276
      A = F * A - B + 1.95160 12046 52572
      BESIK=0.199471140200717*(A-B)/SQRT(V)
      IF(X .LT. 0.0) BESIK=-BESIK
      IF(E)                                          RETURN
      BESIK=EXP(V)*BESIK
                                               RETURN
      ENTRY EBESK0(X)
      E=.TRUE.
      GO TO 11
                                           ENTRY BESK0(X)
      E=.FALSE.
   11 IF(X .LE. 0.0) GO TO 9
      L=.FALSE.
      V=X
      IF(X .LT. 5.0) GO TO 8
      F=20.0/X-2.0
      A =           - 0.00000 00000 00002
      B = F * A     + 0.00000 00000 00011
      A = F * B - A - 0.00000 00000 00079
      B = F * A - B + 0.00000 00000 00581
      A = F * B - A - 0.00000 00000 04580
      B = F * A - B + 0.00000 00000 39044
      A = F * B - A - 0.00000 00003 64547
      B = F * A - B + 0.00000 00037 92996
      A = F * B - A - 0.00000 00450 47338
      B = F * A - B + 0.00000 06325 75109
      A = F * B - A - 0.00001 11066 85197
      B = F * A - B + 0.00026 95326 12763
      A = F * B - A - 0.01131 05046 46928
      A = F * A - B + 1.97681 63484 61652
      BESIK=0.626657068657750*(A-B)/SQRT(X)
      IF(E)                                RETURN
      BESIK=EXP(-X)*BESIK
                                          RETURN
                                     ENTRY EBESK1(X)
      E=.TRUE.
      GO TO 12
                                     ENTRY BESK1(X)
      E=.FALSE.
   12 IF(X .LE. 0.0) GO TO 9
      L=.FALSE.
      V=X
      IF(X .LT. 5.0) GO TO 7
      F=20.0/X-2.0
      A =           + 0.00000 00000 00002
      B = F * A     - 0.00000 00000 00013
      A = F * B - A + 0.00000 00000 00089
      B = F * A - B - 0.00000 00000 00663
      A = F * B - A + 0.00000 00000 05288
      B = F * A - B - 0.00000 00000 45757
      A = F * B - A + 0.00000 00004 35417
      B = F * A - B - 0.00000 00046 45555
      A = F * B - A + 0.00000 00571 32218
      B = F * A - B - 0.00000 08451 72048
      A = F * B - A + 0.00001 61850 63810
      B = F * A - B - 0.00046 84750 28167
      A = F * B - A + 0.03546 52912 43331
      A = F * A - B + 2.07190 17175 44716
      BESIK=0.626657068657750*(A-B)/SQRT(X)
      IF(E)                                    RETURN
      BESIK=EXP(-X)*BESIK
                                               RETURN
    9 BESIK=0.
      PRINT 200,X
200   FORMAT(1X,32HBESIK...NON-POSITIVE ARGUMENT X=,E12.5)
                                                 RETURN
                                                  END

