c  Saturn's magnetic field calculation
*  Ver.1 (28.09.2005)
      INTEGER ut
      DIMENSION X0(3), bm(3), par(11),bimf(3),X0III(3),bcm(7,3),bexp(3),
     *xs(3), x_kg(3), b_s(3), x_kgs(3), sm2gsm(3,3),x1(3),x2(3),x3(3)
     *,wm(3),abc(7), x0_sm(3),bexp1(3),s2car(3,3), rb_s(3),rbm(3)
      COMMON/TRAN/g2gsm(3,3)                                                    
      COMMON/A/Y(3),F(5),V(3),U(3),YF(3)

      OPEN (3,  FILE='in/inf')
      OPEN (4,  FILE='in/infile')
      OPEN (5, FILE='out/lineXYZ')

       rad=180./3.1415926
       h=0.5
       sl=1000.
       kint=1
       kext=1
      
       read (3,307) ut,iyear,im,id
c       write (*,*) ut,iyear,im,id
       read (4,*) bdc11,bt,rd1,rd2,R1,R2,bimf(1),bimf(2),bimf(3)       
      
c      bdc11=3.6
c      bt=-8.7
c      rd1=12.5
c      rd2=6.5
c      R1=17.5
c      R2=14.
c      aj0=0.
c      bimf(1)=0.0
c      bimf(2)=-0.08
c      bimf(3)=-0.08
c      iyear=2004                                                                
c      id=26                                                                   
c      im=1                                                                  
c      ut=10                                                                  

       AJ0=AJ0*1000000.
      write (*,*) 
      call strans (UT,ID,im,iyear,psi,BD)                                       
     
c      psi=10.
      par(1)=psi							      
      par(2)=bdc11							       
      par(3)=bt 							    
      par(4)=rd1								
      par(5)=rd2							       
      par(6)=R1 							       
      par(7)=R2
      par(8)=AJ0 							       
      do 2 i=1,3								
2     par(i+8)=bimf(i)  							
      tpsi=psi/rad                                                             
      call smtogsm(tpsi,sm2gsm)                                                

*---set 2
      do ii=1,9
      amlt=0.01
      alat=72.+2.*(ii)

* x0sm - solar-magnetic coordinates                                             
      x0_sm(1)=-cos(aMLT)*cos(aLAT/rad)                                   
      x0_sm(2)=-sin(aMLT)*cos(aLAT/rad)                                   
      x0_sm(3)=sqrt(1-x0_sm(1)*x0_sm(1)-x0_sm(2)*x0_sm(2))                           
      call pere2(x0_sm,x0,sm2gsm,1)   
c      CALL s_FIELD (UT,ID,im,iyear,X0,par,bm,bcm,kint,1)
      call s_line(UT,ID,im,iyear,x0,par,sl,h,1,kint,1)
      end do

       h=-0.1
*---set 3
      do ii=1,10
      amlt=0.01
      alat=-70.-2.*(ii)

* x0sm - solar-magnetic coordinates                                             
      x0_sm(1)=-cos(aMLT)*cos(aLAT/rad)                                   
      x0_sm(2)=-sin(aMLT)*cos(aLAT/rad)                                   
      x0_sm(3)=-sqrt(1-x0_sm(1)*x0_sm(1)-x0_sm(2)*x0_sm(2))                           
      call pere2(x0_sm,x0,sm2gsm,1)   
c      CALL s_FIELD (UT,ID,im,iyear,X0,par,bm,bcm,kint,1)
      call s_line(UT,ID,im,iyear,x0,par,sl,h,1,kint,1)
      end do

       h=0.1

*---set 4
      do ii=1,5
      amlt=3.1
      alat=90.-2.*(ii)

* x0sm - solar-magnetic coordinates                                             
      x0_sm(1)=-cos(aMLT)*cos(aLAT/rad)                                   
      x0_sm(2)=-sin(aMLT)*cos(aLAT/rad)                                   
      x0_sm(3)=sqrt(1-x0_sm(1)*x0_sm(1)-x0_sm(2)*x0_sm(2))                           
      call pere2(x0_sm,x0,sm2gsm,1)   
c      CALL s_FIELD (UT,ID,im,iyear,X0,par,bm,bcm,kint,1)
      call s_line(UT,ID,im,iyear,x0,par,sl,h,1,kint,1)
      end do

       h=-0.1
*---set 5
      do ii=1,3
      amlt=3.1
      alat=-90.+2.*(ii)

* x0sm - solar-magnetic coordinates                                             
      x0_sm(1)=-cos(aMLT)*cos(aLAT/rad)                                   
      x0_sm(2)=-sin(aMLT)*cos(aLAT/rad)                                   
      x0_sm(3)=-sqrt(1-x0_sm(1)*x0_sm(1)-x0_sm(2)*x0_sm(2))                           
      call pere2(x0_sm,x0,sm2gsm,1)   
c      CALL s_FIELD (UT,ID,im,iyear,X0,par,bm,bcm,kint,1)
      call s_line(UT,ID,im,iyear,x0,par,sl,h,1,kint,1)
      end do

       h=-0.1
*---set 7
      do ii=1,1
      amlt=3.1
      alat=-84.+1.*(ii)

* x0sm - solar-magnetic coordinates                                             
      x0_sm(1)=-cos(aMLT)*cos(aLAT/rad)                                   
      x0_sm(2)=-sin(aMLT)*cos(aLAT/rad)                                   
      x0_sm(3)=-sqrt(1-x0_sm(1)*x0_sm(1)-x0_sm(2)*x0_sm(2))                           
      call pere2(x0_sm,x0,sm2gsm,1)   
c      CALL s_FIELD (UT,ID,im,iyear,X0,par,bm,bcm,kint,1)
      call s_line(UT,ID,im,iyear,x0,par,sl,h,1,kint,1)
      end do

      X0(2)=0.1                                                                 
      X0(3)=0.                                                                  
      do ii=1,14
      X0(1)=29.-3.*ii
c      CALL s_FIELD (UT,ID,im,iyear,X0,par,bm,bcm,kint,1)
      call s_line(UT,ID,im,iyear,x0,par,sl,h,1,kint,1)
      end do
      h=0.1
      do ii=1,14
      X0(1)=29.-3.*ii
c      CALL s_FIELD (UT,ID,im,iyear,X0,par,bm,bcm,kint,1)
      call s_line(UT,ID,im,iyear,x0,par,sl,h,1,kint,1)
      end do




c      write (1,*) sigma,  ns                                                           
c      write (1,*)'INBOUND PARAMETERS:'                                                  
                                                                                
c      write (1,*)'   psi,           bdc11,         bt,           aj0'           
c      write (1,*)(par(jk), jk=1,3), par(8)                                      
                                                                                
c      write (1,*)'   rd1,           rd2,           r1,           r2'            
c      write (1,*)(par(jk), jk=4,7)                                              
                                                                                
c      write (1,*)'   B_IMF'                                                     
c      write (1,*)(par(jk), jk=9,11)                                             
                                                                                
c      write (1,*)                                                               

*magnetodisk coordinates

      X1(1)=24.5                                                                 
      X1(2)=0.                                                                 
      X1(3)=0.01                                                                  
        call pere2(x1,x2,sm2gsm,1)   
c      print *, x2   
      X1(1)=6.5                                                                 
      X1(2)=0.                                                                 
      X1(3)=0.01                                                                  
        call pere2(x1,x2,sm2gsm,1)   
c      print *, x2   
      X1(1)=-24.5                                                                 
      X1(2)=0.                                                                 
      X1(3)=0.01                                                                  
        call pere2(x1,x2,sm2gsm,1)   
c      print *, x2   
      X1(1)=-6.5                                                                 
      X1(2)=0.                                                                 
      X1(3)=0.01                                                                  
        call pere2(x1,x2,sm2gsm,1)   
c      print *, x2   
      

100   format (I4.2,I4.3,4I3.2,I4.3, 7E13.5)
201   format (14f16.8)
202   format (20f9.3)
301   format (5f16.8,' , , , ',41x, 6f16.8)
302   format (2f9.3,' , , , , , , , , , ',62x,9f9.3)
304   format (2f9.3,' , , , ',20x,3f9.3)
306   format (3f15.3)
307   format (1E4.1,1x,I4,1x,I2,1x,I2)
      stop
      end

      FUNCTION IDD(iy,mo,id)                                                    
********************************************************************            
*  Calculation of the day number in a year                         *            
*  INPUT PARAMETERS: year (IY), month (MO), day in the month (ID)  *            
*  Written by V. Kalegaev                                          *            
********************************************************************            
      II=0                                                                      
      IF (MO.EQ.1) GOTO 4                                                       
 5    DO 10 M=1,MO-1                                                            
      GOTO (1,2,1,3,1,3,1,1,3,1,3) M                                            
 1    II=II+31                                                                  
      GOTO 10                                                                   
 2    II=II+28                                                                  
      GOTO 10                                                                   
 3    II=II+30                                                                  
 10   CONTINUE                                                                  
 4    II=II+ID                                                                  
      if (mod(iy,100).eq.0.and.mod(iy,400).ne.0) goto 6                         
      IF (MOD(IY,4).EQ.0.AND.II.GT.59.AND.MO.GT.2) II=II+1                      
 6    IDD=II                                                                    
      RETURN                                                                    
      END                                                                       

