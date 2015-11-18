	program OBfile
c
	character*40 filen*40
c
c transfer the coordinates of 1970 to those of 1950
c
C	filen='[-.lmc.data]lmc_ob_1970'
	filen='test.dat'
	open (10,file=filen,status='old',readonly,shared)
	open (20,file='lmc_ob_1950',status='new')
30	read(10,40,end=222) ln,ih,rmi,jd,jm,nob,dim,age
40	FORMAT(i3,2X,I2,f4.1,2x,i3,i2,2x,i3,1x,f4.1,f5.1)
30	read(10,40,end=222) ih,im,xs,jd,jm,ys
40	FORMAT(2(i3,1x,I2,f5.1))
c
	call radian(ih,im,xs,jd,jm,ys,xrad,yrad)
	TYPE*,ih,im,xs,jd,jm,ys
C	call tbas(20,xrad,yrad)
c	call tbas(25,xrad,yrad)
	call tbas(50,xrad,yrad)
	call degree(xrad,yrad,ih,im,xs,jd,jm,ys)
	write(20,50) ih,im,xs,jd,jm,ys,ln,nob,dim,age
	write(*,50) ih,im,xs,jd,jm,ys,ln,nob,dim,age
50	FORMAT(2(i4,i3,f6.2),2i6,2f5.1)
	goto 30
222	stop
	end
c--------------------------------------------------------------------
c---------------------------------------------------------------------
	subroutine tbas(n,xrad,yrad)
c the reference year is 1950. n is the differnce between the input year 
c and the reference year. The output is then the radian of the 1950.
	pi=3.141593
	rm=46.09905+2.79e-4*n
	rn=20.0426-0.85e-4*n
	oxrad=xrad
	oyrad=yrad
	xrad=oxrad-(rm+rn*sin(oxrad)*tan(oyrad))*n/3600.*pi/180.
	type*,(rm+rn*sin(oxrad)*tan(oyrad))*n/3600.*pi/180.,tan(oyrad)
	yrad=oyrad-(rn*cos(oxrad))*n/3600.*pi/180.
	return
	end
