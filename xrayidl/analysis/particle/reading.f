This algorithm depends on several assumptions.  One, the VG-1 Einstein IPC
data, after removing point sources, has only two components -- the diffuse
X-ray background and particle induced counts.  Two, the particle counts
has a uniform distribution across the detector face, but could be time
dependent, i.e., different images may have different particle level.  Three,
the real X-ray counts passed through the telescope follows a steep vignetting
function, i.e., efficiency dropping fast toward the edge of the detector.  It
is this difference in behavior between particle counts and X-ray count that
allows this algorithm to be able to separate this two components.

If, for example, the ROSAT PSPC, being perfectly designed and engineered,
has no vignetting function, or has a very flat one, then the fitting will
fail as the particle count and X-ray count becomes degenerate, i.e., if you
take a fraction of particle counts as X-ray counts, where will be no or very
little difference in the fitting.

Anyway, as the project for our case, we took about 115 IPC VG-1 images that
cover LMC region and have lots of overlaps.  We assigned each image k with
a particle level Pk.  We then divided the LMC region into small square boxes.
and assigned each box ij with a diffuse X-ray flux Fij.  If image k covered
box ij, then we would have the following theoretical equation:
                Cijk = (Fij * Vijk * Tk + Pk * Tk) Snm
where Cijk is the counts in box ij in image k, Vijk is the value of
vignetting function (or effiency) in image k at box ij (should be close
to 1 if box ij is around the center of image k), Tk is the exposure time
of image k, Snm is the small scale structure (or flat field of the detector
in detector's coordinate nm.  We have IxJ fluxes and K particle levels of
unknown parameters, and a lot more than IxJ+k such equations, thus allow us
to fit simultaneously the Fij's and Pk's.

Note, for a perfect telescope and detector, the vignetting function is flat
and Vijk is a constant C, so the equation becomes:
                Cijk = (Fij * C + Pk) * Tk * Snm
so if you take x out of every Pk's and add x/C to every Fij's, the relation
will never change.

The following is the program READING that has been commented.  The comment
FOLLOWS the code.  Hope it is useful to you to understand the algorithm.
I hope the comment for program THINKING will be ready soon.

Happy Holidays!

-- Xiaoyi

--------------------- beginning of program reading ------------------------
c     PROGRAM READING

      parameter (nx=100,ny=120,nb=30)

c     we divide the LMC region into nx by ny square boxes, and each
      box is nb IPC pixels in size.  here 30*8"=4'

      character*40 fn

c     used for file name of images

      integer*2 na(640,640),nc(nx,ny)

c     na is used to store count array for each image, the element is
c     a IPC pixel.  nc is the count array of the entire LMC region,
c     the element is a box.  to match with the equation, here nc
c     is Fijk in the equation, tc is Tk * Snm, and tv is
c     Vijk * Tk * Snm

      real*4 ss(60,60),ff(640,640),vf(640,640),tc(nx,ny),tv(nx,ny)

c     ss is the small scale structure (or flat field) of the detector.
c     the detector of 1 degree field of view is divided into 60 by 60
c     boxes, and each box should have a value varying around 1, that
c     value should be multiplied to the exposure time for each pixel.
c     note that the ss should be rotated to match with any image.
c     ff is such rotated ss.  vf is vignetting function, tc is the
c     exposure map of the entire LMC which is corrected for the flat
c     field, and the reduced area due to point source removing.
c     tv is the same as tc, but further corrected for the vignetting
c     function

      call radian (5,26,0.0,-68,40,0.0,xc,yc)

c     the center of the LMC region is at 5h 26m -68d 40m.  the subroutine
c     radian convert RA and Dec from hms-dms into radians

      open (11,file='sss.d',status='old',access='direct',recl=14400)
      read (11,rec=1) ss
      close (11)

c     read in the flat field image

      open (11,file='name',status='old')
      open (12,file='total',status='old')
      open (13,file='lattice',status='new',access='direct',recl=14)

c     name is a text file lists the name of each image file.
c     total is a text file lists the point sources in the LMC region
c     the format of each line should be hr min sec deg min sec sigma,
c     where the first six gives the source position, and the seventh
c     gives the signal to noise ratio which is used to decide the size
c     of the region around the source to be removed.
c     lattice is a binary file that store the data for each equation.
c     the first record stores the number such equations for image 1,
c     i.e., how many boxes image 1 covers, the next that many records
c     stores the actual data.  the next record stores the number for
c     image 2, etc

      call vignette (vf)

c     create the vignetting function

      ns=1

c     ns is the record number from which the data for a image starts,
c     now for the first image, it starts from first record

 23   read (11,21,end=22) fn
 21   format (a40)

c     read the name of the first image, or next image

      open (14,file=fn,status='old',access='direct',recl=512)
      call broad (14,xf,yf,ra,et,na)
      close (14)

c     open the image file and read in the broadband data na along
c     with image center xf yf, roll angle ra, exposure time et

      call field (ra,ss,ff)

c     create the flat field ff

      call punch (12,xf,yf,ff)

c     remove all point sources.  the way to remove point sources is
c     to set the ff value in regions around point sources to zero
c     then any later calculations will not include such regions

      call binning (xc,yc,nx,ny,nb,vf,xf,yf,et,na,ff,nc,tc,tv)

c     find the box that each pixel in this image belongs to and
c     bin those pixels together to create the final data

      call record (13,ns,nx,ny,nc,tc,tv)

c     store the data for this image

      goto 23

c     go back to read the next image and process it

 22   write (13,rec=ns) 0

c     mark the end of data

      close (13)
      close (12)
      close (11)
      end

c     SUBROUTINE VIGNETTE

      subroutine vignette (vf)
      real*4 vf(640,640)

c     the vignette function depends only on the pixel's distance to
c     image center.  it's a parabolic function within 12 arc minutes,
c     and a straight line outside

      do j=1,320
         ys=(320.5-float(j))*(320.5-float(j))
         do i=1,j

c     since the vignetting function is circular symmetric, only a pie
c     of one eighth of the circle is needed to calculate. and be
c     mirrored to the other seven pies

            dt=sqrt(ys+(320.5-float(i))*(320.5-float(i)))/7.5

c     one IPC pixel is 8 arc second, so one arc minute is 7.5 pixels.

            if (dt.lt.12.0) then
               vn=1.0-dt*(0.00825+dt/3200.0)
            else
               vn=1.11232-dt*0.02136
            end if
            vf(i,j)=vn
            vf(j,i)=vn
            vf(641-i,j)=vn
            vf(j,641-i)=vn
            vf(i,641-j)=vn
            vf(641-j,i)=vn
            vf(641-i,641-j)=vn
            vf(641-j,641-i)=vn
         end do
      end do
      return
      end

c     SUBROUTINE BROAD

      subroutine broad (kc,xf,yf,ra,et,na)
      integer*2 na(192:831,192:831),ld(0:255)
      integer*2 lm(0:15,0:15),lt(0:3,0:15)
      equivalence (x,ld(63)),(y,ld(65)),(r,ld(67)),(t,ld(151))

c     since you are not going to use IPC images, I will skip this
c     rather peculiar code to read images in IPC format.  For other
c     format of images, you need to get the count array na, exposure
c     time et, roll angle ra (used for flat field only), and RA and
c     Dec of the center of the image xf, yf

      do j=192,831
         do i=192,831
            na(i,j)=0
         end do
      end do
      read (kc,rec=2) lm
      do j=3,12
         do i=3,12
            if (lm(i,j).gt.0) then
               read (kc,rec=lm(i,j)+1) lt
               do k=2,10
                  nt=lt(0,k)*4
                  nl=lt(1,k)
                  nb=lt(2,k)
                  ns=lt(3,k)
 22               if (nt.eq.0.and.nl.eq.0) goto 21
                  read (kc,rec=nb+1) ld
                  if (nt.gt.0) then
                     ne=255
                     nt=nt-1
                  else
                     ne=min(nl+ns-1,255)
                     nl=nl+ns-1-ne
                  end if
                  im=i*64
                  jm=j*64
                  do nd=ns,ne
                     ic=iand(ld(nd),63)+im
                     jc=iand(ishft(ld(nd),-8),63)+jm
                     na(ic,jc)=na(ic,jc)+1
                  end do
                  nb=nb+1
                  ns=0
                  goto 22
 21            end do
            end if
         end do
      end do
      read (kc,rec=1) ld
      xf=x
      yf=y
      ra=r
      et=t
      return
      end

c     SUBROUTINE FIELD

      subroutine field (ra,ss,ff)
      real*4 ss(60,60),ff(640,640)
      cr=cos(ra)
      sr=sin(ra)
      do j=1,640
         yo=float(j)-320.5

c     xo and yo is the coordinates of the pixel in the image with
c     origin at the center

         do i=1,640
            ff(i,j)=0.0
            xo=float(i)-320.5
            xf=xo*cr+yo*sr

c     xf and yf is the coordinates of the same pixel in the original
c     detector's system

            if (abs(xf).gt.225.0) goto 21
            xb=xf+3.8
            if (abs(abs(xb)-148.6).lt.15.0) goto 21
            yf=yo*cr-xo*sr
            if (abs(yf).gt.225.0) goto 21
            yb=yf+12.2
            if (abs(abs(yb)-148.6).lt.15.0) goto 21

c     these lines rejects pixels outside of one degree square, and
c     remove the four 4' wide supporting ribs running across the
c     detector.  for other kind of images, there may not be any
c     such ribs and the field of view may also be different

            is=max(min(int(xf/7.5+31.0),60),1)
            js=max(min(int(yf/7.5+31.0),60),1)

c     is and js is the flat field box that the pixel i j belongs to

            ff(i,j)=ss(is,js)
 21      end do
      end do
      return
      end

c     SUBROUTINE PUNCH

      subroutine punch (kc,xf,yf,ff)
      real*4 ff(640,640)
      rewind (kc)
 22   read (kc,*,end=21) ih,im,xs,jd,jm,ys,sm

c     read the RA and Dec, and sigma of the point source

      call radian (ih,im,xs,jd,jm,ys,xa,ya)

c     convert RA and Dec to radians

      call dist (xf,yf,xa,ya,xd,yd)

c     calculate the distance between the point source and the image center

      rd=alog(3.0*sm)*7.5

c     the region to remove is a circle around the source with its radius
c     rd as such a function of the signal to noise ratio.  This is chosen
c     according to the IPC's point spread function.

      rs=rd*rd
      xb=xd+320.5
      yb=yd+320.5

c     xb and yb is the pixel of the source center

      il=max(int(xb-rd+1.0),1)
      iu=min(int(xb+rd),640)
      jl=max(int(yb-rd+1.0),1)
      ju=min(int(yb+rd),640)

c     set the lower and upper boundaries for the removing region

      if (il.gt.iu.or.jl.gt.ju) goto 22

c     this happens for sources outside of the image

      do j=jl,ju
         ys=(float(j)-yb)*(float(j)-yb)
         do i=il,iu
            ds=ys+(float(i)-xb)*(float(i)-xb)
            if (ds.lt.rs) ff(i,j)=0.0

c     set flat field to zero for pixels in the source region

         end do
      end do
      goto 22

c     go back to read and remove the next source

 21   return
      end

c     SUBROUTINE BINNING

      subroutine binning (xc,yc,nx,ny,nb,vf,xf,yf,et,na,ff,nc,tc,tv)
      integer*2 na(640,640),nc(nx,ny)
      real*4 ff(640,640),vf(640,640),tc(nx,ny),tv(nx,ny)
      do j=1,ny
         do i=1,nx
            nc(i,j)=0
            tc(i,j)=0.0
            tv(i,j)=0.0
         end do
      end do

c     reset the data

      do j=1,640
         ya=float(j)-320.5

c     xa and ya is the distance to the image center

         do i=1,640
            if (ff(i,j).gt.0.1) then

c     only when the flat field value is valid, thus will not include
c     point sources and region of supporting ribs

               xa=float(i)-320.5
               call loct (xa,ya,xf,yf,xb,yb)

c     xb and yb is the RA and Dec of that pixel

               call dist (xc,yc,xb,yb,xd,yd)

c     xd and yd is the distance of that pixel to the center of
c     the defined LMC region

               id=int(xd/float(nb)+float(nx)/2.0+1.0)
               jd=int(yd/float(nb)+float(ny)/2.0+1.0)

c     id and jd is the box number this pixel belongs to

               if (id.ge.1.and.id.le.nx.and.jd.ge.1.and.jd.le.ny) then
                  nc(id,jd)=nc(id,jd)+na(i,j)
                  tc(id,jd)=tc(id,jd)+et*ff(i,j)
                  tv(id,jd)=tv(id,jd)+et*vf(i,j)*ff(i,j)

c     if it's within the define region, update the data for that box.
c     remember, nc here is the Cijk, tc is Tk * Snm, tv is
c     Vijk * Tk * Snm

               end if
            end if
         end do
      end do
      return
      end

c     SUBROUTINE RECORD

      subroutine record (kc,ns,nx,ny,nc,tc,tv)
      integer*2 nc(nx,ny),i,j
      real*4 tc(nx,ny),tv(nx,ny)

c     ns is the starting record for current image

      nr=0
      do j=1,ny
         do i=1,nx
            if (tc(i,j).gt.0.1) then

c     for each valid data point

               nr=nr+1
               write (kc,rec=ns+nr) i,j,nc(i,j),tc(i,j),tv(i,j)

c     record them in file sequentially after the starting record

            end if
         end do
      end do
      write (kc,rec=ns) nr
      ns=ns+nr+1

c     record the number of valid data for this image in the starting
c     record, and advance starting record number for next image

      return
      end
-------------------------------------------------------------------------
The program READING reads the data from each image and sort them into
the small square boxes.  For each image k covering box ij, it stores
in file lattice the nc which is Cijk, tc which is Tk * Snm, and tv
which is Vijk * Tk * Snm, so the equation:
        Cijk = (Fij * Vijk + Pk) * Tk * Snm
becomes equation `nc = Fij * tv + Pk * tc' for each i j k.  Due to the
distribution of the images in the LMC region, not all boxes are covered
by any image; and those that covered by only one image will increase
the number Fij and Cijk each by 1, thus does not contribute to the fitting.
So we will only pick out those boxes that covered by at least two images.
The Chi-Square fitting used here minimized the summary:
        summary of ((Data - Model)/(error of Model))^2
In our case, Data is Cijk or nc, Model is Fij * tv + Pk * tc, and assuming
Gaussion random distribution, the error of Model is just sqrt(Model).
This means to minimize summary of:
        (nc - Fij * tv - Pk * tc)^2 / (Fij * tv + Pk * tc)
To minimize is to find a set of Fij's and Pk's so that the derivative of
that summary to any one of Fij or Pk is zero.  Instead of solving these
thousands of hopelessly complicated (due to the derivative) equations, we
simplify the process by adjusting each and every one of the Fij's and Pk's
one at a time from the current set of F's and P's.  And we repeat these
adjustments 400 times until they stablize, i.e. change any F's or P's will
increase the Chi-Square.  These are the functions of the following program
THINKING.  As with the program reading, comments FOLLOWS the code.

--------------------- beginning of program thinking ---------------------
c     PROGRAM THINKING

      parameter (nx=100,ny=120,nm=6000,nf=130)
      integer*2 me(nx,ny),li(nm),lj(nm),nc(nm,nf)
      real*4 tc(nm,nf),tv(nm,nf),f(nm),p(nf),fm(nx,ny),tm(nx,ny)

c     because there are only about half of the boxes are covered by
c     more then one image, and in order to save memory (may not be a
c     must these days), we map the two dimensional box number ij into
c     one dimensional, and pick only those valid ones.  the array me
c     and li,lj will convert the index between the two.  for example,
c     box ij in 2-D is the same as box me(i,j) in 1-D, and box l in
c     1-D is the same as box i=li(l),j=lj(l) in 2-D.  nc, tc, and tv
c     is the usual data, f and fm is the Fij in 1-D and 2-D, p is Pk

      open (11,file='lattice',status='old',access='direct',recl=14)

c     open the data file

      call overlap (11,nx,ny,nm,nt,mt,np,me,li,lj)

c     pick the valid boxes and make the 1-D and 2-D mapping

      if (nt.gt.nm) then
         write (6,*) 'Number of overlap boxes exceeded maximum.'
         stop
      end if
      if (np.gt.nf) then
         write (6,*) 'Number of image files exceeded maximum.'
         stop
      end if

c     if these happens, you need to increase the nm or nf

      write (6,*) '      Overlap data:',mt
      write (6,*) '       Overlap box:',nt
      write (6,*) '   Number of files:',np
      write (6,*) ' Degree of freedom:',mt-nt-np

c     information only

      call loading (11,nx,ny,nm,nf,me,nc,tc,tv)

c     read the data file again and store them in the proper nc tc and tv

      call fitting (nm,nf,nt,np,nc,tc,tv,f,p)

c     Chi-Square fit the data and find the best F's and P's

      do k=1,nt
         fm(li(k),lj(k))=f(k)
      end do

c     convert the 1-D F's to 2-D

      call growing (11,nx,ny,nf,me,p,fm,tm)

c     once P's are known, calculate the F's for those boxes that covered
c     by only one image

      close (11)
      open (11,file='xpart.d',status='new',form='unformatted')
      write (11) (p(k),k=1,np)
      write (11) fm
      close (11)

c     store the particle level P's, and diffuse X-ray flux F's

      end

c     SUBROUTINE OVERLAP

      subroutine overlap (kc,nx,ny,nm,nt,mt,np,me,li,lj)
      integer*2 me(nx,ny),li(nm),lj(nm),i,j

c     first use the array me to store the number of covered images

      np=0

c     np is the number of images

      ns=1

c     ns is the starting record number for the image

 21   read (kc,rec=ns) nr

c     get the number of data points for this image

      if (nr.gt.0) then

c     if it's not the end

         np=np+1
         do k=1,nr
            read (kc,rec=ns+k) i,j
            me(i,j)=me(i,j)+1

c     get the i and j for each data point, and that box is covered
c     by one more image

         end do
         ns=ns+nr+1

c     advance the record number for the next image

         goto 21
      end if
      nt=0
      mt=0

c     nt is the total number valid boxes
c     mt is the total number data points

      do j=1,ny
         do i=1,nx
            if (me(i,j).gt.1) then

c     covered by more than one image

               nt=nt+1
               mt=mt+me(i,j)

c     advance both total number

               if (nt.gt.nm) return
               me(i,j)=nt
               li(nt)=i
               lj(nt)=j

c     mapping the 1-D and 2-D relationship

            else
               me(i,j)=0

c     mark this box as invalid

            end if
         end do
      end do
      return
      end

c     SUBROUTINE LOADING

      subroutine loading (kc,nx,ny,nm,nf,me,nc,tc,tv)
      integer*2 me(nx,ny),nc(nm,nf),i,j,mc
      real*4 tc(nm,nf),tv(nm,nf)
      ns=1
      np=0

c     ns is the starting record number for this image
c     and np is the image number

 21   read (11,rec=ns) nr
      if (nr.gt.0) then

c     if it's not the end

         np=np+1
         do k=1,nr
            read (11,rec=ns+k) i,j,mc,sc,sv

c     read the 2-D box number i, j, and data

            if (me(i,j).gt.0) then

c     if it's a valid box

               nc(me(i,j),np)=mc
               tc(me(i,j),np)=sc
               tv(me(i,j),np)=sv

c     store them in the right elements (me maps 2-D ij to 1-D)

            end if
         end do
         ns=ns+nr+1
         goto 21
      end if
      return
      end

c     SUBROUTINE FITTING

      subroutine fitting (nm,nf,nt,np,nc,tc,tv,f,p)
      integer*2 nc(nm,nf)
      real*4 tc(nm,nf),tv(nm,nf),f(nm),p(nf)

      call raway (nm,nf,nt,np,nc,tc,tv,f,p)

c     give an initial set of F's and P's, in this case we chose all P's
c     to be zero, and the F's being the average flux over all images

      call chisq (nm,nf,nt,np,nc,tc,tv,f,p,cs)
      write (6,*) cs

c     calculate the Chi-Square of this set and print out the value.  this
c     shows the fitness if one neglect the particle counts all together

      do j=1,400

c     400 iteration of the fine tuning process

         do i=1,nt

c     for each F's

            su=0.0
            sd=0.0

c     reset the numerator and denominator

            do k=1,np
               if (tc(i,k).gt.0.1) then

c     just to make sure the exposure time is positive

                  w=max(f(i)*tv(i,k)+p(k)*tc(i,k),0.1)
                  su=su+(float(nc(i,k))-p(k)*tc(i,k))*tv(i,k)/w
                  sd=sd+tv(i,k)*tv(i,k)/w

c     to simplify the derivative of (nc-f*tv-p*tc)^2/(f*tv+p*tc)
c     we fix the value for the F's and P's in the denominator, so it
c     becomes (nc-f*tv-p*tc)^2/(fo*tv+po*tc) and call the denominator w,
c     and the derivative to any one of F will be tv*(f*tv+p*tc-nc)/w,
c     and the summary of it over all P's will be zero, so we have:
c     f*summary_of(tv*tv/w)=summary_of((nc-p*tc)*tv/w).  su and sd
c     are the right and left side summaries.

               end if
            end do
            f(i)=max(su/sd,0.0)

c     give that F the new (and better) value, but with physical
c     constrain, i.e. greater than 0

         end do
         do k=1,np

c     now it's P's turn

            su=0.0
            sd=0.0
            do i=1,nt
               if (tc(i,k).gt.0.1) then
                  w=max(f(i)*tv(i,k)+p(k)*tc(i,k),0.1)
                  su=su+(float(nc(i,k))-f(i)*tv(i,k))*tc(i,k)/w
                  sd=sd+tc(i,k)*tc(i,k)/w
               end if
            end do
            p(k)=max(su/sd,0.0)
         end do
         if (j.eq.100.or.j.eq.200.or.j.eq.300) then
            call chisq (nm,nf,nt,np,nc,tc,tv,f,p,cs)
            write (6,*) cs
         end if

c     after 100, 200, and 300 iterations, calculate the Chi-Square
c     value and print them out to make sure they don't go wild.

      end do
      call chisq (nm,nf,nt,np,nc,tc,tv,f,p,cs)
      write (6,*) cs

c     finally calculate and print out the final Chi-Square value
c     the success of this fitting process is ensured when the
c     Chi-Square value is decreasing to an acceptable value.

      return
      end

c     SUBROUTINE RAWAY

      subroutine raway (nm,nf,nt,np,nc,tc,tv,f,p)
      integer*2 nc(nm,nf)
      real*4 tc(nm,nf),tv(nm,nf),f(nm),p(nf)

c     actually, there is no need to pass P's here

      do i=1,nt
         su=0.0
         sd=0.0
         do k=1,np
            if (tc(i,k).gt.0.1) then
               su=su+float(nc(i,k))
               sd=sd+tv(i,k)

c     for initial set of F's and P's, set all P's to zero (automatically),
c     then the equation becomes nc=f*tv, simple enough to calculate the F's

            end if
         end do
         f(i)=su/sd
      end do
      return
      end

c     SUBROUTINE CHISQ

      subroutine chisq (nm,nf,nt,np,nc,tc,tv,f,p,cs)
      integer*2 nc(nm,nf)
      real*4 tc(nm,nf),tv(nm,nf),f(nm),p(nf)
      cs=0.0
      do i=1,nt
         do k=1,np
            if (tc(i,k).gt.0.1) then
               cm=f(i)*tv(i,k)+p(k)*tc(i,k)
               er=float(nc(i,k))-cm

c     cm is Model, nc is Data, er is the difference

               cs=cs+er*er/max(cm,0.1)
            end if
         end do
      end do
      return
      end

c     SUBROUTINE GROWING

      subroutine growing (kc,nx,ny,nf,me,p,fm,tm)
      integer*2 me(nx,ny),i,j,mc
      real*4 p(nf),fm(nx,ny),tm(nx,ny)

c     we have calculated the P's for every image, and F's for every
c     box that are covered by more than one image.  now we calculate
c     the F's for those remaining boxes that are covered by only one

      ns=1
      np=0
 21   read (kc,rec=ns) nr
      if (nr.gt.0) then
         np=np+1
         do k=1,nr
            read (kc,rec=ns+k) i,j,mc,sc,sv
            if (me(i,j).eq.0) then

c     me being zero, means it is a single coverage

               fm(i,j)=(float(mc)-p(np)*sc)/sv

c     for single coverage box, where is only one equation with F of
c     that box in it.  so it is a direct calculation

               tm(i,j)=sv
            else
               tm(i,j)=tm(i,j)+sv

c     collect the corrected exposure time in array tm

            end if
         end do
         ns=ns+nr+1
         goto 21
      end if
      return
      end
-----------------------------------------------------------------------------
