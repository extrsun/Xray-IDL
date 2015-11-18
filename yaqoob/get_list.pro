function get_list,seq_no
;+
;
;NAME:
;get_list
;PURPOSE:
;gets photon list from ROSAT SDAS-formatted file
;CATEGORY
;ROSAT IDL tool
;
;CALLING SEQUENCE:
;list = get_list(seq_no)
;INPUTS:
;seq_no = integer number equal to sequence number of the desired observation
;OPTIONAL INPUT PARAMETERS:
;none
;
;OUTPUTS:
;list=structure of type xevents containing photon event data:
;     x,y,pha,pi,time,xd,yd for each detected photon
;
;OPTIONAL OUTPUT PARAMETERS:
;none
;
;
;COMMON BLOCKS:
;none
;SIDE EFFECTS:
;none
;RESTRICTIONS:
;none
;
;PROCEDURE:
;reads the photon list info from the SDAS formatted file and creates structure
;SDAS formatted file should be created from rpxxxxxp.fits file using
;DFITSRD in the following way
;IDL> DFITSRD,'rpxxxxxp.fits','rp123456p_fits' ; creates 3 SDAS files
;IDL> list = get_list(123456) ; access photon list 
;
;MODIFICATION HISTORY:
;Dec 9, 1991 written by M. F. Corcoran
;Dec 16, 1991 changed input from filename to sequence number
;
;-
!seq_no=seq_no
fname=!data_dir+'rp'+strtrim(seq_no,2)+'_fits_3'
tbread,fname,h,tab
sz=size(tab) & num=sz(2)
row={xevent,x:0,y:0,pha:0,pi:0,time:0.0D0,dx:0,dy:0}
a=replicate(row,num)
x=tbget(h,tab,1) & y=tbget(h,tab,2) & pha=tbget(h,tab,3)
pi=tbget(h,tab,4) & time=tbget(h,tab,5) 
dx=tbget(h,tab,6) & dy=tbget(h,tab,7)
a.x=x & a.y=y & a.pha=pha & a.pi=pi & a.time=time
a.dx=dx & a.dy=dy
return,a
end
