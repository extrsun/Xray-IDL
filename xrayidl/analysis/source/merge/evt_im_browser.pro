pro evt_im_browser,filelist,slist,sfname,rmsfname,fdir=fdir,ftail=ftail,instr=instr,psym=psym,symsize=symsize,s_c=s_c,sscale=sscale,greymin=greymin,greymax=greymax,imdim=imdim,nofits=nofits,block=block
;+
; browse a list of images with sources overlaid, with the choice to remove
; sources which can be put into a file
;
;*INPUTS:
; filelist - file list: each row contains a file name (or filehead so that
;		file name = filehead+ftail		
; slist - source structure
;
;*OUTPUTS:
;	TV images
;	rmsfname - name of the file that contains removed sources
;
;*OPTIONAL Inputs:
; fdir - file directory (def ='../xdata/')
; ftail - characters appended to the filenames in filelist (e.g., '.fit')
; instr - instrument type (def=!instr)
; greymin, greymax - lower and upper limits for the image display
; 	def=0.05, 1
; sscale - Gaussian smoothing scale of the TV image
; s_c - source color on TV
;
;*Example:
; evt_im_browser,flist,s,'test',ftail='.fits',fdir='../xdata/',sscale=g2,greymax=2,greymin=0.5
;
; written by wqd 2/29/2001
;-
if n_params() eq 0 then begin
print,'CALL SEQUENCE - evt_im_browser,filelist,slist,sfname,rmsfname,fdir=fdir,ftail=ftail,instr=instr,psym=psym,symsize=symsize,s_c=s_c,sscale=sscale,greymin=greymin,greymax=greymax'
return
endif
if n_elements(instr) eq 0 then instr=!instr
if n_elements(fdir) eq 0 then fdir='../xdata/' 
if n_elements(ftail) eq 0 then ftail=''
;-----
openr,un,filelist,/get
fname=''
k=0
fnamev=strarr(1000)
while not eof(un) do begin
	readf,un,fname
	fnamev(k)=fname
	k=k+1
endwhile
nf=k
fnamev=fdir+fnamev+ftail
if n_elements(greymin) eq 0 then greymin=0.05
if n_elements(greymax) eq 0 then greymax=1.
if n_elements(psym) eq 0 then psym=6
nrm=0
slist_new=slist
slistexist=0
for k=0,nf-1 do begin
	print,'k, fnamev(k) = ',k,' ', fnamev(k)
	evt_image,fnamev(k),instr,imarr,cra=cra,cdec=cdec,hdr=hdr $
          ,imdim=imdim,/nofits,imblock=block
	if n_elements(sscale) ne 0 then imarr=convolve(total(imarr,3),sscale)
	cor=[0,1,0,1]
	cont_grey,imarr,hdr,greymin=greymin,greymax=greymax $
		,/noc,/full,cor=cor,f_c=-1
	source_plot,'',cor,hdr,psym=psym $
		,s_c=s_c,sra=slist_new.ra,sdec=slist_new.dec,symsize=2
	print,'Click the mouse to make change:'
	print,' Left -> next; Middle -> to choose a file; Right -> to select sources'
	pause,/up
	case !err of 
		1: ;step to the next image
		4: begin ;select sources
		 again:
		 print,''
		 print,'Click the left key to select sources:'
		 print,'Middle -> exit; Right -> to the next image'
		 pause
		print,!err
		 case !err of 
			1: begin
			   cursor_posi,cor,hdr,sras,sdecs
			   nrm=nrm+n_elements(sras)
			   sou_match,sras,sdecs,slist_new,slistrm, $
			    selrm=selrm,/norem
			   source_plot,'',cor,hdr,psym=psym+1,s_c=s_c $
			   ,sra=slistrm.ra,sdec=slistrm.dec,symsize=2
			   if slistexist then $
			   	slistrmv=[slistrmv,slistrm] else begin
			   	slistrmv=slistrm
			   	slistexist=1
 			   endelse
			   remove,selrm,slist_new
			   end
			2: goto, out
			4: goto, nextimage
    		 endcase
		 goto, again
		 end
		2: begin
			print,'Give a number of the file to show'
			read,k
			k=k-1
			end
	endcase
nextimage:
if !debug eq 2 then stop
endfor
out:
print,'Number of sources removed = ',nrm

if n_elements(sfname) ne 0 and nrm ne 0 then $
	sou_struct_fits,slist_new,sfname,comm=' from evt_im_browser'
if n_elements(rmsfname) ne 0 and nrm ne 0 then $
	sou_struct_fits,slistrmv,rmsfname,comm=' from evt_im_browser'
return
end
