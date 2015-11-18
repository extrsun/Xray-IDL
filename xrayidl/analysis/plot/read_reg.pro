pro read_reg,un,crval,nshape,sra,sdec,spara,nsign,factor=factor,sname=sname,texto=texto,ctype=ctype,send=send,pixsize=pixsize,imgcen=imgcen
;+
; read and interpret an entry in a region file. Read one line except for
; lines started with # or other lines without "(" 
;
;*Inputs:
; un - index of an opened region file
; crval - the coordinates of the image reference point
;
;Outputs:
; nshape - the shape number to be used by plot_shape (for example)
; sra, sdec - the coordinate center of the region in degree
; spara - the parameter vector that defines the region, depending on
;         the shape; in arcmin
; nsign - the sign of the region
;
;*Optiional Inputs:
; factor - the magnification factor to be applied to the size of the
;          regions (def =1)
; sname - source name defined between # and !! in the region file
; ctype - coordinate type (def='physical')
;           currently, support 'phy' or 'fk5' coordinates.
; pixsize - pixel size in arcsec
; imgcen - image reference point. Its coordinate is pre-defined by keyword 
;          'crval'. For Chandra obs, it should be [4096.5,4096.5]; and 
;          for other obs, it should be crpix*
; send - 1 for indicating the end of a source entry. Otherwsie =0
; 
;Note:
; entries for multiple sources should be separated by an empty line in
; the region file so that send=1 in the output.
;
; written by wqd, July 6, 2007, extracted from plot_reg.pro
; modified by ljt, July 2, 2008, former version don't include pixarcmin for polygon, now add
; modified by sw, Dec. 19, 2012, add calculation of dimension parameters 
;   in arc units to let the 'ctype' work for the region files stored in 
;   'fk5' coordinate; add keyword 'pixsize' and 'imgcen'
;-
if n_params() eq 0 then begin
print,'CALLING SEQUENCE -  read_reg,un,crval,nshape,sra,sdec,spara,nsign,factor=factor,ctype=ctype,send=send,pixsize=pixsize,imgcen=imgcen'
return
endif
if n_elements(factor) eq 0 then factor=1.
;if n_elements(ctype) eq 0 then ctype='fk5'
if n_elements(ctype) eq 0 then ctype='physical'
if keyword_set(pixsize) eq 0 then pixsize=!size_pixel
if keyword_set(imgcen) eq 0 then imgcen=[!pref,!pref]
text=''
    readagain: readf,un,text
    texto=text
;    print,text
    if strlen(text) eq 0 then begin
        send=1 
        return
        endif else send=0
    if strmid(text,0,1) eq '#' then begin
        sname=strtrim(gettok(strmid(text,1),'!!'),2) 
         ;only for the source region file from sou_fits_reg.pro
        goto,readagain
    endif 
    text=strtrim(gettok(texto,'#'),2) & texto=text ;add Dec.19 to avoid end note
    str=strmid(text,0,3)
    ; Correct the ctype based on the header of the region file
    if str eq 'phy' or str eq 'fk5' then ctype=gettok(text,';')
    shape=gettok(text,'(')
    if strlen(text) eq 0 then goto,readagain
    ; Control the positive (normal) or the negative (region with 
    ; oblique) of the region
    if strmid(shape,0,1) eq '-' then begin
        shape=strmid(shape,1,strlen(shape)-1)
        nsign=1
    endif else nsign=0
    if shape eq 'polygon' then begin
        sra=crval(0) & sdec=crval(1)
        if ctype eq 'physical' then begin
            pixarcmin=pixsize/60.*factor 
        endif ;else begin
            ; pixarcmin=1./60.*factor
        ; endelse
    endif else begin
        xp=gettok(text,',')
        yp=gettok(text,',')
        if ctype eq 'physical' then begin
            pixarcmin=pixsize/60.*factor 
               ;to convert to the size in units of arcmin
            trans_loct,xp-imgcen[0],yp-imgcen[1],crval(0),crval(1),sra,sdec $
          ,/deg,pixsize=pixsize*factor 
        endif else begin
           ; pixarcmin=1./60.*factor 
            stringad,xp+' '+yp,sra,sdec
        endelse 
    endelse 
    case  shape of
        'circle': begin
            nshape=0
            spara=(ctype eq 'physical')?gettok(text,')')*pixarcmin:gettok(gettok(text,')'),"'")
        end
        'box': begin
            nshape=1
            dimx=gettok(text,',')
            dimy=gettok(text,',')
            if strlen(text) ne 0 then rangle=gettok(text,')') $
            else begin
                dimy=gettok(dimy,')')
                rangle=0.
            endelse 
            if ctype eq 'phsyical' then $
              spara=[[dimx,dimy]*pixarcmin,rangle] $
            else begin
              dimx=gettok(dimx,"'") & dimy=gettok(dimy,"'")
              spara=[[dimx,dimy],rangle]
            endelse
            end
        'rotbox': begin
            nshape=1
            if ctype eq 'physical' then $
              spara=[gettok(text,','),gettok(text,',')]*pixarcmin $
            else $
              spara=[gettok(gettok(text,','),"'"),gettok(gettok(text,','),"'")]
            spara=[spara,gettok(text,')')]
        end 
        'ellipse': begin
            nshape=2
            scale1=(ctype eq 'physical')?gettok(text,',')*pixarcmin:gettok(gettok(text,','),"'")
            scale2=(ctype eq 'physical')?gettok(text,',')*pixarcmin:gettok(gettok(text,','),"'")
            angle=gettok(text,',')
            if strlen(text) eq 0 then $
              spara=[scale1,1-scale2/float(scale1),angle-90.] else begin
              nshape=5 ;actually this is an ellipse annulus
              spara=[scale1,1-scale2/float(scale1)]
              scale1=(ctype eq 'physical')?angle*pixarcmin:gettok(angle,"'") ;outer major
              if ctype eq 'physical' then $
                scale2=gettok(text,',')*pixarcmin $
              else scale2=gettok(gettok(text,','),"'") ;outer minor
              spara=[spara,scale1,1-scale2/float(scale1),gettok(text,')')-90.]
             endelse 
        end 
        'polygon': begin
            nshape=3
            spara=[-999.D]
            text=gettok(text,')')
            while strlen(text) ne 0 do begin
                spara=[spara,double(gettok(text,','))]
            endwhile 
            spara=[spara(1:*),spara(1:2)]
            nspara=n_elements(spara)/2
            ; print,spara[0:1],imgcen
           ;  help,spara
           ;  help,imgcen
           ;  print,(spara[0:1]-imgcen)*60
            if ctype eq 'physical' then $
              spara=(reform(spara,2,nspara)-rebin(imgcen,2,nspara))*pixarcmin $
            else spara=(reform(spara,2,nspara)-rebin(imgcen,2,nspara))*60.
            ; print,spara[0:1],imgcen,ctype
        end
        'annulus': begin
            nshape=4
            scale1=gettok(text,',')
            if ctype eq 'physical' then $
              spara=[scale1,gettok(text,')')]*pixarcmin $
            else spara=[gettok(scale1,"'"),gettok(gettok(text,')'),"'")]
        end
        else: print,'The region shape ',shape,' needs to be implemented!'
    endcase
;if !debug eq 3 then stop
end
