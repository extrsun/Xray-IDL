pro get_detmap,bands,bandws,ib,nimap,bandadd,instmap,rmap,hdmap,detmap $
, bandcr=bandcr
;-
; get a broad band detector map
; bands - vector containing starting bands
; bandws - vector containing the number of bands for each starting band
; ib - the index of the above two vectors to be considered
; nimap - number of bands for each PSPC from defimap.pro
; bandadd - 0 or 1 depending on PSPC C or B
; instmap - string vector containing the file names of the detector maps
; bandcr - user supplied count spectrum for the bands
;*OUTPUTS:
; rmap - the array containing the broad band detector map
; hdmap - the header of the map
;
; extracted from make_emap to compile a broad band detector map
; WQD, Oct. 11, 1993
;+
; The broad band exposure map should be a function of the spectrum of the 
; incident X-rays and may be calculated by adding the spectral weight.
; The following is only good for the incident X-ray spectrum similar to that
; of the average sky
;
dir=!imapdir ;'~wqd/rosat/expmaps/'
if bands(ib) eq 0 then begin
 maskfile='mask.fits'
 rmap=readfits(dir+maskfile,hdmap) ;from get_mask.pro 
endif else begin
 tscale=0.
 bandcr=[0.927122,1.39611,0.119149,0.197304,0.240584,0.263655,0.170624]
; from rp900017
 detmap=''
 rmap=0.
 for k=0,bandws(ib)-1 do begin
  ibb = bands(ib)+k + bandadd*nimap     ;don't forget to use the right set
  detfile = instmap(ibb-1)
  detmap=detmap+','+detfile
;
; Read the instrument map
;
  print,'    Reading instrument map ',detfile
  rmap = rmap+readfits(detfile,hdmap)*bandcr(bands(ib)+k-1)  
  tscale=tscale+bandcr(bands(ib)+k-1)
 endfor
 rmap=rmap/tscale
endelse
if !debug eq 1 then stop
end