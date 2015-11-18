;     SUBROUTINE STAR_SEARCH_MAP
;-
;     This subroutine is to search for sources from the input array SIGMA 
;     using a pixel-dependent searching area and is called by scan_map.pro
;
; ssr - a scalar or a vector containing the source search radius 
;		in units of array bins. The size of the vector should
;		the same as that of bin_sel!
; prob - only used to select the search bins. The source centroids are
; 	searched, using sigma_array
; s_sel - centroid index of bin_sel and prob.
;
; writen by WQD, April, 16 1996.
; various modifications made to be called by anal_v.pro. WQD Sept 13, 1993.
; prob and s_sel keywords are included.
;
;+
pro star_search_map,sigma_array,cntr_array,ssr,n_newsource $
	,x_core,y_core,sigma,cntr,array_size=array_size,threshold=threshold $
	,bin_sel=bin_sel,prob=prob,s_sel=s_sel
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - star_search,sigma_array,cntr_array,ssr'
print,',n_newsource,x_core,y_core,sigma,cntr,array_size=array_size'
print,',threshold=threshold,bin_sel=bin_sel,prob=prob,s_sel=s_sel'
return
endif

sz=size(sigma_array)
if n_elements(threshold) eq 0 then threshold=!threshold
if n_elements(bin_sel) eq 0 then bin_sel=lindgen(sz(1),sz(2))

if n_elements(prob) eq 0 then $
	s=where (sigma_array(bin_sel) gt threshold,nbin) else $
	s=where (prob lt threshold,nbin)
if nbin eq 0 then begin
   n_newsource=0
   print,'No useful bin having sigma larger than the threshold in the image'
   return
endif 

s_sel=bin_sel(s)
if n_elements(ssr) eq 1 then sr=s_sel*0.+ssr else $
	sr=ssr(s) 
;the number of elements in ssr should be the same as that in sel
;
j=s_sel/sz(1) & i=s_sel MOD sz(1)
;
;calculate the min and max of the i,j for the box
;
isr=nint(sr)
imin=(i-isr) > 0 
imax=(i+isr) < (sz(1)-1) 
jmin=(j-isr) > 0
jmax=(j+isr) < (sz(2)-1)  
ic=float(i-imin)
jc=float(j-jmin)
boxdimx=imax-imin+1
boxdimy=jmax-jmin+1

; obtain the distances of the bins from the center of the box
;boxdim=2*isr+1              ;dimension of the box
;
sigmamax=bytarr(nbin) ; to store the locations of local sigma  maxima
;
for k=0L,(nbin-1L) do begin
      sigmabox=sigma_array(imin(k):imax(k),jmin(k):jmax(k))  
;    	dist_circle,dis,boxdim(k),isr(k),isr(k)
    dist_circle,dis,[boxdimx(k),boxdimy(k)],ic(k),jc(k)
      good=where(dis le sr(k) and $
		(sigmabox gt sigma_array(s_sel(k))),count)
      if count eq 0 then begin
	sigmamax(k)=1b
endif
endfor

c=where(sigmamax eq 1b,n_newsource)
s_sel=s_sel(c)
sigma=sigma_array(s_sel)
cntr=cntr_array(s_sel)
y_core=s_sel/sz(1)
x_core=s_sel MOD sz(1) 
ssr=sr(c)
;
s_sel=s(c)
;stop
return
end
