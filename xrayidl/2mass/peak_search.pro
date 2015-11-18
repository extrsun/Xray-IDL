;     SUBROUTINE PEAK_SEARCH
;+
;     This subroutine is to search for peaks from the input array SIGMA 
;     using a pixel-dependent searching area
;
; ssr - a scalar or a vector containing the source search radius 
;		in units of array bins. The size of the vector should
;		the same as that of bin_sel!
;
; writen by WQD, JUne, 5 2000
;-
pro peak_search,sigma_array,ssr,n_newsource $
	,x_core,y_core,sigma,threshold=threshold $
	,bin_sel=bin_sel
;
if n_params() eq 0 then begin
print,'CALLING SEQUENCE - peak_search,sigma_array,ssr'
print,',n_newsource,x_core,y_core,sigma'
print,',threshold=threshold,bin_sel=bin_sel'
return
endif

sz=size(sigma_array)
if n_elements(threshold) eq 0 then threshold=!threshold
if n_elements(bin_sel) eq 0 then bin_sel=lindgen(sz(1),sz(2))
s=where (sigma_array(bin_sel) ge threshold,nbin)
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
isr=fix(sr)
jmin=(j-isr) 
jmax=(j+isr) 
imin=(i-isr) 
imax=(i+isr) 

; obtain the distances of the bins from the center of the box
boxdim=2*isr+1              ;dimension of the box
;
sigmamax=bytarr(nbin) ; to store the locations of local sigma  maxima
;
for k=0L,(nbin-1L) do begin
      sigmabox=sigma_array(imin(k):imax(k),jmin(k):jmax(k))  
    	dist_circle,dis,boxdim(k),isr(k),isr(k)
      good=where(dis le sr(k) and $
		(sigmabox gt sigma_array(s_sel(k))),count)
      if count eq 0 then begin
	sigmamax(k)=1b
endif
endfor

c=where(sigmamax eq 1b,n_newsource)
s_sel=s_sel(c)
sigma=sigma_array(s_sel)
y_core=s_sel/sz(1)
x_core=s_sel MOD sz(1) 
ssr=sr(c)
;
end
