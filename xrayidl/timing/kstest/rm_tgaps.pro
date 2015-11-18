pro rm_tgaps,fname,tsin,tsout,sel,tmin=tmin
;-
; remove time gaps between individual observing segments
; used before the KS test of x-ray sources in the M101 field.
; fname - name of the file containing the intervals.
; tsin - input presorted time series of photons
; tsout - output time series with gaps removed
; sel - output selected photon location in the input time series
; tmin - minimum time interval to be kept in the output series
;
; written by wqd, Dec. 23, 1998
;+
if ( N_params() LT 4 ) then begin
    print,'Syntax - rm_tgaps,fname,tsin,tsout,sel,tmin=tmin'
    return
 endif

openr,un,fname,/get
ndata=0L
readf,un,ndata
ti=dblarr(ndata)
readf,un,ti
free_lun,un
ind=lindgen(ndata/2)*2
tlo=ti(ind)
thi=ti(ind+1)

nt=n_elements(tsin)
tabinv,tsin,tlo(1:*),tloloc
tloloc=[0L,long(tloloc)+1]
tabinv,tsin,thi(0:ndata/2-2),thiloc
thiloc=[long(thiloc),nt-1]
;there is slight roundoff error here; as a result about < 1/1000 counts get lost
;remove intervals that contain no count
sel=where(thiloc lt tloloc,nsel)
if nsel ne 0 then begin
	thiloc(sel)=tloloc(sel)
	thi(sel)=tlo(sel)
endif

if n_elements(tmin) eq 0 then tmin=0.
sel=where((thi-tlo) gt tmin,nsel)
if nsel ne 0 then begin
	tloloc=tloloc(sel)
	thiloc=thiloc(sel)
	tlo=tlo(sel)
	thi=thi(sel)
endif else stop,'no time intervals with duration greater than tmin'

totgap=tlo(0)
ntsout=total(thiloc-tloloc+1)
sel=lonarr(ntsout)
tsout=dblarr(ntsout)
tsout(0:thiloc(0)-tloloc(0))=tsin(tloloc(0):thiloc(0))-totgap
sel(0:thiloc(0)-tloloc(0))=tloloc(0)+lindgen(thiloc(0)-tloloc(0)+1)
ind=thiloc(0)-tloloc(0)+1
for k=1L,nsel-1 do begin
	totgap=tlo(k)-thi(k-1)+totgap
	indhi=ind+thiloc(k)-tloloc(k)
	tsout(ind:indhi)=tsin(tloloc(k):thiloc(k))-totgap
	sel(ind:indhi)=tloloc(k)+lindgen(thiloc(k)-tloloc(k)+1)
	ind=indhi+1
endfor
print,'total exp and counts = ',total(thi-tlo),ntsout
return
end
