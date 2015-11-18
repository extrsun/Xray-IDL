pro spec_get_main,source_no
tran=(180*60./!pi)^2*1.e-6
rate_s=[475.,0.1,73.,119.,0.]*tran ;in units of cts/s deg^2
sigrate_s=[320.,100.,39.,3.,100.]*tran ;basically assume all counts in hard band are extragal
bgroup=intarr(5,2)
; the group cannot be interrupted.
bgroup(0,0)=[20,42,52,91,202]
bgroup(0,1)=[41,51,90,201,247]
mtime_s=1.
numpix_s=1.
; output the spectral files
inputs='FNAME='+strtrim(!seq_no,2)+'_'+strtrim(source_no,2)+'_s,'+ $
'BKFIL=none'
;
print,'output the spectra into the files ',inputs
;
if !debug eq 1 then stop
print,'group = ',bgroup
make_pha,inputs,rate_s,sigrate_s,mtime_s,numpix_s,group=bgroup 
;
end
