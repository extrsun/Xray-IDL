pro tmflt,qlist,nfiles
if n_params(0) eq 0 then begin
print,' TMFLT, qlist, nfiles '
print,' Filters a photon list using up to 10 pre-made ascii GTI files '
print,' The new qlist contains events only from time-sections which '
print,' are good and common to ALL files. EACH file must not contain '
print,' overlapping time intervals but the different files may do '
retall
end
if n_elements(nfiles) eq 0 then read,' How many input files ',nfiles
ntmx=1000
np=(size(qlist))(1)
print,' size of photon list on entry ',np
tmask=intarr(np)
ptime=qlist.time
fnames=strarr(nfiles) & fname=' '
i=0
for k=0,nfiles-1 do begin
  read,' Enter ascii time intervals file name ',fname & fnames(k)=fname
  openr,1,fnames(k)
while (not(eof(1))) do begin
  readf,1,tbeg,tend,d1,d2,ityp & i=i+1
  print,i,tbeg,tend
  tmask = tmask + ((ptime gt tbeg) and (ptime lt tend))
endwhile
close,1
endfor
ans=' ' & read,' Modify photon list ?(y/n) ',ans
if ans eq 'n' then begin
  sz=(size(qlist(where(tmask eq nfiles))))(1)
  print,' Number of selected photons would have been ',sz
  goto, fin
endif
qlist=qlist(where(tmask eq nfiles))
nnp=(size(qlist))(1) & print,' new photon list size ',nnp
fin: return
end
