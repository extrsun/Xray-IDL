pro particle,sctime,mvr,dir=dir,obseq=obseq

if n_elements(dir) eq 0 then dir=!data_dir
if n_elements(obseq) eq 0 then obseq=!seq_no
;
rsgetevr,0,0,sctime,rates
mvr=rates.mvrate
;
fname=dir+obseq+'_gti.dat'
filter_time,fname,sctime,indsel
stop
sctime=sctime(indsel)
mvr=mvr(indsel)
end