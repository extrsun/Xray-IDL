pro wt_spec,spec,spece,phafil=phafil,bck=bck,rsp=rsp,arffil=arffil
fildir='/data5/ASCA/A0401/evt/'
evtfile=fildir+'ad82010000g200170m.evt'
get_actime,a,b,fname=evtfile,nr=nr
gti=dblarr(nr,2) ; format is the same as used in wrtfitspec.pro
gti(*,0)=a
gti(*,1)=b

if n_elements(phafil) eq 0 then phafil='source.pha'
if n_elements(rsp) eq 0 then $
	rsp='/caldb/caldbtop/data/asca/gis/cpf/95mar06/gis2v4_0.rmf'
if n_elements(arffil) eq 0 then arffil='source.arf'
mk_fitspec,spec,spece,gti,evtfile,sname=phafil,bck=bck,rsp=rsp,arf=arffil,qual=qual
stop
return
end
