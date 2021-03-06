;+
; VAR_CHI
;
; Main program for Chi^2 variability tests of a Chandra ACIS source
;
; Requirements:
; 	ACIS_VAR_MAIN_PRE and ACIS_VAR_MAIN_LOOP have been carried out.
;	sou_no - source number is given
;
; Outputs:
;	on-screan output 
;
; written by wqd, Sept 14, 2002.
;-
cbm=total(cbma(*,*,blow(nb):bhi(nb)),3)
; choose the events in the energy band and within the source radius:
sb=where(list.energy ge bmin(blow(nb)) and list.energy le bmax(bhi(nb)) $
          and bcid eq 1,tnb)
ss=where(list.energy ge bmin(blow(nb)) and list.energy le bmax(bhi(nb)) $
   and ((list.x-nxp(sou_no))^2+(list.y-nyp(sou_no))^2) le srr(sou_no),nss)

if n_elements(auto) eq 0 then auto=0
if auto eq 1 or n_elements(nstep) eq 0 then $
	 nstep=float(nss)/20. ;nstep=20
step=long(total(thi-tlow)/float(nstep)) 
print,'nstep, time step, avg(counts)/step = ',nstep,step,float(nss)/nstep

;source count histogram
source_thist,list(ss).time,tlow,thi,cr,cre,t1,t2,step=step,/relax

;background count histogram
source_thist,list(sb).time,tlow,thi,crb,crbe,t1,t2,step=step,/relax

;calculate the chi^2:
crb=crb*sbarea(sou_no)*cbm(loc(sou_no))/tnb
crn=(cr-crb)
crne=sqrt((crb+avg(crn))/(t2-t1))*1.e3
crn=crn*1.e3
avg_least,crn,crne,mcrn,crnme,/pri,/logprob

;plot
t=(t1+t2)*0.5
tmin=min(tlow)
sh_plot,(t-tmin)/(3600.),crn,crne,t*0+mcrn,xtit='Time (Hour)',ytit='!6 X-ray Count Rate (10!U-3!N ct s!U-1!N!N)!6',xrange=([t1(0),t2(n_elements(t2)-1)]-tmin)/(3600.),errl=(t1-tmin)/(3600.),erru=(t2-tmin)/(3600.)   
end
