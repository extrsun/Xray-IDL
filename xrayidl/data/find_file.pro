pro find_file,text,ih,im,is,jd,jm,js,gl=gl,gb=gb,equi=equi $
,dir=dir,fname=fname,delbrigh=delbright

if n_elements(explimit) eq 0 then explimit=0.
read_list,ra,dec,text,dir=dir,fname=fname,delbrigh=delbright,/screen,
explimit=explimit,gl=gl,gb=gb,equi=equi
