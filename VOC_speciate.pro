pro VOC_speciate

;read lump table
speciate='CB05'
file0_in='/home/liufei/GFED/'+speciate+'.csv'
file0_in=strcompress(file0_in, /REMOVE_ALL)
matrix_speciate = read_csv(file0_in)
;print,matrix_speciate.field01[2]
;HELP, /STRUCT,matrix_speciate
tnames=TAG_NAMES(matrix_speciate)
;tindex=WHERE(STRCMP(tnames,'FIELD03') EQ 1)
;print,'tname',tnames
;print,'tindex',tindex
;data=matrix_speciate.(tindex)
;print,data
;multiple coefficient 
speciesarr=matrix_speciate.field01
tot=N_ELEMENTS(speciesarr)

;define speciate:CB05
Categoryarr=['CB05_PAR','CB05_OLE','CB05_TOL','CB05_XYL','CB05_FORM','CB05_ALD2','CB05_ETH','CB05_ISOP','CB05_MEOH','CB05_ETOH','CB05_CH4','CB05_ETHA','CB05_IOLE','CB05_ALDX','CB05_TERP','CB05_UNR','CB05_NVOL']
tot_Category=N_ELEMENTS(Categoryarr)
for Categoryid=0,tot_Category-1 do begin
for year=2010,2010 do begin
for month=1,12 do begin
for day=1,31 do begin
Yr4 = string(year, format='(i4.4)')
Mon2 = string(month,format='(i2.2)')
Day2=string(day,format='(i2.2)')
nymd=year*10000L+month*100L+day*1L
print,nymd
if (nymd eq 20030229) then continue
if (nymd eq 20030230) then continue
if (nymd eq 20030231) then continue
if (nymd eq 20030431) then continue
if (nymd eq 20030631) then continue
if (nymd eq 20030931) then continue
if (nymd eq 20031131) then continue
if (nymd eq 20040229) then continue
if (nymd eq 20040230) then continue
if (nymd eq 20040231) then continue
if (nymd eq 20040431) then continue
if (nymd eq 20040631) then continue
if (nymd eq 20040931) then continue
if (nymd eq 20041131) then continue
if (nymd eq 20050229) then continue
if (nymd eq 20050230) then continue
if (nymd eq 20050231) then continue
if (nymd eq 20050431) then continue
if (nymd eq 20050631) then continue
if (nymd eq 20050931) then continue
if (nymd eq 20051131) then continue
if (nymd eq 20060229) then continue
if (nymd eq 20060230) then continue
if (nymd eq 20060231) then continue
if (nymd eq 20060431) then continue
if (nymd eq 20060631) then continue
if (nymd eq 20060931) then continue
if (nymd eq 20061131) then continue
if (nymd eq 20070229) then continue
if (nymd eq 20070230) then continue
if (nymd eq 20070231) then continue
if (nymd eq 20070431) then continue
if (nymd eq 20070631) then continue
if (nymd eq 20070931) then continue
if (nymd eq 20071131) then continue
if (nymd eq 20080230) then continue
if (nymd eq 20080231) then continue
if (nymd eq 20080431) then continue
if (nymd eq 20080631) then continue
if (nymd eq 20080931) then continue
if (nymd eq 20081131) then continue
if (nymd eq 20090229) then continue
if (nymd eq 20090230) then continue
if (nymd eq 20090231) then continue
if (nymd eq 20090431) then continue
if (nymd eq 20090631) then continue
if (nymd eq 20090931) then continue
if (nymd eq 20091131) then continue
if (nymd eq 20100229) then continue
if (nymd eq 20100230) then continue
if (nymd eq 20100231) then continue
if (nymd eq 20100431) then continue
if (nymd eq 20100631) then continue
if (nymd eq 20100931) then continue
if (nymd eq 20101131) then continue
Res=fltarr(tot,845,565,24)
for speciesid=0,tot-1 do begin
species=speciesarr[speciesid]
file1_in='/z4/liufei/GFED/Result/hourly/'+Yr4+'/VOC/GFED_'+Yr4+Mon2+Day2+'_'+species+'_0.1x0.1.nc'
file1_in=strcompress(file1_in, /REMOVE_ALL)
fid=NCDF_OPEN(file1_in)
varid=NCDF_VARID(fid,'Fraction_of_Emissions')
NCDF_VARGET, fid, varid, DATA
varlatid=NCDF_VARID(fid,'lat')
NCDF_VARGET,fid,varlatid,latitude
varlonid=NCDF_VARID(fid,'lon')
NCDF_VARGET,fid,varlonid,longitude
NCDF_CLOSE, fid
;find matrix
index = string(Categoryid+2,format='(i2.2)')
tindex=WHERE(STRCMP(tnames,'FIELD'+index) EQ 1)
Res[speciesid,*,*,*]=DATA[*,*,*]*matrix_speciate.(tindex)[speciesid]
;print,species,Res[speciesid,496,230,1],DATA[496,230,1],matrix_speciate.field02[speciesid]
endfor
Result=TOTAL(Res,1)
;print,Result[1,1,1]

;write netCDF file
outfilename=string('/z4/liufei/GFED/Result/hourly/'+Yr4+'/VOC_Speciate/GFED_'+Yr4+Mon2+Day2+'_'+Categoryarr[Categoryid]+'_0.1x0.1.nc')
outfilename=strcompress(outfilename, /REMOVE_ALL)
outid=NCDF_CREATE(outfilename,/CLOBBER)
NCDF_ATTPUT,outid,/GLOBAL,'Conventions','Please contact Liu Fei (Email: liuf1010@gmail.com) for any question'
NCDF_ATTPUT,outid,/GLOBAL,'Title','Mole number per hour per day of biomass flux from GFED'
timeid=NCDF_DIMDEF(outid,'time',24)
latid=NCDF_DIMDEF(outid,'lat',565)
lonid=NCDF_DIMDEF(outid,'lon',845)

vtimeid=NCDF_VARDEF(outid,'time',[timeid],/SHORT)
vlatid=NCDF_VARDEF(outid,'lat',[latid],/FLOAT)
vlonid=NCDF_VARDEF(outid,'lon',[lonid],/FLOAT)
veid=NCDF_VARDEF(outid,'Fraction_of_Emissions',[lonid,latid,timeid],/FLOAT)
NCDF_ATTPUT,outid,vtimeid,'long_name','time'
NCDF_ATTPUT,outid,vtimeid,'units','The hour on the date'
NCDF_ATTPUT,outid,vlatid,'long_name','latitude'
NCDF_ATTPUT,outid,vlatid,'units','degrees_north'
NCDF_ATTPUT,outid,vlonid,'long_name','longitude'
NCDF_ATTPUT,outid,vlonid,'units','degrees_east'
NCDF_ATTPUT,outid,veid,'units','Mole number species/m2/hour'
NCDF_ATTPUT,outid,veid,'long_name','Fraction of Emissions'
NCDF_ATTPUT,outid,veid,'FillValue','-999.f'
NCDF_CONTROL,outid,/ENDEF
NCDF_VARPUT,outid,vtimeid,[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
NCDF_VARPUT,outid,vlatid,latitude
NCDF_VARPUT,outid,vlonid,longitude
NCDF_VARPUT,outid,veid,Result
NCDF_CLOSE,outid


endfor
endfor
endfor
endfor
end
