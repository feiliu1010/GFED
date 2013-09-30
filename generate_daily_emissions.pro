pro generate_daily_emissions

nlon=720 ; number of grid points as a function of longitude
nlat=360 ; number of grid points as a function of latitude
gfedmly=fltarr(nlon,nlat) ; array containing monthly GFEDv3 fire emissions
gfeddly=fltarr(nlon,nlat) ; array containing daily GFEDv3 fire emissions
;You need to read monthly GFEDv3 fire emissions to generate the daily fluxes. For example, if
;you want to get daily emissions on January 21st, 2004, so you need read monthly data in January
;2004 first:
;speciesarr=['BC','C','C2H4','C2H4O','C2H5OH','C2H6','C2H6S','C3H6','C3H6O','C3H8','C4','C5H8','CH2O','CH3OH','CH4','CO','CO2','DM','H2','Higher_Alkanes','Higher_Alkenes','N2O','NH3','NMHC','NOx','OC','PM2p5','SO2','TC','TPM','Terpenes','Toluene_lum']
;VOC
speciesarr=['C','C2H4','C2H4O','C2H5OH','C2H6','C2H6S','C3H6','C3H6O','C3H8','C4','C5H8','CH2O','CH3OH','CH4','DM','Higher_Alkanes','Higher_Alkenes','NMHC','TC','Terpenes','Toluene_lump']

;BC,OC,PM
;speciesarr=['BC','OC','PM2p5','TPM']
;NON-VOC which needs to be converted to mole number
;speciesarr=['CO','CO2','NH3','NOx','SO2']
;molarr=[28,44,17,30,64]

tot=N_ELEMENTS(speciesarr)
for speciesid=0,tot-1 do begin
species=speciesarr[speciesid]
for year=2010,2010 do begin
for month=1,12 do begin
Yr4 = string(year, format='(i4.4)')
Mon2 = string(month,format='(i2.2)')

file0_in='/z4/liufei/GFED/Data/monthly/GFED3.1_'+species+'/GFED3.1_'+Yr4+Mon2+'_'+species+'.txt'
file0_in=strcompress(file0_in, /REMOVE_ALL)
gfedmly = read_ascii( file0_in )
gfedmly = gfedmly.field001
; reverse the direction of latitude with monthly emissions to combine with daily fire fractions.
for j=1, nlat/2 do begin
tmp = gfedmly[*,j-1]
gfedmly[*,j-1] = gfedmly[*,nlat-j]
gfedmly[*,nlat-j] = tmp
endfor
undefine, tmp
; Then, you can read daily fire fractions from the netcdf file.
for day=1,31 do begin
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


file1_in = string('/z4/liufei/GFED/Data/daily/'+Yr4+'/'+Yr4+'/fraction_emissions_'+Yr4+Mon2+Day2+'.nc')
file1_in=strcompress(file1_in, /REMOVE_ALL)
fid=NCDF_OPEN(file1_in)
varid=NCDF_VARID(fid,'Fraction_of_Emissions')
NCDF_VARGET, fid, varid, DATA
;NCDF_CLOSE, fid

;exclude all of grid cells with "DataSources" equal to "0"
zeroid=NCDF_VARID(fid,'DataSources')
NCDF_VARGET, fid,zeroid,ZERO
NCDF_CLOSE, fid
source=fltarr(nlon,nlat)
for j=0,nlon-1 do begin
for i=0,nlat-1 do begin
if (ZERO[j,i] eq 0) then begin
source[j,i]=0
endif else begin
source[j,i]=1
endelse
endfor
endfor 

gfeddly=gfedmly*DATA*source
;convert to molar mass
;mol=molarr[speciesid]
;gfeddly=gfeddly/mol*1000000000

;nlon=720 ; number of grid points as a function of longitude
;nlat=360 ; number of grid points as a function of latitude
nhor=8 ; numbers of 3-hourly intervals each day
;gfeddly=fltarr(nlon,nlat) ; array containing daily GFEDv3 fire emissions
gfed3hly=fltarr(nlon,nlat,nhor) ; array containing 3-hourly GFEDv3 fire emissions
nhour=24
gfedhly=fltarr(nlon,nlat,nhour)
file_in = string('/z4/liufei/GFED/Data/3-hourly/'+Yr4+'/'+Yr4+'/fraction_emissions_'+Yr4+Mon2+'.nc')
file_in=strcompress(file_in, /REMOVE_ALL)
fid=NCDF_OPEN(file_in)
varid=NCDF_VARID(fid,'Fraction_of_Emissions')
NCDF_VARGET, fid, varid, DATA
varlatid=NCDF_VARID(fid,'lat')
NCDF_VARGET,fid,varlatid,latitude
varlonid=NCDF_VARID(fid,'lon')
NCDF_VARGET,fid,varlonid,longitude
NCDF_CLOSE, fid

for nh=0,nhor-1 do begin
gfed3hly[*,*,nh]=gfeddly*DATA[*,*,nh]
nhr=(nh+1)*3-1
for i=nh*3,nhr do begin
gfedhly[*,*,i]=gfed3hly[*,*,nh]/3
endfor
endfor
;cut 36km region of China
;lon=[68,152.5],lat=[8,64.5]

longitude_part=fltarr(169)
latitude_part=fltarr(113)
gfedhly_part=fltarr(169,113,24)

longitude_part_regrid=fltarr(845)
latitude_part_regrid=fltarr(565)


nj1=WHERE(longitude EQ 68.25)
nj2=WHERE(longitude EQ 152.25)
;print,'nj1=',nj1,'nj2=',nj2
ni1=WHERE(latitude EQ 8.25)
ni2=WHERE(latitude EQ 64.25)
;print,'ni1=',ni1,'ni2=',ni2
longitude_part=longitude[nj1:nj2]
latitude_part=latitude[ni1:ni2]
gfedhly_part=gfedhly[nj1:nj2,ni1:ni2,*]
;interpolate 0.1X0.1
gfedhly_part_regrid=fltarr(845,565,24)
longitude_part_regrid=INTERPOL(longitude_part,845)
latitude_part_regrid=INTERPOL(latitude_part,565)
;create format of output
lon_grid =dblarr(845)
lat_grid = dblarr(565)
for i=0,843 do begin
lon_grid[i+1]=lon_grid[i]+0.2
;print,i,lon_grid[i]
endfor
for j=0,563 do begin
lat_grid[j+1]=lat_grid[j]+0.2
;print,lat_grid[j]
endfor

for i=0,23 do begin
gfedhly_part_regrid[*,*,i]=INTERPOLATE(reform(gfedhly_part[*,*,i]),lon_grid,lat_grid,/GRID)
endfor
;print,gfedhly_part_regrid[0,0,1]
;print,gfedhly_part[0,0,1]

outfilename=string('/z4/liufei/GFED/Result/hourly/'+Yr4+'/VOC/GFED_'+Yr4+Mon2+Day2+'_'+species+'_0.1x0.1.nc')
;outfilename=string('/z4/liufei/GFED/Result/hourly/'+Yr4+'/GFED_'+Yr4+Mon2+Day2+'_'+species+'.nc')
outfilename=strcompress(outfilename, /REMOVE_ALL)
outid=NCDF_CREATE(outfilename,/CLOBBER)
NCDF_ATTPUT,outid,/GLOBAL,'Conventions','Please contact Liu Fei (Email: liuf1010@gmail.com) for any question'
;NCDF_ATTPUT,outid,/GLOBAL,'Title','Mole number per hour per day of biomass flux from GFED'
NCDF_ATTPUT,outid,/GLOBAL,'Title','g species per hour per day of biomass flux from GFED'
timeid=NCDF_DIMDEF(outid,'time',24)
latid=NCDF_DIMDEF(outid,'lat',565)
lonid=NCDF_DIMDEF(outid,'lon',845)

;latid=NCDF_DIMDEF(outid,'lat',113)
;lonid=NCDF_DIMDEF(outid,'lon',169)
;latid=NCDF_DIMDEF(outid,'lat',360)
;lonid=NCDF_DIMDEF(outid,'lon',720)
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
NCDF_ATTPUT,outid,veid,'units','g species/m2/hour'
NCDF_ATTPUT,outid,veid,'long_name','Fraction of Emissions'
NCDF_ATTPUT,outid,veid,'FillValue','-999.f'
NCDF_CONTROL,outid,/ENDEF
NCDF_VARPUT,outid,vtimeid,[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23]
;NCDF_VARPUT,outid,vlatid,latitude
;NCDF_VARPUT,outid,vlonid,longitude
;NCDF_VARPUT,outid,veid,gfedhly
NCDF_VARPUT,outid,vlatid,latitude_part_regrid
NCDF_VARPUT,outid,vlonid,longitude_part_regrid
NCDF_VARPUT,outid,veid,gfedhly_part_regrid
NCDF_CLOSE,outid

;print some results
;print,gfeddly[496,230]

;threehly=float(0)
;hly=float(0)
;for i=0,7 do begin
;threehly=threehly+gfed3hly[496,230,i]
;endfor
;print,threehly
;for i=0,23 do begin
;hly=hly+gfedhly_part[0,34,i]
;endfor
;print,hly

endfor;day
endfor;month
endfor;year
endfor;species
end
