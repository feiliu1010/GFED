load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRF_contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
begin
nlat_36km=115
nlon_36km=169
;pre_net=new((/nlat,nlon,2/),float)
;define the lat and lon of 0.5x0.5 net [8.25,64.25]X[68.25,152.25]
;do i=0,nlat-1
;do j=0,nlon-1
;pre_net(i,j,:)=(/(8.25+i*0.5),(68.25+j*0.5)/)
;end do
;end do
;print(pre_net)
;input the lat and lon of 36kmX36km net
filename = "/home/liufei/GFED/36km_net.csv"
post_net = asciiread(filename,(/19435,2/),"float")
;print(post_net(0,1))
lat2d = onedtond(post_net(:,0),(/nlat_36km,nlon_36km/))
lon2d = onedtond(post_net(:,1),(/nlat_36km,nlon_36km/))
lat2d!0="lat"
lat2d!1="lon"
lon2d!0="lat"
lon2d!1="lon"

do year=2009,2009
Yr4= sprinti("%0.4i",year)
;inputdir = "/z4/liufei/GFED/Result/hourly/"+Yr4+"/Mole/"
;inputdir = "/z4/liufei/GFED/Result/hourly/"+Yr4+"/Gram/"
inputdir = "/home/liufei/GFED/Result/hourly/"+Yr4+"/VOC/"
inputfile=systemfunc("ls "+inputdir+"*.nc")
len= num(inputfile.ne.0)
;print(inputfile)
;print(len)

do i=0,len-1
gfedhly = addfile(inputfile(i),"r")
time = gfedhly->time
lat1d = gfedhly->lat
lon1d = gfedhly->lon
Emission = gfedhly->Fraction_of_Emissions
outfile = rgrid2rcm(lat1d,lon1d,Emission,lat2d,lon2d,0)
;print(outfile)
;print(lon1d)
;print(lon2d)
outfile!0="time"
outfile!1="lat"
outfile!2="lon"

;output nc file
ntime = dimsizes(time)
;nlat = dimsizes(lat1d)
;nlon = dimsizes(lon1d)
nlat = nlat_36km
nlon = nlon_36km

;diro = "/z4/liufei/GFED/Result/projection/"+Yr4+"/Mole/"
;diro = "/z4/liufei/GFED/Result/projection/"+Yr4+"/Gram/"
diro ="/home/liufei/GFED/Result/projection/"+Yr4+"/VOC/"

;filo = str_sub_str(inputfile(i), "/z4/liufei/GFED/Result/hourly/"+Yr4+"/Mole/", "")
;filo = str_sub_str(inputfile(i), "/z4/liufei/GFED/Result/hourly/"+Yr4+"/Gram/", "")
filo = str_sub_str(inputfile(i), "/home/liufei/GFED/Result/hourly/"+Yr4+"/VOC/", "")
print(filo)
system("/bin/rm -f " + diro + filo)
fout = addfile(diro+filo,"c")

setfileoption(fout,"DefineMode",True)

fAtt = True
fAtt@Conventions = "Please contact Liu Fei (Email: liuf1010@gmail.com) for any question"
fAtt@Title = "Mole number per hour per day of biomass flux from GFED"
;fAtt@Title = "g per hour per day of biomass flux from GFED"
;fAtt@creation_date = systemfunc ("date")        
fileattdef(fout,fAtt)

dimNames = (/"time","lat","lon"/)  
dimSizes = (/24,nlat,nlon/) 
dimUnlim = (/False,False,False/)   
filedimdef(fout,dimNames,dimSizes,dimUnlim)

filevardef(fout,"time",typeof(time),getvardims(time))
;filevardef(fout,"lat",typeof(lat2d),getvardims(lat2d))
;filevardef(fout,"lon",typeof(lon2d),getvardims(lon2d))
filevardef(fout,"Emission",typeof(outfile),getvardims(outfile))

filevarattdef(fout,"time",time)
;filevarattdef(fout,"lat",lat2d)
;filevarattdef(fout,"lon",lon2d)
filevarattdef(fout,"Emission",outfile)

setfileoption(fout,"DefineMode",False)

fout->time =(/time/)
;fout->lat =(/lat2d/)
;fout->lon =(/lon2d/)
fout->Emission = (/outfile/)

end do
end do
end
