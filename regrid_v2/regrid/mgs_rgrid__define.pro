;+
; NAME:
;   MGS_RGrid (object)
;
; PURPOSE:
;     This object serves as a utility program for rectangular,
;   horizontal geophysical grids. It allows the user
;      (1) to create a grid from a predefined catalogue (which is easy
;      to adapt). Selection can be done interactively or via the grid
;      name. 
;      (2) to create a grid using a given LonVector and LatVector
;      object.
;      (3) to retrieve various properties of the grid
;      (4) to determine grid box areas and weighting factors (yet to
;      be done)
;      (5) to regrid data from another rectangular grid with optional
;      area weighting.
;   The program was designed for global grids, but it should also work
;   for regional grids.
;
; AUTHOR:
;
;   Dr. Martin Schultz
;   Max-Planck-Institut fuer Meteorologie
;   Bundesstr. 55, D-20146 Hamburg
;   email: martin.schultz@dkrz.de
;
; CATEGORY:
;   General object programming
;
; CALLING SEQUENCE:
;
; NOTES:
;
; EXAMPLE:
;    See example procedure at the end of this file
;
;
; MODIFICATION HISTORY:
;   Martin G. Schultz, 08 May 2001: VERSION 0.10
;   mgs, 01 Jun 2001: VERSION 0.20 (GetProperty and Init(../Select)
;                                   now work)
;   mgs, 20 Aug 2001: VERSION 1.0
;   mgs, 23 Aug 2001: - small bug fix in regrid to prevent reporting
;                       of floating point underflow errors
;   mgs, 30 Aug 2001: - fixed yet another bug in the catalogue
;                       definition of the Lin&Rood grid
;   mgs, 04 Sep 2001: - added isglobal property to catalogue entries
;   mgs, 24 Jun 2002: - added (EP)TOMS grid to catalogue
;   mgs, 08 Jul 2002: - added 3.75x2.5 grid to catalogue
;   mgs, 24 Jan 2002: - added .552x.552 grid to catalogue (+ howto
;                           comment; thanks to Claire)
;   mgs, 14 Feb 2005: - added GetLon, GetLat, GetArea, GetName and
;                           Interpolate routines.
;   mgs, 14 Apr 2005: - added several grid definitions for IPCC AR4
;                       Photocomp
;   mgs, 15 Apr 2005: - bug fixes in interpolation and nlat keyword to
;                       getlat
;
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000-2001 Martin Schultz
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################


; =====================================================================================
; Regridding and Interpolation:
;   Regrid: a slow but conserving method to convert "flux" data from
;   one grid to another. The value for each grid box of the output
;   grid is computed as average of all grid boxes that overlap with
;   the area of this grid box.
;   Interpolate: IDL's bilinear interpolation routine is applied.


; -------------------------------------------------------------------------------------
; Regrid:
;    This method accepts a 2-dimensional data array and a grid
; definition object for this array and converts it to the new
; resolution defined by the current grid object. Set the area_weighted
; flag if you want area weighted averaging; otherwise the data will be
; linearily interpolated.

FUNCTION MGS_RGrid::Regrid, data, oldgrid, area_weighted=area_weighted, $
                            ok=ok


   ok=0

   ;; Check validity of old grid definition
   IF NOT Obj_Valid(oldgrid) THEN BEGIN
      Message,'oldgrid must be an object of type MGS_RGrid!', /Continue
      RETURN, -1L
   ENDIF 
   IF NOT Obj_IsA(oldgrid, 'MGS_RGrid') THEN BEGIN
      Message,'oldgrid must be an object of type MGS_RGrid!', /Continue
      RETURN, -1L
   ENDIF 

   ;; Extract longitude and latitude object and info from old grid
   ;; Note that global integral of wlat is 2, but here we don't care 
   oldgrid->GetProperty, nlon=nolon, nlat=nolat, elon=eolon, elat=eolat, $
      wlon=wlon, wlat=wlat
   olon = oldgrid->GetLonObject()
   olat = oldgrid->GetLatObject()

   ;; Check validity of data array
   ddims = Size(data, /Dimensions)
   IF N_Elements(ddims) NE 2 THEN BEGIN
      Message, 'Data array must be two-dimensional!', /Continue
      RETURN, -1L
   ENDIF 
   IF ddims[0] NE nolon OR ddims[1] NE nolat THEN BEGIN
      Message, 'Data array must have same dimensions as grid object!', $
               /Continue
      oldgrid->GetProperty,clon=clon,clat=clat
      help,data,clon,clat
      RETURN, -1L
   ENDIF 

   ;; Set up result array
   self->GetProperty, nlon=nnlon, nlat=nnlat, elon=enlon, elat=enlat
   nlon = self->GetLon()
   nlat = self->GetLat()
   result = DblArr(nnlon, nnlat)

   ;; Loop through latitudes and longitudes of result array and get
   ;; the indices and fractional coverages of the old grid
   t0 = systime(1)
   report_time  = 0

   FOR j=0L, nnlat-1 DO BEGIN
      oldj = olat->GetIndex(enlat[j:j+1],fraction=flat,/Exclusive)
      IF self.debug EQ 1 THEN print,'Latitude ',j,' of ',nnlat-1
      IF self.debug EQ 3 THEN BEGIN
         print
         print,'------------ new latitude ----------------'
         print,'#### j = ',j,' target edge: ',enlat[j:j+1], $
               format='(A,i4,A,2f8.2)'
         print,'#### oldgrid indices  :',oldj
         print,'#### oldgrid edges    :',olat->GetValue(oldj,/edge,/plus_one)
         print,'#### oldgrid fractions:',flat
         print,'#### oldgrid weights  :',wlat[oldj]
         print
      ENDIF 
      IF oldj[0] GE 0 THEN BEGIN
         FOR i=0L, nnlon-1 DO BEGIN
            oldi = olon->GetIndex(enlon[i:i+1],fraction=flon,/Exclusive)
            IF oldi[0] GE 0 THEN BEGIN

               ;; DEBUG output
               IF self.debug EQ 3 THEN BEGIN
                  print,'new lon ',i,': ',enlon[i:i+1]
                  print,'old lon edges:',olon->GetValue(oldi,/edge,/plus_one)
                  print,'old lon fracs:',flon
               ENDIF 

               ;; Compute weighting factor for average
               oldval = Double((data[oldi,*])[*,oldj])
               IF Keyword_Set(area_weighted) THEN BEGIN
                  wflon = flon * wlon[oldi]
                  wflat = flat * wlat[oldj]
               ENDIF ELSE BEGIN
                  wflon = flon
                  wflat = flat
               ENDELSE 
               f = wflon # wflat
               ftot = Total(f)
               ;; Compute new data value
               IF ABS(ftot) GT 1.d-90 THEN BEGIN
                  result[i,j] = total(f/ftot*oldval)
               ENDIF ELSE BEGIN
                  result[i,j] = 0.d0
               ENDELSE 
               
               IF self.debug EQ 3 THEN BEGIN
                  print,' result=',result[i,j]
               ENDIF 
            ENDIF 
         ENDFOR 
      ENDIF 
      IF j EQ 1 THEN BEGIN
         t1 = systime(1)
         esttime = (t1-t0)*nnlat*0.5
         IF esttime GT 10. THEN BEGIN
            print,'estimated time for regridding : ',esttime,' s'
            report_time = 1
         ENDIF 
      ENDIF 
      IF report_time AND j MOD 20 EQ 0 THEN $
         print,esttime-(systime(1)-t0),' s left...'
   ENDFOR 

   ;; Convert result to original data type
   dtype = Size(data,/TYPE)

   !EXCEPT=0
   result = Fix(result, TYPE=dtype)
   test = Check_Math(Mask=32B)
   !EXCEPT=1

   IF report_time THEN print,'Done.'
   ok = 1
   RETURN, result

END

; -------------------------------------------------------------------------------------
; Interpolate:
;   This method uses IDL's built-in bilinear interpolation function to
;   interpolate a 2D data array from one grid to the current
;   grid. This method is faster than Regrid, but not necessarily mass
;   conserving.
;     The ix and iy keywords can be used to store the target
;   coordinate values from one interpolation to the next. This will
;   significantly speed up the interpolation process for multiple
;   levels, time steps, etc..

FUNCTION MGS_RGrid::Interpolate, data, oldgrid, ix=ix, iy=iy

   ;; Check validity of old grid definition
   IF NOT Obj_Valid(oldgrid) THEN BEGIN
      Message,'oldgrid must be an object of type MGS_RGrid!', /Continue
      RETURN, -1L
   ENDIF 
   IF NOT Obj_IsA(oldgrid, 'MGS_RGrid') THEN BEGIN
      Message,'oldgrid must be an object of type MGS_RGrid!', /Continue
      RETURN, -1L
   ENDIF 

   ;; obtain grid coordinates etc.
   oldgrid->getproperty, clon=olon, clat=olat, nlon=nolon, nlat=nolat
   self->getproperty, clon=nlon, clat=nlat, nlon=nnlon, nlat=nnlat

   ;; Check validity of data array
   ddims = Size(data, /Dimensions)
   IF N_Elements(ddims) NE 2 THEN BEGIN
      Message, 'Data array must be two-dimensional!', /Continue
      RETURN, -1L
   ENDIF 
   IF ddims[0] NE nolon OR ddims[1] NE nolat THEN BEGIN
      Message, 'Data array must have same dimensions as grid object!', $
               /Continue
      help,data,olon,olat
      RETURN, -1L
   ENDIF 

   ;; set-up result array
   result = fltarr(nnlon, nnlat)

   ;; extend data array in longitude to ensure smooth interpolation
   ;; across grid limits
   tmpdat = [ data[nolon-1,*], data, data[0,*] ]

   ;; check if ix and iy can be used from previous interpolation
   if n_elements(ix) ne nnlon*nnlat or n_elements(iy) ne nnlon*nnlat then begin
      olonrange = max(olon)-min(olon)
      ix = (nlon-olon[0])/olonrange
      ;; compute index -- +1 for extended grid
      ix = (ix*(nolon-1)+nolon) mod nolon + 1

      olatrange = max(olat)-min(olat)
;;      iy = (nlat-abs(olat[0]))/olatrange   ;; assuming that latitudes are symmetric
      iy = (nlat-olat[0])/olatrange   ;; assuming that latitudes are symmetric
      iy = iy*(nolat-1)
      if olat[1] lt olat[0] then iy = -iy
   endif
;stop
   ;; do the interpolation
   result = bilinear(tmpdat, ix, iy)

   ;; Convert result to original data type
   dtype = Size(data,/TYPE)

   !EXCEPT=0
   result = Fix(result, TYPE=dtype)
   test = Check_Math(Mask=32B)
   !EXCEPT=1

   return, result

END

; =====================================================================================

; -------------------------------------------------------------------------------------
; GetLonObject:
;   This is a simple service function to return the object reference
; of the longitude grid vector object to the user.

FUNCTION MGS_RGrid::GetLonObject

   IF Obj_Valid(self.longitude) THEN RETURN, self.longitude

   self->ErrorMessage, 'No longitude vector defined'

   RETURN, Obj_New()

END


; -------------------------------------------------------------------------------------
; GetLatObject:
;   This is a simple service function to return the object reference
; of the latitude grid vector object to the user.

FUNCTION MGS_RGrid::GetLatObject

   IF Obj_Valid(self.latitude) THEN RETURN, self.latitude

   self->ErrorMessage, 'No latitude vector defined'

   RETURN, Obj_New()

END


; -------------------------------------------------------------------------------------
; GetIndex:
;   This method returns an index vector for a given longitude/latitude
; range. If either LonRange or LatRange is not provided, the global
; range is assumed. The default form of the result is a 1-D vector
; corresponding to the result of a Where function if
; longitude-latitude pairs were stored in a vector (see NOTE 1
; below). This behaviour can be consistently ported to non-rectangular
; grids. Set the Only_Lon or Only_Lat keyword if you want to retrieve
; the index array along one dimension only. This cannot be reproduced
; for non-rectangular grids.
; NOTES:
; (1) In order to better understand how GetIndex operates, assume a
; 2-dimensional data field such as:
;           LON  0   90   180   270
;       LAT
;        90     16   17   18    19 
;        30     12   13   14    15
;         0      8    9   10    11
;       -30      4    5    6     7
;       -90      0    1    2     3
; The data values in this field correspond to the index number of the
; longitude-latitude pair. Hence,
;    INDEX   LAT   LON
;      0     -90    0
;      1     -90   90
;      2     -90  180
;  etc.
; The order of the latitudes will be reversed if the grid is defined
; north->south. 
;
; (2) If you want to retrieve the index list for longitudes and
; latitudes seperately, you need to call GetIndex twice, once with
; keyword Only_Lon, and once with keyword Only_Lat.
;
; (3) Unlike the GetIndex function for the GridVector object,
; RGrid::GetIndex has currently no support for wrapping or coordinate
; reversal. The range keywords must correspond to the range that is
; defined in the coordinate vectors.

FUNCTION MGS_RGrid::GetIndex, LonRange=lonrange, LatRange=latrange, $
                  Only_Lon=only_lon, Only_Lat=only_lat, $
                  exclusive=exclusive, _Extra=extra

   ;; Make sure coordinate vectors are defined
   IF Obj_Valid(self.longitude) EQ 0 OR Obj_Valid(self.latitude) EQ 0 THEN BEGIN
      Help, self.longitude, self.latitude, output=o
      self->ErrorMessage, ['Longitude or latitude coordinates not defined',o]
      RETURN, -1L
   ENDIF 

   ;; Define default ranges for missing arguments
   self.longitude->GetProperty, limit=defaultlonrange
   IF N_Elements(LonRange) EQ 0 THEN LonRange = defaultlonrange
   self.latitude->GetProperty, limit=defaultlatrange
   IF N_Elements(LatRange) EQ 0 THEN LatRange = defaultlatrange

   ;; Simple cases first: analyse only_lon and only_lat keywords
   IF Keyword_Set(only_lon) THEN BEGIN
      retval = self.longitude->GetIndex(LonRange, exclusive=exclusive, _Extra=extra)
      RETURN, retval
   ENDIF 

   IF Keyword_Set(only_lat) THEN BEGIN
      retval = self.latitude->GetIndex(LatRange, exclusive=exclusive, _Extra=extra)
      RETURN, retval
   ENDIF 

   ;; Now the 2-D case: create a pair of coordinate vectors, then find
   ;; the index
   self->GetCoordinatePairs, thelats, thelons, /edges

   IF Keyword_Set(exclusive) THEN BEGIN
      retval = Where(thelons GT LonRange[0] AND thelons LT LonRange[1] $
                     AND thelats GT LatRange[0] AND thelats LT LatRange[1])
   ENDIF ELSE BEGIN
      retval = Where(thelons GE LonRange[0] AND thelons LE LonRange[1] $
                     AND thelats GE LatRange[0] AND thelats LE LatRange[1])
   ENDELSE 

   RETURN, retval

END


; -------------------------------------------------------------------------------------
; GetCoordinatePairs:
;    This utility method extracts the complete range of longitudes and
; latitudes from the gridvectors and creates two vectors of equal
; length containing all longitude-latitude combinations. This is a
; generic way to represent coordinates in any grid system.
;    The default is to return center coordinate vectors. Set the edges
; keyword to return edge coordinate vectors instead.

PRO MGS_RGrid::GetCoordinatePairs, lats, lons, edges=edges

   ;; Define defaults
   lats = [ !Values.F_NaN ]
   lons = [ !Values.F_NaN ]

   ;; Make sure coordinate vectors are defined
   IF Obj_Valid(self.longitude) EQ 0 OR Obj_Valid(self.latitude) EQ 0 THEN BEGIN
      Help, self.longitude, self.latitude, output=o
      self->ErrorMessage, ['Longitude or latitude coordinates not defined',o]
      RETURN
   ENDIF 

   IF Keyword_Set(edges) THEN BEGIN
      self.longitude->GetProperty, edges=lonvec
      self.latitude->GetProperty, edges=latvec
   ENDIF ELSE BEGIN
      self.longitude->GetProperty, centers=lonvec
      self.latitude->GetProperty, centers=latvec
   ENDELSE 

   nlon = N_Elements(lonvec)
   nlat = N_Elements(latvec)
   nelem = nlon*nlat
   tmp = DblArr(nlat)+1.d0
   lons = Reform( lonvec # transpose(tmp), nelem)
   tmp = DblArr(nlon)+1.d0
   lats = Reform( tmp # transpose(latvec), nelem)

END


; -------------------------------------------------------------------------------------
; Select:
;    This method displays a list widget with the predefined grid names
; and allows the user to select one. The gridname argument is used on
; input to make a preselection, and it returns the selected gridname
; on output.

PRO MGS_RGrid::Select, gridname

   ;; Create default gridname
   IF N_Elements(gridname) EQ 0 THEN gridname = ''

   ;; Get predefined grid names
   ok = self->GetCatalogueEntry(names=names)

   ;; Create list widget
   thelist = Obj_New('MGS_List',value=names, $
                     widget_title=self.seltitle, $
                     window_title='MGS_RGrid', $
                     xsize=15,ysize=12)

   IF gridname NE '' THEN thelist->SelectValue, gridname

   ;; Display the list widget
   thelist->GUI, /block

   ;; Get the selected grid name
   gridname = ( thelist->GetValue() )[0]

   ;; Destroy list widget
   Obj_Destroy, thelist

END


; -------------------------------------------------------------------------------------
; GetCatalogueEntry: (private)
;   This method returns grid variables for a number of predefined
; grids. The return value of this function is 1 if the gridname was
; found, and 0 otherwise. The function returns silently if gridname is
; undefined or an empty string. Otherwise, an error message is
; displayed if the gridname is not found in the catalogue.
; Arguments and keywords:
; gridname (input): The name of the grid to look for
; loninfo, latinfo (output): A structure containing the number of grid
;    boxes, the first value, the increment, and the "axis"type.
; names (output): A list of grid names contained in the catalogue (for
;    use in widgets etc.)
;
; To add a new catalogue entry:
; 1. add the name of the new grid to the names list
; 2. add a CASE statement element describing the new grid
; 3. makesure that the grid name in the CASE statement is written in
;    uppercase characters
;
; Note: before adding a new grid, please check if you cannot achieve the
; same result using the shiftlon and reverse_latitude keywords. For
; example: if you need a 1x1 degree grid starting at -180 degrees W,
; create the object as grid=obj_new('mgs_rgrid', gridname='1x1',
; shiftlon=-180.). Setting /reverse_latitude will change the latitude
; order. An exception to this general rule is made for standard model
; grids where the model name is easier to remember...

FUNCTION MGS_RGrid::GetCatalogueEntry, gridname, $
                  loninfo=loninfo, latinfo=latinfo, $
                  names=names, isglobal=isglobal

   retval = 0

   names = [ '5x5', '2x2', '1x1', '0.5x0.5', '3x2', '3.75x2.5', $
             'CHASER', 'FRSGC', 'GEOS-CHEM', 'GISS', 'GMI', $
             'IASB', 'IMAGES-NCAR', 'INCA', 'LLNL-IMPACT', $
             'MATCH-MPIC', 'MOZ2G', 'MOZECH', 'NCAR', $
             'OSLOCTM2', 'PTOMCAT', 'STOCED', 'STOCHEM', $
             'TM4', 'UMCAM', $
             'T21LR', 'T31LR', 'T42LR', 'T63LR', 'T85LR', 'T106LR', $
             'T21', 'T31', 'T42', 'T63', 'T85', 'T106', $
             'TOMS', '.552x.552' ]

   isglobal = 1    ;; per default, all grids defined here are global 
   ;; this implies that latitudes always range from -90 to 90 or from
   ;; 90 to -90.

   IF Arg_Present(loninfo) EQ 0 AND Arg_Present(latinfo) EQ 0 THEN RETURN, 1

   IF N_Elements(gridname) EQ 0 THEN RETURN, retval
   IF StrTrim(gridname,2) EQ '' THEN RETURN, retval

   CASE StrUpCase(StrTrim(gridname,2)) OF
      ;; regular 5x5 degree grid with lon centers starting at 0 and 
      ;; lats from south to north
      '5X5' : BEGIN
         retval = 1
         loninfo = { nvalues:72L, start:0.d0, increment:5.d0, type:'regular' }
         latinfo = { nvalues:35L, start:-85.d0, increment:5.d0, type:'regular' }
      END

      ;; regular 2x2 degree grid with lon centers starting at 0 and 
      ;; lats from south to north
      '2X2' : BEGIN
         retval = 1
         loninfo = { nvalues:180L, start:1.0d0, increment:2.d0, type:'regular' }
         latinfo = { nvalues:90L, start:-89.d0, increment:2.d0, type:'regular' }
      END

      ;; regular 1x1 degree grid with lon centers starting at 0 and 
      ;; lats from south to north
      '1X1' : BEGIN
         retval = 1
         loninfo = { nvalues:360L, start:0.5d0, increment:1.d0, type:'regular' }
         latinfo = { nvalues:180L, start:-89.5d0, increment:1.d0, type:'regular' }
      END

      ;; regular 0.5x0.5 degree grid with lon centers starting at 0 and 
      ;; lats from south to north
      '0.5X0.5' : BEGIN
         retval = 1
         loninfo = { nvalues:720L, start:0.25d0, increment:0.5d0, type:'regular' }
         latinfo = { nvalues:360L, start:-89.75d0, increment:0.5d0, type:'regular' }
      END

      ;; regular grid with unequal lons and lats (TM4 model)
      '3X2' : BEGIN
         retval = 1
         loninfo = { nvalues:120L, start:-178.5d0, increment:3.d0, type:'regular' }
         latinfo = { nvalues:90L, start:-89.d0, increment:2.d0, type:'regular' }
      END

      ;; regular grid with unequal lons and lats (UM model)
      '3.75X2.5' : BEGIN
         retval = 1
         loninfo = { nvalues:96L, start:1.875d0, increment:3.75d0, type:'regular' }
         latinfo = { nvalues:72L, start:-88.75d0, increment:2.5d0, type:'regular' }
      END

      ;; CHASER model (FRSGC, Japan) gaussian T42 grid with start 0
      ;; (IPCC AR4)
      'CHASER' : BEGIN
         retval = 1
         nlon = 128L
         nlat = 64L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90., increment:-1., type:'gaussian' }
      END 

      ;; FRSGC model (Japan) irregular "T42" grid with start 0
      ;; NOTE: the FRSCG grid is not truely gaussian, but gaussian
      ;; fits better than regular (IPCC AR4)
      'FRSGC' : BEGIN
         retval = 1
         nlon = 128L
         nlat = 64L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:1., type:'gaussian' }
      END 

      ;; 5x4 degree grid from the GEOS-CHEM model (IPCC AR4)
      ;; NOTE: the polar latitudes are actually -89 and 89 instead of
      ;; -90 and 90.
      'GEOS-CHEM' : BEGIN
         retval = 1
         loninfo = { nvalues:72L, start:-180.d0, increment:5.d0, type:'regular' }
         latinfo = { nvalues:46L, start:-90.d0, increment:4.d0, type:'regular' }
      END

      ;; 5x4 degree GMI (IPCC AR4)
      'GMI' : BEGIN
         retval = 1
         loninfo = { nvalues:72L, start:0.d0, increment:5.d0, type:'regular' }
         latinfo = { nvalues:46L, start:-90.d0, increment:4.d0, type:'regular' }
      END

      ;; 5x5 degree grid from the GISS GCM (IPCC AR4)
      ;; NOTE: the lat values from the netcdf file are different and
      ;; not symmetric around the equator. This is suspicious, and
      ;; therefore we use this regular definition until GISS grid is
      ;; confirmed. 
      ;; Polar latitudes probably not exactly -90 and 90.
      'GISS' : BEGIN
         retval = 1
         loninfo = { nvalues:72L, start:-177.5d0, increment:5.d0, type:'regular' }
         latinfo = { nvalues:46L, start:-90.d0, increment:4.d0, type:'regular' }
      END

      ;; IMAGES grid 5x5 degrees from the IASB model (IPCC AR4)
      'IASB' : BEGIN
         retval = 1
         loninfo = { nvalues:72L, start:0.d0, increment:5.d0, type:'regular' }
         latinfo = { nvalues:35L, start:85.d0, increment:-5.d0, type:'regular' }
      END

      ;; IMAGES grid 5x5 degrees from older NCAR results (natural emissions)
      'IMAGES-NCAR' : BEGIN
         retval = 1
         loninfo = { nvalues:72L, start:5.d0, increment:5.d0, type:'regular' }
         latinfo = { nvalues:35L, start:-85.d0, increment:5.d0, type:'regular' }
      END

      ;; INCA grid 3.75x2.5 degrees (IPCC AR4)
      'INCA' : BEGIN
         retval = 1
         loninfo = { nvalues:96L, start:-180.d0, increment:3.75d0, type:'regular' }
         latinfo = { nvalues:73L, start:90.d0, increment:-2.5d0, type:'regular' }
      END

      ;; LLNL 5x4 grid, regular
      'LLNL-IMPACT' : BEGIN
         retval = 1
         loninfo = { nvalues:72L, start:0.d0, increment:5.d0, type:'regular' }
         latinfo = { nvalues:46L, start:-90.d0, increment:4.d0, type:'regular' }
      END

      ;; MATCH T21 (roughly 5x5 degrees) (IPCC AR4)
      'MATCH-MPIC' : BEGIN
         retval = 1
         nlon = 64L
         nlat = 32L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:1., type:'gaussian' }
      END

      ;; MOZ2G T63 (GFDL MOZART, roughly 1.9x1.9 degrees) (IPCC AR4)
      'MOZ2G' : BEGIN
         retval = 1
         nlon = 192L
         nlat = 96L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ;; Gaussian grid corresponding to T63 spectral truncation
      ;; MOZECH/ECHAM GCM grid (IPCC AR4)
      'MOZECH' : BEGIN
         retval = 1
         nlon = 192L
         nlat = 96L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-1., type:'gaussian' }
      END

      ;; pseudo T42 (roughly 2.8x2.8 degrees) regular "Lin&Rood" grid (Mozart)
      'NCAR' : BEGIN 
         retval = 1
         nlon = 128L
         nlat = 64L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ;; Oslo CTM2 T42 (roughly 2.8x2.8 degrees) (IPCC AR4)
      'OSLOCTM2' : BEGIN
         retval = 1
         nlon = 128L
         nlat = 64L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:1., type:'gaussian' }
      END

      ;; Gaussian grid corresponding to T42 spectral truncation
      ;; pTOMCAT (IPCC AR4)
      'PTOMCAT' : BEGIN
         retval = 1
         nlon = 128L
         nlat = 64L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-1., type:'gaussian' }
      END

      ;; STOCHEM model university of Edingburgh (IPCC AR4)
      'STOCED' : BEGIN
         retval = 1
         loninfo = { nvalues:72L, start:2.5d0, increment:5.d0, type:'regular' }
         latinfo = { nvalues:36L, start:87.5d0, increment:-5.d0, type:'regular' }
      END

      ;; STOCHEM model Hadley Centre (IPCC AR4)
      'STOCHEM' : BEGIN
         retval = 1
         nlon = 96L
         nlat = 72L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:1.875d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:88.75d0, increment:-2.5d0, type:'regular' }
      END

      ;; TM4 model university of KNMI (IPCC AR4)
      'TM4' : BEGIN
         retval = 1
         loninfo = { nvalues:120L, start:-178.5d0, increment:3.d0, type:'regular' }
         latinfo = { nvalues:90L, start:-89.d0, increment:2.d0, type:'regular' }
      END

      ;; UM Cambridge (IPCC AR4)
      'UMCAM' : BEGIN
         retval = 1
         nlon = 96L
         nlat = 73L
         loninfo = { nvalues:nlon, start:0.d0, increment:3.75, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-2.5d0, type:'regular' }
      END

      ;; pseudo T21 (roughly 5x5 degrees) regular "Lin&Rood" grid (Mozart)
      'T21LR' : BEGIN
         retval = 1
         nlon = 64L
         nlat = 32L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ;; pseudo T31 (roughly 3.7x3.7 degrees) regular "Lin&Rood" grid (Mozart)
      'T31LR' : BEGIN    
         retval = 1
         nlon = 96L
         nlat = 48L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ;; pseudo T42 (roughly 2.8x2.8 degrees) regular "Lin&Rood" grid (Mozart)
      'T42LR' : BEGIN 
         retval = 1
         nlon = 128L
         nlat = 64L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ;; pseudo T63 (roughly 1.9x1.9 degrees) regular "Lin&Rood" grid (Mozart)
      'T63LR' : BEGIN
         retval = 1
         nlon = 192L
         nlat = 96L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ;; pseudo T85 (roughly 1.1x1.1 degrees) regular "Lin&Rood" grid (Mozart)
      'T85LR' : BEGIN
         retval = 1
         nlon = 256L
         nlat = 128L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ;; pseudo T106 (roughly 0.9x0.9 degrees) regular "Lin&Rood" grid (Mozart)
      'T106LR' : BEGIN
         retval = 1
         nlon = 320L
         nlat = 160L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ;; Gaussian grid corresponding to T21 spectral truncation
      ;; ECHAM GCM grid
      'T21' : BEGIN 
         retval = 1
         nlon = 64L
         nlat = 32L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-1., type:'gaussian' }
      END

      ;; Gaussian grid corresponding to T31 spectral truncation
      ;; ECHAM GCM grid
      'T31' : BEGIN
         retval = 1
         nlon = 96L
         nlat = 48L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-1., type:'gaussian'}
      END

      ;; Gaussian grid corresponding to T42 spectral truncation
      ;; ECHAM GCM grid
      'T42' : BEGIN
         retval = 1
         nlon = 128L
         nlat = 64L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-1., type:'gaussian' }
      END

      ;; Gaussian grid corresponding to T63 spectral truncation
      ;; ECHAM GCM grid (IPCC AR4)
      'T63' : BEGIN
         retval = 1
         nlon = 192L
         nlat = 96L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-1., type:'gaussian' }
      END

      ;; Gaussian grid corresponding to T85 spectral truncation
      ;; ECHAM GCM grid
      'T85' : BEGIN
         retval = 1
         nlon = 256L
         nlat = 128L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-1., type:'gaussian' }
      END

      ;; Gaussian grid corresponding to T106 spectral truncation
      ;; ECHAM GCM grid
      'T106' : BEGIN
         retval = 1
         nlon = 320L
         nlat = 160L
         dlon = 360.d0/double(nlon)
         loninfo = { nvalues:nlon, start:0.d0, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:90.d0, increment:-1., type:'gaussian' }
      END

      ;; TOMS total ozone column grid (1.25 degrees in lon and 1
      ;; degree in lat)
      'TOMS' : BEGIN
         retval = 1
         loninfo = { nvalues:288L, start:-179.375d0, increment:1.25d0, type:'regular' }
         latinfo = { nvalues:180L, start:-89.5d0, increment:1.d0, type:'regular' }
      END

      ;; strange grid from some satellite data set (which ?)
      '.552X.552' : BEGIN
         retval = 1
         nlon = 652L
         nlat = 326L
         dlon = 360.d0/double(nlon)
         dlat = 180.d0/double(nlat-1)
         loninfo = { nvalues:nlon, start:dlon*0.5, increment:dlon, type:'regular' }
         latinfo = { nvalues:nlat, start:-90., increment:dlat, type:'regular' }
      END

      ELSE : BEGIN
         self->ErrorMessage, 'Grid '+gridname+' not in catalogue.'
      ENDELSE

   ENDCASE

   RETURN, retval

END


; =====================================================================================
; Information routines:
;     GetLon      : retrieve center or edge values of longitudes
;     GetLat      : retrieve center or edge values of latitudes
;     GetArea     : retrieve gridbox areas as 2D field
;     GetName     : return the grid name

; -------------------------------------------------------------------------------------
; GetLon:
; This method returns the longitude center values. Longitude edge
; coordinates can be returned in a named variable passed with the
; edges keyword, the number of longitude values can be returned via
; the nlon keyword, and the weight factors via the weights keyword.
; Weights are defined as delta(lon)/nlon and are generally equal for
; all longitudes.

FUNCTION MGS_RGrid::GetLon, edges=edges, nlon=nlon, weights=weights

   self.longitude->GetProperty, nvalues=nlon, centers=clon, edges=elon, $
      weights=wlon
   if arg_present(edges) then edges = elon
   if arg_present(weights) then weights = wlon
   return, clon

END 


; -------------------------------------------------------------------------------------
; GetLat:
; This method returns the latitude center values. Latitude edge
; coordinates can be returned in a named variable passed with the
; edges keyword, the number of latitude values can be returned via
; the nlon keyword, and the weight factors via the weights keyword.
; Note that weights are defined according to the gaussian weights,
; i.e. their total is 2 rather than 1!

FUNCTION MGS_RGrid::GetLat, edges=edges, nlat=nlat, weights=weights

   self.latitude->GetProperty, nvalues=nlat, centers=clat, edges=elat, $
      weights=wlat, reversed=north_south
   if arg_present(edges) then edges = elat
   if arg_present(weights) then weights = wlat
   return, clat

END


; -------------------------------------------------------------------------------------
; GetArea:
; This method returns the grid box areas as a 2D array. The default
; unit is square meters, use the km2 keyword to return square
; kilometers. 
; This function uses a mean Earth radius of 6371. square kilometers.

FUNCTION MGS_RGrid::GetArea, km2=km2

   self->GetProperty, wlon=wlon, wlat=wlat, nlon=nlon, nlat=nlat
   R = 6371000.d0
   A = 4.*!DPI*R*R
   h1 = fltarr(nlat)+1.
   h2 = fltarr(nlon)+1.
   result = A * ( wlon # h1 ) * ( h2 # wlat ) * 0.5
   ;; factor 0.5 because total(wlat) is 2.
   if keyword_set(km2) then result = result * 1.d-6
   return, float(result)

END


; -------------------------------------------------------------------------------------
; GetName:
; Return the grid name. 

FUNCTION MGS_RGrid::GetName, gridtype=gridtype

   gridtype = self.gridtype
   return, self.name

END


; =====================================================================================
; Standard object methods:
;     GetProperty : retrieve object values
;     SetProperty : set object values
;     Cleanup     : free object pointers and destroy
;     Init        : initialize object

; -------------------------------------------------------------------------------------
; GetProperty:
; This method extracts specific object values and returns them to the
; user.

PRO MGS_RGrid::GetProperty,  $
             gridname=gridname,  $
             gridtype=gridtype,  $
             longitude=longitude, $  ; the longitude gridvector object
             latitude=latitude, $    ; the latitude grid vector object
             nlon=nlon,   $  ; number of longitude centers
             clon=clon,   $  ; center coordinates of longitudes
             elon=elon,   $  ; longitude edge coordinates
             wlon=wlon,   $  ; longitude weights
             nlat=nlat,   $  ; number of latitude centers
             clat=clat,   $  ; center coordinates of latitudes
             elat=elat,   $  ; latitude edge coordinates
             wlat=wlat,   $  ; latitude weights
             north_south=north_south, $
             _Ref_Extra=extra
                                ; Inherited keywords:
                                ; name      : The variable name
                                ; uvalue    : a user-defined value
                                ; no_copy   : Copy pointer values?
                                ; no_dialog : Object uses dialogs for messages?
                                ; debug     : Object is in debug mode?
                                ; status    : Object valid? (string)


   IF self.debug GT 1 THEN print,'## '+Routine_Name()+' in '+self.name

   ;; Call GetProperty method from parent object
   ;; Note that execution continues even if retrieval of a baseobject
   ;; property fails.
   self->MGS_BaseObject::GetProperty, _Extra=extra

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      self->ErrorMessage, 'Error retrieving grid properties!'
      RETURN
   ENDIF

   gridname = self.name
   gridtype = self.gridtype
   longitude = self.longitude
   latitude = self.latitude

   self.longitude->GetProperty, nvalues=nlon, centers=clon, edges=elon, $
      weights=wlon

   self.latitude->GetProperty, nvalues=nlat, centers=clat, edges=elat, $
      weights=wlat, reversed=north_south
;;   IF north_south THEN clat = Reverse(clat)

END


; -------------------------------------------------------------------------------------
; SetProperty:
;    This method sets specific object values. It needs to be
; redefined, because the debug attribute must be passed on to the
; latitude and longitude vector.

PRO MGS_RGrid::SetProperty,  $
                   lonname=lonname,  $   ; A new name for the longitude vector
                   latname=latname,  $   ; A new name for the latitude vector
                   shitflon=shiftlon, $  ; a new value for the longitude shift
                   debug=debug, $        ; A new debug level
                   _Extra=extra
                                ; Inherited keywords:
                                ; name      : The variable name
                                ; uvalue    : a user-defined value
                                ; no_copy   : don't copy data when
                                ;             creating pointers
                                ; no_dialog : Don't display
                                ;             interactive dialogs

   ;; Call SetProperty method of BaseObject
   self->MGS_BaseObject::SetProperty, debug=debug, _Extra=extra

   ;; ... and of the lonvector and latvector objects
   IF Obj_Valid(self.longitude) THEN $
      self.longitude->SetProperty, name=lonname, debug=debug
   IF Obj_Valid(self.latitude) THEN $
      self.latitude->SetProperty, name=latname, debug=debug

   IF n_elements(shiftlon) gt 0 then begin
      self.longitude->Shift, shiftlon[0], $
         /adjust_limit
   endif 

   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      self->ErrorMessage, 'Error setting grid properties!'
      result = 'CANCELLED'
      RETURN
   ENDIF

END

; -------------------------------------------------------------------------------------
; Cleanup:
; This method frees all data stored in the object. If dimvars are
; objects pointing to other variables, these will be left intact.

PRO MGS_RGrid::Cleanup

   Obj_Destroy, self.longitude
   Obj_Destroy, self.latitude

   ;; Call parent's cleanup method
   self->MGS_BaseObject::Cleanup

END

; -------------------------------------------------------------------------------------
; Init:
;   This method initializes the rectangular grid object. The longitudes
; and latitudes can be defined either by passing a predefined
; LonVector and LatVector object, or they will be created
; automatically using predefined grid definitions (the "catalogue").
;   Grids from the catalogue can be manipulated by using the keywords
; reverse_latitude, shiftlon, shiftlat, limitlon, or limitlat.
;
; NOTE:
; (1) Although both name and gridname are stored in the object's name
; property, there is an important difference: gridname is used as
; argument to CreateGrid, so the grid catalogue will be searched for a
; matching string, whereas name is stored as a (new) grid name.

FUNCTION MGS_RGrid::Init,  $
                           longitude_object=longitude, $ ; A predefined lonvector object
                           latitude_object=latitude,   $ ; A predefined latvector object 
                           gridname=gridname,  $ ; An alias for name used for selection
                           select_grid=select, $ ; Select gridname interactively
                           seltitle=seltitle, $ ; title for selection dialog
                           reverse_latitude=reverse_lat, $ ; Reverse latitudes
                           shiftlon=shiftlon, $ ; shift grid by XX degrees in longitude
                           shiftlat=shiftlat, $ ; shift grid in latitude
                           limitlon=limitlon, $ ; limit longitude range
                           limitlat=limitlat, $ ; limit latitude range
                           _Extra=extra ; For future additions
                                ; Inherited keywords:
                                ; name      : The variable name
                                ; uvalue    : a user-defined value
                                ; no_dialog : Don't display message dialogs
                                ; debug     : Put object in debugging state


   ;; Initialize parent object first
   IF NOT self->MGS_BaseObject::Init(_Extra=extra) THEN RETURN, 0

;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      Catch, /Cancel
;      self->ErrorMessage, 'Error initializing grid object!'
;      RETURN, 0
;   ENDIF


   ;; (1) Check if longitude and latitude object were predefined
   IF Obj_Valid(longitude) AND Obj_Valid(latitude) THEN BEGIN
      ;; Check object class
      ;; Note: should be type LonVector and LatVector, but we check
      ;;   the parent object type to allow greater flexibility
      IF NOT Obj_IsA(longitude, 'MGS_GridVector') THEN BEGIN
         self->ErrorMessage, 'Longitude object must be of class MGS_Gridvector!'
         RETURN, 0
      ENDIF 
      IF NOT Obj_IsA(latitude, 'MGS_GridVector') THEN BEGIN
         self->ErrorMessage, 'Latitude object must be of class MGS_Gridvector!'
         RETURN, 0
      ENDIF 

      ;; Store the coordinate vectors and exit
      self.longitude = longitude
      self.latitude = latitude

      RETURN, 1
   ENDIF 
   
   ;; (2) otherwise: Create vector objects from predefined catalogue

   ;; Select gridname interactively if desired
   IF N_Elements(seltitle) EQ 0 THEN seltitle = 'Select a grid:'
   self.seltitle = seltitle
   IF Keyword_Set(select) THEN BEGIN
      self->Select, gridname
      IF StrTrim(gridname,2) EQ '' THEN RETURN, 0
   ENDIF 

   ;; Check if gridname is in catalogue and compose _Extra options for
   ;; grid vector initialisation
   isstored = self->GetCatalogueEntry(gridname, loninfo=loninfo, latinfo=latinfo, $
                                     isglobal=isglobal)

   ;; Create grid vector objects
   IF isstored THEN BEGIN
      self.longitude = Obj_New('MGS_LonVector', name='Longitude', $
                               _Extra=loninfo, debug=self.debug)
      IF NOT Obj_Valid(self.longitude) THEN BEGIN
         self->ErrorMessage, 'Could not create longitude vector!'
stop
         RETURN, 0
      ENDIF 

      self.latitude  = Obj_New('MGS_LatVector', name='Latitude', $
                               _Extra=latinfo, isglobal=isglobal, $
                               debug=self.debug)
      IF NOT Obj_Valid(self.latitude) THEN BEGIN
         self->ErrorMessage, 'Could not create latitude vector!'
stop
         Obj_Destroy, self.longitude
         RETURN, 0
      ENDIF 

      ;; Modifiy longitude or latitude properties according to
      ;; keywords
      IF Keyword_Set(reverse_lat) THEN $
         self.latitude->Reverse
      IF N_Elements(shiftlon) GT 0 THEN $
         self.longitude->Shift, shiftlon, $
         /adjust_limit
      IF N_Elements(shiftlat) GT 0 THEN $
         self.latitude->Shift, shiftlat, $
         /adjust_limit, hard_limit=[-90., 90.]
      IF N_Elements(limitlon) GT 0 THEN $
         ok = self.longitude->Truncate(limitlon)
      IF N_Elements(limitlat) GT 0 THEN $
         ok = self.latitude->Truncate(limitlat)
      
      ;; If name is not given explicitely, use gridname
      IF N_Elements(name) EQ 0 THEN name = gridname
      self.name = name

      retval = 1

   ENDIF ELSE BEGIN

      self->ErrorMessage, [ 'Rectangular grid could not be created!', $
              'You must either pass a predefined LonVector and LatVector object', $
              'or select a gridname from the catalogue. Specify the select', $
              'keyword to choose a grid definition interactively.']
      
      retval = 0
   ENDELSE 

   RETURN, retval

END


; -------------------------------------------------------------------------------------
; This is the object definition for the grid object.
; NOTE:
; (1) modified is treated as bit flag: 0 = no modification, 
; 1 = longitude shifted, 2 = longitude truncated, 4 = longitude
; overwritten, 8 = latitude shifted, 16 = latitude truncated, 
; 32 = latitude overwritten, 64 = latitude reversed -- all values with
; respect to the predefined catalogue entries.

PRO MGS_RGrid__Define

   objectClass = { MGS_RGrid,  $    ; The object class
                   gridtype      : '',         $ ; a grid type label
                   longitude     : Obj_New(),  $ ; the longitude grid vector
                   latitude      : Obj_New(),  $ ; the latitude grid vector
                   incatalogue   : 0,          $ ; grid name contained in catalogue
                   seltitle      : '',         $ ; title for selection dialog          

                   INHERITS MGS_BaseObject  $
                 }
                   
END


; -------------------------------------------------------------------------------------
; Example: 
;    This example demonstrates a few features of the Grid
; object. Two grids are created, one of which the user has to select
; interactively. Then, a dummy data array is created and regridded
; from one grid definition to the other.
;    If a data array is passed into the example routine, it will be
; linearily interpolated (using Congrid) before being regridded.
;    For detailed debugging output, set debug to 3
;
; Example session:
; IDL> example,result=r1,nlon=20,nlat=10
; ; select T30 when asked for grid name
; IDL> example,result=r2,nlon=20,nlat=10
; ; select T42 when asked for grid name
; IDL> print,r1,format='(20i3)'
; IDL> print,r2,format='(20i3)'
; ; the results should be identical
; IDL> example,result=r3,nlon=20,nlat=10,shiftlon=-180.
; ; select any grid when asked
; IDL> r3 = [ r3[10:19,*], r3[0:9,*] ]
; IDL> print,r3,format='(20i3)'
; ; again, the results should be (almost) identical
  
PRO Example, data=data, result=result, oldgrid=o, newgrid=n, $
             area_weighted=area_weighted, $
             nlon=nlon, nlat=nlat, _Extra=extra


   ;; Create "source" grid. The user will be prompted to select a grid
   ;; from a standard grid catalogue
   o = Obj_New('MGS_RGrid', name='Source Grid', /Select, _Extra=extra)

   ;; Create a simple coarse resolution test grid 10x10 degrees
   ;; as target grid. Here, the grid is composed from a predefined
   ;; longitude and latitude vector. Note, that the respective start
   ;; value defines the first center coordinate.
   IF N_Elements(nlon) EQ 0 THEN nlon = 36L
   incr = 360.d0/Double(nlon)
   lon = Obj_New('MGS_LonVector', $
                 start=0.,incr=incr,nval=nlon, $
                 type='regular',_Extra=extra)
   IF N_Elements(nlat) EQ 0 THEN nlat = 18L
   incr = 180.d0/Double(nlat)
   lat = Obj_New('MGS_LatVector', $
                 start=-90.+0.5*incr,incr=incr,nval=nlat, $
                 type='regular',_Extra=extra)
   n = Obj_New('MGS_RGrid', name='Target Grid', $
               Longitude=lon, Latitude=lat, _Extra=extra)
   lon->printstatus
   lat->printstatus

   ;; Get number for longitude and latitude values from source grid
   o->GetProperty, nlon=nlon, nlat=nlat

   ;; Check if data array is available, else create dummy data
   data_generated = N_Elements(data) EQ 0
   IF data_generated THEN BEGIN
      data = Dist(nlon, nlat)
   ENDIF ELSE BEGIN
      ;; Data array must have 2 dimensions
      IF Size(data, /N_Dim) NE 2 THEN BEGIN
         Message, 'Data array must be 2-dimensional', /Continue
         Obj_Destroy, o
         Obj_Destroy, n
         RETURN
      ENDIF 

      ;; Interpolate data array if necessary
      ;; (in serious applications, this is of course not done!)
      data = Congrid(data, nlon, nlat)
   ENDELSE

   ;; Convert data to double
   data = Double(data)
   maxd = Max(data)
   IF data_generated THEN data = data/maxd*100.

   ;; Perform regridding operation. Regridding will be done with area
   ;; weighting if area_weighted option is set.
   result = n->Regrid(data, o, area_weighted=area_weighted)

   IF NOT Arg_Present(o) THEN Obj_Destroy, o
   IF NOT Arg_Present(n) THEN Obj_Destroy, n

END


pro test_grid, gridname, grid=grid, lon=clon, lat=clat

   grid = obj_new('mgs_rgrid', gridname=gridname)
   grid->getproperty, clon=clon, clat=clat
   if arg_present(grid) eq 0 then obj_destroy, grid

   print, 'latitudes:'
   print,clat, format='(10f9.3)'
   print, 'longitudes: '
   print,clon, format='(10f9.3)'

end
