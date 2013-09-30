

PRO test_regrid, only_plot=only_plot, plot_step=plot_step, big_region=big_region

   dpath = '~/scr1/tmp/'
   ;; user params
   ;; mark stations 
   testnames = ['Lagos', 'Alert', 'Ascension', 'Cape Grim', 'Izana', 'Boston', 'Capetown' ]
   testlon = [ 3.33, -62., -14.42, 144.7, -16.48, -73.4, 18.41 ]
   testlat = [ 6.55, 82.45, -7.92, -40.68, 28.3, 41.3, -33.91 ]
   ww = where(testlon lt 0.,cnt)
   if cnt gt 0 then testlon[ww] = testlon[ww]+360.
   if keyword_set(big_region) then begin
      testlon = 70.
      testlat = 20.
   endif 

   ;; define models (grids)
   models = [ 'CHASER', 'FRSGC', 'GEOS-CHEM', 'GISS', 'IASB', $
              'LLNL-IMPACT', 'MATCH-MPIC', 'MOZ2G', 'MOZECH', $
              'NCAR', 'OSLOCTM2', 'STOCED', 'STOCHEM', 'TM4', 'UMCAM', $
              'GMI', 'INCA', 'PTOMCAT' ]

   if keyword_set(only_plot) then goto, mark1


   ;; 1. create a 1x1 degree test file
   grid = obj_new('mgs_rgrid', gridname='1x1')
   lon = grid->getlon(nlon=nlon)
   lat = grid->getlat(nlat=nlat)
   obj_destroy, grid

   field = fltarr(nlon, nlat)
   if keyword_set(big_region) then begin
      pa = [ 40., 40., 40., 40., 40., 40. ]
      pb = [ 0., 0., 0., 0., 0., 0. ]
      pattern1 = [ pa, pb, pa, pb, pa, pb, pa, pb, pa, pb, pa, pb ]
      pattern2 = [ pb, pa, pb, pa, pb, pa, pb, pa, pb, pa, pb, pa ]
      npat = n_elements(pattern1)
      for jj=90L,120L do begin
         if jj mod 12 lt 6 then begin
            field[30:30+npat-1,jj] = pattern1
         endif else begin
            field[30:30+npat-1,jj] = pattern2
         endelse 
      endfor 
   endif else begin
      for i=0L,n_elements(testlon)-1 do begin
         dx = abs(lon-testlon[i])
         dy = abs(lat-testlat[i])
         wx = (where(dx eq min(dx)))[0]
         wy = (where(dy eq min(dy)))[0]
         field[wx,wy] = 40.
      endfor 
   endelse 
   ncdata = { lon:lon, lat:lat, field:field }
   lonattr = { units:'degrees_east' }
   latattr = { units:'degrees_north' }
   fieldattr = { units:'none' }
   attr = { lon:lonattr, lat:latattr, field:fieldattr }
   ncdf_write, dpath+'ori_test.1x1.nc', ncdata, ['lon','lat'], attr=attr

   ;; 2. regrid to IPCC models
   for i=0L, n_elements(models)-1 do begin
      print, 'Regridding to '+models[i]+' grid...'
      ncregrid, dpath+'ori_test.1x1.nc', '1x1', models[i], $
         outfile=dpath+models[i]+'_test.nc' ;; ,/conserve
   endfor 

   ;; 3. and back to 1x1
   for i=0L, n_elements(models)-1 do begin
      print, 'Regridding from '+models[i]+' grid...'
      ncregrid, dpath+models[i]+'_test.nc', models[i], '1x1', $
         outfile=dpath+models[i]+'_test.1x1.nc'
   endfor 

   print, 'Done.'

   ;; 4. plot results 
mark1:
   loadct, 27, bottom=10,ncolors=30
   tvlct, [255, 0, 192], [255, 0, 192], [255, 0, 192], 0
   !p.background = 0
   !p.color = 1
   window, 0, xsize=860, ysize=1020
   if n_elements(plot_step) eq 0 then plot_step = 2
   for i=0L,n_elements(testlon)-1 do begin
      !p.multi = [0, 3, 6]
      if keyword_set(big_region) then begin
         x0 = testlon[i]-50.
         x1 = testlon[i]+50.
         y0 = testlat[i]-25.
         y1 = testlat[i]+25.
         gdlon = 20.
      endif else begin
         x0 = testlon[i]-20.
         x1 = testlon[i]+20.
         y0 = testlat[i]-20.
         y1 = testlat[i]+20.
         gdlon = 2.
      endelse 
      for j=0L,n_elements(models)-1 do begin
         icol = j mod 3
         irow = fix(j/3.)
         pwx = 0.3
         pwy = 0.15
         limit=[y0>(-90.)<90.,x0,y1>(-90.)<90.,x1]
         map_set, 0., testlon[i], limit=limit, /advance, /noborder, $
            position=[0.05+icol*pwx, 0.93-(irow+1)*pwy, $
                      0.01+(icol+1)*pwx,0.9-irow*pwy]
         map_continents, /fill, color=2
         map_grid, /box_axes, charsize=0.8, londel=gdlon
         if plot_step lt 2 then fname = dpath+models[j]+'_test.nc' $
            else fname = dpath+models[j]+'_test.1x1.nc'
         ncdf_read, d, file=fname, /all
         ww = where(d.lon lt 0., nwest)
         if nwest gt 0 then d.lon[ww] = d.lon[ww]+360.
         wx = where(d.lon ge x0 and d.lon le x1, nx)
         wy = where(d.lat ge y0 and d.lat le y1, ny)
         if nx gt 0 and ny gt 0 then begin
            field = ((d.field)[wx,*])[*,wy]
            print,models[j],':',max(field)
            for ki = 1L,nx-2 do begin
               for kj=1L,ny-2 do begin
                  if field[ki,kj] gt 0.2 then begin
                     px0 = 0.5*(d.lon[wx[ki-1]]+d.lon[wx[ki]])
                     px1 = 0.5*(d.lon[wx[ki]]+d.lon[wx[ki+1]])
                     py0 = 0.5*(d.lat[wy[kj-1]]+d.lat[wy[kj]])
                     py1 = 0.5*(d.lat[wy[kj]]+d.lat[wy[kj+1]])
                     px0 = px0 > 0. < 360.
                     px1 = px1 > 0. < 360.
                     py0 = py0 > (-90.) < 90.
                     py1 = py1 > (-90.) < 90.
                     color = (field[ki,kj]+11) < 39
                     polyfill, [px0,px0,px1,px1,px0],[py0,py1,py1,py0,py0],color=color
                     lx = !x.window[0]+0.1*(!x.window[1]-!x.window[0])
                     ly = !y.window[0]+0.8*(!y.window[1]-!y.window[0])
                  endif 
               endfor 
            endfor ;; grid loop
            map_continents, color=2
         endif 
         xyouts, lx, ly, models[j], /norm, charsize=1.4
      endfor    ;; models
      xyouts, 0.5, 0.92, testnames[i], /norm, align=0.5, charsize=1.8
      save_screen, 'gridtest_'+string(i,format='(i2.2)')+'_'+ $
         string(plot_step,format='(i1.1)')+'.png'
   endfor       ;; test sites
;;stop

end 


PRO old_test_regrid, oldgridname, newgridname, result=result, debug=debug


   oldgrid = Obj_New('MGS_RGrid', gridname=oldgridname, $
                     select=(N_Elements(oldgridname) EQ 0) )

   newgrid = Obj_New('MGS_RGrid', gridname=newgridname, $
                     select=(N_Elements(newgridname) EQ 0) )


   ;; create data array with all 1's
   oldgrid->GetProperty, nlon=nlon, nlat=nlat
   data = fltarr(nlon, nlat)+1.

   ;; Set debug level to 3 in order to see something
   newgrid->SetProperty, debug=debug
   oldgrid->SetProperty, debug=debug

   ;; Do the regridding
   result = newgrid->Regrid(data, oldgrid)

   ;; Destroy the objects
   Obj_Destroy, oldgrid
   Obj_Destroy, newgrid

END
