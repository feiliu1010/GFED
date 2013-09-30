;+
; NAME:
;     save_screen
; PURPOSE:
;     read image from display window and save as png file.
; CALLING SEQUENCE:
;     save_screen, filename
; ARGUMENTS:
;     filename (string) -> name of the png output file
;           including the '.png' . The user must assure that 
;           the file name is valid and can be written.
; NOTES:
;     This routine is a workaround for a limitation when reading 
;     images from a 16-bit Truecolor display. As stated in the IDL
;     documentation, in this case, only the 5-6 most significant
;     bits are read. This leads to a distracting gray background
;     in the resulting image files. David Fanning suggested to use the
;     CUBE keyword in COLOR_QUAN to remedy this problem, but I found
;     that this can produce ugly speckles in the image. The solution
;     applied here is to force all "very light gray" values (r, g, 
;     and b greater than threshold of 240B) to be exactly white.
;     Apparently, this works great for line plots and normal color 
;     images, but I would not recommend to use this routine to save
;     for example medical grayscale images.
; AUTHOR:
;     Martin Schultz
;     Max Planck institute for Meteorology, Hamburg, Germany
; VERSION HISTORY:
;     version 1.0 - 05 Jan 2005
;-

pro save_screen, filename

   img = tvread(order=1)    ;; read image from screen display
   ;; adjust light gray to white
   r = reform(img[0,*,*])
   g = reform(img[1,*,*])
   b = reform(img[2,*,*])
   w = where(r gt 240 and g gt 240 and b gt 240, cnt)
   if cnt gt 0 then begin
      r[w] = 255B
      g[w] = 255B
      b[w] = 255B
      img[0,*,*] = r
      img[1,*,*] = g
      img[2,*,*] = b
   endif 
   write_png, filename, img, /verbose

end
