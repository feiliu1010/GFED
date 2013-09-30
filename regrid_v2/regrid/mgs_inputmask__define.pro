;+
; NAME:
;    MGS_InputMask
;
; PURPOSE:
;    This widget object analyses a structure that is passed to the
;  Init method and builds a widget with one input field per structure
;  tag using the tag value as default (must be scalars). The
;  individual structure tags can be of any numerical or string
;  type. This widget makes it extremely easy to inspect or modify
;  structure values and it demonstrates the power of the MGS_BaseGUI
;  class hierarchy. All input fields are simple MGS_Field objects
;  which are added to the object's compound container. No event
;  handling etc. must be programmed, this is all done within the
;  individual compound widget objects.
;     The GetValue method returns a structure with the new field values.
;
; CATEGORY:
;  Widget Objects
;
; CALLING SEQUENCE:
;    Demo = { Name: 'Martin Schultz', Height: 180.,  $
;             email:'martin.schultz@dkrz.de', phone_ext: 308 }
;    themask = Obj_New('MGS_InputMask', Demo [,keywords])
;    themask->GUI, /block
;    newvalues = themask->GetValue()
;    help, newvalues, /Structure
;    Obj_Destroy, themask
;
;    MGS_inputmask can itself also be used as a compound widget!
;
; ARGUMENTS:
;
; KEYWORDS:
;
; MODIFICATION HISTORY:
;    mgs, 04 Apr 2001: Version 1.0 released
;    mgs, 27 Apr 2001: - adapted for use with new BaseGUI object
;       - added SetFieldValue method and more error checking
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2001 Martin Schultz
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



; -----------------------------------------------------------------------------
; GetState:
;   This method returns the currently selected range as a structure.

;FUNCTION MGS_InputMask::GetState

;   RETURN, self->GetValue()

;END


; -----------------------------------------------------------------------------
; GetValue:
;   This method collects the current values from all fields and stores
; them in a structure.

FUNCTION MGS_InputMask::GetValue

   ;; Get the object identifiers for the field widgets
   fobs = self.compound->Get(/All)

   FOR i=0L, N_Elements(fobs)-1 DO BEGIN
      IF Obj_Valid(fobs[i]) THEN BEGIN
         ;; get label text and value for this field
         thevalue = fobs[i]->GetValue()
         fobs[i]->GetProperty, labeltext=thelabel
         IF N_Elements(retval) EQ 0 THEN retval = Create_Struct(thelabel, thevalue) $
            ELSE retval = Create_Struct(retval, thelabel, thevalue)
      ENDIF
   ENDFOR

   RETURN, retval

END


; -----------------------------------------------------------------------------
; Validate:
;    Check input argument (must be a structure, and all tags must be
; scalar)
; NOTE:
; (1) Presently, no SetValue method is implemented to alter all
; input field values with one command. Instead you must use the
; SetFieldValue method to alter fields individually.

FUNCTION MGS_InputMask::Validate, arg, result=result

   result = 'INVALID_DATA'

   IF NOT ChkStru(arg) THEN BEGIN
      self->ErrorMessage, 'Argument must be a structure'
      RETURN, 0
   ENDIF 

   FOR i=0L, N_Tags(arg)-1 DO BEGIN
      IF N_Elements(arg.(i)) NE 1 THEN BEGIN
         self->ErrorMessage, 'Structure tag '+(Tag_Names(arg))[i]+' must be scalar!'
         RETURN, 0
      ENDIF 
   ENDFOR 

   result = 'OK'
   RETURN, arg

END


; -----------------------------------------------------------------------------
; SetFieldValue:
;    Change the value of a specific input field. Fieldname must be a
; string that conforms to a tag name of the structure that was used in
; the object initialisation; value must be scalar, but could
; theoretically change the field's data type.
;    Use the result keyword to obtain an error status of the
; operation. 
; NOTE:
; (1) You can use wildcard expressions to identify the field of which
; you want to change the value, but the search pattern must lead to a
; unique match.

PRO MGS_InputMask::SetFieldValue, fieldname, value, result=result

   result = 'INVALID_FIELD'

   theName = StrUpCase(StrTrim(fieldname,2))

   ;; Search for the matching input field
   IF Obj_Valid(self.compound) THEN BEGIN
      theField=self.compound->Get(Name=theName)
      IF N_Elements(theField) GT 1 THEN BEGIN
         self->ErrorMessage, 'More than one field matches '+theName
         RETURN
      ENDIF 
      IF NOT Obj_Valid(theField) THEN BEGIN
         self->ErrorMessage, 'No field found that matches '+theName
         RETURN
      ENDIF 
      theField->GetProperty, name=theName
   ENDIF 

   ;; Make sure new value is scalar, then set field value
   result = 'INVALID_DATA'
   IF N_Elements(value) NE 1 THEN BEGIN
      self->ErrorMessage, 'Value for field '+theName+' must be scalar!'
      RETURN
   ENDIF 

   theField->SetValue, value, ok=ok
   IF ok THEN result = 'OK'

END


; -----------------------------------------------------------------------------
; GetProperty:
; This method extracts specific object values and returns them to the
; user. Normally, the user should use the GetState() method, or more
; specific, GetValue(), to retrieve the object state in a usable form.

PRO MGS_InputMask::GetProperty, $
   _Ref_Extra=extra  ; Extra keywords from inherited objects
                                ;
                                ; Inherited keywords (from
                                ; MGS_BaseGUI and MGS_BaseObject):
                                ; name      : The object name
                                ; no_copy   : Don't retain a copy
                                ;             of uvalue
                                ; no_dialog : Don't display
                                ;             interactive dialogs
                                ; uvalue    : a user-defined value
                                ; window_title
                                ; widget_title
                                ; row_layout (ignored)
                                ; no_frame (ignored)
                                ; widget_defaultfont
                                ; widget_labelfont
                                ; set_value_pro
                                ; get_value_func


   ;; Get properties from base object
   self->MGS_BaseGUI::GetProperty, _Extra=extra

   ;; Error Handler
   Catch, theError
   IF theError NE 0 THEN BEGIN
      self->ErrorMessage, 'Error retrieving object properties!'
      RETURN
   ENDIF

   ;; nothing extra for now. Maybe later ...
END


; -----------------------------------------------------------------------------
; SetProperty:
; This method sets specific object values.
; NOTE: XSize and YSize only take effect when the widget is rebuilt.

PRO MGS_InputMask::SetProperty, $
   _Extra=extra  ; Extra keywords from inherited objects
                                ;
                                ; Inherited keywords (from
                                ; MGS_BaseGUI and MGS_BaseObject):
                                ; name      : The object name
                                ; no_copy   : Don't retain a copy
                                ;             of uvalue
                                ; no_dialog : Don't display
                                ;             interactive dialogs
                                ; uvalue    : a user-defined value
                                ; window_title
                                ; widget_title
                                ; row_layout (ignored)
                                ; no_frame (ignored)
                                ; widget_defaultfont
                                ; widget_labelfont
                                ; set_value_pro
                                ; get_value_func


   ;; Set Properties of base object
   self->MGS_BaseGUI::SetProperty, _Extra=extra

   ;; Error Handler
   Catch, theError
   IF theError NE 0 THEN BEGIN
      self->ErrorMessage, 'Error setting object properties'
      RETURN
   ENDIF

   ;; Set extra properties for all compound objects
   IF N_Elements(extra) GT 0 THEN BEGIN
      fobj = self.compound->Get(name='Field*')
      FOR i=0L, N_Elements(fobj)-1 DO BEGIN
         IF Obj_Valid(fobj[i]) THEN fobj[i]->SetProperty, _Extra=extra
      ENDFOR
   ENDIF

   ;; Make sure object is up-to-date and redisplay
   self->UpdateObject
   self->Show

END

; -----------------------------------------------------------------------------
; Cleanup:
;   It is not necessary to overwrite the genric GUI object cleanup method!

; -----------------------------------------------------------------------------
; Init:
;   This method initializes the range field object.
; For now, the only fields that need to be specified individually to
; each of the compound field widgets are label and
; value. Alternatively, you can give range as a 2-element vector.
; All other options that are possible for MGS_Field are passed through
; the extra mechanism and will thus apply to both fields.

FUNCTION MGS_InputMask::Init, $
   structure,       $  ; A data structure defining the labels (tag_names) and values
   _Extra=extra  ; Extra keywords from inherited objects
                                ;
                                ; Inherited keywords (from
                                ; MGS_BaseGUI and MGS_BaseObject):
                                ; name      : The object name
                                ; no_copy   : Don't retain a copy
                                ;             of uvalue
                                ; no_dialog : Don't display
                                ;             interactive dialogs
                                ; uvalue    : a user-defined value
                                ; window_title
                                ; widget_title
                                ; row_layout (ignored)
                                ; no_frame (ignored)
                                ; widget_defaultfont
                                ; widget_labelfont
                                ; set_value_pro
                                ; get_value_func


   ;; Initialize parent object
   IF not self->MGS_BaseGUI::Init(_Extra=extra) THEN RETURN, 0

   ;; Error Handler
;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      self->ErrorMessage, 'Error initialising object'
;      RETURN, 0
;   ENDIF

   ;; Check keywords and parameters.
   IF ChkStru(structure) EQ 0 THEN BEGIN
      self->ErrorMessage, 'A data structure is mandatory'
      RETURN, 0
   ENDIF

   ;; Make sure structure is "valid" in the sense of this object
   thestructure = self->Validate(structure, result=result)
   IF result NE 'OK' THEN RETURN, 0

   labels = Tag_Names(thestructure)

   ;; Create compound widget objects (text fields)
   FOR i=0L, N_Elements(labels)-1 DO BEGIN
      theObj = Obj_New('MGS_Field', $
                       labeltext=String(labels[i]), $
                       value=thestructure.(i), $
                       Name=String(labels[i]), $
;;                       Name='Field'+String(i+1,format='(i3.3)'), $
                       /no_frame, $
                       _Extra=extra)
      IF Obj_Valid(theObj) THEN self.compound->Add, theObj
   ENDFOR

   ;; Reset some of the generic default properties
   IF self.window_title EQ 'Generic widget object' THEN $
      self.window_title = 'Input mask'

   ;; Set layout to row
   self.column_layout = 1

   self.layout_frame = 1    ;; Put a frame around this widget

   RETURN, 1
END


; -----------------------------------------------------------------------------
; MGS_InputMask__Define:
; This is the object definition for the input mask object.
; It inherits from MGS_BaseGUI which provides the generic widget
; functionality, and from MGS_BaseObject the abilities to set and
; query an object name and a uvalue. The base object also provides a
; general method for display of error messages which can be directed
; to a message dialog or to the log screen via the no_dialog flag.

PRO MGS_InputMask__Define

   struct = { MGS_InputMask, $

              inherits MGS_BaseGUI  }

END


; -----------------------------------------------------------------------------
; Example:
;    Demonstrate the functionality of this object

PRO Example, structure, block=block, object=object, _EXTRA=extra

   IF N_Elements(structure) EQ 0 THEN $
      structure = { Name: 'Martin Schultz', $
                    Institution: 'Max Planck Institute for Meteorology', $
                    Address: 'Bundesstr. 55, 20146 Hamburg', $
                    Country: 'Germany', $
                    email:'martin.schultz@dkrz.de', $
                    home_page: 'http://www.mpimet.mpg.de/~schultz.martin/', $
                    favorite_number: !DPI }

   IF Keyword_Set(block) THEN thebuttons = ['Accept','Cancel','Reset'] $
      ELSE thebuttons = ['Apply','Close','Reset']

   thegui = Obj_New('MGS_InputMask', structure, _Extra=extra, $
                   buttons=thebuttons, xsize=45, labelsize=120)

   IF NOT Obj_Valid(thegui) THEN RETURN

   thegui->GUI, block=keyword_Set(block)

   keywords = thegui->GetValue()
   help, keywords, /structure
   IF Arg_Present(object) EQ 0 THEN Obj_Destroy, thegui ELSE object=thegui

END
