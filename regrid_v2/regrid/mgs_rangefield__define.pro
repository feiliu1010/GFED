;+
; NAME:
;    MGS_RangeField
;
; PURPOSE:
;    This widget object combines two MGS_Field objects to create a
;  range input compound widget with a "lower limit" and "upper
;  limit" value. The two fields are stored in the compound container,
;  and a NotifyObject communication is set up to ensure that the left
;  field always has a lower value than the right field (RangeCheck
;  method). In order to set and retrieve the object's value as a
;  2-element vector, the SetValue, GetValue, and Validate methods are
;  overwritten. 
;
; CATEGORY:
;  Widget Objects
;
; CALLING SEQUENCE:
;    .run MGS_RangeField__Define
;    example
;
; ARGUMENTS:
;
; KEYWORDS:
;
; MODIFICATION HISTORY:
;    mgs, 04 Apr 2001: Version 1.0 released
;    mgs, 27 Apr 2001: - partly rewritten to use new BaseGUI
;         properties (especially Notify and Reset functionality)
;       - labeltext now 2-element string vector
;       - bug fix in GetProperty: call to inherited method must be at
;         the end!
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

;FUNCTION MGS_RangeField::GetState

;   RETURN, { range:self->GetValue() }

;END


; -----------------------------------------------------------------------------
; GetValue:
;    This method returns the latest valid "value" of the widget in a
; useful format. Depending on the value of self.updatemode, the
; UpdateObject method is called beforhand to make sure that the
; objects value field contains the most recent validated data.
;    You should overwrite the UpdateObject method in order to store
; the relevant widget data in a suitable form. You probably don't need
; to overwrite the GetValue method itself.
; NOTE:
; (1) The values of updatemode are:
;      0 = never update automatically
;      1 = update automatically 
;      2 = update immediately
; See MGS_Field for a more detailed description.

FUNCTION MGS_RangeField::GetValue, ok=ok, _Extra=e

   ;; Error handler
   ;; ...

   ok = 0

   IF self.updatemode EQ 1 THEN self->UpdateObject

   ;; Get the object identifiers for the two field widgets
   fobs = self.compound->Get(name='Field*')
   ;; (little debug)
   IF N_Elements(fobs) NE 2 THEN BEGIN
      self->ErrorMessage, 'Why are there not two field objects here?'
      stop
   ENDIF

   value1 = fobs[0]->GetValue()
   value2 = fobs[1]->GetValue()

   retval = [ value1, value2 ]
   ok = 1

   RETURN, retval
END


; -----------------------------------------------------------------------------
; SetValue:
;   This method accepts a value argument, validates it, and stores it
; in the object's value field if the validation was successful or
; displays an error message otherwise.
; NOTE:
; (1) Validate must take care of all error checking including a test
; for an undefined argument. The result keyword of validate must
; return 'OK' if validation was successful.

PRO MGS_RangeField::SetValue, value, ok=ok

   ;; Error handler ...

   newvalue = self->Validate(value, result=result)

   ok = ( StrUpCase(result) EQ 'OK' ) 

   IF ok THEN BEGIN
      ;; Store values in compound widget fields
      cwobjects = self.compound->Get(name='Field*')
      cwobjects[0]->SetValue, newvalue[0]
      cwobjects[1]->SetValue, newvalue[1]
      ;; Update display
      self->Show
   ENDIF ELSE BEGIN
      self->ErrorMessage, 'Invalid value argument. Result='+result
   ENDELSE 

   RETURN
END


; -----------------------------------------------------------------------------
; Validate:
;    Check input argument (must be numeric and have two elements) and
; return a 2-element vector that can be stored in the object's value
; field.  
; NOTE:
; (1) The range values are not checked for their minimum and maximum
; allowed range here. This is done in the individual field objects. I
; am not sure this is sufficient.

FUNCTION MGS_RangeField::Validate, arg, result=result

   result = 'INVALID_DATA'

   IF N_Elements(arg) NE 0 THEN BEGIN
      IF N_Elements(arg) NE 2 THEN BEGIN
         self->ErrorMessage, 'Range value must be a 2-element vector'
         RETURN, 0
      ENDIF
      type = Size(arg, /TName)
      IF type EQ 'STRING' THEN BEGIN
         self->ErrorMessage, 'Range value must be of numeric type'
         RETURN, 0
      ENDIF
   ENDIF

   result = 'OK'
   RETURN, arg

END


; -----------------------------------------------------------------------------
; RangeCheck: (private)
;   This method is called by the basegui event handler if one of the
; field compound widgets generates an event.

FUNCTION MGS_RangeField::RangeCheck, event

   ;; Error handler ...

   IF self.debug GT 1 THEN BEGIN
      print,'### in RangeCheck!###'
      help,event,/stru
   ENDIF 

   ;; See which of the fields generated the event
   event.object->GetProperty, name=fieldname

   ;; If not allowed to wrap, make sure that lower bound is not
   ;; greater than upper bound
   range = self->GetValue()
   IF range[0] GT range[1] AND NOT self.allow_wrap THEN BEGIN
      ;; If the error was made in Field1...
      IF StrUpCase(fieldname) EQ 'FIELD1' THEN BEGIN
         self->ErrorMessage, 'The lower value must be smaller than the upper value'
      ENDIF ELSE BEGIN
         self->ErrorMessage, 'The upper value must be greater than the lower value'
      ENDELSE 
      void = event.object->ResetDialog(event)
      event.object->SetFocus
   ENDIF

   RETURN, 0

END


; -----------------------------------------------------------------------------
; BuildGUI:  (private)
;   Not necessary to overwrite this method: All the compound widgets
; will be added automatically. 

; PRO MGS_RangeField::BuildGUI
;
;
; END


; -----------------------------------------------------------------------------
; GetProperty:
; This method extracts specific object values and returns them to the
; user. Normally, the user should use the GetState() method, or more
; specific, GetValue(), to retrieve the object state in a usable form.

PRO MGS_RangeField::GetProperty, $
   value1=value1,   $  ; The value of the first text box
   value2=value2,   $  ; The value of the second text box
   range=range,     $  ; A 2-element vector with [value1, value2] as alternative
   labeltext=label, $  ; The text of the two labels
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


   ;; Error Handler
   Catch, theError
   IF theError NE 0 THEN BEGIN
      self->ErrorMessage, 'Error retrieving object properties!'
      RETURN
   ENDIF

   ;; We will need to retrieve properties of the compound widgets anyway,
   ;; so let's get the object pointers here
   fobs = self.compound->Get(name='Field*')
   ;; (little debug)
   IF N_Elements(fobs) NE 2 THEN BEGIN
      self->ErrorMessage, 'Why are there not two field objects here?'
      stop
   ENDIF

   ;; Get the range and value properties through the GetValue method
   fobs[0]->GetProperty, labeltext=label1, _Extra=extra
   fobs[1]->GetProperty, labeltext=label2, _Extra=extra
   label = [ label1, label2 ]

   range = self->GetValue()
   value1 = range[0]
   value2 = range[1]

   ;; Get properties from base object
   self->MGS_BaseGUI::GetProperty, _Extra=extra

END


; -----------------------------------------------------------------------------
; SetProperty:
; This method sets specific object values.
; NOTE: XSize and YSize only take effect when the widget is rebuilt.

PRO MGS_RangeField::SetProperty, $
   value1=value1,   $  ; The value of the first text box
   value2=value2,   $  ; The value of the second text box
   range=range,     $  ; A 2-element vector with [value1, value2] as alternative
   labeltext=label, $  ; The text of the two labels
   allow_wrap=allow_wrap, $ ; Allow "upper" value to be smaller than "lower" value
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

   ;; Set new text field values
   IF N_Elements(value1) GT 0 THEN BEGIN
      fobj = self.compound->Get(name='Field1')
      IF Obj_Valid(fobj) THEN fobj->SetValue, value1
   ENDIF
   IF N_Elements(value2) GT 0 THEN BEGIN
      fobj = self.compound->Get(name='Field2')
      IF Obj_Valid(fobj) THEN fobj->SetValue, value2
   ENDIF
   IF N_Elements(range) GT 0 THEN BEGIN
      IF N_Elements(range) NE 2 THEN BEGIN
         self->ErrorMessage, 'range must be a 2-element vector'
         RETURN
      ENDIF
      self->SetValue, range
   ENDIF

   ;; Set new labels
   IF N_Elements(label) GT 0 THEN BEGIN
      IF N_Elements(label) NE 2 THEN BEGIN
         self->ErrorMessage, 'Labeltext must be 2-element string vector'
         RETURN
      ENDIF 
      fobj = self.compound->Get(name='Field1')
      IF Obj_Valid(fobj) THEN fobj->SetProperty, labeltext=label[0]
      fobj = self.compound->Get(name='Field2')
      IF Obj_Valid(fobj) THEN fobj->SetProperty, labeltext=label[1]
   ENDIF

   ;; Set other properties do it for both objects
   IF N_Elements(extra) GT 0 THEN BEGIN
      fobj = self.compound->Get(name='Field1')
      IF Obj_Valid(fobj) THEN fobj->SetProperty, _Extra=extra
      fobj = self.compound->Get(name='Field2')
      IF Obj_Valid(fobj) THEN fobj->SetProperty, _Extra=extra
   ENDIF

   IF N_Elements(allow_wrap) GT 0 THEN BEGIN
      self.allow_wrap = Keyword_Set(allow_wrap)
   ENDIF

   ;; Make sure object is up-to-date and redisplay
   self->UpdateObject
   self->Show

END

; -----------------------------------------------------------------------------
; Cleanup:
;   It is not necessary to overwrite the generic GUI object cleanup
; method as no new pointers or objects are added to BaseGUI.

; -----------------------------------------------------------------------------
; Init:
;   This method initializes the range field object.
; The initial value of the range field must be given as a 2-element
; vector. If labeltext is a scalar string, an empty string will be
; used for the second label. All other options that are possible for
; MGS_Field are passed through the extra mechanism and will thus apply
; to both fields. 
; NOTE:
; (1) See the use of NotifyObject as an example for object
; communication. 

FUNCTION MGS_RangeField::Init, $
   value=value,     $  ; The initial range value
   labeltext=label, $  ; The text of the label(s)
   allow_wrap=allow_wrap, $ ; Allow "upper" value to be smaller than "lower" value
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
                                ; widget_defaultfont
                                ; widget_labelfont
                                ; no_frame


   ;; Initialize parent object
   IF not self->MGS_BaseGUI::Init(_Extra=extra) THEN RETURN, 0

   ;; Error Handler
;   Catch, theError
;   IF theError NE 0 THEN BEGIN
;      self->ErrorMessage, 'Error initialising object'
;      RETURN, 0
;   ENDIF

   ;; Check keywords and parameters.
   IF N_Elements(value) GT 0 THEN BEGIN
      thevalue = self->Validate(value)
   ENDIF ELSE BEGIN
      thevalue = [0., 1.]
   ENDELSE 
   IF N_Elements(label) GT 0 THEN BEGIN
      thelabel = label
      IF N_Elements(thelabel) EQ 1 THEN thelabel = [ thelabel, '' ]
   ENDIF ELSE BEGIN
      thelabel = [ 'Range:', '' ]
   ENDELSE 
 
   ;; Create compound widget objects (text fields)
   theObj = Obj_New('MGS_Field', $
                    labeltext=String(thelabel[0]), $
                    value=thevalue[0], $
                    /no_frame, $
                    notifyobject={object:self, method:'RANGECHECK'}, $
                    _Extra=extra)

   IF Obj_Valid(theObj) THEN self.compound->Add, theObj

   ;; Need to overwrite name property (_extra takes precedence over
   ;; name here!)
   theObj->SetProperty, name='Field1'

   theObj = Obj_New('MGS_Field', $
                    labeltext=String(thelabel[1]), $
                    value=thevalue[1], $
                    /no_frame, $
                    notifyobject={object:self, method:'RANGECHECK'}, $
                    _Extra=extra)

   IF Obj_Valid(theObj) THEN self.compound->Add, theObj
   theObj->SetProperty, name='Field2'

   ;; Store value in object
   self->SetValue, thevalue

   ;; Reset some of the generic default properties
   IF self.window_title EQ 'Generic widget object' THEN $
      self.window_title = 'Select Range:'

   ;; Set layout to row
   self.column_layout = 0

   self.allow_wrap = Keyword_Set(allow_wrap)

   RETURN, 1
END


; -----------------------------------------------------------------------------
; MGS_RangeField__Define:
; This is the object definition for the color picker object.
; It inherits from MGS_BaseGUI which provides the generic widget
; functionality, and from MGS_BaseObject the abilities to set and
; query an object name and a uvalue. The base object also provides a
; general method for display of error messages which can be directed
; to a message dialog or to the log screen via the no_dialog flag.

PRO MGS_RangeField__Define

   struct = { MGS_RangeField, $

              allow_wrap : 0,  $  ; Allow "upper" value to be smaller than "lower" value

              inherits MGS_BaseGUI  }

END


; -----------------------------------------------------------------------------
; Example:
;    Demonstrate the functionality of this object

PRO Example, range, block=block, object=object, _EXTRA=extra

   IF N_Elements(range) EQ 0 THEN range = [0.,1.]
   thegui = Obj_New('MGS_RangeField', value=range, _Extra=extra)
   IF Keyword_Set(block) THEN BEGIN
      buttons=['Cancel','Accept','Reset']
   ENDIF ELSE BEGIN
      buttons=['Close','Apply','Reset']
   ENDELSE 
   thegui->GUI, block=keyword_Set(block), buttons=buttons
   range = thegui->GetValue()
   print, 'New range : ',range
   IF Arg_Present(object) EQ 0 THEN Obj_Destroy, thegui ELSE object=thegui

END
