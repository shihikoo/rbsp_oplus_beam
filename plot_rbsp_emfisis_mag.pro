;+
; PROCEDURE: plot_rbsp_emfisis_mag
;
; PURPOSE: To load and plot RBSP/REPT L2 energy spectra from cdf files in:
;          http://emfisis.physics.uiowa.edu/Flight/RBSP-A/L3/
;          
; 
; SET TIME INTERVAL: Time interval is set using the tplot timespan procedure:
;                    Example: 
;                    timespan, '2013-01-01/00:00:00', 1, /D
;                    (keywords: SECONDS, MINUTES, HOURS, DAYS (DEFAULT))
;
; INPUT PARAMETERS: probe --> VAP id array ('A','B' | 'a', 'b' | 1,2)
;                   coordinates --> 'GSE' | 'GSM' | 'GEI' | 'GEO' | 'SM'
;                   resolution --> '1sec' | '4sec' (def) | 'hires'
;
; KEYWORDS: 
;           aux_data --> Load auxiliary data avalable in the cdf file
;           eph_data --> Load the ephemeris data from the cdf file
;           varname --> A string to overwite the default tplot_variable name
;           path --> A sting to overwite the default data path
;           fln --> A sting to overwite the default data file name
;
; NOTES:
; 
; CREATED BY: C Mouikis
;
; LAST MODIFICATION: 04/06/2015
;
; MODIFICATION HISTORY: 
;-
PRO plot_rbsp_emfisis_mag, $
  probe, $
  coordinates, $
  resolution=resolution, $
  eph_data=eph_data, $
  aux_data=aux_data, $
  new_name=new_name, $
  path=path, $
  fln=fln

  ; Reset common error message variables
  COMMON get_error, get_err_no, get_err_msg, default_verbose  
  get_err_no = 0 & get_err_msg=''

  ;>>--------------------------------------------------------------------
  ; Input checks
  ;----------------------------------------------------------------------
  ; Check if input parameters are within the allowed limits
  probe_str = STRARR(N_ELEMENTS(probe))
  FOR ipar = 0, N_ELEMENTS(probe)-1 DO BEGIN
    IF SIZE(probe(ipar), /TYPE) EQ 7 THEN BEGIN
      probe_str(ipar) = STRUPCASE(probe(ipar))
    ENDIF ELSE BEGIN
      IF probe(ipar) EQ 1 THEN probe_str(ipar) = 'A'
      IF probe(ipar) EQ 2 THEN probe_str(ipar) = 'B'
    ENDELSE
    IF probe_str(ipar) NE 'A' AND probe_str(ipar) NE  'B' THEN BEGIN
      PRINT, 'Allowed values for probe are: 1 or 2 or a or b or A or B '
    ENDIF
  ENDFOR
  ;<<--------------------------------------------------------------------

  IF NOT KEYWORD_SET(resolution) THEN resolution = '4sec'

  ;>>--------------------------------------------------------------------
  ; Loop over all probes
  ;----------------------------------------------------------------------
  FOR ii=0, N_ELEMENTS(probe)-1 DO BEGIN

    ; Reset error tracking
    get_err_no = 0 & get_err_msg=''

    ; Read the data for each probe/species combination
     IF KEYWORD_SET(new_name) THEN name = new_name ELSE name=''

    get_rbsp_emfisis_l3_mag, $
       probe_str(ii), $
       coordinates, $
       resolution, $
       trange=trange, $
       aux_data=aux_data, $
       eph_data=eph_data, $
       name=name, $
       path=path, $
       fln=fln

; Jing: add a check for 'data files not found for time interval' so the routine won't crash
    If ~keyword_set(name) THEN GOTO, next

    IF get_err_no GT 0 THEN GOTO, next   
  
    ; Set plot attributes
    get_data, name, dlim=dlim
  
    plot_vector, name, dlim.ytitle, 1, units=dlim.units, /polar, /mag_pressure


    next:
 ENDFOR
 ;<<--------------------------------------------------------------------
  
END
 