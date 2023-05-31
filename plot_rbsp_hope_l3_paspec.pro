;+
; PROCEDURE: plot_rbsp_hope_l3_paspec
;
; PURPOSE: To load and plot RBSP/HOPE L3 pitch angle spectra from cdf files in:
;           http://www.rbsp-ect.lanl.gov/data_pub/rbspa/hope/level3/pitchangle/
;           http://www.rbsp-ect.lanl.gov/data_pub/rbspb/hope/level3/pitchangle/
; 
; SET TIME INTERVAL: Time interval is set using the tplot timespan procedure:
;                    Example: 
;                    timespan, '2013-01-01/00:00:00', 1, /D
;                    (keywords: SECONDS, MINUTES, HOURS, DAYS (DEFAULT))
;
; INPUT PARAMETERS: probe:   VAP id array ('A','B' | 'a', 'b' | 1,2)
;                   species: 0: H+, 1: e-, 2: He+, 3: O+
;                   units:   Units for pitch angle spectra 
;                            'DIFF FLUX', 'EFLUX'
;
; KEYWORDS: ENERGY_RANGE --> min max energy for the pitch angle spectrum in eV
;           ENERGY_UNITS_IN_FLUX -->
;           aux_data --> Load auxiliary data avalable in the cdf file
;           eph_data --> Load the ephemeris data from the cdf file
;           NEW_NAME --> A string to overwite the default tplot_variable name
;           PATH --> A string to overwite the default data path
;           FLN --> A string to overwite the default data file name
;
; NOTES:
; 
; CREATED BY: C Mouikis
;
; LAST MODIFICATION: 03/06/2018
;
; MODIFICATION HISTORY: 
;-
PRO plot_rbsp_hope_l3_paspec, $
   probe, $
   species, $
   units, $
   energy_range=energy_range, $
   energy_bins_range=energy_bins_range, $
   energy_units_in_flux=energy_units_in_flux, $
   aux_data=aux_data, $
   eph_data=eph_data, $
   new_name=new_name, $
   path=path, $
   fln=fln

                                ; Reset common error message variables
  COMMON get_error, get_err_no, get_err_msg, default_verbose
  get_err_no = 0 & get_err_msg=''
  
                                ;>>--------------------------------------------------------------------
                                ; Input checks
                                ;----------------------------------------------------------------------
                                ; Check if probe and species arrays have the same number of elements
  IF N_ELEMENTS(probe) NE N_ELEMENTS(species) THEN BEGIN
     PRINT, 'The probe array and the species array MUST have'
     PRINT, 'the same number of elements.'
     stop
  ENDIF
  
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

     IF species(ipar) LT 0 OR species(ipar) GT 3 THEN BEGIN
        PRINT, 'Allowed values for species are: 0, 1, 3 or 3'
        STOP
     ENDIF
  ENDFOR
                                ;<<--------------------------------------------------------------------

                                ; Set variables
  species_str = ['H!U+!N','e!U-!N','He!U+!N','O!U+!N']
  IF ~KEYWORD_SET(energy_range) THEN energy_range = [1., 60000.]
  IF ~KEYWORD_SET(energy_units_in_flux) THEN energy_units_in_flux = 'keV'

; >>--------------------------------------------------------------------
; Loop over all probe/species combinations
;----------------------------------------------------------------------
  FOR ii = 0, N_ELEMENTS(species)-1 DO BEGIN
     
; Reset error tracking
     get_err_no = 0 & get_err_msg=''

; Read the data for each probe/species combination
     IF KEYWORD_SET(new_name) THEN name = new_name ELSE name=''
     get_rbsp_hope_l3_paspec, $
        probe_str(ii), $
        species(ii), $
     ;   units, $
        energy_range, $
   ;     energy_units_in_flux = energy_units_in_flux, $
        energy_bins_range=energy_bins_range, $
        aux_data=aux_data, $
        eph_data=eph_data, $
        name = name, $
        path=path, $
        fln=fln

     energy_range_string = STRING(energy_range[0], FORMAT='(i5.5)') + $
                           '-' + STRING(energy_range[1], FORMAT='(i5.5)')
     options, name, 'ytitle', $
              'PROBE ' + STRUPCASE(probe_str(ii)) + '!C!C' + $
              species_str(species(ii))+' (eV)' + '!C!C' + energy_range_string
     
     uname = ''
     IF units EQ 'DIFF FLUX' THEN BEGIN
        IF energy_units_in_flux EQ 'eV' THEN BEGIN
           uname = '1/cm!E2!N-s-sr-(eV/e)'
        ENDIF ELSE BEGIN
           uname = '1/cm!E2!N-s-sr-(keV/e)'
        ENDELSE
     ENDIF
     
     IF units EQ 'EFLUX' THEN BEGIN
        IF energy_units_in_flux EQ 'eV' THEN BEGIN
           uname = 'eV/cm!E2!N-s-sr-(eV/e)'
        ENDIF ELSE BEGIN
           uname = 'keV/cm!E2!N-s-sr-(keV/e)'
        ENDELSE
     ENDIF
     
     options, name, 'spec',1
     options, name, 'x_no_interp',1
     options, name, 'y_no_interp',1
     
     options, name, 'ztitle', uname
     
     IF units EQ 'DIFF FLUX' AND energy_units_in_flux EQ 'eV' THEN BEGIN
        CASE species(ii) OF
           0: zlim, name,  1.e+0, 1.e4, 1
           1: zlim, name,  1.e+2, 1.e6, 1
           2: zlim, name,  1.e-1, 1.e3, 1
           3: zlim, name,  1.e+0, 1.e4, 1          
        ENDCASE
     ENDIF ELSE BEGIN
        CASE species(ii) OF
           0: zlim, name,  1.e+3, 1.e7, 1
           1: zlim, name,  1.e+5, 1.e9, 1
           2: zlim, name,  1.e+2, 1.e6, 1
           3: zlim, name,  1.e+3, 1.e7, 1
        ENDCASE
     ENDELSE
     
     next:
  ENDFOR
                                ;<<--------------------------------------------------------------------
  
END
