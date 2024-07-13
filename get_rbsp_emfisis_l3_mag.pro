;+
; PROCEDURE:	read_cdf_hope_ions_spectra
;
; PURPOSE: To read the EMFISIS magnetic field data
;
; INPUTS: probe_str --> probe (a | b)
;         coordinates --> 'GSE' | 'GSM' | 'GEI' | 'GEO' | 'SM'
;         resolution --> '1sec' | '4sec' (def) | 'hires'
;
; KEYWORDS: 
;   time_range --> 2 element vector specifying the time range.
;   aux_data --> Set to store the auxiliary data
;   eph_data --> set to store the ephemeris data
;   new_name --> tplot variable name (string) to overwite the default name
;   path --> path (string) to overwrite default path
;   fln --> filename (string) to overwrite default file name
;
; CREATED BY:	C. Mouikis 
;
; MODIFICATION HISTORY:
;
;-
PRO get_rbsp_emfisis_l3_mag, $
  probe_str, $
  coordinates, $
  resolution, $
  trange=trange, $
  aux_data=aux_data, $
  eph_data=eph_data, $
  name=name, $
  path=path, $
  fln=fln

  COMMON get_error, get_err_no, get_err_msg, default_verbose  

  ;>>--------------------------------------------------------------------
  ; Find data files to read
  ;----------------------------------------------------------------------
  ; Get data path
  IF ~KEYWORD_SET(path) THEN path2 ='' ELSE path2 = path
  IF path2 EQ '' THEN path2 = GETENV('RBSP_' + probe_str + '_EMFISIS_L3_MAG')

  ; Find data file with manually entered filename
  IF NOT KEYWORD_SET(fln)  THEN BEGIN
    fln2 =''
  ENDIF ELSE BEGIN
    fln2 = fln
    files_found = FILE_SEARCH(path2 + '/' + fln2, count=ifln)
    IF ifln LE 0 THEN BEGIN
      get_err_no = 1
      get_err_msg = 'File ' + fln + ' not found'
      MESSAGE, get_err_msg, /CONTINUE
      RETURN
    ENDIF
  ENDELSE

  ; Find data files. 
  ; Filenames are reconstructed using the timespan set 
  IF fln2 EQ '' THEN BEGIN

    ; Find days that correspond to time interval selected
    get_timespan, time_interval

    t_s=gettime(time_interval(0)) ; start time in tplot-time
    t_e=gettime(time_interval(1)) ; end time in tplot-time  
  
    t_s_str = time_struct(t_s)    ; start_time tplot time structure
    t_e_str = time_struct(t_e)    ; end_time tplot time structure
  
    mjd_s = JULDAY(t_s_str.month, t_s_str.date, t_s_str.year) ; start julian day
    mjd_e = JULDAY(t_e_str.month, t_e_str.date, t_e_str.year) ; end julian day
  
    ndys = (mjd_e - mjd_s) + 1 ; number of days to be loaded

    ;Last day is not included if hour=min=sec=0
    IF t_e_str.hour EQ 0 AND t_e_str.min EQ 0 AND t_e_str.sec EQ 0 THEN $
      ndys = ndys - 1
    
    ; Reconstruct date strings and search for the corresponding files 
    files_found = ['']
    FOR indys = 0, ndys-1 DO BEGIN

      date = time_double(STRMID(time_string(time_interval(0)), 0, 4) + $
                          STRMID(time_string(time_interval(0)), 5, 2) + $
                          STRMID(time_string(time_interval(0)), 8, 2)) + $
                          indys * 86400.

      year_str = STRMID(time_string(date), 0, 4)
      month_str = STRMID(time_string(date), 5, 2)
      day_str = STRMID(time_string(date), 8, 2)
      date_str =  year_str + month_str + day_str
                  
      fln2 = 'rbsp-' + STRLOWCASE(probe_str) + '_magnetometer_' + resolution + '-' + STRLOWCASE(coordinates) + '_emfisis-*3_' + date_str + '*.cdf'

      path_fln = FILE_SEARCH(path2 + '/' + fln2, count=ifln)
      
      ; If more than one files for the same date are found the last one
      ; in the list is selected. It is assumed that this will be the most
      ; recent one.
      IF ifln GT 0 THEN BEGIN
        files_found = [files_found, path_fln(ifln-1)]
      ENDIF ELSE BEGIN
        print, 'data files not found for time interval'
	return
        
        serverdir='http://emfisis.physics.uiowa.edu/Flight/RBSP-' + STRUPCASE(probe_str) + '/L3/' + $
          year_str + '/' + month_str + '/' + day_str + '/'

         fln2 = plato_file_retreive(pathnames=fln2, $
                                    serverdir=serverdir, $
                                    localdir=GETENV('PLATO_LOCAL_DATA') + '/rbsp-' + STRLOWCASE(probe_str) + '/emf/L3/', $
                                    error=error)

         IF N_ELEMENTS(fln2) EQ 1 AND fln2(0) NE '' THEN files_found = [files_found, fln2(0)]

      ENDELSE
    ENDFOR

    IF N_ELEMENTS(files_found)-1 EQ 0 THEN BEGIN

      get_err_no = 1
      get_err_msg = 'Data files not found for time interval'
      MESSAGE, get_err_msg, /CONTINUE
      RETURN
    ENDIF ELSE BEGIN
      files_found = files_found(1:N_ELEMENTS(files_found)-1)
    ENDELSE
      
  ENDIF
  ;<<--------------------------------------------------------------------

  ;>>--------------------------------------------------------------------
  ; Open and read CDF files
  ;----------------------------------------------------------------------
  ; Determine the variables to be read from the CDF files
  var2get=['EPOCH', 'MAG']
  IF KEYWORD_SET(eph_data) THEN var2get = [var2get, ['COORDINATES']]
  IF KEYWORD_SET(aux_data) THEN var2get = [var2get, ['','']]

  IF not keyword_set(trange) THEN get_timespan,tr else tr=time_double(trange)
  IF n_elements(tr) EQ 1 THEN tr = [tr, tr+86399d0]

  ; Loop through the CDF files found and read the data
  append_flag = 0
  FOR iday = 0, N_ELEMENTS(files_found)-1 DO BEGIN

    ds = get_cdf_data(file=files_found(iday), var2get=var2get)

    t_mag = REFORM(ds.epoch.data)
    time = time_double(t_mag, /TT200)

    mag_data = TRANSPOSE(ds.mag.data)
    IF KEYWORD_SET(eph_data) THEN BEGIN
      pos_data = TRANSPOSE(ds.coordinates.data)
    ENDIF
    IF KEYWORD_SET(eph_data) THEN BEGIN

    ENDIF

    ; Limit data arrays to time interval requested
    IF KEYWORD_SET(trange) THEN BEGIN
      get_timespan, tt
      itime = WHERE(time GE tt(0) AND time LE tt(1), c_itime)
      IF c_itime LE 1 THEN BEGIN
        get_err_no = 1
        get_err_msg = 'Less than 2 data points found for time interval'
        MESSAGE, get_err_msg, /CONTINUE
        RETURN
      ENDIF

      mag_data = mag_data(*,itime)
      IF KEYWORD_SET(eph_data) THEN BEGIN
        pos_data = pos_data(*,itime)
      ENDIF
    ENDIF

    IF append_flag EQ 0 THEN BEGIN
      data_x = time
      data_y = mag_data
      IF KEYWORD_SET(eph_data) THEN BEGIN
        data_pos = pos_data
      ENDIF
      IF KEYWORD_SET(aux_data) THEN BEGIN

      ENDIF

      append_flag = 1
    ENDIF ELSE BEGIN
      data_x = [data_x, time]
      data_y = [data_y, mag_data]
      IF KEYWORD_SET(eph_data) THEN BEGIN
        data_pos = [data_pos, pos_data]
      ENDIF
      IF KEYWORD_SET(aux_data) THEN BEGIN

      ENDIF
    ENDELSE

  ENDFOR
  ;<<--------------------------------------------------------------------

  ;>>--------------------------------------------------------------------
  ; Create tplot variables
  ;----------------------------------------------------------------------
  IF NOT KEYWORD_SET(name) THEN BEGIN
    name = 'RBSP' + probe_str + $
              '_EMFISIS_L3_MAG' + $
              '_' + STRUPCASE(resolution) + $
              '_' + STRUPCASE(coordinates)
  ENDIF

  store_data, name, $
              data={x:data_x, y:data_y}, $
              dlim={panel_size:2, ylog:0, ytitle:'B ' + STRUPCASE(coordinates), units:ds.mag.units}

  IF KEYWORD_SET(eph_data) THEN BEGIN
    store_data, name + '_POS', data = {x:data_x, y:data_pos / 6371.0}, $
     dlim={panel_size:2, ytitle:'Pos'}
    store_data, name + '_DIST', data = {x:data_x, $
      y:SQRT(data_pos(*,0)^2 + data_pos(*,1)^2 + data_pos(*,2)^2) / 6371.0}, $
     dlim={panel_size:2, ytitle:'Dist'}
  ENDIF

  IF KEYWORD_SET(aux_data) THEN BEGIN
    
  ENDIF
  ;<<--------------------------------------------------------------------
    
END

