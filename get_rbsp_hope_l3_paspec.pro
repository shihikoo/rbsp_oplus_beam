;+
; NAME: get_hope_l3_pa
;
; PURPOSE: To read the HOPE-L3_PA cdf files and reconstruct the CODIF 
;          equivalent 3D distribution that can be fed into the CODIF routines
;          in order to produce 3D based products
;
; INPUT: sat --> 1 or 2 for probes a or b
;
; KEYWORDS:
;        species --> 0,1,2 or 3 for species H+, e-, He+, O+
;        name --> 
;        distribution --> 'Original' : Use the original, pitch angle gyrophase resolution
;                         'Gyrotropic' ; Modify original distribution to reflect gyrotropy
;                         'Isotropic' ; Modify original distribution to reflect isotropy
;        path --> Data file path if not the one defined by the RBSP_A_HOPE_PA or 
;                 RBSP_B_HOPE_PA environment variables
;        fln --> Filename if not the default
;        pa_weighting --> Apply pitch angle weighting (for testing purposes)
;
; CREATED by: C.G. Mouikis (11/2013)
;
; MODIFICATION HISTORY:
;
; FILE SOURCE: http://www.rbsp-ect.lanl.gov/data_pub/rbspa/hope/level3/pitchangle/ or
;              http://www.rbsp-ect.lanl.gov/data_pub/rbspb/hope/level3/pitchangle/
;
; ATTENTION: 
;            - The file name specification is: 'rbsp' + probe_str + '_ect-hope-PA*-L3_' + date + '*.cdf'
;            which makes does not restrict which file version (3D or 4D) is selected. So, if both file types 
;            for a particular date exist in the sape directory, it becomes random which one is chosen. This
;            can be done by specifying the file name using the "fln" keyword
;
;            - Also, if two data files exist in the path directory with different version numbers the one with 
;            the higher version number is selected
;
; CATEGORY: 
;           RBSP, HOPE, Core
;
; @author Chris Mouikis
;
;-
PRO get_rbsp_hope_l3_paspec, $
  probe_str, $
  species, $
  energy_range, $
  energy_bins_range=energy_bins_range, $
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
  IF path2 EQ '' THEN path2 = GETENV('RBSP_' + probe_str + '_HOPE_PA')

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
    files_found = ''
    FOR indys = 0, ndys-1 DO BEGIN

      date = time_double(STRMID(time_string(time_interval(0)), 0, 4) + $
                          STRMID(time_string(time_interval(0)), 5, 2) + $
                          STRMID(time_string(time_interval(0)), 8, 2)) + $
                          indys * 86400.

      date_str = STRMID(time_string(date), 0, 4) + $
                  STRMID(time_string(date), 5, 2) + $
                  STRMID(time_string(date), 8, 2)

      fln2 = 'rbsp' + STRLOWCASE(probe_str) + '*_ect-hope-*_' + date_str + '*.cdf'

      path_fln = FILE_SEARCH(path2 + '/' + fln2, count=ifln)
      ; If more than one files for the same date are found the last one
      ; in the list is selected. It is assumed that this will be the most
      ; recent one if the version numbers are properly incremented.
      IF ifln GT 0 THEN BEGIN
        files_found = [files_found, path_fln(ifln-1)]
      ENDIF
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
  CASE species OF
     0: BEGIN
          var2get = ['FPDU',  'Epoch_Ion', 'PITCH_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
          IF KEYWORD_SET(eph_data) THEN BEGIN
            var2get = [var2get, ['POSITION_ION', 'L_ION', 'L_STAR_ION', 'MLT_ION']]
          ENDIF
          IF KEYWORD_SET(aux_data) THEN BEGIN
            var2get = [var2get, ['B_CALC_ION', 'B_EQ_ION', 'I_ION']]
          ENDIF
        END
     1: BEGIN
          var2get = ['FEDU',  'Epoch_Ele', 'PITCH_ANGLE', 'HOPE_ENERGY_Ele', 'ENERGY_Ele_DELTA']
          IF KEYWORD_SET(eph_data) THEN BEGIN
            var2get = [var2get, 'Position_Ele', 'L_Ele', 'L_star_Ele', 'MLT_Ele']
          ENDIF
          IF KEYWORD_SET(aux_data) THEN BEGIN
            var2get = [var2get, ['B_CALC_ELE', 'B_EQ_ELE', 'I_ELE']]
          ENDIF
        END
     2: BEGIN
          var2get = ['FHEDU', 'Epoch_Ion', 'PITCH_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
          IF KEYWORD_SET(eph_data) THEN BEGIN
            var2get = [var2get, 'Position_Ion', 'L_Ion', 'L_star_Ion', 'MLT_Ion']
          ENDIF
          IF KEYWORD_SET(aux_data) THEN BEGIN
            var2get = [var2get, ['B_CALC_ION', 'B_EQ_ION', 'I_ION']]
          ENDIF
        END
     3: BEGIN
          var2get = ['FODU',  'Epoch_Ion', 'PITCH_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
          IF KEYWORD_SET(eph_data) THEN BEGIN
            var2get = [var2get, 'Position_Ion', 'L_Ion', 'L_star_Ion', 'MLT_Ion']
          ENDIF
          IF KEYWORD_SET(aux_data) THEN BEGIN
            var2get = [var2get, ['B_CALC_ION', 'B_EQ_ION', 'I_ION']]
          ENDIF
        END
  ENDCASE

  ; Loop through the CDF files found and read the data
  append_flag = 0
  FOR iday = 0, N_ELEMENTS(files_found)-1 DO BEGIN ; Loop over daily files
  
    ds = get_cdf_data(file=files_found(iday) , var2get=var2get)

  ; -------------------------------------------------------------------
  ; Get data
  ; -------------------------------------------------------------------
    ; Read Epoch Time
    IF species EQ 1 THEN BEGIN ; epoch time
      t_ele = REFORM(ds.epoch_ele.data)
      t_str = time_double(t_ele, /EPOCH)
    ENDIF ELSE BEGIN
      t_ion = REFORM(ds.epoch_ion.data)
      t_str = time_double(t_ion, /EPOCH)
    ENDELSE ; epoch time

    ; Limit data arrays to time interval requested
    get_timespan, tt
    itime = WHERE(t_str GE tt(0) AND t_str LE tt(1), c_itime)
    IF c_itime LE 1 THEN BEGIN
      get_err_no = 1
      get_err_msg = 'Less than 2 data points found for time interval'
      MESSAGE, get_err_msg, /CONTINUE
      RETURN
    ENDIF

    time = t_str[itime]

    ; Set energy and pa arrays
    pa_data = ds.pitch_angle.data
    IF species EQ 1 THEN BEGIN
      en_data = REVERSE(ds.hope_energy_ele.data[*,itime])
      en_delta_data = REVERSE(ds.energy_ele_delta.data[*,itime])
    ENDIF ELSE BEGIN
      en_data = REVERSE(ds.hope_energy_ion.data(*,itime))
      en_delta_data = REVERSE(ds.energy_ion_delta.data[*,itime])
    ENDELSE
      
    e_data = en_data

    CASE species OF
      0: BEGIN
        data_or = REVERSE(ds.fpdu.data[*,*,itime],1)
      END
      1: BEGIN
        data_or = REVERSE(ds.fedu.data[*,*,itime],1)
      END
      2: BEGIN
        data_or = REVERSE(ds.fhedu.data[*,*,itime],1)
      END
      3: BEGIN
        data_or = REVERSE(ds.fodu.data[*,*,itime],1)
      END
    ENDCASE

    fill_index = WHERE(data_or EQ -1.00000e+31)
    data_or[fill_index] = !values.f_nan

    en_data_middle = REFORM(en_data(*,0))
    en_data_l = REFORM(en_data(*,0)) - (REFORM(en_delta_data(*,0)) / 2.)
    en_data_u = REFORM(en_data(*,0)) + (REFORM(en_delta_data(*,0)) / 2.)

    IF KEYWORD_SET(energy_bins_range) THEN BEGIN
      IF energy_bins_range[0] LE energy_bins_range[1] THEN BEGIN
        en_ind_array = INDGEN(energy_bins_range[1] - energy_bins_range[0] + 1) + energy_bins_range[0]
      ENDIF ELSE BEGIN
        en_ind_array = INDGEN(energy_bins_range[0] - energy_bins_range[1] + 1) + energy_bins_range[1]
      ENDELSE
      energy_range[0] = en_data_middle(en_ind_array[N_ELEMENTS(en_ind_array)-1])
      energy_range[1] = en_data_middle(en_ind_array[0])
    ENDIF ELSE BEGIN
      en_start = MIN(ABS(en_data_l - energy_range(0)), en_index_l)
      en_end   = MIN(ABS(en_data_u - energy_range(1)), en_index_u)
      en_ind_array = INDGEN((en_index_l - en_index_u + 1)) + en_index_u
      energy_range[0] = en_data_middle(en_ind_array[N_ELEMENTS(en_ind_array)-1])
      energy_range[1] = en_data_middle(en_ind_array[0])
    ENDELSE

;Jing
;    data_y = data_or
;    data_y = REFORM(MEAN(data_y(en_ind_array,*,*), DIM=1, /NaN))
     daty_avg_int = data_or
     daty_avg_int = REFORM(MEAN( daty_avg_int(en_ind_array,*,*), DIM=1, /NaN))
     ifill = WHERE( daty_avg_int EQ -1.00000e+31, c_ifill) 
     IF c_ifill GT 0 THEN  daty_avg_int(ifill) = !VALUES.F_NAN     
     
;    ifill = WHERE(data_y EQ -1.00000e+31, c_ifill) 
;    IF c_ifill GT 0 THEN BEGIN
;       data_y(ifill) = !VALUES.F_NAN
;    ENDIF

    ; Get ephemeris parameters
    IF KEYWORD_SET(eph_data) THEN BEGIN
      IF SPECIES EQ 1 THEN BEGIN
        dist_data = SQRT(REFORM(ds.position_ele.data(0,*)^2) + $
            REFORM(ds.position_ele.data(1,*)^2) + $
            REFORM(ds.position_ele.data(2,*)^2)) / 6371.0
        l_data = REFORM(ds.l_ele.data)
        l_star_data = REFORM(ds.l_star_ele.data)
        mlt_data = REFORM(ds.mlt_ele.data)
      ENDIF ELSE BEGIN
        dist_data = SQRT(REFORM(ds.position_ion.data(0,*)^2) + $
            REFORM(ds.position_ion.data(1,*)^2) + $
            REFORM(ds.position_ion.data(2,*)^2)) / 6371.0
        l_data = REFORM(ds.l_ion.data)
        l_star_data = REFORM(ds.l_star_ion.data)
        mlt_data = REFORM(ds.mlt_ion.data)
      ENDELSE
    ENDIF

    ; Get auxiliary parameters
    IF KEYWORD_SET(aux_data) THEN BEGIN
      IF SPECIES EQ 1 THEN BEGIN
        b_calc_data = REFORM(ds.b_calc_ele.data)
        b_eq_data = REFORM(ds.b_eq_ele.data)
        i_data = REFORM(ds.i_ele.data)
      ENDIF ELSE BEGIN
        b_calc_data = REFORM(ds.b_calc_ion.data)
        b_eq_data = REFORM(ds.b_eq_ion.data)
        i_data = REFORM(ds.i_ion.data)
      ENDELSE
    ENDIF
; Jing
;    daty_avg_int = data_y
    IF append_flag EQ 0 THEN BEGIN ; append_flag
      data_x = t_str
      data_y = TRANSPOSE(daty_avg_int)
      data_v = TRANSPOSE(e_data)
      IF KEYWORD_SET(eph_data) THEN BEGIN
        data_dist = dist_data
        data_L = l_data
        data_l_star = l_star_data
        data_mlt = mlt_data
      ENDIF
      IF KEYWORD_SET(aux_data) THEN BEGIN
        data_b_calc = b_calc_data
        data_b_eq = b_eq_data
        data_i = i_data
      ENDIF

      append_flag = 1
    ENDIF ELSE BEGIN
        data_x  = [data_x, t_str]
        data_y  = [data_y, TRANSPOSE(daty_avg_int)]
        data_v  = [data_v, TRANSPOSE(e_data)]
        IF KEYWORD_SET(eph_data) THEN BEGIN
        data_dist   = [data_dist, dist_data]
        data_l      = [data_l, l_data]
        data_l_star = [data_l_star, l_star_data]
        data_mlt    = [data_mlt, mlt_data]
      ENDIF
      IF KEYWORD_SET(aux_data) THEN BEGIN
        data_b_calc = [data_b_calc, b_calc_data]
        data_b_eq   = [data_b_eq, b_eq_data]
        data_i      = [data_i, i_data]
      ENDIF
    ENDELSE ; append_flag

  ENDFOR ; Loop over daily files

  ifill = WHERE(data_y EQ -1.00000e+31, c_ifill) 
  IF c_ifill GT 0 THEN BEGIN
     data_y(ifill) = !VALUES.F_NAN
  ENDIF

  ;>>--------------------------------------------------------------------
  ; Create tplot variables
  ;----------------------------------------------------------------------
  species_str = ['H!U+!N', 'e!U-!N', 'He!U+!N', 'O!U+!N']

  flux_units = 'DIFF FLUX'
  IF ~KEYWORD_SET(name) THEN BEGIN
    name = 'RBSP' + probe_str + $
           '_HOPE_l3_paspec_sp' + STRING(species, FORMAT='(i1.1)') + $
           '_' + STRUPCASE(STRCOMPRESS(flux_units,/REMOVE_ALL)) + $
           '_' + STRING(energy_range(0), FORMAT='(i5.5)') + '_' + STRING(energy_range(1), FORMAT='(i5.5)')
  ENDIF

  store_data, name, $
      data={x:data_x, y:data_y, v:pa_data}, $
      dlim={spec:1, no_interp:1, zlog:1, ylog:0}

  IF KEYWORD_SET(eph_data) THEN BEGIN
    store_data, name + '_DIST', data = {x:data_x, y:data_dist}, $
     dlim={panel_size:1, ytitle:'Dist'}
    store_data, name + '_L', data = {x:data_x, y:data_l}, $
     dlim={panel_size:1, ytitle:'L'}
    store_data, name + '_L_STAR', data = {x:data_x, y:data_l_star}, $
     dlim={panel_size:1, ytitle:'L*'}
    store_data, name + '_MLT', data = {x:data_x, y:data_mlt}, $
     dlim={panel_size:1, ytitle:'MLT'}

    ylim, name + '_DIST', 0, 8
    ylim, name + '_L', 0, 8
    ylim, name + '_L_STAR', 0, 8
    ylim, name + '_MLT', 0, 24
  ENDIF

  IF KEYWORD_SET(aux_data) THEN BEGIN
    store_data, name + '_B_CALC', data = {x:data_x, y:data_b_calc}, $
     dlim={panel_size:1, ytitle:'B model'}
    store_data, name + '_B_EQ', data = {x:data_x, y:data_b_eq}, $
     dlim={panel_size:1, ytitle:'B eq model'}
    store_data, name + '_I', data = {x:data_x, y:data_i}, $
     dlim={panel_size:1, ytitle:'I (bounce)'}

    ylim, name + '_B_CALC', 0, 400
    ylim, name + '_B_EQ', 0, 400
    ylim, name + '_I', 0, 5
  ENDIF

  IF KEYWORD_SET(eph_data) THEN BEGIN
    store_data, name + '_DIST', data = {x:data_x, y:data_dist}, $
     dlim={panel_size:1, ytitle:'Dist'}
    store_data, name + '_L', data = {x:data_x, y:data_l}, $
     dlim={panel_size:1, ytitle:'L'}
    store_data, name + '_L_STAR', data = {x:data_x, y:data_l_star}, $
     dlim={panel_size:1, ytitle:'L*'}
    store_data, name + '_MLT', data = {x:data_x, y:data_mlt}, $
     dlim={panel_size:1, ytitle:'MLT'}

    ylim, name + '_DIST', 0, 8
    ylim, name + '_L', 0, 8
    ylim, name + '_L_STAR', 0, 8
    ylim, name + '_MLT', 0, 24
  ENDIF

  IF KEYWORD_SET(aux_data) THEN BEGIN
    store_data, name + '_B_CALC', data = {x:data_x, y:data_b_calc}, $
     dlim={panel_size:1, ytitle:'B model'}
    store_data, name + '_B_EQ', data = {x:data_x, y:data_b_eq}, $
     dlim={panel_size:1, ytitle:'B eq model'}
    store_data, name + '_I', data = {x:data_x, y:data_i}, $
     dlim={panel_size:1, ytitle:'I (bounce)'}

    ylim, name + '_B_CALC', 0, 400
    ylim, name + '_B_EQ', 0, 400
    ylim, name + '_I', 0, 5
  ENDIF
  ;<<--------------------------------------------------------------------

END
