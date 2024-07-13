;+
; NAME: get_hope_3d
;
; PURPOSE: To read the HOPE-L3_PA cdf files and reconstruct the CODIF 
;          equivalent 3D distribution that can be fed into the CODIF routines
;          in order to produce 3D based products
;
; INPUT: sat --> 1 or 2 for probes a or b
;
; KEYWORDS:
;        specie --> 0,1,2 or 3 for species H+, e-, He+, O+
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
;  (02/26/2014) - Originaly the routine was written to use the 4D (energy, gyrophase, pitch angle, time)
;                 data HOPE-L3-PA data. This modification allows the use of 3D (energy, pitch angle, time)
;                 data files as well.
;  (03/06/2014) - Bug fix. The routine would not handle intervals that span over more than ones days.
;
; FILE SOURCE: http://www.rbsp-ect.lanl.gov/data_pub/rbspa/hope/level3/ or
;              http://www.rbsp-ect.lanl.gov/data_pub/rbspb/hope/level3/ 
;
; ATTENTION: 
;            - The file name specification is: 'rbsp' + probe_str(sat-1) + '_ect-hope-PA*-L3_' + date + '*.cdf'
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


; Sort out the data with gyrophase included
;
PRO sort_4d_data, t_str, data_or, en_data, en_delta_data, var2get, specie, distribution, retdata

  data_or = TRANSPOSE(data_or)
  en_data = TRANSPOSE(en_data)
  en_delta_data = TRANSPOSE(en_delta_data)

  d_size = SIZE(data_or)
  nenergy = d_size(1)
  nphi    = d_size(2)
  ntheta  = d_size(3)
  ntime   = d_size(4)
  nangles = ntheta * nphi
  ndatapoints = N_ELEMENTS(t_str)

  get_hope_angles, nangles, theta, phi, dtheta, dphi, pa_weight

  th     = REPLICATE(1, 1, nangles) * theta
  theta  = REBIN(th, nenergy, nangles)
  ph     = REPLICATE(1, 1, nangles) * phi
  phi    = REBIN(ph, nenergy, nangles)

  dth     = REPLICATE(1, 1, nangles) * dtheta
  dtheta  = REBIN(dth, nenergy, nangles)
  dph     = REPLICATE(1, 1, nangles) * dphi
  dphi    = REBIN(dph, nenergy, nangles)

  paw       = REPLICATE(1, 1, nangles) * pa_weight
  pa_weight = REBIN(paw, nenergy, nangles)

  data        = FLTARR(nenergy,nangles,ndatapoints)
  energy_arr  = FLTARR(nenergy,nangles,ndatapoints)
  denergy_arr = FLTARR(nenergy,nangles,ndatapoints)

  IF NOT KEYWORD_SET(PA_WEIGHTING) THEN pa_weight(*) = 1.0

  FOR itheta = 0, ntheta-1 DO BEGIN
     FOR iphi = 0, nphi-1 DO BEGIN
        iangle = (itheta * nphi) + iphi

        CASE distribution OF
           'Original': BEGIN
              data(*,iangle,*) = data_or(*,iphi,itheta,*) * pa_weight(0,iangle)
           END
           'Gyrotropic': BEGIN
              data(*,iangle,*) = MEAN(data_or(*,*,itheta,*) * pa_weight(0,iangle), DIMENSION=2, /NaN)
           END

           'Isotropic': BEGIN
              IF itheta EQ 0 AND iphi EQ 0 THEN BEGIN
                 isotropic_flux = MEAN((MEAN(data_or(*,*,*,*) * pa_weight(iangle), DIMENSION=2, /NaN)), DIMENSION=2, /NaN)
              ENDIF
              data(*,iangle,*) = isotropic_flux
           END
        ENDCASE

        energy_arr(*,iangle,*) = en_data
        denergy_arr(*,iangle,*) = 2.*en_delta_data

     ENDFOR
  ENDFOR

  specie_str = ['H!U+!N', 'e!U-!N', 'He!U+!N', 'O!U+!N']
  mass = [0.0104389, 0.0208778, 0.0417556, 0.167022]

  sensitivity = REPLICATE(1, ndatapoints)
  k1 = REPLICATE(255, ndatapoints)
  k2 = REPLICATE(255, ndatapoints)

  retdata = {                                                               $
            project_name:        'RBSP HOPE',                               $
            data_name:           var2get(0),                                $
            species:             specie_str(specie),                        $
            units_name:          'DIFF FLUX',                               $
            units_procedure:     '',                                        $
            valid:               1,                                         $
            time:                t_str,                                     $
            end_time:            t_str,                                     $
            delta_t:             22.7,                                      $
            integ_t:             22.7,                                      $
            geom_factor:         1.,                                        $
            nenergy:             nenergy,                                   $
            nbins:               nangles,                                   $
            bins:                REPLICATE(1b, nenergy, nangles),           $
            energy:              energy_arr,                                $
            denergy:             denergy_arr,                               $
            theta:               theta,                                     $
            phi:                 phi,                                       $
            dtheta:              dtheta,                                    $
            dphi:                dphi,                                      $
            k1:                  k1,                                        $
            k2:                  k2,                                        $
            data:                data,                                      $
            mass:                mass(specie),                             $
;              scale:               fltarr(nenergy,nangles),                   $
;              pac:                 fix(cal_struct1.post_accel_volt),          $
;              anode_effic:         cal_struct1.anode_effic_coeff_table,       $
;              onboard_anode_effic:cal_struct1.onboard_anode_effic_coeff_table,$
;              absol_effic:        cal_struct2.absolute_efficiencies,          $
;              phase_inst:         basic_data.header(ind_s:ind_e).phase_instr, $
              sensitivity:        sensitivity                                $
;              op_mode:            op_mode,                                    $
;              phase_instr:        basic_data.header(ind_s:ind_e).phase_instr  $
            }

END

; Sort out the data without gyrophase included
;
PRO sort_3d_data, t_str, data_or, en_data, en_delta_data, var2get, specie, distribution, retdata

  data_or = TRANSPOSE(data_or)
  en_data = TRANSPOSE(en_data)
  en_delta_data = TRANSPOSE(en_delta_data)

  d_size = SIZE(data_or)
  nenergy = d_size(1)
  nphi    = 20
  ntheta  = d_size(2)
  ntime   = d_size(3)
  nangles = ntheta * nphi
  ndatapoints = N_ELEMENTS(t_str)

  get_hope_angles, nangles, theta, phi, dtheta, dphi, pa_weight

  th     = REPLICATE(1, 1, nangles) * theta
  theta  = REBIN(th, nenergy, nangles)
  ph     = REPLICATE(1, 1, nangles) * phi
  phi    = REBIN(ph, nenergy, nangles)

  dth     = REPLICATE(1, 1, nangles) * dtheta
  dtheta  = REBIN(dth, nenergy, nangles)
  dph     = REPLICATE(1, 1, nangles) * dphi
  dphi    = REBIN(dph, nenergy, nangles)

  paw       = REPLICATE(1, 1, nangles) * pa_weight
  pa_weight = REBIN(paw, nenergy, nangles)

  data        = FLTARR(nenergy,nangles,ndatapoints)
  energy_arr  = FLTARR(nenergy,nangles,ndatapoints)
  denergy_arr = FLTARR(nenergy,nangles,ndatapoints)

  IF NOT KEYWORD_SET(PA_WEIGHTING) THEN pa_weight(*) = 1.0

  FOR itheta = 0, ntheta-1 DO BEGIN
     FOR iphi = 0, nphi-1 DO BEGIN
        iangle = (itheta * nphi) + iphi

        CASE distribution OF
           'Original': BEGIN
              data(*,iangle,*) = data_or(*,itheta,*) * pa_weight(0,iangle)
           END
           'Gyrotropic': BEGIN
              data(*,iangle,*) = data_or(*,itheta,*) * pa_weight(0,iangle)
           END

           'Isotropic': BEGIN
              IF itheta EQ 0 AND iphi EQ 0 THEN BEGIN
                 isotropic_flux = MEAN(data_or(*,*,*) * pa_weight(iangle), DIMENSION=2, /NaN)
              ENDIF
              data(*,iangle,*) = isotropic_flux
           END
        ENDCASE

        energy_arr(*,iangle,*) = en_data
        denergy_arr(*,iangle,*) = 2.*en_delta_data

     ENDFOR
  ENDFOR

  specie_str = ['H!U+!N', 'e!U-!N', 'He!U+!N', 'O!U+!N']
  mass = [0.0104389, 0.0208778, 0.0417556, 0.167022]

  sensitivity = REPLICATE(1, ndatapoints)
  k1 = REPLICATE(255, ndatapoints)
  k2 = REPLICATE(255, ndatapoints)

  retdata = {                                                               $
            project_name:        'RBSP HOPE',                               $
            data_name:           var2get(0),                                $
            species:             specie_str(specie),                        $
            units_name:          'DIFF FLUX',                               $
            units_procedure:     '',                                        $
            valid:               1,                                         $
            time:                t_str,                                     $
            end_time:            t_str,                                     $
            delta_t:             22.7,                                      $
            integ_t:             22.7,                                      $
            geom_factor:         1.,                                        $
            nenergy:             nenergy,                                   $
            nbins:               nangles,                                   $
            bins:                REPLICATE(1b, nenergy, nangles),           $
            energy:              energy_arr,                                $
            denergy:             denergy_arr,                               $
            theta:               theta,                                     $
            phi:                 phi,                                       $
            dtheta:              dtheta,                                    $
            dphi:                dphi,                                      $
            k1:                  k1,                                        $
            k2:                  k2,                                        $
            data:                data,                                      $
            mass:                mass(specie),                             $
;              scale:               fltarr(nenergy,nangles),                   $
;              pac:                 fix(cal_struct1.post_accel_volt),          $
;              anode_effic:         cal_struct1.anode_effic_coeff_table,       $
;              onboard_anode_effic:cal_struct1.onboard_anode_effic_coeff_table,$
;              absol_effic:        cal_struct2.absolute_efficiencies,          $
;              phase_inst:         basic_data.header(ind_s:ind_e).phase_instr, $
              sensitivity:        sensitivity                                $
;              op_mode:            op_mode,                                    $
;              phase_instr:        basic_data.header(ind_s:ind_e).phase_instr  $
            }

END

; Keyword DISTRIBUTION : 'Original', 'Gyrotropic', 'Isotropic'
FUNCTION get_hope_3d, sat, $
                      specie=specie, $
                      name=name, $
                      distribution=distribution, $
                      path=path, $
                      fln=fln, $
											pa_weighting=pa_weighting

  IF NOT KEYWORD_SET(distribution) THEN distribution = 'Original'

  ; -------------------------------------------------------------------
  ; Open file
  ; -------------------------------------------------------------------
  
  probe_str = ['a','b']
  
  IF NOT KEYWORD_SET(path) THEN path2 ='' ELSE path2 = path
  IF path2 EQ '' THEN path2 = GETENV('RBSP_' + STRUPCASE(probe_str(sat-1)) + '_HOPE_PA')

  IF NOT KEYWORD_SET(fln)  THEN BEGIN
    fln2 ='' 
  ENDIF ELSE BEGIN
    fln2 = fln
    files_found = FILE_SEARCH(path2 + '/' + fln2, count=ifln)
    IF ifln LE 0 THEN BEGIN
      MESSAGE, 'No file found', /CONTINUE
    ENDIF
  ENDELSE
  
  IF fln2 EQ '' THEN BEGIN

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
    
    files_found = ''
    FOR indys = 0, ndys-1 DO BEGIN

      date = time_double(STRMID(time_string(time_interval(0)), 0, 4) + $
                          STRMID(time_string(time_interval(0)), 5, 2) + $
                          STRMID(time_string(time_interval(0)), 8, 2)) + $
                          indys * 86400.

      date_str = STRMID(time_string(date), 0, 4) + $
                  STRMID(time_string(date), 5, 2) + $
                  STRMID(time_string(date), 8, 2)

      fln2 = 'rbsp' + probe_str(sat-1) + '*_ect-hope-PA*-L3_' + date_str + '*.cdf'

      path_fln = FILE_SEARCH(path2 + '/' + fln2, count=ifln)
      IF ifln LE 0 THEN BEGIN

      ENDIF ELSE BEGIN
        files_found = [files_found, path_fln(ifln-1)]
      ENDELSE
    ENDFOR

    IF N_ELEMENTS(files_found) EQ 1 THEN BEGIN
      MESSAGE, 'No file found', /CONTINUE
    ENDIF ELSE BEGIN
      files_found = files_found(1:N_ELEMENTS(files_found)-1)
    ENDELSE

  ENDIF
  
  CASE specie OF
     0: var2get = ['FPDU',  'Epoch_Ion', 'PITCH_ANGLE', 'GYRO_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
     1: var2get = ['FEDU',  'Epoch_Ele', 'PITCH_ANGLE', 'GYRO_ANGLE', 'HOPE_ENERGY_Ele', 'ENERGY_Ele_DELTA']
     2: var2get = ['FHEDU', 'Epoch_Ion', 'PITCH_ANGLE', 'GYRO_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
     3: var2get = ['FODU',  'Epoch_Ion', 'PITCH_ANGLE', 'GYRO_ANGLE', 'HOPE_ENERGY_Ion', 'ENERGY_Ion_DELTA']
  ENDCASE

  append_flag = 0
  FOR iday = 0, N_ELEMENTS(files_found)-1 DO BEGIN
  
    dd = get_cdf_data(file=files_found(iday), var2get=var2get)

  ; -------------------------------------------------------------------
  ; Get data
  ; -------------------------------------------------------------------

    IF specie EQ 1 THEN BEGIN
      t_ele = REFORM(dd.epoch_ele.data)
      t_str = time_double(t_ele, /EPOCH)
    ENDIF ELSE BEGIN
      t_ion = REFORM(dd.epoch_ion.data)
      t_str = time_double(t_ion, /EPOCH)
    ENDELSE

    get_timespan, tt
    itime = WHERE(t_str GE tt(0) AND t_str LE tt(1), c_itime)
    IF c_itime GT 1 THEN BEGIN
      t_str = t_str(itime)

      pa_data = dd.pitch_angle.data
      IF specie EQ 1 THEN BEGIN
        en_data = REVERSE(dd.hope_energy_ele.data(*,itime))
        en_delta_data = REVERSE(dd.energy_ele_delta.data(*,itime))
      ENDIF ELSE BEGIN
        en_data = REVERSE(dd.hope_energy_ion.data(*,itime))
        en_delta_data = REVERSE(dd.energy_ion_delta.data(*,itime))
      ENDELSE

      CASE specie OF
        0: BEGIN
          IF SIZE(dd.fpdu.data, /N_DIMENSIONS) EQ 4 THEN BEGIN
            data_size = 4
            data_or = REVERSE(dd.fpdu.data(*,*,*,itime),1)
          ENDIF ELSE BEGIN
            data_size = 3
            data_or = REVERSE(dd.fpdu.data(*,*,itime),1)
          ENDELSE
        END

        1: BEGIN
          IF SIZE(dd.fedu.data, /N_DIMENSIONS) EQ 4 THEN BEGIN
            data_size = 4
            data_or = REVERSE(dd.fedu.data(*,*,*,itime),1)
          ENDIF ELSE BEGIN
            data_size = 3
            data_or = REVERSE(dd.fedu.data(*,*,itime),1)
          ENDELSE
        END

        2: BEGIN
          IF SIZE(dd.fhedu.data, /N_DIMENSIONS) EQ 4 THEN BEGIN
            data_size = 4
            data_or = REVERSE(dd.fhedu.data(*,*,*,itime),1)
          ENDIF ELSE BEGIN
            data_size = 3
            data_or = REVERSE(dd.fhedu.data(*,*,itime),1)
          ENDELSE
        END

        3: BEGIN
          IF SIZE(dd.fodu.data, /N_DIMENSIONS) EQ 4 THEN BEGIN
            data_size = 4
            data_or = REVERSE(dd.fodu.data(*,*,*,itime),1)
          ENDIF ELSE BEGIN
            data_size = 3
            data_or = REVERSE(dd.fodu.data(*,*,itime),1)
          ENDELSE
        END

      ENDCASE

      IF append_flag EQ 0 THEN BEGIN

        data_x = t_str
        data_y = TRANSPOSE(data_or)
        data_v = TRANSPOSE(en_data)
        data_dv = TRANSPOSE(en_delta_data)

        append_flag = 1

      ENDIF ELSE BEGIN

        data_x  = [data_x, t_str]
        data_y  = [data_y, TRANSPOSE(data_or)]
        data_v  = [data_v, TRANSPOSE(en_data)]
        data_dv = [data_dv, TRANSPOSE(en_delta_data)]

      ENDELSE

    ENDIF else begin
      

    endelse 
  ENDFOR

  ; if ~keyword_set(data_y) then data_y = !VALUES.F_NAN 
  ifill = WHERE(data_y EQ -1.00000e+31, c_ifill) 
  IF c_ifill GT 0 THEN BEGIN
     data_y(ifill) = !VALUES.F_NAN
  ENDIF

  CASE data_size OF
    3: sort_3d_data, data_x, data_y, data_v, data_dv, var2get, specie, distribution, retdata
    4: sort_4d_data, data_x, data_y, data_v, data_dv, var2get, specie, distribution, retdata
  ENDCASE

  RETURN, retdata

END
