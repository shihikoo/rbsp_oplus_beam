FUNCTION calculate_delayed_solarwind_parameter,varname, outvarname, min_time, time
  get_data, varname, data = data
   
  output = DBLARR(N_ELEMENTS(time))
  output(*) = !VALUES.F_NAN

  FOR ii = 0, N_ELEMENTS(time)-1 DO BEGIN 
     IF FINITE(min_time(ii)) THEN BEGIN 
        index = WHERE(data.x GE min_time(ii) and data.x LE time(ii),ct)
        IF ct GT 0 THEN output[ii] = TOTAL(data.y[index],/NAN)/ct
     ENDIF 
  ENDFOR 

  store_data, outvarname, data ={x:time, y:output}
  RETURN, outvarname

END


PRO calculate_solarwind_delayed, o1_energy_name, dist_name, varnames, outvarnames
  Earth_r = 6371. ;km
  mass_o = 16*1.6e-27*(1e3)^2/(1.6e-19) ; unit: ev/(km/s)^2
  default_o_en = 1338.02 ; eV

  get_data, o1_energy_name, data = data
  time_avg = data.x
  o_en = data.y
  o_v = sqrt(2*o_en/mass_o)
  
  index = WHERE(~FINITE(o_v), ct)
  IF ct GT 0 THEN o_v[index] = sqrt(2*default_o_en/mass_o)
  
  get_data, dist_name, data = data
  dist = data.y

  min_time = time_avg - ABS(dist*Earth_r/o_v)

  FOR ii = 0, N_ELEMENTS(varnames)-1 DO newnames = calculate_delayed_solarwind_parameter(varnames(ii), outvarnames(ii), min_time, time_avg)

END
