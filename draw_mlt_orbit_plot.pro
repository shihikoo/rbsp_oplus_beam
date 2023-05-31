PRO draw_mlt_orbit_plot, data_pos, filename = filename, title=title, xtitle = xtitle, ytitle = ytitle, xrange = xrange, yrange = yrange, xlog = xlog, ylog = ylog, ps_plot = ps_plot, grid=grid
;constants
  norm_factor_mlt=1/24.*360/180*!PI
  la_x = 20 & la_y = -20  

; default keywords settings
  IF KEYWORD_SET(ps_plot) AND ~KEYWORD_SET(filename) THEN filename = 'sampling_map.ps'
  IF ~KEYWORD_SET(grid) THEN grid = 1.
   
; basice settings 
  data_pos_mlt =  norm_factor_mlt * data_pos(*,0)
  data_pos_ilat = data_pos(*,1)
 
  r_range = ABS(yrange(0) - yrange(1))

; open the ps plot
  IF KEYWORD_SET(ps_plot) THEN popen, filename, /land 

  PLOT, [0, 0, -100, 100], [-100, 100, 0, 0] $
        , title = title, xtitle = xtitle, ytitle = ytitle, xrange = [-r_range,r_range], yrange = [-r_range, r_range] $
        , XSTYLE = 5, ystyle = 5, charsize = 1.2, position = [0.15, 0.15, 0.85, 0.85], xlog = xlog, ylog = ylog, xcharSIZE = 2, ycharsize = 2
  
  IF keyword_set(ps_plot) then psym_point_map = 3 else psym_point_map = 3

; plot data that has no beam
  oplot, data_pos_ilat, data_pos_mlt, color = 2, psym = psym_point_map, /polar

; legend         
;  xyouts, la_x+7, la_y+4.5*grid, 'no events', color = 2
;  xyouts, la_x+7, la_y, 'beam events', color = 2, charsize = 2

  xyouts, r_range, 0, '0', charsize = 2
  xyouts, 0, r_range, '6', charsize = 2
  xyouts, -r_range*1.05,0, '12', charsize = 2
  xyouts, 0 ,-r_range*1.05, '18', charsize = 2

; draw the grid lines
  for i=0, 11 do oplot,[0,r_range],[i*30./180.*!PI,i*30./180.*!PI],/polar
  for j=0, (r_range/10)-1 do  oplot, replicate(10*(j+1),360),indgen(360)*!PI/180.,/polar

; close the ps plot
  IF KEYWORD_SET(ps_plot) THEN pclose ELSE stop

END
