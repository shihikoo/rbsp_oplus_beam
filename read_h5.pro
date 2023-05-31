;------------------------------
;read_h5
;Descritpion:read h5 file with given strid
;Inputs: filename: h5 filepath
;        strid: data id to be extracted
;Output: data
;example:
;filename = 'rbspa_def_MagEphem_OP77Q_20130101_v3.0.0.h5'
;strid = 'IsoTime'
;read_h5, filename, strid, data
;comments: error handling to be added
; written by Jing Liao
; written on 2021/10/21
;-----------------------------------------
pro read_h5, filename, strid, data

if (~file_exist(filename) then stop ; error handing

h5_list, filename, output = strids
if total(('/' + strid) EQ strids[1,*]) NE 1 then stop ; error handling code

data = H5_GETDATA(filename, strid)

stop
end
