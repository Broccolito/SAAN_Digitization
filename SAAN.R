##############################################################
# Siggaard Andersen Nomogram Digitization
# Author: Wanjun Gu
# Email: wag001@ucsd.edu
# Reference: https://www.ncbi.nlm.nih.gov/pubmed/1166141
# Date: March 7th 2019
# University of California, San Diego
# UCSD School of Medicine
# Simonson Lab of Physiological Genomics of Altitude Adaptation
##############################################################

#Make sure the rda files are accessible
SAAN = function(pco2 = 22.3, ph = 7.31, hb = 16.7){
  
  if(pco2 < 10 | pco2 > 100){
    stop("pco2 value out of bound...")
  }
  
  if(ph < 6.6 | ph > 8){
    stop("ph value out of bound...")
  }
  
  if(hb < 0 | hb > 100){
    stop("hb value out of bound...")
  }
  
  load("hb_bex_mat.rda")
  load("hco3_mat.rda")
  load("pco2_mat.rda")
  load("ph_mat.rda")
  load("tco2_mat.rda")
  
  round_to_half = function(n){
    return(round(2*n, 0)/2)
  }
  
  from_point_to_line = function(m, b, x, y){
    return(abs(b + m * x - y)/(1 + m^2)^0.5)
  }
  
  if(pco2 > 39.5){
    pco2 = round(pco2, 0)
  }else{
    pco2 = round_to_half(pco2)
  }
  
  hb = round_to_half(hb)
  
  #For APH
  x_30 = pco2_mat[pco2_mat$v == 30,]$x
  y_30 = pco2_mat[pco2_mat$v == 30,]$y
  
  #For BPH
  x_60 = pco2_mat[pco2_mat$v == 60,]$x
  y_60 = pco2_mat[pco2_mat$v == 60,]$y
  
  #Find PCO2 point
  x_pco2 = pco2_mat[pco2_mat$v == pco2,]$x
  y_pco2 = pco2_mat[pco2_mat$v == pco2,]$y
  
  #Find PH point
  ph_mat$v = round(ph_mat$v, 2) #Weird Error comming out from here
  x_ph = ph_mat[ph_mat$v == ph,]$x
  y_ph = ph_mat[ph_mat$v == ph,]$y
  
  #Find HB Xs and Ys
  eval(parse(text = paste0(
    "x_hb_bex = hb_bex_mat$x_", hb
  )))
  
  eval(parse(text = paste0(
    "y_hb_bex = hb_bex_mat$y_", hb
  )))
  
  #The parameters for the line connecting PCO2 and PH
  m1 = (y_pco2 - y_ph)/(x_pco2 - x_ph)
  b1 = y_pco2 - m1 * x_pco2
  
  #The parameters for the line fitted from hb_bex mat
  l = lm(y_hb_bex ~ x_hb_bex)
  names(l$coefficients) = NULL
  m2 = l$coefficients[2]
  b2 = l$coefficients[1]
  
  #Find the intersect
  x_intersect = (b2 - b1)/(m1 - m2)
  y_intersect = m1 * x_intersect + b1
  
  #Find parameters for the line passing APH
  m_aph = (y_intersect - y_30)/(x_intersect - x_30)
  b_aph = y_intersect - m_aph * x_intersect
  
  #Find parameters for the line passing BPH
  m_bph = (y_intersect - y_60)/(x_intersect - x_60)
  b_bph = y_intersect - m_bph * x_intersect
  
  aph = ph_mat$v[which.min(from_point_to_line(m = m_aph, b = b_aph, x = ph_mat$x, y = ph_mat$y))]
  bph = ph_mat$v[which.min(from_point_to_line(m = m_bph, b = b_bph, x = ph_mat$x, y = ph_mat$y))]
  bex = hb_bex_mat$v[which.min(from_point_to_line(m = m1, b = b1, x = hb_bex_mat$x_0, y = hb_bex_mat$y_0))]
  hco3 = hco3_mat$v[which.min(from_point_to_line(m = m1, b = b1, x = hco3_mat$x, y = hco3_mat$y))]
  tco2 = tco2_mat$v[which.min(from_point_to_line(m = m1, b = b1, x = tco2_mat$x, y = tco2_mat$y))]
  
  result = c(aph, bph, bex, hco3, tco2)
  names(result) = c("APH", "BPH", "BEX", "hco3", "tco2")
  
  return(result)
  
}

