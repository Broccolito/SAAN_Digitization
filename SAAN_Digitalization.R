##############################################################
# Siggaard-Andersen Alignment Nomogram Digitalization
# Author: Wanjun Gu
# Email: wag001@ucsd.edu
# Reference: Siggaard-Andersen Alignment Nomogram
# University of California, San Diego
# UCSD School of Medicine
# Simonson Lab of Physiological Genomics of Altitude Adaptation
##############################################################

#First run the code to define the function in R workspace,
#Then try:    SAAN()   in R terminal to get output
#Input parameters can be changed

SAAN = function(pco2 = 25, ph = 7.4, hemoglobin = 16){
  #This function will return the a and b values for a line given 2 points
  get_formula = function(...){
    cord = as.vector(unlist(...))
    x1 = cord[1]
    y1 = cord[2]
    x2 = cord[3]
    y2 = cord[4]
    #Make sure the line is not perpendicular
    if(x1 == x2){
      x2 = x2 + 0.00001
    }
    slope = (y1 - y2)/(x1 - x2)
    intersection = y1 - (x1 * slope)
    return(c(slope, intersection))
  }
  
  #Get a and b values for a line from slope and a point on the line
  formula_from_slope = function(slope, cord){
    x = cord[1]
    y = cord[2]
    b = y - slope * x
    formula_vec = as.vector(unlist(c(slope, b)))
    names(formula_vec) = c("a", "b")
    return(formula_vec)
  }
  
  #This function will return the distance between 2 points in pixels
  get_distance = function(...){
    cord = as.vector(unlist(...))
    x1 = cord[1]
    y1 = cord[2]
    x2 = cord[3]
    y2 = cord[4]
    return(
      ((x2 - x1)^2 + (y2 - y1)^2)^0.5
    )
  }
  
  #This function takes in 2 a,b values and give the cord of intersection
  linear_solve = function(...){
    cord = as.vector(unlist(...))
    a1 = cord[1]
    b1 = cord[2]
    a2 = cord[3]
    b2 = cord[4]
    #Check if lines are parallel
    if(a1 == a2){
      return("This equation cannot be solved.")
    }
    x = (b2 - b1)/(a1 - a2)
    y = a1 * x + b1
    return(c(x, y))
  }
  
  find_approximate_from_value = function(value, value_list){
    return(
      value_list[which.min(abs(value - value_list))])
  }
  
  find_approximate_from_pixel = function(value, value_matrix){
    distance = vector()
    for(i in 1:dim(value_matrix)[1]){
      distance = c(distance, get_distance(c(
        value, value_matrix[i,]
      )))
    }
    return(value_matrix[which.min(distance),])
  }
  
  #Load data
  #Defining points of SAAN
  
  pco2_points = c(3869, 231, 3855, 5201)
  #pco2 data points
  {
    pco2_mat = t(matrix(nrow = 3,
                        data = c(
                          3869, 319, 10.5, 
                          3869, 407, 11, 
                          3869, 491, 11.5, 
                          3868, 565, 12, 
                          3868, 643, 12.5, 
                          3868, 713, 13, 
                          3868, 783, 13.5, 
                          3867, 853, 14, 
                          3867, 915, 14.5, 
                          3867, 979, 15, 
                          3867, 1041, 15.5, 
                          3867, 1099, 16, 
                          3867, 1153, 16.5, 
                          3866, 1207, 17, 
                          3866, 1261, 17.5, 
                          3866, 1311, 18, 
                          3866, 1361, 18.5, 
                          3866, 1411, 19, 
                          3866, 1461, 19.5, 
                          3866, 1503, 20, 
                          3866, 1551, 20.5, 
                          3865, 1597, 21, 
                          3865, 1641, 21.5, 
                          3865, 1683, 22, 
                          3865, 1721, 22.5, 
                          3865, 1763, 23, 
                          3865, 1803, 23.5, 
                          3865, 1843, 24, 
                          3865, 1879, 24.5, 
                          3864, 1917, 25, 
                          3864, 1953, 25.5, 
                          3864, 1986, 26, 
                          3864, 2022, 26.5, 
                          3864, 2055, 27, 
                          3864, 2089, 27.5, 
                          3864, 2122, 28, 
                          3864, 2156, 28.5, 
                          3864, 2187, 29, 
                          3864, 2219, 29.5, 
                          3864, 2248, 30, 
                          3863, 2280, 30.5, 
                          3863, 2311, 31, 
                          3863, 2339, 31.5, 
                          3863, 2368, 32, 
                          3863, 2397, 32.5, 
                          3863, 2424, 33, 
                          3863, 2450, 33.5, 
                          3863, 2479, 34, 
                          3863, 2504, 34.5, 
                          3863, 2533, 35, 
                          3863, 2558, 35.5, 
                          3863, 2583, 36, 
                          3863, 2606, 36.5, 
                          3862, 2633, 37, 
                          3862, 2656, 37.5, 
                          3862, 2683, 38, 
                          3862, 2707, 38.5, 
                          3862, 2728, 39, 
                          3862, 2752, 39.5, 
                          3862, 2773, 40, 
                          3862, 2821, 41, 
                          3862, 2865, 42, 
                          3862, 2912, 43, 
                          3862, 2951, 44, 
                          3861, 2991, 45, 
                          3861, 3033, 46, 
                          3861, 3070, 47, 
                          3861, 3107, 48, 
                          3861, 3145, 49, 
                          3861, 3185, 50, 
                          3861, 3220, 51, 
                          3861, 3258, 52, 
                          3861, 3290, 53, 
                          3861, 3326, 54, 
                          3860, 3357, 55, 
                          3860, 3390, 56, 
                          3860, 3425, 57, 
                          3860, 3458, 58, 
                          3860, 3486, 59, 
                          3860, 3516, 60, 
                          3860, 3548, 61, 
                          3860, 3579, 62, 
                          3860, 3607, 63, 
                          3860, 3637, 64, 
                          3860, 3662, 65, 
                          3859, 3693, 66, 
                          3859, 3722, 67, 
                          3859, 3749, 68, 
                          3859, 3773, 69, 
                          3859, 3803, 70, 
                          3859, 3829, 71, 
                          3859, 3852, 72, 
                          3859, 3879, 73, 
                          3859, 3903, 74, 
                          3859, 3927, 75, 
                          3859, 3951, 76, 
                          3859, 3977, 77, 
                          3859, 4002, 78, 
                          3859, 4020, 79, 
                          3859, 4043, 80, 
                          3858, 4068, 81, 
                          3858, 4091, 82, 
                          3858, 4115, 83, 
                          3858, 4134, 84, 
                          3858, 4159, 85, 
                          3858, 4180, 86, 
                          3858, 4202, 87, 
                          3858, 4223, 88, 
                          3858, 4242, 89, 
                          3858, 4262, 90, 
                          3858, 4283, 91, 
                          3858, 4302, 92, 
                          3858, 4324, 93, 
                          3858, 4340, 94, 
                          3858, 4361, 95, 
                          3858, 4381, 96, 
                          3858, 4400, 97, 
                          3857, 4418, 98, 
                          3857, 4438, 99, 
                          3857, 4456, 100,  
                          3855, 5201, 150
                        )))
  }
  
  ph_points = c(2159, 995, 2151, 3946)
  #ph data points
  {
    ph_mat = t(matrix(nrow = 3,
                      data = c(
                        2159, 995, 8.00,
                        2159, 1016, 7.99,
                        2159, 1037, 7.98,
                        2159, 1058, 7.97,
                        2159, 1079, 7.96,
                        2159, 1100, 7.95,
                        2159, 1121, 7.94,
                        2159, 1143, 7.93,
                        2159, 1164, 7.92,
                        2158, 1185, 7.91,
                        2158, 1206, 7.90,
                        2158, 1227, 7.89,
                        2158, 1248, 7.88,
                        2158, 1269, 7.87,
                        2158, 1290, 7.86,
                        2158, 1311, 7.85,
                        2158, 1332, 7.84,
                        2158, 1353, 7.83,
                        2158, 1374, 7.82,
                        2158, 1395, 7.81,
                        2158, 1417, 7.80,
                        2158, 1438, 7.79,
                        2158, 1459, 7.78,
                        2158, 1480, 7.77,
                        2158, 1501, 7.76,
                        2158, 1522, 7.75,
                        2158, 1543, 7.74,
                        2157, 1564, 7.73,
                        2157, 1585, 7.72,
                        2157, 1606, 7.71,
                        2157, 1627, 7.70,
                        2157, 1648, 7.69,
                        2157, 1670, 7.68,
                        2157, 1691, 7.67,
                        2157, 1712, 7.66,
                        2157, 1733, 7.65,
                        2157, 1754, 7.64,
                        2157, 1775, 7.63,
                        2157, 1796, 7.62,
                        2157, 1817, 7.61,
                        2157, 1838, 7.60,
                        2157, 1859, 7.59,
                        2157, 1880, 7.58,
                        2157, 1901, 7.57,
                        2157, 1922, 7.56,
                        2156, 1944, 7.55,
                        2156, 1965, 7.54,
                        2156, 1986, 7.53,
                        2156, 2007, 7.52,
                        2156, 2028, 7.51,
                        2156, 2049, 7.50,
                        2156, 2070, 7.49,
                        2156, 2091, 7.48,
                        2156, 2112, 7.47,
                        2156, 2133, 7.46,
                        2156, 2154, 7.45,
                        2156, 2175, 7.44,
                        2156, 2196, 7.43,
                        2156, 2218, 7.42,
                        2156, 2239, 7.41,
                        2156, 2260, 7.40,
                        2156, 2281, 7.39,
                        2155, 2302, 7.38,
                        2155, 2323, 7.37,
                        2155, 2344, 7.36,
                        2155, 2365, 7.35,
                        2155, 2386, 7.34,
                        2155, 2407, 7.33,
                        2155, 2428, 7.32,
                        2155, 2449, 7.31,
                        2155, 2471, 7.30,
                        2155, 2492, 7.29,
                        2155, 2513, 7.28,
                        2155, 2534, 7.27,
                        2155, 2555, 7.26,
                        2155, 2576, 7.25,
                        2155, 2597, 7.24,
                        2155, 2618, 7.23,
                        2155, 2639, 7.22,
                        2155, 2660, 7.21,
                        2154, 2681, 7.20,
                        2154, 2702, 7.19,
                        2154, 2723, 7.18,
                        2154, 2745, 7.17,
                        2154, 2766, 7.16,
                        2154, 2787, 7.15,
                        2154, 2808, 7.14,
                        2154, 2829, 7.13,
                        2154, 2850, 7.12,
                        2154, 2871, 7.11,
                        2154, 2892, 7.10,
                        2154, 2913, 7.09,
                        2154, 2934, 7.08,
                        2154, 2955, 7.07,
                        2154, 2976, 7.06,
                        2154, 2997, 7.05,
                        2154, 3019, 7.04,
                        2153, 3040, 7.03,
                        2153, 3061, 7.02,
                        2153, 3082, 7.01,
                        2153, 3103, 7.00,
                        2153, 3124, 6.99,
                        2153, 3145, 6.98,
                        2153, 3166, 6.97,
                        2153, 3187, 6.96,
                        2153, 3208, 6.95,
                        2153, 3229, 6.94,
                        2153, 3250, 6.93,
                        2153, 3271, 6.92,
                        2153, 3293, 6.91,
                        2153, 3314, 6.90,
                        2153, 3335, 6.89,
                        2153, 3356, 6.88,
                        2153, 3377, 6.87,
                        2153, 3398, 6.86,
                        2152, 3419, 6.85,
                        2152, 3440, 6.84,
                        2152, 3461, 6.83,
                        2152, 3482, 6.82,
                        2152, 3503, 6.81,
                        2152, 3524, 6.80,
                        2152, 3546, 6.79,
                        2152, 3567, 6.78,
                        2152, 3588, 6.77,
                        2152, 3609, 6.76,
                        2152, 3630, 6.75,
                        2152, 3651, 6.74,
                        2152, 3672, 6.73,
                        2152, 3693, 6.72,
                        2152, 3714, 6.71,
                        2152, 3735, 6.70,
                        2152, 3756, 6.69,
                        2152, 3777, 6.68,
                        2151, 3798, 6.67,
                        2151, 3820, 6.66,
                        2151, 3841, 6.65,
                        2151, 3862, 6.64,
                        2151, 3883, 6.63,
                        2151, 3904, 6.62,
                        2151, 3925, 6.61,
                        2151, 3946, 6.60
                      )))
  }
  
  hemoglobin_points = c(1893, 3356, 1663, 4971)
  #Hemoglobin data points
  {
    hemoglobin_mat = t(matrix(nrow = 3,
                              data = c(
                                1893, 3356, 25, 
                                1887, 3401, 22.5, 
                                1880, 3446, 20, 
                                1872, 3504, 19, 
                                1869, 3522, 18, 
                                1866, 3545, 17, 
                                1862, 3571, 16, 
                                1857, 3612, 15, 
                                1847, 3678, 14, 
                                1843, 3704, 13, 
                                1839, 3732, 12, 
                                1835, 3764, 11, 
                                1829, 3805, 10, 
                                1817, 3887, 9, 
                                1810, 3941, 8, 
                                1801, 4002, 7,
                                1793, 4056, 6, 
                                1784, 4123, 5, 
                                1764, 4264, 4, 
                                1745, 4393, 3, 
                                1725, 4538, 2, 
                                1698, 4727, 1, 
                                1663, 4971, 0 
                              )))
  }
  
  bex_points = c(845, 1604, 1273, 3888)
  #Base Excess data points
  {
    bex_mat = t(matrix(nrow = 3,
                       data = c(
                         845, 1604, 5, 
                         857, 1665, 4, 
                         865, 1711, 3, 
                         875, 1765, 2, 
                         886, 1821, 1, 
                         897, 1881, 0, 
                         909, 1943, -1, 
                         921, 2007, -2, 
                         933, 2071, -3, 
                         946, 2137, -4, 
                         959, 2205, -5, 
                         971, 2271, -6, 
                         985, 2345, -7, 
                         998, 2411, -8, 
                         1012, 2485, -9, 
                         1026, 2563, -10, 
                         1041, 2639, -11, 
                         1057, 2727, -12, 
                         1074, 2815, -13, 
                         1091, 2907, -14, 
                         1109, 2999, -15, 
                         1127, 3095, -16, 
                         1145, 3191, -17, 
                         1162, 3281, -18, 
                         1183, 3391, -19, 
                         1203, 3495, -20, 
                         1226, 3617, -21, 
                         1248, 3735, -22, 
                         1277, 3888, -23 
                       )))
  }
  
  baseline_points = c(1239,1129,1809,2930)
  #Baseline data points
  #Currently the baseline data is not needed
  
  hco3_points = c(603,305,531,4197)
  #Currently not needed
  
  #Convert all data to data frame
  pco2_mat = as.data.frame(pco2_mat)
  names(pco2_mat) = c("x", "y", "value")
  ph_mat = as.data.frame(ph_mat)
  names(ph_mat) = c("x", "y", "value")
  hemoglobin_mat = as.data.frame(hemoglobin_mat)
  names(hemoglobin_mat) = c("x", "y", "value")
  bex_mat = as.data.frame(bex_mat)
  names(bex_mat) = c("x", "y", "value")
  
  #func data stores all the line formulas in 'y=ax+b' form
  pco2_func = get_formula(pco2_points)
  ph_func = get_formula(ph_points)
  hemoglobin_func = get_formula(hemoglobin_points)
  bex_func = get_formula(bex_points)
  baseline_func = get_formula(baseline_points)
  hco3_func = get_formula(hco3_points)
  
  #approximate the inputs and get the coordinates of them
  pco2 = find_approximate_from_value(pco2, pco2_mat$value)
  ph = find_approximate_from_value(ph, ph_mat$value)
  hemoglobin = find_approximate_from_value(hemoglobin, hemoglobin_mat$value)
  
  pco2_input_cord = as.vector(pco2_mat[pco2_mat$value == pco2,][,1:2])
  ph_input_cord = as.vector(ph_mat[ph_mat$value == ph,][,1:2])
  hemoglobin_input_cord = as.vector(hemoglobin_mat[hemoglobin_mat$value == hemoglobin,][,1:2])
  
  pco2_30_cord = as.vector(pco2_mat[pco2_mat$value == 30,][,1:2])
  pco2_60_cord = as.vector(pco2_mat[pco2_mat$value == 60,][,1:2])
  
  #What to do here to calculate APH30, BPH60 and Base Excess
  #The first line passes through pco2 and ph, call it first_line
  #The second line goes parallel with the baseline and passes through Hemoglobin, call it second_line
  #Then get the intersection of the two lines
  #Connect the intersection with pco2 at 30, make it line_1
  #Connect the intersection with pco2 at 60, make it line_2
  #The intersection of line_1 and ph gives you APH30
  #The intersection of line_2 and ph gives you BPH60
  #The intersection of first_line and Base Excess gives you Base Excess
  
  first_line = get_formula(c(ph_input_cord, pco2_input_cord))
  second_line = formula_from_slope(slope = baseline_func[1],
                                   cord = hemoglobin_input_cord)
  intersection = linear_solve(c(first_line, second_line))
  line_1 = get_formula(c(intersection, pco2_30_cord))
  line_2 = get_formula(c(intersection, pco2_60_cord))
  APH30_pixel = linear_solve(c(line_1, ph_func))
  BPH60_pixel = linear_solve(c(line_2, ph_func))
  if(any(
    which(ph_mat$x == as.numeric(find_approximate_from_pixel(value = APH30_pixel,
                                                             value_matrix = ph_mat[,1:2])[1])) ==
    which(ph_mat$y == as.numeric(find_approximate_from_pixel(value = APH30_pixel,
                                                             value_matrix = ph_mat[,1:2])[2]))
  )){
    APH30 = ph_mat[which(ph_mat$y == as.numeric(find_approximate_from_pixel(value = APH30_pixel,
                                                                            value_matrix = ph_mat[,1:2])[2])),3]
  }
  if(any(
    which(ph_mat$x == as.numeric(find_approximate_from_pixel(value = BPH60_pixel,
                                                             value_matrix = ph_mat[,1:2])[1])) ==
    which(ph_mat$y == as.numeric(find_approximate_from_pixel(value = BPH60_pixel,
                                                             value_matrix = ph_mat[,1:2])[2]))
  )){
    BPH60 = ph_mat[which(ph_mat$y == as.numeric(find_approximate_from_pixel(value = BPH60_pixel,
                                                                            value_matrix = ph_mat[,1:2])[2])),3]
  }
  bex_pixel = linear_solve(c(bex_func, first_line))
  
  if(any(
    which(bex_mat$x == as.numeric(find_approximate_from_pixel(value = bex_pixel,
                                                              value_matrix = bex_mat[,1:2])[1])) == 
    which(bex_mat$y == as.numeric(find_approximate_from_pixel(value = bex_pixel,
                                                              value_matrix = bex_mat[,1:2])[2]))
  )){
    Base_Excess = bex_mat[which(bex_mat$y == as.numeric(find_approximate_from_pixel(value = bex_pixel,
                                                                                    value_matrix = bex_mat[,1:2])[2])),3]
  }
  result = c(APH30, BPH60, Base_Excess)
  names(result) = c("APH30", "BPH60", "BEX")
  return(result)
}