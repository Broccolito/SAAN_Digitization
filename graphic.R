plot_SAAN = function(){
  
  graphics.off()
  
  get_directory = function(){
    args <- commandArgs(trailingOnly = FALSE)
    file <- "--file="
    rstudio <- "RStudio"
    
    match <- grep(rstudio, args)
    if(length(match) > 0){
      return(dirname(rstudioapi::getSourceEditorContext()$path))
    }else{
      match <- grep(file, args)
      if (length(match) > 0) {
        return(dirname(normalizePath(sub(file, "", args[match]))))
      }else{
        return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
      }
    }
  }
  
  setwd(get_directory())
  
  windows(720,1080)
  
  #Load all the rda files
  for(i in 1:length(list.files(pattern = "rda"))){
    load(list.files(pattern = "rda")[i])
  }
  rm(i)
  
  #Initialize the plotting device
  plot(0, cex = 0, xlim = c(0,1000), 
       ylim = c(0,1000), 
       xaxt = "n", yaxt = "n",
       xlab = "", ylab = "",
       main = "Siggaard Anderson Alignment Nomogram")
  
  #For pco2 line
  segments(pco2_mat$x[1], pco2_mat$y[1], pco2_mat$x[dim(pco2_mat)[1]], pco2_mat$y[dim(pco2_mat)[1]])
  points(pco2_mat$x, pco2_mat$y, cex = 0.5, pch = 16)
  
  #For PH line
  segments(ph_mat$x[1], ph_mat$y[1], ph_mat$x[dim(ph_mat)[1]], ph_mat$y[dim(ph_mat)[1]])
  points(ph_mat$x, ph_mat$y, cex = 0.5, pch = 16)
  
  #For hco3 line
  segments(hco3_mat$x[1], hco3_mat$y[1], hco3_mat$x[dim(hco3_mat)[1]], hco3_mat$y[dim(hco3_mat)[1]])
  points(hco3_mat$x, hco3_mat$y, cex = 0.5, pch = 16)
  
  #For Total CO2 line
  #segments(tco2_mat$x[1], tco2_mat$y[1], tco2_mat$x[dim(tco2_mat)[1]], tco2_mat$y[dim(tco2_mat)[1]])
  points(tco2_mat$x, tco2_mat$y, cex = 0.5, pch = 16)
  
  #For hb bex matrix
  x_value = paste0("x_", seq(0, 25, 0.5))
  y_value = paste0("y_", seq(0, 25, 0.5))
  
  for(i in seq(0,25,0.5))S
    eval(parse(text = paste0(
      "points(hb_bex_mat$x_", i, ", hb_bex_mat$y_", i, ", cex = 0.1)"
    )))
  
}

plot_SAAN()
