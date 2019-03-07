library(readxl)

hb_bex_mat = as.data.frame(read_excel("hb_bex_mat.xlsx"))
hco3_mat = as.data.frame(read_excel("hco3_mat.xlsx"))
pco2_mat = as.data.frame(read_excel("pco2_mat.xlsx"))
ph_mat = as.data.frame(read_excel("ph_mat.xlsx"))
tco2_mat = as.data.frame(read_excel("tco2_mat.xlsx"))

save(hb_bex_mat, file = "hb_bex_mat.rda")
save(hco3_mat, file = "hco3_mat.rda")
save(pco2_mat, file = "pco2_mat.rda")
save(ph_mat, file = "ph_mat.rda")
save(tco2_mat, file = "tco2_mat.rda")

load("hb_bex_mat.rda")
load("hco3_mat.rda")
load("pco2_mat.rda")
load("ph_mat.rda")
load("tco2_mat.rda")