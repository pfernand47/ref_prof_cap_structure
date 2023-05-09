# This source code replicates Tables 2, 3, 4, and 5 in Danis, Rettl, and Whited, 2014, 
# "Refinancing, profitability and capital structure", Journal of Financial Economics.
# Please put the source code and the data file in the same folder. Open the source code file
# from that folder. The default behavior in R is to set that folder as the working 
# directory and load the data file from there. This way you don't have to change any 
# paths in this source code file. Also, all tables will be written in the same folder.

rm(list=ls())
gc()

library(quadprog)
library(zoo)
library(tseries)
library(lmtest)
library(sandwich)
library(xtable)
library(matlab)
library(Formula)
library(plm)
library(pscl)
library(quantmod) 


# Cargar funciones
source("funciones.R")
# Leer datos
source("read_data.R")
# -----------------------
# Generacion de tablas
source("tabla_2.R")
source("tabla_3.R")
source("tabla_4.R")
source("tabla_5.R")
