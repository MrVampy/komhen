require("R.rsp") || install.packages("R.rsp")
library("R.rsp")
require("R.devices") || install.packages("R.devices")
library("R.devices")

setwd("/home/thomas/Dropbox/komhen/data/doc")

options("devNew/args/par"=list(lwd=2))

rfile("Dynamic_LaTeX_reports_with_RSP.tex", path=paste(getwd(), sep=""))
