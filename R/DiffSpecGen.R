# Rafael de Souza, UNC
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License version 3 as published by
#the Free Software Foundation.
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#' @title  Spectra generator
#' @aliases DiffSpecGen
#' @usage DiffSpecGen(par)
#' @param par par
#' @author Rafael de Souza, UNC
#' @export
DiffSpecGen <- function(par){
  gen <- par[1:9]
  Nb  <- par[10]
  Nbkg <- par[11]
  frac <- gen/sum(gen)
  fb <- frac[1:8]
  fbkg <- frac[9]

  out <- Nb*(fb %*% Diffbasis) + Nbkg*fbkg*bkg
  out2 <- as.numeric(out)
  return(out2)
}
