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
#' @title  Spectral fiting
#' @aliases runSpecFit
#' @usage runSpecFit(obsSpec)
#' @param par par
#' @author Rafael de Souza, UNC
#' @export
runSpecFit <- function(obsSpec,maxiter = 1000){

ObsSpecdiff <- obsSpec %>% mutate(Count = append(0,diff(Count)))

loss <- function(par){
    fitness(DiffSpecGen(par),ObsSpecdiff$Count)
}
outDE <- DEoptim(loss, lower = rep(0,11),
                   upper = rep(50,11),control = list(NP = 200,itermax = maxiter))

out <- data.frame(w = ObsSpec0$w,
                  count = SpecGen(outDE$optim$bestmem))

BR <- function(x){
  vt <- x[-c(9:11)]
  vt <- vt/sum(vt)
  return(vt)
}
bestfit <- BR(outDE$optim$bestmem)*100

return(list(spec = out, bestfit = bestfit))
}
