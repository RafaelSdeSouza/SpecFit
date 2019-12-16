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
#' @title  Fitness function
#' @aliases fitness
#' @usage fitness(x1,x2)
#' @param x1 x1
#' @param x2  x2
#' @author Rafael de Souza, UNC
#' @export
fitness <- function(x1,x2){
  sum(sqrt((x1-x2)^2))
}

