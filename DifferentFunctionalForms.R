#################################### in the beginning ax2+b and then cx2+dx+e ###########################################
quad_pw <- function(dummyFrame,inf_point) {
  dummyFrame$tinflec1 <- 0
  dummyFrame$tinflec1[dummyFrame$num_timing<inf_point] <- 1
  
  dummyFrame$tinflec2 <- 0
  dummyFrame$tinflec2[dummyFrame$num_timing>=inf_point] <- 1
  
  dummyFrame$shift <- dummyFrame$num_timing - inf_point
  dummyFrame$num_timing2a <- dummyFrame$num_timing^2
  dummyFrame$num_timing2b <- dummyFrame$shift^2 * dummyFrame$tinflec2
  dummyFrame$num_timing1 <- dummyFrame$shift * dummyFrame$tinflec2
  #dummyFrame$num_timing1 <- dummyFrame$num_timing
  return(dummyFrame)
}
#########################################################################################################################
#################################### in the beginning ax2+bx+e and then cx2+dx+e ###########################################
quad_pw_full <- function(dummyFrame,inf_point) {
  dummyFrame$tinflec1 <- 0
  dummyFrame$tinflec1[dummyFrame$num_timing<inf_point] <- 1
  
  dummyFrame$tinflec2 <- 0
  dummyFrame$tinflec2[dummyFrame$num_timing>=inf_point] <- 1
  
  dummyFrame$shift <- dummyFrame$num_timing - inf_point
  dummyFrame$num_timing2a <- dummyFrame$num_timing^2
  dummyFrame$num_timing2b <- dummyFrame$shift^2 * dummyFrame$tinflec2
  dummyFrame$num_timing1 <- dummyFrame$shift * dummyFrame$tinflec2
  #dummyFrame$num_timing1 <- dummyFrame$num_timing
  return(dummyFrame)
}
#########################################################################################################################
#################################### in the beginning ax+e and then cx2+bx+e ###########################################
linear_quad_pw <- function(dummyFrame,inf_point) {
  dummyFrame$tinflec2 <- 0
  dummyFrame$tinflec2[dummyFrame$num_timing>=inf_point] <- 1
  
  dummyFrame$shift <- dummyFrame$num_timing - inf_point
  dummyFrame$num_timing1 <- dummyFrame$num_timing
  dummyFrame$num_timing2 <- dummyFrame$shift^2 * dummyFrame$tinflec2
  dummyFrame$num_timing1b <- dummyFrame$shift * dummyFrame$tinflec2
  return(dummyFrame)
}
#########################################################################################################################
#################################### in the beginning ax+e and then cx2+bx+e ###########################################
linear_quad_pw_myway <- function(dummyFrame,inf_point) {
  dummyFrame$tinflec1 <- 0
  dummyFrame$tinflec1[dummyFrame$num_timing<inf_point] <- 1
  dummyFrame$tinflec2 <- 0
  dummyFrame$tinflec2[dummyFrame$num_timing>=inf_point] <- 1
  
  dummyFrame$shift <- dummyFrame$num_timing - inf_point
  dummyFrame$num_timing1 <- dummyFrame$num_timing * dummyFrame$tinflec1 + inf_point * dummyFrame$tinflec2
  dummyFrame$num_timing2 <- dummyFrame$shift^2 * dummyFrame$tinflec2
  dummyFrame$num_timing1b <- dummyFrame$shift * dummyFrame$tinflec2
  return(dummyFrame)
}
#########################################################################################################################
#################################### in the beginning ax+e and then dx3+cx2+bx+e ###########################################
linear_third_pw <- function(dummyFrame,inf_point) {
  dummyFrame$tinflec2 <- 0
  dummyFrame$tinflec2[dummyFrame$num_timing>=inf_point] <- 1
  
  dummyFrame$shift <- dummyFrame$num_timing - inf_point
  dummyFrame$num_timing1 <- dummyFrame$num_timing
  dummyFrame$num_timing2 <- dummyFrame$shift^2 * dummyFrame$tinflec2
  dummyFrame$num_timing1b <- dummyFrame$shift * dummyFrame$tinflec2
  dummyFrame$num_timing3 <- dummyFrame$shift^3 * dummyFrame$tinflec2
  return(dummyFrame)
}
##########################################################################################################################
#################################### in the beginning ax+e and then dx3+cx2+bx+e my way ###########################################
linear_third_pw_myway <- function(dummyFrame,inf_point) {
  dummyFrame$tinflec1 <- 0
  dummyFrame$tinflec1[dummyFrame$num_timing<inf_point] <- 1
  dummyFrame$tinflec2 <- 0
  dummyFrame$tinflec2[dummyFrame$num_timing>=inf_point] <- 1
  
  dummyFrame$shift <- dummyFrame$num_timing - inf_point
  dummyFrame$num_timing1 <- dummyFrame$num_timing * dummyFrame$tinflec1 + inf_point * dummyFrame$tinflec2
  dummyFrame$num_timing2 <- dummyFrame$shift^2 * dummyFrame$tinflec2
  dummyFrame$num_timing1b <- dummyFrame$shift * dummyFrame$tinflec2
  dummyFrame$num_timing3 <- dummyFrame$shift^3 * dummyFrame$tinflec2
  return(dummyFrame)
}
#########################################################################################################################
################################################ 3rd order polynomial regression ########################################
third_order <- function(dummyFrame) {
  dummyFrame$num_timing3 <- dummyFrame$num_timing^3
  dummyFrame$num_timing2 <- dummyFrame$num_timing^2
  return(dummyFrame)
}
#########################################################################################################################
################################################ 4th order polynomial regression ########################################
fourth_order <- function(dummyFrame) {
  dummyFrame$num_timing3 <- dummyFrame$num_timing^4
  dummyFrame$num_timing3 <- dummyFrame$num_timing^3
  dummyFrame$num_timing2 <- dummyFrame$num_timing^2
  return(dummyFrame)
}
#########################################################################################################################
########################################## 2-piece linear regression- traditional way ###################################
piece_2trad <- function(dummyFrame, inf_point) {
  dummyFrame$shift <- dummyFrame$num_timing - inf_point
  dummyFrame$tinflec <- 1
  dummyFrame$tinflec[dummyFrame$num_timing<inf_point] <- 0
  
  dummyFrame$num_timing1b <- dummyFrame$shift*dummyFrame$tinflec
  return(dummyFrame)
}
#########################################################################################################################
########################################## 2 -piece linear regression - my way ##########################################
piece_2myway <- function(dummyFrame, inf_point) {
  dummyFrame$shift <- dummyFrame$num_timing - inf_point
  dummyFrame$tinflec1 <- 0
  dummyFrame$tinflec1[dummyFrame$num_timing<inf_point] <- 1
  dummyFrame$tinflec2 <- 0
  dummyFrame$tinflec2[dummyFrame$num_timing>=inf_point] <- 1
  
  dummyFrame$num_timing1a <- dummyFrame$num_timing*dummyFrame$tinflec1
  dummyFrame$num_timing1b <- dummyFrame$shift*dummyFrame$tinflec2
  dummyFrame$num_timing1aI2 <- dummyFrame$num_timing*dummyFrame$tinflec1 + inf_point * dummyFrame$tinflec2
  return(dummyFrame)
}
#########################################################################################################################
#################################### variables for distinct intercepts modeling #########################################
dist_intercepts <- function(dummyFrame) {
  dummyFrame$time01 <- dummyFrame$time0 + dummyFrame$time1
  dummyFrame$time12 <- dummyFrame$time1 + dummyFrame$time2
  dummyFrame$time23 <- dummyFrame$time2 + dummyFrame$time3
  dummyFrame$time34 <- dummyFrame$time3 + dummyFrame$time4
  dummyFrame$time45 <- dummyFrame$time4 + dummyFrame$time5
  #dummyFrame$time56 <- dummyFrame$time5 + dummyFrame$time6 # only for t-cells
  
  dummyFrame$time012 <- dummyFrame$time0 + dummyFrame$time1 + dummyFrame$time2
  dummyFrame$time123 <- dummyFrame$time1 + dummyFrame$time2 + dummyFrame$time3
  dummyFrame$time234 <- dummyFrame$time2 + dummyFrame$time3 + dummyFrame$time4
  dummyFrame$time345 <- dummyFrame$time3 + dummyFrame$time4 + dummyFrame$time5
  #dummyFrame$time456 <- dummyFrame$time4 + dummyFrame$time5 + dummyFrame$time6 # only for t-cells
  
  dummyFrame$time0123 <- dummyFrame$time0 + dummyFrame$time1 + dummyFrame$time2 + dummyFrame$time3
  dummyFrame$time1234 <- dummyFrame$time1 + dummyFrame$time2 + dummyFrame$time3 + dummyFrame$time4
  dummyFrame$time2345 <- dummyFrame$time2 + dummyFrame$time3 + dummyFrame$time4 + dummyFrame$time5
  #dummyFrame$time3456 <- dummyFrame$time3 + dummyFrame$time4 + dummyFrame$time5 + dummyFrame$time6 # only for t-cells
  
  dummyFrame$time01234 <- dummyFrame$time0 + dummyFrame$time1 + dummyFrame$time2 + dummyFrame$time3 + dummyFrame$time4
  dummyFrame$time12345 <- dummyFrame$time1 + dummyFrame$time2 + dummyFrame$time3 + dummyFrame$time4 + dummyFrame$time5
  #dummyFrame$time23456 <- dummyFrame$time2 + dummyFrame$time3 + dummyFrame$time4 + dummyFrame$time5 + dummyFrame$time6 # only for t-cells
  
  #dummyFrame$time123456 <- dummyFrame$time1 + dummyFrame$time2 + dummyFrame$time3 + dummyFrame$time4 + dummyFrame$time5 + dummyFrame$time6 # only for t-cells
  
  return(dummyFrame)
}
#########################################################################################################################
######################################## 3-piece linear regression  - traditional way ###################################
piece_3trad <- function(dummyFrame,inf_point1,inf_point2) {
  
dummyFrame$shift1 <- dummyFrame$num_timing - inf_point1
dummyFrame$shift2 <- dummyFrame$num_timing - inf_point2
dummyFrame$tinflec1 <- 1
dummyFrame$tinflec1[dummyFrame$num_timing<inf_point1] <- 0
dummyFrame$tinflec2 <- 1
dummyFrame$tinflec2[dummyFrame$num_timing<inf_point2] <- 0

## bu kisim deneme
#dummies$tinflec1 <- 0
#dummies$tinflec1[dummies$num_timing<knotp1] <- 1
#dummies$tinflec2 <- 0
#dummies$tinflec2[knotp1<=dummies$num_timing & dummies$num_timing<knotp2] <- 1
#dummies$tinflec3 <- 0
#dummies$tinflec3[knotp2<=dummies$num_timing] <- 1

dummyFrame$num_timing1a <- dummyFrame$num_timing
dummyFrame$num_timing1b <- dummyFrame$shift1 * dummyFrame$tinflec1
dummyFrame$num_timing1c <- dummyFrame$shift2 * dummyFrame$tinflec2
return(dummyFrame)
}
#########################################################################################################################