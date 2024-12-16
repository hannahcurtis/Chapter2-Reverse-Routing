# smoothing function (can change weights)
smooth_elevation <- function(elevation, weights=c(1/3,1/3,1/3)) {
  
  moving_avg_elevation <- rep(0,length(weights))
  for (i in 1:length(weights)) {
    j <- length(elevation) - (length(weights) - i)
    moving_avg_elevation <- moving_avg_elevation + weights[i]*elevation[i:j]
  }
  return(moving_avg_elevation)
}

#Elevation and storage capacity relationships:
# elevation values from 0 to 7 ft by 0.1 ft increments
elevation_relationship_ft <- matrix(c(seq(70)*0.1))

# elevation and storage capacity
# assume ponds are oval cylindrical bowls
calc_pond_volume <- function(elevation, L_basin, W_basin) {
  return(pi * elevation * L_basin/2 * W_basin/2)
}

# Outflow Equations

#DobA5 and Garner
# V-notch weir function
# H_0 is from the sensor to the bottom of the weir
# h_k is a weir parameter
# C_d is weir coefficient
# theta is the weir angle
calc_outflow_vweir <- function(elevation, H_0, h_k, C_d, theta) {
  outflow <- (8/15)*C_d*sqrt(2*32.2)*tan(theta/2)*(((na.omit(elevation)-H_0)+h_k)^(5/2))
  for (i in 1:length(na.omit(elevation))) {
    if (na.omit(elevation)[i] < H_0) {
      outflow[i] <- 0 
    } 
  }
  return(outflow)
}

# For Manitou pond specifically
# V-notch weir + rectangular weir
# H_0 is from the sensor to the bottom of the weir
# H_1 is from the bottom of the v-notch weir to the bottom of the rectangular weir section
# h_k, C_d, L_w, and L_k are weir parameters/coefficients
calc_outflow_manitou <- function(elevation, H_0, H_1, h_k, C_d, theta, L_w) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < (H_0+H_1)) {
      outflow[i] <- (8/15)*C_d*sqrt(2*32.2)*tan(theta/2)*(((na.omit(elevation[i])-H_0)+h_k)^(5/2)) # v-notch weir
    } else {
      outflow[i] <- (8/15)*C_d*sqrt(2*32.2)*tan(theta/2)*((H_1+h_k)^(5/2)) +          
        3.0888*L_w*((na.omit(elevation[i])-(H_1+H_0))^(3/2)) # v-notch weir (at H_1) and rectangular weir
    }
  }
  return(outflow)
}

#Elver, Door Creek, Comm Ave, Greentree (assuming water level never gets to top of structure so treat it as a rect weir)
# Rectangular weir function
# H_0 is from the sensor to the bottom of the weir
# L_w is the length of the weir
# L_k is weir parameter
# C_d is a weir coefficient
calc_outflow_rweir <- function(elevation, H_0, L_w, L_k, C_d) {
  outflow <- (3/2)*C_d*sqrt(2*32.2)*(L_w+L_k)*(((elevation-H_0)+0.00328)^(3/2))
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0 
    } 
  }
  return(outflow)
}

# Simplified broad-crested rectangular weir
# H_0 is from the sensor to the bottom of the weir
# L_w is the length of the weir
calc_outflow_broadweir <- function(elevation, H_0, L_w) {
  outflow <- 3.0888*L_w*((elevation-H_0)^(3/2))
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0 
    } 
  }
  return(outflow)
}

#DobA8
# Rectangular weir function
# H_0 is from the sensor to the bottom of the small rectangular opening
# H_1 is from the sensor to the top of the small rectangular opening
# H_2 is from the sensor to the bottom of the large rectangular weir
# L_w is the length of the weir
# L_k is weir parameter
# C_d is a weir coefficient
calc_outflow_doba8 <- function(elevation, H_0, H_1, H_2, L_w_1, L_w_2, L_k, C_d) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_1) {
      outflow[i] <- (3/2)*C_d*sqrt(2*32.2)*(L_w_1+L_k)*(((elevation[i]-H_0)+0.00328)^(3/2)) # rectangular weir
    } else if (elevation[i] < H_2) {
      outflow[i] <- (3/2)*C_d*sqrt(2*32.2)*(L_w_1+L_k)*(((H_1-H_0)+0.00328)^(3/2))
    } else {    
      outflow[i] <- (3/2)*C_d*sqrt(2*32.2)*(L_w_1+L_k)*(((H_1-H_0)+0.00328)^(3/2)) + (3/2)*C_d*sqrt(2*32.2)*(L_w_2+L_k)*(((elevation[i]-H_2)+0.00328)^(3/2)) # small rectangular opening (up to H_1) and overflow rectangular weir (above H_2)
    }
  }
  return(outflow)
} 

calc_outflow_doba8_2 <- function(elevation, H_0, H_1, H_2, L_w_1, L_w_2) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_1) {
      outflow[i] <- 3.0888*L_w_1*((elevation[i]-H_0)^(3/2)) # rectangular weir
    } else if (elevation[i] < H_2) {
      outflow[i] <- 3.0888*L_w_1*((H_1-H_0)^(3/2))
    } else {    
      outflow[i] <- 3.0888*L_w_1*((H_1-H_0)^(3/2)) + 3.0888*L_w_2*((elevation[i]-H_2)^(3/2)) # small rectangular opening (up to H_1) and overflow rectangular weir (above H_2)
    }
  }
  return(outflow)
} 

#Midtown
# Rectangular weir function
# H_0 is from the sensor to the bottom of the weir
# H_1 is from the sensor to the bottom of the larger overflow rectangular weir
# L_w is the length of the weir
# L_k is weir parameter
# C_d is a weir coefficient
calc_outflow_midtown <- function(elevation, H_0, H_1, L_w_1, L_w_2, L_k, C_d) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_1) {
      outflow[i] <- (3/2)*C_d*sqrt(2*32.2)*(L_w_1+L_k)*(((elevation[i]-H_0)+0.00328)^(3/2)) # rectangular weir
    } else {
      outflow[i] <- (3/2)*C_d*sqrt(2*32.2)*(L_w_1+L_k)*(((H_1-H_0)+0.00328)^(3/2)) +          
        (3/2)*C_d*sqrt(2*32.2)*(L_w_2+L_k)*(((elevation[i]-H_1)+0.00328)^(3/2)) # small rectangular weir (up to H_1) and overflow rectangular weir (above H_1)
    }
  }
  return(outflow)
} 

calc_outflow_midtown_2 <- function(elevation, H_0, H_1, L_w_1, L_w_2) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_1) {
      outflow[i] <- 3.0888*L_w_1*((elevation[i]-H_0)^(3/2)) # rectangular weir
    } else {
      outflow[i] <- 3.0888*L_w_1*((H_1-H_0)^(3/2)) +          
        3.0888*L_w_1*((elevation[i]-H_1)^(3/2)) # small rectangular weir (up to H_1) and overflow rectangular weir (above H_1)
    }
  }
  return(outflow)
} 

#HP Church
# Rectangular weir function
# H_0 is from the sensor to the bottom of the pipes
# H_1 is from the sensor to the top of the pipes
# H_2 is from the sensor to the bottom of the rectangular weir
# H_3 is from the sensor to the bottom of the larger overflow rectangular weir
# L_w is the length of the weir
# L_k is weir parameter
# C_d is a weir coefficient
calc_outflow_hpchurch <- function(elevation, H_0, H_1, H_2, H_3, L_w_1, L_w_2, L_k, C_d, B) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_1) {
      outflow[i] <- 3*((2/3)*B*(elevation[i]-H_0)*sqrt((2/3)*32.2*(elevation[i]-H_0))) # 3 partially full orifices
    } else if (elevation[i] < H_2) {
      outflow[i] <- 0.8*B*(H_1-H_0)*sqrt(2*32.2*((elevation[i]-H_0)-(0.8*(H_1-H_0)))) # 3 full orifices
    } else if (elevation[i] < H_3) {
      outflow[i] <- 0.8*B*(H_1-H_0)*sqrt(2*32.2*((elevation[i]-H_0)-(0.8*(H_1-H_0)))) + (3/2)*C_d*sqrt(2*32.2)*(L_w_1+L_k)*(((elevation-H_2)+0.00328)^(3/2)) # 3 full orifices and rectangular weir
    } else {
      outflow[i] <- 0.8*B*(H_1-H_0)*sqrt(2*32.2*((elevation[i]-H_0)-(0.8*(H_1-H_0)))) + (3/2)*C_d*sqrt(2*32.2)*(L_w_1+L_k)*(((H_3-H_2)+0.00328)^(3/2)) +          
        (3/2)*C_d*sqrt(2*32.2)*(L_w_2+L_k)*(((elevation-H_3)+0.00328)^(3/2)) # 3 full orifices, small rectangular weir (up to H_3), and overflow rectangular weir (above H_3)
    }
    return(outflow)
  }
}

calc_outflow_hpchurch_2 <- function(elevation, H_0, H_1, H_2, H_3, L_w_1, L_w_2, C_d, B) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_1) {
      outflow[i] <- 0
    } else if (elevation[i] < H_2) {
      outflow[i] <- 0
    } else if (elevation[i] < H_3) {
      outflow[i] <-3.0888*L_w_1*((elevation[i]-H_2)^(3/2)) # 3 full orifices and rectangular weir
    } else {
      outflow[i] <-3.0888*L_w_1*((H_3-H_2)^(3/2)) +          
        3.0888*L_w_2*((elevation[i]-H_3)^(3/2)) # 3 full orifices, small rectangular weir (up to H_3), and overflow rectangular weir (above H_3)
    }
  }
  return(outflow)
}

#Baxter
calc_outflow_baxter <- function(elevation, H_0, H_1, H_2, slope, D, L_w, L_k, C_d, C_c, L) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_0 + D) {
      theta_pipe <- (2*acos(((D/2)-(D-(elevation[i]-H_0)))/(D/2))) # radians
      area_open <- (pi*((D/2)^2))-(((D/2)^2*(theta_pipe-sin(theta_pipe)))/2)
      hydraulic_radius_open <- area_open/((2*pi*(D/2))-((D/2)*theta_pipe))
      outflow[i] <- (1.49/0.012)*area_open*(hydraulic_radius_open^(2/3))*((slope/100)^(1/2))
    } else if (elevation[i] < H_1) {
      area_full <- pi*((D/2)^2)
      hydraulic_radius_full <- area_full/((2*pi*(D/2))-((D/2)))
      outflow[i] <- area_full*1.318*120*(hydraulic_radius_full^0.63)*((slope/100)^0.54)
    } else if (elevation[i] < H_2) {
      area_full <- pi*((D/2)^2)
      hydraulic_radius_full <- area_full/((2*pi*(D/2))-((D/2)))
      outflow[i] <- area_full*1.318*120*(hydraulic_radius_full^0.63)*((slope/100)^0.54) + (3/2)*C_d*sqrt(2*32.2)*(L_w+L_k)*(((elevation[i]-H_1)+0.00328)^(3/2))
    } else {
      area_full <- pi*((D/2)^2)
      hydraulic_radius_full <- area_full/((2*pi*(D/2))-((D/2)))
      outflow[i] <- area_full*1.318*120*(hydraulic_radius_full^0.63)*((slope/100)^0.54) + (3/2)*C_d*sqrt(2*32.2)*(L_w+L_k)*(((elevation[i]-H_1)+0.00328)^(3/2)) + C_c*L*((2*32.2*(elevation[i]-H_2))^(3/2))
    }
  }
  return(outflow)
}


#DobA7
# Trapezoidal weir function
# H_0 is from the sensor to the bottom of the standpipe
# H_1 is from the sensor to the bottom of the weir
# L_w is the length of the weir (bottom)
# C_d/C_a are weir coefficients
# D is diameter of standpipe
calc_outflow_tweir <- function(elevation, H_0, H_1, L_w, C_d, C_a, D) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_1) {  
      outflow[i] <- C_a*pi*D*((2*32.2*(elevation[i]-H_0))^(3/2)) # Grated stand pipe
    } else  {
      outflow[i] <- C_d*sqrt(2*32.2)*L_w*((elevation[i]-H_1)^(3/2)) + C_a*pi*D*((2*32.2*(H_1-H_0))^(3/2)) # Grated stand pipe + trapezoidal weir
    } 
  }
  return(outflow)
}

#Lot 60, Two Fountains, DobA4, Marion-Dunn, Mad City
# Pipe function
# Use Manning's equation for partially full pipe
# Use Hazen-Williams for pipe flow 
# D is diameter of pipe
calc_outflow_pipe <- function(elevation, H_0, slope, D) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    # theta_pipe <- (2*acos(((D/2)-(D-(elevation[i]-H_0)))/(D/2))) # radians
    # area_open <- (pi*((D/2)^2))-(((D/2)^2*(theta_pipe-sin(theta_pipe)))/2)
    # hydraulic_radius_open <- area_open/((2*pi*(D/2))-((D/2)*theta_pipe))
    # area_full <- pi*((D/2)^2)
    # hydraulic_radius_full <- area_full/((2*pi*(D/2))-((D/2)))
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < (H_0 + D)) {
      theta_pipe <- (2*acos(((D/2)-(D-(elevation[i]-H_0)))/(D/2))) # radians
      area_open <- (pi*((D/2)^2))-(((D/2)^2*(theta_pipe-sin(theta_pipe)))/2)
      hydraulic_radius_open <- area_open/((2*pi*(D/2))-((D/2)*theta_pipe))
      outflow[i] <- (1.49/0.012)*area_open*(hydraulic_radius_open^(2/3))*((slope/100)^(1/2))
    } else {
      area_full <- pi*((D/2)^2)
      hydraulic_radius_full <- area_full/((2*pi*(D/2))-((D/2)))
      outflow[i] <- area_full*1.318*120*(hydraulic_radius_full^0.63)*((slope/100)^0.54)
    }
  }
  return(outflow) 
}

# Pipe (culvert) function
# Equations 10-12 and 10-13 from Open Channel Flow textbook
# H_0 is from the sensor to the bottom of the pipe
# H_t is from the sensor to the top of the pipe
# B is the width of the pipe
calc_outflow_culvert <- function(elevation, H_0, H_t, B) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0 # below pipe
    } else if (elevation[i] < 1.2*H_t) {
      outflow[i] <- (2/3)*B*(elevation[i]-H_0)*sqrt((2/3)*32.2*(elevation[i]-H_0)) # pipe partially full
    } else {
      outflow[i] <- 0.8*B*(H_t-H_0)*sqrt(2*32.2*((elevation[i]-H_0)-(0.8*(H_t-H_0)))) #pipe submerged
    }
  }
  return(outflow)
}


# For Owen pond specifically
# Rectangular weir at 12 ft, orifice at 13 ft
calc_outflow_owen <- function(elevation, H_0, H_1, H_2, L_w, L_k, C_d, B) {
  outflow <- elevation
  for (i in 1:length(elevation)) {
    if (elevation[i] < H_0) {
      outflow[i] <- 0
    } else if (elevation[i] < H_1) {
      outflow[i] <- (3/2)*C_d*sqrt(2*32.2)*(L_w+L_k)*(((elevation[i]-H_0)+0.00328)^(3/2))
    } else {
      outflow[i] <- 0.8*B*(H_2-H_0)*sqrt(2*32.2*((elevation[i]-H_0)-(0.8*(H_2-H_0)))) #pipe submerged
    }
  } 
  return(outflow)
}


