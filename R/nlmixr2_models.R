#' nlmixr2 model: 1-cmt linear model (oral)
#' 
#' @export
nlmixr2_pk_1cmt_oral_linear <- function() {
  ini({
    tka     <- log(0.5)
    tCL     <- log(5) 
    tV      <- log(50)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_ka   ~ 0.3
    prop_sd <- 0.3
  })
  model({
    ka <- exp(tka + eta_ka)
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    # d/dt(A1) = -ka*A1
    # d/dt(A2) = -(CL/V)*A1 + ka*A1
    # Cp = A2 / V
    # Cp ~ prop(prop_sd)
    linCmt() ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 1-cmt linear model (iv)
#' 
#' @export
nlmixr2_pk_1cmt_iv_linear <- function() {
  ini({
    tCL     <- log(5) 
    tV      <- log(50)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    prop_sd <- 0.15
  })
  model({
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    # d/dt(A1) = -Ka*A1
    # d/dt(A2) = -(CL/V)*A1 + Ka*A1
    # Cp = A2 / V
    # Cp ~ prop(prop_sd) + add(add_sd)
    linCmt() ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 2-cmt linear model (oral)
#' 
#' @export
nlmixr2_pk_2cmt_oral_linear <- function() {
  ini({
    tka     <- log(0.5)
    tCL     <- log(5) 
    tV      <- log(50)
    tQ      <- log(10) 
    tV2     <- log(100)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_Q    ~ 0.3
    eta_V2   ~ 0.3
    eta_ka   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    ka <- exp(tka + eta_ka)
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    Q  <- exp(tQ  + eta_Q)
    V2 <- exp(tV2 + eta_V2)
    # d/dt(A1) = -Ka*A1
    # d/dt(A2) = -(CL/V)*A1 + Ka*A1
    # Cp = A2 / V
    # Cp ~ prop(prop_sd) + add(add_sd)
    linCmt() ~ prop(prop_sd)
  })
}

#' nlmixr2 model: 2-cmt linear model (iv)
#' 
#' @export
nlmixr2_pk_2cmt_iv_linear <- function() {
  ini({
    tCL     <- log(5) 
    tV      <- log(50)
    tQ      <- log(10) 
    tV2     <- log(100)
    eta_CL   ~ 0.3
    eta_V    ~ 0.3
    eta_Q    ~ 0.3
    eta_V2   ~ 0.3
    prop_sd <- 0.15
  })
  model({
    CL <- exp(tCL + eta_CL)
    V  <- exp(tV  + eta_V)
    Q  <- exp(tQ  + eta_Q)
    V2 <- exp(tV2 + eta_V2)
    # d/dt(A1) = -Ka*A1
    # d/dt(A2) = -(CL/V)*A1 + Ka*A1
    # Cp = A2 / V
    # Cp ~ prop(prop_sd) + add(add_sd)
    linCmt() ~ prop(prop_sd)
  })
}
