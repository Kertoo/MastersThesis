bread.vglm <- function(x, ...) {
  vcov(x, ...) * nobs(x)
}

estfun.vglm <- function(x, ...) {
  X <- model.matrix(x, "vlm")
  w <- weights(x, type = "prior")
  extra <- x@extra
  eta <- x@predictors
  
  res <- eval(x@family@deriv)
  
  res <- do.call(
    cbind,
    lapply(
      1:NCOL(res),
      FUN = function(t) {
        rr <- X[(1:NROW(X) %% NCOL(res)) == (NCOL(res) - t), ]
        rr[, !apply(rr == 0, 2, all)] * res[, t]
      }
    )
  )
  
  res
}

vcovHC.vglm <- function(x, 
                        type = c("HC3", "const", "HC", 
                                 "HC0", "HC1", "HC2", 
                                 "HC4", "HC4m", "HC5"), 
                        omega = NULL, 
                        sandwich = TRUE, 
                        ...) {
  type <- match.arg(type)
  estfun <- estfun.vglm(x, ...)
  
  X <- model.matrix(x, "vlm")
  w <- weights(x, type = "prior")
  
  n <- nobs(x)
  k <- NCOL(X)
  
  df <- df.residual(x)
  hat <- as.vector(hatvalues(x, ...))
  
  extra <- x@extra
  eta <- x@predictors
  res <- eval(x@family@deriv) |> as.vector()
  
  if (is.null(omega)) {
    if (type == "HC") 
      type <- "HC0"
    switch(type, const = {
      omega <- function(residuals, diaghat, df) rep(1, length(residuals)) * sum(residuals^2)/df
    }, HC0 = {
      omega <- function(residuals, diaghat, df) residuals^2
    }, HC1 = {
      omega <- function(residuals, diaghat, df) residuals^2 * 
        length(residuals)/df
    }, HC2 = {
      omega <- function(residuals, diaghat, df) residuals^2/(1 - diaghat)
    }, HC3 = {
      omega <- function(residuals, diaghat, df) residuals^2/(1 - diaghat)^2
    }, HC4 = {
      omega <- function(residuals, diaghat, df) {
        n <- length(residuals)
        p <- as.integer(round(sum(diaghat), digits = 0))
        delta <- pmin(4, n * diaghat/p)
        residuals^2/(1 - diaghat)^delta
      }
    }, HC4m = {
      omega <- function(residuals, diaghat, df) {
        gamma <- c(1, 1.5)
        n <- length(residuals)
        p <- as.integer(round(sum(diaghat), digits = 0))
        delta <- pmin(gamma[1], n * diaghat/p) + pmin(gamma[2], 
                                                      n * diaghat/p)
        residuals^2/(1 - diaghat)^delta
      }
    }, HC5 = {
      omega <- function(residuals, diaghat, df) {
        k <- 0.7
        n <- length(residuals)
        p <- as.integer(round(sum(diaghat), digits = 0))
        delta <- pmin(n * diaghat/p, pmax(4, n * k * max(diaghat)/p))
        residuals^2 / sqrt((1 - diaghat)^delta)
      }
    })
  }
  if (is.function(omega)) 
    omega <- omega(res, hat, df)
  # Fixing convention
  rval <- sqrt(omega) * do.call(
    rbind,
    lapply(
      1:NCOL(eta),
      FUN = function(t) {
        X[(1:NROW(X) %% NCOL(eta)) == (NCOL(eta) - t), ]
      }
    )
  )
  rval <- crossprod(rval)/n
  
  if (sandwich) 
    rval <- sandwich(x, meat. = rval, .bread = bread.vglm(x, ...), ...)
  rval
}

# test that this works
if (FALSE) {
  N <- 200
  x <- sin(runif(n = N))
  x1 <- rbinom(n = N, prob = .5, size = 1)
  y <- x * (1 + x1) - 1 + 
    rnorm(n = N, sd = exp(.5 + 1 * x1 + rnorm(N, mean = 0, sd = .25)))
  df <- data.frame(x = x, y = y, x1 = x1)
  
  m2 <- vglm(
    y ~ x * x1, 
    data = df, 
    family = uninormal(zero = NULL),
    constraints = list(
      "(Intercept)" = diag(2),
      "x" = rbind(1, 0),
      "x1" = rbind(0, 1),
      "x:x1" = rbind(1, 0)
    )
  )
  
  vcov(m2)
  vcovHC(m2, type = "HC0")
  vcovHC(m2, type = "HC1")
  vcovHC(m2, type = "HC2")
  vcovHC(m2, type = "HC3")
  vcovHC(m2, type = "HC4")
  vcovHC(m2, type = "HC4m")
  vcovHC(m2, type = "HC5")
}

# Defining methods under different names due to issues with parallel

AA <- function(x, ...) {
  vcov(x, ...) * nobs(x)
}

AA1 <- function(x, ...) {
  X <- model.matrix(x, "vlm")
  w <- weights(x, type = "prior")
  extra <- x@extra
  eta <- x@predictors
  
  res <- eval(x@family@deriv)
  
  res <- do.call(
    cbind,
    lapply(
      1:NCOL(res),
      FUN = function(t) {
        rr <- X[(1:NROW(X) %% NCOL(res)) == (NCOL(res) - t), ]
        rr[, !apply(rr == 0, 2, all)] * res[, t]
      }
    )
  )
  
  res
}

AA2 <- function(x, 
                type = c("HC3", "const", "HC", 
                         "HC0", "HC1", "HC2", 
                         "HC4", "HC4m", "HC5"), 
                omega = NULL, 
                sandwich = TRUE, 
                ...) {
  type <- match.arg(type)
  estfun <- AA1(x, ...)
  
  X <- model.matrix(x, "vlm")
  w <- weights(x, type = "prior")
  
  n <- nobs(x)
  k <- NCOL(X)
  
  df <- df.residual(x)
  hat <- as.vector(hatvalues(x, ...))
  
  extra <- x@extra
  eta <- x@predictors
  res <- eval(x@family@deriv) |> as.vector()
  
  if (is.null(omega)) {
    if (type == "HC") 
      type <- "HC0"
    switch(type, const = {
      omega <- function(residuals, diaghat, df) rep(1, length(residuals)) * sum(residuals^2)/df
    }, HC0 = {
      omega <- function(residuals, diaghat, df) residuals^2
    }, HC1 = {
      omega <- function(residuals, diaghat, df) residuals^2 * 
        length(residuals)/df
    }, HC2 = {
      omega <- function(residuals, diaghat, df) residuals^2/(1 - diaghat)
    }, HC3 = {
      omega <- function(residuals, diaghat, df) residuals^2/(1 - diaghat)^2
    }, HC4 = {
      omega <- function(residuals, diaghat, df) {
        n <- length(residuals)
        p <- as.integer(round(sum(diaghat), digits = 0))
        delta <- pmin(4, n * diaghat/p)
        residuals^2/(1 - diaghat)^delta
      }
    }, HC4m = {
      omega <- function(residuals, diaghat, df) {
        gamma <- c(1, 1.5)
        n <- length(residuals)
        p <- as.integer(round(sum(diaghat), digits = 0))
        delta <- pmin(gamma[1], n * diaghat/p) + pmin(gamma[2], 
                                                      n * diaghat/p)
        residuals^2/(1 - diaghat)^delta
      }
    }, HC5 = {
      omega <- function(residuals, diaghat, df) {
        k <- 0.7
        n <- length(residuals)
        p <- as.integer(round(sum(diaghat), digits = 0))
        delta <- pmin(n * diaghat/p, pmax(4, n * k * max(diaghat)/p))
        residuals^2 / sqrt((1 - diaghat)^delta)
      }
    })
  }
  if (is.function(omega)) 
    omega <- omega(res, hat, df)
  # Fixing convention
  rval <- sqrt(omega) * do.call(
    rbind,
    lapply(
      1:NCOL(eta),
      FUN = function(t) {
        X[(1:NROW(X) %% NCOL(eta)) == (NCOL(eta) - t), ]
      }
    )
  )
  rval <- crossprod(rval)/n
  
  if (sandwich) 
    rval <- 1/n * (AA(x, ...) %*% rval %*% AA(x, ...))
  rval
}