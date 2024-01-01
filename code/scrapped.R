#### Dixon coles model ####

dixoncolesff <- function() {
  new("vglmff",
      blurb = c("Dixon coles distribution\n", 
                "Links:    ",
                "log(alpha1)", ", ", "log(alpha2)", 
                ", ", "log(beta1)", ", ", "log(beta2)",
                ", ", "log(gamma)", ", ", "rhobit(rho)"), 
      deviance = function(mu, y, w, residuals = FALSE, eta, extra = NULL, summation = TRUE) { 
        #TODO:: This may not be needed
        # devy <- -log(y) - 1 
        # devmu <- -log(mu) -y/mu 
        # devi <- 2 * (devy - devmu)
        # 
        # if (residuals) {
        #   sign(y - mu) * sqrt(abs(devi) * c(w))
        # } else {
        #   dev.elts <- c(w) * devi 
        #   if (summation) sum(dev.elts) else dev.elts
        # }
        NULL
      },
      loglikelihood = function(mu, y, w, residuals = FALSE, eta, extra = NULL, summation = TRUE) { 
        alpha1 <- eta2theta(
          eta[, 1, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        alpha2 <- eta2theta(
          eta[, 2, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        beta1 <- eta2theta(
          eta[, 3, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        beta2 <- eta2theta(
          eta[, 4, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        gamma <- eta2theta(
          eta[, 5, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        rho <- eta2theta(
          eta[, 6, drop = FALSE], "rhobitlink", 
          list(theta = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        if (residuals) {
          stop("Not implemented")
        } else {
          lambda1 <- alpha1 * beta2 * gamma
          lambda2 <- alpha2 * beta1
          
          tau <- ifelse(
            y[, 1] > 1 | y[, 2] > 1, 1,
            ifelse(
              y[, 1] == 1,
              ifelse(
                y[,2] == 1, 1 - rho,
                1 + lambda2 * rho
              ),
              ifelse(
                y[,2] == 1, 1 + lambda1 * rho,
                1 - lambda1 * lambda2 * rho
              )
            )
          )
          
          ll <- c(w) * (log(tau) + 
                          dpois(x = y[, 1], lambda = lambda1, log = TRUE) + 
                          dpois(x = y[, 2], lambda = lambda2, log = TRUE))
          
          if (summation) sum(ll) else ll
        }
      }, 
      initialize = expression({
        if (NCOL(y) != 2) stop("This model only works on bivariate arguments")
        
        if (criterion == "deviance") criterion <- "loglikelihood"
        extra$type.fitted <- "mean"
        extra$colnames.y <- colnames(y)
        
        extra$M1 <- M <- M1 <- 6
        extra$ncoly <- ncoly <- ncol(y)
        
        predictors.names <- c("log(alpha1)", "log(alpha2)", 
                              "log(beta1)",  "log(beta2)",
                              "log(gamma)", "rhobit(rho)")
        
        #change this
        # etastart <- cbind(log(pmax(1, y[,1])), log(pmax(1, y[,2])), 0, 0, 0,
        # rhobitlink(cor(y[,1], y[,2])))
        
        etastart <- cbind(
          log(pmax(1, y[,1])), 
          log(pmax(1, y[,2])), 
          log(pmax(1, mean(y[,1]))), 
          log(pmax(1, mean(y[,2]))), 
          1,
          rhobitlink(cor(y[,1], y[,2]))
        )
        
        etastart
      }), 
      linkinv = function(eta, extra = NULL) {
        type.fitted <- if (length(extra$type.fitted)) 
          extra$type.fitted
        else {
          type.fitted <- "mean"
          warning("cannot find 'type.fitted'. ", "Returning the 'mean'.")
          "mean"
        }
        type.fitted <- match.arg(type.fitted, c("mean", "home_adv", "attack", "defence", "tau"))[1]
        alpha1 <- eta2theta(
          eta[, 1, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        alpha2 <- eta2theta(
          eta[, 2, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        beta1 <- eta2theta(
          eta[, 3, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        beta2 <- eta2theta(
          eta[, 4, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        gamma <- eta2theta(
          eta[, 5, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        rho <- eta2theta(
          eta[, 6, drop = FALSE], "rhobitlink", 
          list(theta = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        
        mu <- cbind(alpha1 * beta2 * gamma, alpha2 * beta1)
        colnames(mu) <- c("Home", "Away")
        
        gamma <- as.matrix(gamma)
        colnames(gamma) <- "Home-advantage"
        
        # tau <- (1 - dpois(0, lambda = lambda1) - dpois(1, lambda = lambda1)) * 
        #   (1 - dpois(0, lambda = lambda2) - dpois(1, lambda = lambda2)) +
        #   (1 - rho) ^ 2 * dpois(1, lambda = lambda1) * dpois(1, lambda = lambda2) +
        #   (1 - lambda1 * lambda2 * rho) ^ 2 * dpois(0, lambda = lambda1) * 
        #   dpois(0, lambda = lambda2) + (1 + lambda1 * rho) ^ 2 *
        #   dpois(0, lambda = lambda1) * dpois(1, lambda = lambda2) + 
        #   (1 + lambda2 * rho) ^ 2 * dpois(1, lambda = lambda1) * dpois(0, lambda = lambda2)
        # tau <- as.matrix(tau)
        # colnames(tau) <- "E[tau]"
        
        ans <- switch(type.fitted, 
                      mean = mu,
                      home_adv = gamma,
                      attack = mu[, 1, drop = FALSE],
                      defence = mu[, 2, drop = FALSE],
                      tau = 1)
        
        ans
      },
      last = expression({
        misc$link <- rep(c("loglink", "rhobit"), c(5, 1))
      }), 
      linkfun = function() NULL, 
      vfamily = "Dixon_coles", 
      deriv = expression({
        # values
        alpha1 <- eta2theta(
          eta[, 1, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        alpha2 <- eta2theta(
          eta[, 2, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        beta1 <- eta2theta(
          eta[, 3, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        beta2 <- eta2theta(
          eta[, 4, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        gamma <- eta2theta(
          eta[, 5, drop = FALSE], "loglink", 
          list(theta = NULL, bvalue = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        rho <- eta2theta(
          eta[, 6, drop = FALSE], "rhobitlink", 
          list(theta = NULL, inverse = FALSE, deriv = 0, 
               short = TRUE, tag = FALSE)
        )
        # derivatives of inv links
        dalpha1.deta <- dtheta.deta(alpha1, "loglink")
        dalpha2.deta <- dtheta.deta(alpha2, "loglink")
        dbeta1.deta  <- dtheta.deta(beta1,  "loglink")
        dbeta2.deta  <- dtheta.deta(beta2,  "loglink")
        dgamma.deta  <- dtheta.deta(gamma,  "loglink")
        drho.deta    <- dtheta.deta(rho,    "rhobitlink")
        
        lambda1 <- alpha1 * beta2 * gamma
        lambda2 <- alpha2 * beta1
        
        tau <- ifelse(
          y[, 1] > 1 | y[, 2] > 1, 1,
          ifelse(
            y[, 1] == 1,
            ifelse(
              y[,2] == 1, 1 - rho,
              1 + lambda2 * rho
            ),
            ifelse(
              y[,2] == 1, 1 + lambda1 * rho,
              1 - lambda1 * lambda2 * rho
            )
          )
        )
        
        dl.dalpha1 <- y[,1] / alpha1 - beta2 * gamma + 
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -rho * beta2 * gamma * alpha2 * beta1,
                 ifelse(y[, 1] == 0 & y[, 2] == 1, 
                        rho * beta2 * gamma, 0)) / tau
        
        dl.dalpha2 <- y[,2] / alpha2 - beta1 +
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -rho * alpha1 * beta2 * gamma * beta1,
                 ifelse(y[, 1] == 1 & y[, 2] == 0, 
                        rho * beta1, 0)) / tau
        
        dl.dbeta1 <- y[,2] / beta1 - alpha2 + 
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -rho * alpha1 * beta2 * gamma * alpha2,
                 ifelse(y[, 1] == 1 & y[, 2] == 0, 
                        rho * alpha2, 0)) / tau
        
        dl.dbeta2 <- y[,1] / beta2 - alpha1 * gamma +
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -rho * alpha1 * gamma * alpha2 * beta1,
                 ifelse(y[, 1] == 0 & y[, 2] == 1, 
                        rho * alpha1 * gamma, 0)) / tau
        
        dl.dgamma <- y[,1] / gamma - alpha1 * beta2 +
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -rho * alpha1 * beta2 * alpha2 * beta1,
                 ifelse(y[, 1] == 0 & y[, 2] == 1, 
                        rho * alpha1 * beta2, 0)) / tau
        
        dl.drho <- 
          ifelse(y[, 1] == 0 & y[, 2] == 0,
                 -alpha1 * beta2 * gamma * alpha2 * beta1,
                 ifelse(y[, 1] == 0 & y[, 2] == 1,
                        alpha1 * beta2 * gamma,
                        ifelse(y[, 1] == 1 & y[, 2] == 0,
                               beta1 * alpha2,
                               ifelse(y[, 1] == 1 & y[, 2] == 1, -1, 0)))) / tau
        
        # first derivative w.r. to eta's
        c(w) * cbind(
          dl.dalpha1 * dalpha1.deta,
          dl.dalpha2 * dalpha2.deta,
          dl.dbeta1  * dbeta1.deta,
          dl.dbeta2  * dbeta2.deta,
          dl.dgamma  * dgamma.deta,
          dl.drho    * drho.deta
        )
      }), 
      weight = expression({ 
        wz <- matrix(0, n, 21)
        
        #const2 <- (1 - rho * alpha1 * beta2 * alpha2 * beta1 * gamma) ^ -1
        lambda1 <- alpha1 * beta2 * gamma
        lambda2 <- beta1 * alpha2
        # rho * alpha1 * beta2 * alpha2 * beta1 * gamma
        
        const1 <- exp(-(lambda1 + lambda2))
        
        #Ed2l.dalpha1.dalpha1
        ## Today's checkmark
        wz[, iam(1, 1, 6)] <- -beta2 * gamma / alpha1 - const1 * (
          lambda2 * (beta2 * rho * gamma) ^ 2 / (1 + lambda1 * rho) +
            (alpha2 * beta1 * beta2 * rho * gamma) ^ 2 / (1 - lambda1 * lambda2 * rho)
        )
        wz[, iam(1, 1, 6)] <- wz[, iam(1, 1, 6)] * dalpha1.deta ^ 2
        #Ed2l.dalpha1.dalpha2
        wz[, iam(1, 2, 6)] <- -const1 * 
          (rho * beta2 * beta1 * gamma + 
             (rho * beta2 * beta1 * gamma) ^ 2 * alpha1 * alpha2 / 
             (1 - rho * lambda1 * lambda2))
        wz[, iam(1, 2, 6)] <- wz[, iam(1, 2, 6)] * dalpha2.deta * dalpha1.deta
        #Ed2l.dalpha1.dbeta1
        wz[, iam(1, 3, 6)] <- -const1 * rho * beta2 * gamma * alpha2 - 
          const1 * (rho * beta2 * alpha2 * gamma) ^ 2 * alpha1 * beta1 / 
          (1 - rho * lambda1 * lambda2)
        wz[, iam(1, 3, 6)] <- wz[, iam(1, 3, 6)] * dbeta1.deta * dalpha1.deta
        #Ed2l.dalpha1.dbeta2
        ## Today's checkmark
        wz[, iam(1, 4, 6)] <- -gamma - const1 * (
          (alpha1 * beta2 * (rho * beta1 * alpha2 * gamma) ^ 2 / 
             (1 - lambda1 * lambda2 * rho) + alpha2 * beta1 * rho * gamma) +
            lambda2 * (alpha1 * (rho * gamma) ^ 2 * beta2 / (1 + lambda1 * rho) - rho * gamma)
        )
        wz[, iam(1, 4, 6)] <- wz[, iam(1, 4, 6)] * dbeta2.deta * dalpha1.deta
        #Ed2l.dalpha1.dgamma
        ## Today's checkmark
        wz[, iam(1, 5, 6)] <- -beta2 - const1 * (
          alpha2 * beta1 * beta2 * rho + (rho * beta2 * alpha2 * beta1) ^ 2 * 
            alpha1 * gamma / (1 - lambda1 * lambda2 * rho) +
            lambda2 * (alpha1 * gamma * (beta2 * rho) ^ 2 / (1 + lambda1 * rho) - beta2 * rho)
        )
        wz[, iam(1, 5, 6)] <- wz[, iam(1, 5, 6)] * dgamma.deta * dalpha1.deta
        #Ed2l.dalpha1.drho
        wz[, iam(1, 6, 6)] <- -const1 * (rho * (beta2 * alpha2 * beta1 * gamma) ^ 2 / 
                                           (1 - lambda1 * lambda2 * rho) + beta2 * gamma * alpha2 * beta1) -
          const1 * lambda2 * (rho * alpha1 * (beta2 * gamma) ^ 2 / 
                                (1 + lambda1 * rho) + beta2 * gamma * alpha2 * beta1)
        wz[, iam(1, 6, 6)] <- wz[, iam(1, 6, 6)] * drho.deta * dalpha1.deta
        
        
        #Ed2l.dalpha2.dalpha2
        ## Today's checkmark
        wz[, iam(2, 2, 6)] <- -beta1 / alpha2 - const1 * (
          (rho * alpha1 * beta2 * alpha2 * beta1 * gamma) ^ 2 / 
            (1 - lambda1 * lambda2 * rho) + lambda1 * (beta1 * rho) ^ 2 / 
            (1 + lambda2 * rho)
        )
        wz[, iam(2, 2, 6)] <- wz[, iam(2, 2, 6)] * dalpha2.deta ^ 2
        #Ed2l.dalpha2.dbeta1 
        ## Today's checkmark
        wz[, iam(2, 3, 6)] <- - 1 - const1 * (
          lambda1 * (-alpha2 * beta1 * rho ^ 2 / (1 + lambda2 * rho) + rho) -
            alpha1 * beta2 * rho * gamma - (rho * alpha1 * beta2 * gamma) ^ 2 * 
            alpha2 * beta1 / (1 - lambda1 * lambda2 * rho)
        )
        wz[, iam(2, 3, 6)] <- wz[, iam(2, 3, 6)] * dbeta1.deta * dalpha2.deta
        #Ed2l.dalpha2.dbeta2
        wz[, iam(2, 4, 6)] <- -const1 * (rho * gamma * alpha1 * beta1 +
                                           (rho * alpha1 * beta1 * gamma) ^ 2 * beta2 * alpha2 /
                                           (1 - lambda1 * lambda2 * rho))
        wz[, iam(2, 4, 6)] <- wz[, iam(2, 4, 6)] * dbeta2.deta * dalpha2.deta
        #Ed2l.dalpha2.dgamma
        wz[, iam(2, 5, 6)] <- -const1 * (rho * beta2 * alpha1 * beta1 +
                                           (rho * alpha1 * beta1 * beta2) ^ 2 * alpha2 * gamma /
                                           (1 - lambda1 * lambda2 * rho))
        wz[, iam(2, 5, 6)] <- wz[, iam(2, 5, 6)] * dgamma.deta * dalpha2.deta
        #Ed2l.dalpha2.drho
        wz[, iam(2, 6, 6)] <- -const1 * (gamma * beta2 * alpha1 * beta1 +
                                           (gamma * alpha1 * beta1 * beta2) ^ 2 * alpha2 * rho /
                                           (1 - lambda1 * lambda2 * rho)) +
          lambda2 * const1 * (beta1 - alpha2 * rho * beta1 ^ 2 / (1 + lambda2 * rho))
        wz[, iam(2, 6, 6)] <- wz[, iam(2, 6, 6)] * drho.deta * dalpha2.deta
        
        
        #Ed2l.dbeta1.dbeta1
        wz[, iam(3, 3, 6)] <- -alpha2 * gamma / beta1 - const1 *
          (rho * alpha1 * beta2 * alpha2 * gamma) ^ 2 / 
          (1 - rho * lambda1 * lambda2) - const1 * 
          (rho * alpha2) ^ 2 * alpha1 * beta2 * gamma / (1 + rho * lambda2)
        wz[, iam(3, 3, 6)] <- wz[, iam(3, 3, 6)] * dbeta1.deta ^ 2
        #Ed2l.dbeta1.dbeta2
        wz[, iam(3, 4, 6)] <- -const1 * (rho * gamma * alpha1 * alpha2 +
                                           (rho * alpha1 * alpha2 * gamma) ^ 2  * beta1 * beta2 / 
                                           (1 - rho * lambda1 * lambda2))
        wz[, iam(3, 4, 6)] <- wz[, iam(3, 4, 6)] * dbeta2.deta * dbeta1.deta
        #Ed2l.dbeta1.dgamma
        wz[, iam(3, 5, 6)] <- -const1 * (rho * beta2 * alpha1 * alpha2 +
                                           (rho * alpha1 * alpha2 * beta2) ^ 2  * beta1 * gamma / 
                                           (1 - rho * lambda1 * lambda2))
        wz[, iam(3, 5, 6)] <- wz[, iam(3, 5, 6)] * dgamma.deta * dbeta1.deta
        #Ed2l.dbeta1.drho
        wz[, iam(3, 6, 6)] <- -const1 * (beta2 * alpha1 * gamma * alpha2 +
                                           (gamma * alpha1 * alpha2 * beta2) ^ 2  * beta1 * rho / 
                                           (1 - rho * lambda1 * lambda2)) + lambda2 * const1 *
          (alpha2 - rho * beta1 * alpha2 ^ 2 / (1 + rho * lambda2))
        wz[, iam(3, 6, 6)] <- wz[, iam(3, 6, 6)] * drho.deta * dbeta1.deta
        
        
        #Ed2l.dbeta2.dbeta2
        ## Today's checkmark
        wz[, iam(4, 4, 6)] <- -alpha1 * gamma / beta2 - const1 * (
          lambda2 * (alpha1 * rho * gamma) ^ 2 / (1 + lambda1 * rho) +
            (alpha1 * alpha2 * beta1 * rho * gamma) ^ 2 / 
            (1 - lambda1 * lambda2 * rho)
        )
        wz[, iam(4, 4, 6)] <- wz[, iam(4, 4, 6)] * dbeta2.deta ^ 2
        #Ed2l.dbeta2.dgamma
        ## Today's checkmark
        wz[, iam(4, 5, 6)] <- -alpha1 - const1 * (
          lambda2 * (-alpha1 * rho + (alpha1 * rho) ^ 2 * beta2 * gamma / (1 + lambda1 * rho)) +
            (alpha1 * alpha2 * beta1 * rho + (rho * alpha1 * alpha2 * beta1) ^ 2 * 
               beta2 * gamma / (1 - lambda1 * lambda2 * rho))
        )
        wz[, iam(4, 5, 6)] <- wz[, iam(4, 5, 6)] * dgamma.deta * dbeta2.deta
        #Ed2l.dbeta2.drho
        wz[, iam(4, 6, 6)] <- -const1 * (alpha1 * beta2 * gamma * alpha2 + 
                                           (alpha1 * alpha2 * beta1 * gamma) ^ 2 * beta2 * rho /
                                           (1 - lambda1 * lambda2 * rho)) + const1 * lambda2 *
          (alpha1 * gamma - rho * beta2 * (alpha1 * gamma) ^ 2 / (1 + rho * lambda1))
        wz[, iam(4, 6, 6)] <- wz[, iam(4, 6, 6)] * drho.deta * dbeta2.deta
        
        
        #Ed2l.dgamma.dgamma
        ## Today's checkmark
        wz[, iam(5, 5, 6)] <- -alpha1 * beta2 / gamma - const1 * (
          lambda2 * (alpha1 * beta2 * rho) ^ 2 / (1 + lambda1 * rho) +
            (alpha1 * alpha2 * beta1 * beta2 * rho) ^ 2 / (1 - lambda1 * lambda2 * rho)
        )
        wz[, iam(5, 5, 6)] <- wz[, iam(5, 5, 6)] * dgamma.deta ^ 2
        #Ed2l.dgamma.drho
        wz[, iam(5, 6, 6)] <- -const1 * (alpha1 * alpha2 * beta1 * beta2 +
                                           (alpha1 * beta2 * alpha2 * beta1) ^ 2  * gamma * rho / 
                                           (1 - lambda1 * lambda2 * rho)) + lambda2 * const1 *
          (alpha1 * beta2 - rho * gamma * (alpha1 * beta2) ^ 2 / (1 + rho * lambda1))
        wz[, iam(5, 6, 6)] <- wz[, iam(5, 6, 6)] * drho.deta * dgamma.deta
        
        #Ed2l.drho.drho
        wz[, iam(6, 6, 6)] <- -const1 * (
          (alpha1 * beta2 * alpha2 * beta1 * gamma) ^ 2 / (1 - lambda1 * lambda2 * rho) +
            (alpha1 * beta2 * gamma) ^ 2 * alpha2 * beta1 / (1 + lambda1 * rho) +
            alpha1 * beta2 * (alpha2 * beta1) ^ 2 * gamma / (1 + lambda2 * rho) +
            alpha1 * beta2 * alpha2 * beta1 * gamma / (1 - rho)
        )
        wz[, iam(6, 6, 6)] <- wz[, iam(6, 6, 6)] * drho.deta ^ 2
        
        wz <- -wz
        # print(sum(is.na(wz)))
        xx <- matrix(
          nrow = 6, ncol = 6,
          data = c(
            wz[1, iam(1, 1, 6)], wz[1, iam(1, 2, 6)], wz[1, iam(1, 3, 6)], wz[1, iam(1, 4, 6)], wz[1, iam(1, 5, 6)], wz[1, iam(1, 6, 6)],
            wz[1, iam(2, 1, 6)], wz[1, iam(2, 2, 6)], wz[1, iam(2, 3, 6)], wz[1, iam(2, 4, 6)], wz[1, iam(2, 5, 6)], wz[1, iam(2, 6, 6)],
            wz[1, iam(3, 1, 6)], wz[1, iam(3, 2, 6)], wz[1, iam(3, 3, 6)], wz[1, iam(3, 4, 6)], wz[1, iam(3, 5, 6)], wz[1, iam(3, 6, 6)],
            wz[1, iam(4, 1, 6)], wz[1, iam(4, 2, 6)], wz[1, iam(4, 3, 6)], wz[1, iam(4, 4, 6)], wz[1, iam(4, 5, 6)], wz[1, iam(4, 6, 6)],
            wz[1, iam(5, 1, 6)], wz[1, iam(5, 2, 6)], wz[1, iam(5, 3, 6)], wz[1, iam(5, 4, 6)], wz[1, iam(5, 5, 6)], wz[1, iam(5, 6, 6)],
            wz[1, iam(6, 1, 6)], wz[1, iam(6, 2, 6)], wz[1, iam(6, 3, 6)], wz[1, iam(6, 4, 6)], wz[1, iam(6, 5, 6)], wz[1, iam(6, 6, 6)]
          )
        )
        print(det(xx[1:2, 1:2]))
        #print(xx[1:3, 1:3])
        print(det(xx[1:3, 1:3]))
        print(det(xx[1:4, 1:4]))
        #print(xx[1:4, 1:4])
        print(det(xx[1:5, 1:5]))
        #print(xx[1:5, 1:5])
        print(det(xx[1:6, 1:6]))
        print(xx[1:6, 1:6])
        #print(rho[1])
        stop("abc")
        # print(head(eta))
        # print(head(cbind(lambda1, lambda2, rho)))
        # print(criterion)
        # print(tfun)
        # stop("abc")
        c(w) * wz
      })
  )
}

model <- vglm(
  cbind(scoreHome, scoreAway) ~ 1, 
  family = dixoncolesff(),
  data = dfAnalysis,
  control = vglm.control(criterion = "loglikelihood",
                         trace = TRUE)
)
