# Fast candidate evaluation for stepwise selection
#
# The formula interface refits a complete lm()/glm() for every candidate
# variable at every step. Most of that time goes into model.frame(),
# model.matrix() and object construction rather than into the numerical fit.
# The functions in this file build the design matrix once per stepwise() call
# and evaluate candidate models on column subsets of it:
#
#  * linear models: all candidates of one step are scored from a
#    single QR decomposition of the current model. Entry candidates are
#    orthogonalised against the current fit (the reduction in the residual
#    cross-product matrix is then a rank-k update), removal candidates use the
#    closed form b_D' (V_DD)^-1 b_D from the current coefficients and
#    (X'X)^-1. Only the winning model is decomposed again.
#  * glm (binomial, poisson, Gamma): candidates are fitted with glm.fit() on
#    column subsets (bit-identical to what glm() does internally). With
#    metric = "SL" the Rao score test for entry is computed from the current
#    fit alone, exactly as anova.glm() does, and the Wald test for removal from
#    the current coefficient table, exactly as summary.glm() does.
#
# Whenever a column subset of one design matrix would not reproduce the
# formula-based fits exactly, buildDesignContext() returns NULL and the
# refitting code path in stepwiseUtils.R is used. This happens for: Cox and
# negative binomial models, missing values in the model frame,
# strata()/offset() terms, multivariate responses with metric = "SL",
# and factor codings that depend on which other terms are in the model
# (no intercept with factors, or an interaction containing a factor whose
# margin is itself a selectable candidate).
#
# @author Junhui Li

canonicalTerm <- function(x) {
  vapply(strsplit(x, ":", fixed = TRUE), function(p) paste(sort(p), collapse = ":"), character(1))
}

# Build the design context used by the fast evaluators, or NULL when the
# original refitting path has to be used.
buildDesignContext <- function(data, type, intercept, x_name, y_name, include, weight, n_y) {
  if (!type %in% c("linear", "logit", "poisson", "gamma")) return(NULL)
  all_terms <- unique(c(include, x_name))
  if (length(all_terms) == 0) return(NULL)
  if (any(grepl("strata\\(|offset\\(", all_terms))) return(NULL)
  fm <- reformulate(c(intercept, all_terms), y_name)
  mf <- tryCatch(model.frame(fm, data = data, na.action = na.pass), error = function(e) NULL)
  if (is.null(mf) || anyNA(mf)) return(NULL)
  if (!is.null(weight) && (anyNA(weight) || length(weight) != nrow(mf))) return(NULL)
  tt <- attr(mf, "terms")
  labels <- attr(tt, "term.labels")
  fac <- attr(tt, "factors")
  if (length(labels) == 0) return(NULL)
  vars <- rownames(fac)
  factor_like <- vapply(vars, function(v) {
    z <- mf[[v]]
    is.factor(z) || is.character(z) || is.logical(z)
  }, logical(1))
  # coding of a factor inside a term depends on which margins are present
  used <- vars[rowSums(fac > 0) > 0]
  if (intercept != "1" && any(factor_like[used])) return(NULL)
  candidates <- canonicalTerm(setdiff(x_name, include))
  for (j in seq_along(labels)) {
    v_in <- vars[fac[, j] > 0]
    if (length(v_in) > 1) {
      for (v in v_in[factor_like[v_in]]) {
        margin <- canonicalTerm(paste(setdiff(v_in, v), collapse = ":"))
        if (margin %in% candidates) return(NULL)
      }
    }
  }
  X <- model.matrix(tt, mf)
  assign <- attr(X, "assign")
  canon_labels <- canonicalTerm(labels)
  idx <- match(canonicalTerm(all_terms), canon_labels)
  if (anyNA(idx)) return(NULL)
  term_cols <- lapply(idx, function(j) which(assign == j))
  names(term_cols) <- all_terms
  if (any(lengths(term_cols) == 0)) return(NULL)
  col_term <- rep("", ncol(X))
  for (tm in all_terms) col_term[term_cols[[tm]]] <- tm
  family <- switch(type, logit = binomial(), poisson = poisson(), gamma = Gamma(), NULL)
  w <- if (is.null(weight)) rep(1, nrow(X)) else weight
  Y <- model.response(mf)
  ctx <- list(type = type,
              X = X,
              Y = Y,
              n = nrow(X),
              n_ok = sum(w != 0),
              n_data = nrow(data),
              intercept = (intercept == "1"),
              icol = which(assign == 0),
              term_cols = term_cols,
              term_order = setNames(attr(tt, "order")[idx], all_terms),
              col_term = col_term,
              n_y = n_y,
              w = w,
              family = family,
              est_disp = (type == "gamma"),
              fast_sl = (type != "linear" || n_y == 1))
  if (type == "linear") {
    ## weighted least squares as lm.wfit() does it: scale rows by sqrt(w); the
    ## residuals kept in the states are then the weighted residuals and the
    ## residual cross-products are weighted sums of squares (the deviance)
    sw <- sqrt(w)
    ctx$Xw <- X * sw
    ctx$Yw <- as.matrix(Y) * sw
    ctx$colnorm <- sqrt(colSums(ctx$Xw^2))
  }
  ctx
}

# Columns of the design matrix for a model made of the given terms, in the
# order model.matrix() would produce them for reformulate(c(intercept, terms)):
# intercept first, then terms sorted by degree (stable).
modelColumns <- function(ctx, terms_in) {
  if (length(terms_in)) {
    ord <- order(ctx$term_order[terms_in], seq_along(terms_in))
    cols <- unlist(ctx$term_cols[terms_in[ord]], use.names = FALSE)
  } else {
    cols <- integer(0)
  }
  c(ctx$icol, cols)
}

makeState <- function(ctx, terms_in) {
  if (ctx$type == "linear") lmState(ctx, terms_in) else glmState(ctx, terms_in)
}

isState <- function(x) inherits(x, "stepreg_state")

## ---- linear models --------------------------------------------------------

lmState <- function(ctx, terms_in) {
  cols <- modelColumns(ctx, terms_in)
  Y <- ctx$Yw
  if (length(cols) == 0L) {
    q <- NULL; rank <- 0L; R <- Y
  } else {
    q <- qr(ctx$Xw[, cols, drop = FALSE], tol = 1e-07)
    rank <- q$rank
    R <- if (rank > 0L) qr.resid(q, Y) else Y
  }
  structure(list(terms = terms_in, cols = cols, qr = q, rank = rank, resid = R, intercept = ctx$intercept),
            class = c("stepreg_lm_state", "stepreg_state"))
}

# Orthonormal basis of the columns of Z that lm.fit()'s pivoted QR would keep
# when they are appended to a model whose column space has already been
# projected out of Z: a column is negligible when its remaining norm falls
# below tol times its original norm (the dqrdc2 rule).
acceptedBasis <- function(Z, orig_norm, tol) {
  U <- NULL
  orig_norm[orig_norm == 0] <- 1
  for (j in seq_len(ncol(Z))) {
    z <- Z[, j]
    if (!is.null(U)) {
      z <- z - U %*% crossprod(U, z)
      z <- z - U %*% crossprod(U, z)
    }
    nz <- sqrt(sum(z^2))
    if (nz >= orig_norm[j] * tol) U <- cbind(U, z / nz)
  }
  U
}

# Residual cross-product matrix and rank of every model obtained by adding one
# candidate term to the current model.
lmScoreAdd <- function(ctx, st, cand_terms) {
  R <- st$resid
  E0 <- crossprod(R)
  cand_cols <- ctx$term_cols[cand_terms]
  allc <- unlist(cand_cols, use.names = FALSE)
  Z <- ctx$Xw[, allc, drop = FALSE]
  if (st$rank > 0L) Z <- qr.resid(st$qr, Z)
  pos <- split(seq_along(allc), rep(seq_along(cand_terms), lengths(cand_cols)))
  out <- vector("list", length(cand_terms))
  for (i in seq_along(cand_terms)) {
    U <- acceptedBasis(Z[, pos[[i]], drop = FALSE], ctx$colnorm[cand_cols[[i]]], 1e-07)
    if (is.null(U)) {
      out[[i]] <- list(E = E0, rank = st$rank)
    } else {
      UR <- crossprod(U, R)
      out[[i]] <- list(E = E0 - crossprod(UR), rank = st$rank + ncol(U))
    }
  }
  names(out) <- cand_terms
  out
}

# Residual cross-product matrix and rank of every model obtained by removing
# one term from the current model.
lmScoreRemove <- function(ctx, st, cand_terms) {
  full_rank <- st$rank > 0L && st$rank == length(st$cols)
  if (full_rank) {
    V <- chol2inv(qr.R(st$qr))
    B <- qr.coef(st$qr, ctx$Yw)
    E0 <- crossprod(st$resid)
    out <- lapply(cand_terms, function(tm) {
      D <- match(ctx$term_cols[[tm]], st$cols)
      BD <- B[D, , drop = FALSE]
      list(E = E0 + crossprod(BD, solve(V[D, D, drop = FALSE], BD)), rank = st$rank - length(D))
    })
  } else {
    out <- lapply(cand_terms, function(tm) {
      s2 <- lmState(ctx, setdiff(st$terms, tm))
      list(E = crossprod(s2$resid), rank = s2$rank)
    })
  }
  names(out) <- cand_terms
  out
}

# (Weighted) total sum of squares as summary.lm() centres it.
lmTSS <- function(ctx) {
  y <- as.matrix(ctx$Y)
  w <- ctx$w
  if (ctx$intercept) {
    m <- colSums(w * y) / sum(w)
    sum(w * sweep(y, 2, m)^2)
  } else {
    sum(w * y^2)
  }
}

# Information criteria for a linear model from its residual cross-product
# matrix E and rank p; mirrors getModelFitStat() for type = "linear".
lmMetric <- function(metric, E, p, ctx, sigma_value) {
  n <- ctx$n_ok
  nY <- ncol(E)
  SSE <- abs(det(E))
  if (metric == "adjRsq") {
    df.int <- as.integer(ctx$intercept)
    if (p == 0L || p == df.int) return(0)
    rss <- SSE
    mss <- lmTSS(ctx) - rss
    r2 <- mss / (mss + rss)
    return(1 - (1 - r2) * ((n - df.int) / (n - p)))
  }
  switch(metric,
         "AIC"     = n * log(SSE / n) + 2 * p * nY + nY * (nY + 1) + n,
         "AICc"    = n * log(SSE / n) + n * (n + p) * nY / (n - p - nY - 1),
         "CP"      = SSE / sigma_value + 2 * p - n,
         "HQ"      = n * log(SSE / n) + 2 * log(log(n)) * p * nY,
         "HQc"     = n * log(SSE / n) + 2 * log(log(n)) * p * nY * n / (n - p - nY - 1),
         "IC(1)"   = n * log(SSE / n) + p * nY + nY * (nY + 1) + n,
         "IC(3/2)" = n * log(SSE / n) + 1.5 * p * nY + nY * (nY + 1) + n,
         "BIC"     = n * log(SSE / n) + 2 * (2 + p) * (n * sigma_value / SSE) - 2 * (n * sigma_value / SSE) * (n * sigma_value / SSE),
         "SBC"     = n * log(SSE / n) + log(n) * p * nY)
}

# Second row of anova(model1, model2, test = "F") as anova.lmlist() computes
# it, from residual sums of squares and residual degrees of freedom.
anovaF2 <- function(rss1, df1, rss2, df2) {
  big <- if (df2 < df1) 2L else 1L
  scale <- if (big == 1L) rss1 / df1 else rss2 / df2
  Fv <- ((rss1 - rss2) / (df1 - df2)) / scale
  if (df1 == df2) Fv <- NA_real_
  if (!is.na(Fv) && Fv < 0) Fv <- NA_real_
  c(statistics = Fv, pic = pf(Fv, abs(df1 - df2), if (big == 1L) df1 else df2, lower.tail = FALSE))
}

# Replicates getGoodnessFit(): TRUE when the overall F test of the model is
# undefined (summary.lm()'s fstatistic or its p-value is NaN).
lmGoodnessBreak <- function(st, ctx) {
  p <- st$rank
  df.int <- as.integer(ctx$intercept)
  if (p <= df.int) return(FALSE)
  n <- ctx$n_ok
  Y <- as.matrix(ctx$Y)
  nY <- ncol(Y)
  w <- ctx$w
  y <- Y[, nY]
  rss <- sum(st$resid[, nY]^2)
  tss <- if (ctx$intercept) sum(w * (y - sum(w * y) / sum(w))^2) else sum(w * y^2)
  mss <- tss - rss
  rdf <- n - p
  Fv <- (mss / (p - df.int)) / (rss / rdf)
  pval <- if (is.nan(Fv)) NaN else pf(Fv, p - df.int, rdf, lower.tail = FALSE)
  is.nan(pval)
}

## ---- generalized linear models ---------------------------------------------

glmState <- function(ctx, terms_in) {
  cols <- modelColumns(ctx, terms_in)
  fit <- glm.fit(x = ctx$X[, cols, drop = FALSE], y = ctx$Y, weights = ctx$w,
                 family = ctx$family, intercept = ctx$intercept)
  fit$terms <- terms_in
  fit$cols <- cols
  fit$intercept <- ctx$intercept
  class(fit) <- c("stepreg_glm_state", "stepreg_state")
  fit
}

# Dispersion as summary.glm() estimates it.
glmDispersion <- function(fit, ctx) {
  if (!ctx$est_disp) return(1)
  df.r <- fit$df.residual
  if (df.r > 0) sum((fit$weights * fit$residuals^2)[fit$weights > 0]) / df.r else NaN
}

# Information criteria for a glm from its aic and rank; mirrors
# getModelFitStat() for the likelihood-based types (logLik.glm adds one
# parameter for families with an estimated dispersion).
glmMetric <- function(metric, aic, rank, ctx) {
  p <- rank + as.integer(ctx$est_disp)
  ll <- p - aic / 2
  n <- ctx$n_data
  switch(metric,
         "IC(1)"   = -2 * ll + p,
         "IC(3/2)" = -2 * ll + 1.5 * p,
         "SBC"     = -2 * ll + p * log(n),
         "AICc"    = -2 * ll + n * (n + p) / (n - p - 2),
         "AIC"     = -2 * ll + 2 * p,
         "HQ"      = -2 * ll + 2 * p * log(log(n)),
         "HQc"     = -2 * ll + 2 * p * n * log(log(n)) / (n - p - 2))
}

# Second row of anova(model1, model2, test = "LRT"/"Rao") as anova.glmlist()
# and stat.anova() compute it. 'score' is the Rao statistic computed with the
# smaller model as m1 (see glmRaoScores()).
anovaChisq2 <- function(dev1, df1, dev2, df2, scale1, scale2, test, score = NULL) {
  big <- if (df2 < df1) 2L else 1L
  scale <- if (big == 1L) scale1 else scale2
  Df <- df1 - df2
  stat <- if (test == "Rao") score * sign(Df) else dev1 - dev2
  val <- stat / scale * sign(Df)
  if (Df == 0 || (!is.na(val) && val < 0)) val <- NA_real_
  c(statistics = stat, pic = pchisq(val, abs(Df), lower.tail = FALSE))
}

# Rao score statistics for adding each candidate term to the model 'st',
# computed as anova.glmlist() does: a Gaussian weighted least-squares fit of
# the working residuals of the smaller model on the larger design, using its
# working weights; the statistic is the reduction from the null deviance.
# Returns, per candidate, the score and the rank of the larger design.
glmRaoScores <- function(ctx, st, cand_terms) {
  r <- st$residuals
  w <- st$weights
  sw <- sqrt(w)
  wtdmu <- if (ctx$intercept) sum(w * r) / sum(w) else 0
  nulldev <- sum(w * (r - wtdmu)^2)
  rw <- r * sw
  tol <- 1e-11
  if (length(st$cols)) {
    q0 <- qr(ctx$X[, st$cols, drop = FALSE] * sw, tol = tol)
    res0 <- if (q0$rank > 0L) qr.resid(q0, rw) else rw
  } else {
    q0 <- NULL; res0 <- rw
  }
  cand_cols <- ctx$term_cols[cand_terms]
  allc <- unlist(cand_cols, use.names = FALSE)
  Z <- ctx$X[, allc, drop = FALSE] * sw
  orig <- sqrt(colSums(Z^2))
  if (!is.null(q0) && q0$rank > 0L) Z <- qr.resid(q0, Z)
  pos <- split(seq_along(allc), rep(seq_along(cand_terms), lengths(cand_cols)))
  dev0 <- sum(res0^2)
  out <- lapply(seq_along(cand_terms), function(i) {
    U <- acceptedBasis(Z[, pos[[i]], drop = FALSE], orig[pos[[i]]], tol)
    if (is.null(U)) list(score = nulldev - dev0, rank = st$rank)
    else list(score = nulldev - (dev0 - sum(crossprod(U, res0)^2)), rank = st$rank + ncol(U))
  })
  names(out) <- cand_terms
  out
}

# Wald statistics (z^2 or t^2) and p-values for every non-aliased coefficient
# of 'fit', computed as summary.glm() does. Rows are named by the term the
# column belongs to.
glmWald <- function(fit, ctx) {
  p <- fit$rank
  p1 <- seq_len(p)
  Qr <- fit$qr
  coef.p <- fit$coefficients[Qr$pivot[p1]]
  covmat <- glmDispersion(fit, ctx) * chol2inv(Qr$qr[p1, p1, drop = FALSE])
  tvalue <- coef.p / sqrt(diag(covmat))
  pvalue <- if (!ctx$est_disp) 2 * pnorm(-abs(tvalue))
            else if (fit$df.residual > 0) 2 * pt(-abs(tvalue), fit$df.residual)
            else rep(NA_real_, p)
  cols <- fit$cols[Qr$pivot[p1]]
  keep <- !(cols %in% ctx$icol)
  list(term = ctx$col_term[cols[keep]], statistics = unname(tvalue[keep]^2), pic = unname(pvalue[keep]))
}

## ---- one selection step ----------------------------------------------------

# Fast replacement for getCandStepModel(); same return value. 'fit_cur' is
# the state of the current model (or any other object on the first step, in
# which case the state is built from the term list).
getCandStepModelFast <- function(add_or_remove, ctx, type, metric, x_in_model, x_notin_model, intercept, include, test_method, sigma_value, feature_ratio, fit_cur) {
  st <- if (isState(fit_cur)) fit_cur else makeState(ctx, c(include, x_in_model))
  BREAK <- FALSE
  if (add_or_remove == "add") {
    x_test <- sampleCandidates(x_notin_model, feature_ratio)
  } else {
    x_test <- x_in_model
  }
  if (length(x_test) == 0) {
    return(list("BREAK" = TRUE))
  }
  new_terms <- lapply(x_test, function(x) if (add_or_remove == "add") c(x_in_model, x) else setdiff(x_in_model, x))
  names(new_terms) <- x_test
  f_set <- NULL
  new_state_of <- function(var) makeState(ctx, c(include, new_terms[[var]]))

  if (type == "linear") {
    n <- ctx$n_ok
    sc <- if (add_or_remove == "add") lmScoreAdd(ctx, st, x_test) else lmScoreRemove(ctx, st, x_test)
    if (metric == "SL") {
      # anova(candidate, current, test = "F") for entry, anova(reduced, current)
      # for removal: in both cases the candidate model is model 1
      rss_cur <- sum(st$resid^2)
      f_pic <- vapply(sc, function(s) anovaF2(s$E[1, 1], n - s$rank, rss_cur, n - st$rank), numeric(2))
      pic_set <- f_pic["pic", ]
      f_set <- f_pic["statistics", ]
      names(pic_set) <- names(f_set) <- x_test
    } else if (add_or_remove == "remove" & length(x_test) == 1 & intercept == "0") {
      pic_set <- if (metric == "adjRsq") 0 else Inf
      names(pic_set) <- x_test
    } else {
      pic_set <- vapply(sc, function(s) lmMetric(metric, s$E, s$rank, ctx, sigma_value), numeric(1))
      names(pic_set) <- x_test
    }
  } else {
    if (metric == "SL") {
      if (add_or_remove == "remove") {
        wt <- glmWald(st, ctx)
        pic_set <- wt$pic
        f_set <- wt$statistics
        names(pic_set) <- names(f_set) <- wt$term
        pic_set <- pic_set[!names(pic_set) %in% include]
        f_set <- f_set[!names(f_set) %in% include]
      } else {
        scale_cur <- glmDispersion(st, ctx)
        rao <- if (test_method == "Rao") glmRaoScores(ctx, st, x_test) else NULL
        if (test_method == "Rao" && !ctx$est_disp) {
          # dispersion is 1: no candidate fit needed at all
          f_pic <- vapply(x_test, function(v) {
            anovaChisq2(NA_real_, ctx$n - rao[[v]]$rank, NA_real_, ctx$n - st$rank, 1, 1, "Rao", score = rao[[v]]$score)
          }, numeric(2))
        } else {
          # LRT needs the candidate deviance; Gamma needs its dispersion
          f_pic <- vapply(x_test, function(v) {
            s2 <- new_state_of(v)
            anovaChisq2(s2$deviance, s2$df.residual, st$deviance, st$df.residual,
                        glmDispersion(s2, ctx), scale_cur, test_method,
                        score = if (test_method == "Rao") rao[[v]]$score else NULL)
          }, numeric(2))
        }
        pic_set <- f_pic["pic", ]
        f_set <- f_pic["statistics", ]
        names(pic_set) <- names(f_set) <- x_test
      }
    } else if (add_or_remove == "remove" & length(x_test) == 1 & intercept == "0") {
      pic_set <- Inf
      names(pic_set) <- x_test
    } else {
      pic_set <- vapply(x_test, function(v) {
        s2 <- new_state_of(v)
        glmMetric(metric, s2$aic, s2$rank, ctx)
      }, numeric(1))
      names(pic_set) <- x_test
    }
  }

  if (metric == "adjRsq" | (metric == "SL" & add_or_remove == "remove")) {
    pic <- max(pic_set)
    minmax_var <- names(which.max(pic_set))
  } else {
    pic <- min(pic_set)
    minmax_var <- names(which.min(pic_set))
    if (sum(pic_set %in% pic) > 1 & metric == "SL") {
      ## tied p-values (e.g. underflow to 0): among the tied candidates take the
      ## largest test statistic
      tied <- names(pic_set)[pic_set %in% pic]
      minmax_var <- tied[which.max(abs(f_set[tied]))]
      pic <- pic_set[minmax_var]
    }
  }
  best_state <- new_state_of(minmax_var)
  if (add_or_remove == "add" && best_state$rank == st$rank) {
    BREAK <- TRUE
  }
  list("pic" = pic, "minmax_var" = minmax_var, "best_candidate_model" = best_state, "BREAK" = BREAK, "pic_list" = pic_set)
}

## ---- best subset -----------------------------------------------------------

# Fast replacement for the per-combination fitting in getFinalSubSet():
# returns the metric (or the F/Rao/LRT statistic for metric = "SL") of every
# model in 'term_sets' (a list of character vectors, each already including
# the 'include' terms).
subsetStatsFast <- function(ctx, metric, term_sets, test_method, sigma_value) {
  if (ctx$type == "linear") {
    n <- ctx$n_ok
    if (metric == "SL") {
      rss0 <- lmTSS(ctx)
      df0 <- n - as.integer(ctx$intercept)
    }
    vapply(term_sets, function(tm) {
      s <- lmState(ctx, tm)
      E <- crossprod(s$resid)
      if (metric == "SL") anovaF2(rss0, df0, E[1, 1], n - s$rank)[["statistics"]]
      else lmMetric(metric, E, s$rank, ctx, sigma_value)
    }, numeric(1))
  } else {
    if (metric == "SL") {
      st0 <- glmState(ctx, character(0))
      scale0 <- glmDispersion(st0, ctx)
      if (test_method == "Rao") {
        # anova(reduced, full, test = "Rao"): the score only needs the reduced
        # fit's working residuals/weights and the full design
        r <- st0$residuals; w <- st0$weights; sw <- sqrt(w)
        wtdmu <- if (ctx$intercept) sum(w * r) / sum(w) else 0
        nulldev <- sum(w * (r - wtdmu)^2)
        rw <- r * sw
        Xw <- ctx$X * sw
        vapply(term_sets, function(tm) {
          cols <- modelColumns(ctx, tm)
          q <- qr(Xw[, cols, drop = FALSE], tol = 1e-11)
          dev <- if (q$rank > 0L) sum(qr.resid(q, rw)^2) else sum(rw^2)
          score <- nulldev - dev
          if (ctx$est_disp) {
            s <- glmState(ctx, tm)
            anovaChisq2(st0$deviance, st0$df.residual, s$deviance, s$df.residual, scale0, glmDispersion(s, ctx), "Rao", score = score)[["statistics"]]
          } else {
            score
          }
        }, numeric(1))
      } else {
        vapply(term_sets, function(tm) {
          s <- glmState(ctx, tm)
          anovaChisq2(st0$deviance, st0$df.residual, s$deviance, s$df.residual, scale0, glmDispersion(s, ctx), "LRT")[["statistics"]]
        }, numeric(1))
      }
    } else {
      vapply(term_sets, function(tm) {
        s <- glmState(ctx, tm)
        glmMetric(metric, s$aic, s$rank, ctx)
      }, numeric(1))
    }
  }
}
