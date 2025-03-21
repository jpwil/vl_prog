# this is an edited version of the mice sampler file for error catching purposes (JW)

sampler <- function (data, m, ignore, where, imp, blocks, method, visitSequence, 
    predictorMatrix, formulas, blots, post, fromto, printFlag, 
    ...) 
{
    from <- fromto[1]
    to <- fromto[2]
    maxit <- to - from + 1
    r <- !is.na(data)
    chainMean <- chainVar <- mice:::initialize.chain(blocks, maxit, 
        m)
    if (maxit < 1) 
        iteration <- 0
    if (maxit >= 1) {
        if (printFlag) {
            cat("\n iter imp variable")
        }
        for (k in from:to) {
            iteration <- k
            for (i in seq_len(m)) {
                if (printFlag) {
                  cat("\n ", iteration, " ", i)
                }
                for (h in visitSequence) {
                  for (j in blocks[[h]]) {
                    y <- data[, j]
                    ry <- r[, j]
                    wy <- where[, j]
                    data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], 
                      i]
                  }
                }
                for (h in visitSequence) {
                  ct <- attr(blocks, "calltype")
                  calltype <- ifelse(length(ct) == 1, ct[1], 
                    ct[h])
                  b <- blocks[[h]]
                  if (calltype == "formula") 
                    ff <- formulas[[h]]
                  else ff <- NULL
                  type <- predictorMatrix[h, ]
                  user <- blots[[h]]
                  theMethod <- method[h]
                  empt <- theMethod == ""
                  univ <- !empt && !is.passive(theMethod) && 
                    !handles.format(paste0("mice.impute.", theMethod))
                  mult <- !empt && !is.passive(theMethod) && 
                    handles.format(paste0("mice.impute.", theMethod))
                  pass <- !empt && is.passive(theMethod) && length(blocks[[h]]) == 
                    1
                  if (printFlag & !empt) {
                    cat(" ", b)
                    current_state[[1]] <<- iteration
                    current_state[[2]] <<- i
                    current_state[[3]] <<- b
                    }  # this is my bad error catching code (please don't judge)
                  oldstate <- get("state", pos = parent.frame())
                  newstate <- list(it = k, im = i, dep = h, meth = theMethod, 
                    log = oldstate$log)
                  assign("state", newstate, pos = parent.frame(), 
                    inherits = TRUE)
                  if (univ) {
                    for (j in b) {
                      imp[[j]][, i] <- sampler.univ(data = data, 
                        r = r, where = where, type = type, formula = ff, 
                        method = theMethod, yname = j, k = k, 
                        calltype = calltype, user = user, ignore = ignore, 
                        ...)
                      data[(!r[, j]) & where[, j], j] <- imp[[j]][(!r[, 
                        j])[where[, j]], i]
                      cmd <- post[j]
                      if (cmd != "") {
                        eval(parse(text = cmd))
                        data[(!r[, j]) & where[, j], j] <- imp[[j]][(!r[, 
                          j])[where[, j]], i]
                      }
                    }
                  }
                  if (mult) {
                    mis <- !r
                    mis[, setdiff(colnames(data), b)] <- FALSE
                    data[mis] <- NA
                    fm <- paste("mice.impute", theMethod, sep = ".")
                    if (calltype == "formula") {
                      imputes <- do.call(fm, args = list(data = data, 
                        formula = ff, ...))
                    }
                    else if (calltype == "type") {
                      imputes <- do.call(fm, args = list(data = data, 
                        type = type, ...))
                    }
                    else {
                      stop("Cannot call function of type ", calltype, 
                        call. = FALSE)
                    }
                    if (is.null(imputes)) {
                      stop("No imputations from ", theMethod, 
                        h, call. = FALSE)
                    }
                    for (j in names(imputes)) {
                      imp[[j]][, i] <- imputes[[j]]
                      data[!r[, j], j] <- imp[[j]][, i]
                    }
                  }
                  if (pass) {
                    for (j in b) {
                      wy <- where[, j]
                      ry <- r[, j]
                      imp[[j]][, i] <- model.frame(as.formula(theMethod), 
                        data[wy, ], na.action = na.pass)
                      data[(!ry) & wy, j] <- imp[[j]][(!ry)[wy], 
                        i]
                    }
                  }
                }
            }
            k2 <- k - from + 1L
            if (length(visitSequence) > 0L) {
                for (h in visitSequence) {
                  for (j in blocks[[h]]) {
                    if (!is.factor(data[, j])) {
                      chainVar[j, k2, ] <- apply(imp[[j]], 2L, 
                        var, na.rm = TRUE)
                      chainMean[j, k2, ] <- colMeans(as.matrix(imp[[j]]), 
                        na.rm = TRUE)
                    }
                    if (is.factor(data[, j])) {
                      for (mm in seq_len(m)) {
                        nc <- as.integer(factor(imp[[j]][, mm], 
                          levels = levels(data[, j])))
                        chainVar[j, k2, mm] <- var(nc, na.rm = TRUE)
                        chainMean[j, k2, mm] <- mean(nc, na.rm = TRUE)
                      }
                    }
                  }
                }
            }
        }
        if (printFlag) {
            r <- get("loggedEvents", parent.frame(1))
            ridge.used <- any(grepl("A ridge penalty", r$out))
            if (ridge.used) {
                cat("\n * Please inspect the loggedEvents \n")
            }
            else {
                cat("\n")
            }
        }
    }
    list(iteration = maxit, imp = imp, chainMean = chainMean, 
        chainVar = chainVar)
}
