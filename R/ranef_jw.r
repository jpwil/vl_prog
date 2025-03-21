# A single object matching ‘ranef.merMod’ was found
# It was found in the following places
#   registered S3 method for ranef from namespace lme4
#   namespace:lme4
# with value

function (object, condVar = TRUE, drop = FALSE, whichel = names(ans), 
    postVar = FALSE, ...) 
{
    if (length(L <- list(...)) > 0) {
        warning(paste("additional arguments to ranef.merMod ignored:", 
            paste(names(L), collapse = ", ")))
    }
    if (!missing(postVar) && missing(condVar)) {
        warning(sQuote("postVar"), " is deprecated: please use ", 
            sQuote("condVar"), " instead")
        condVar <- postVar
    }
    ans <- object@pp$b(1)
    if (!is.null(fl <- object@flist)) {
        levs <- lapply(fl, levels)
        asgn <- attr(fl, "assign")
        cnms <- object@cnms
        nc <- lengths(cnms)
        nb <- diff(object@Gp)
        nbseq <- rep.int(seq_along(nb), nb)
        ml <- split(ans, nbseq)
        for (i in seq_along(ml)) ml[[i]] <- matrix(ml[[i]], ncol = nc[i], 
            byrow = TRUE, dimnames = list(NULL, cnms[[i]]))
        ans <- lapply(seq_along(fl), function(i) {
            m <- ml[asgn == i]
            b2 <- vapply(m, nrow, numeric(1))
            ub2 <- unique(b2)
            if (length(ub2) > 1) 
                stop("differing numbers of b per group")
            rnms <- if (ub2 == length(levs[[i]])) 
                levs[[i]]
            else seq(ub2)
            data.frame(do.call(cbind, m), row.names = rnms, check.names = FALSE)
        })
        names(ans) <- names(fl)
        stopifnot(is(whichel, "character"))
        whchL <- names(ans) %in% whichel
        ans <- ans[whchL]
        if (condVar) {
            sigsqr <- sigma(object)^2
            rp <- rePos$new(object)
            if (any(lengths(rp$terms) > 1L)) {
                vv <- arrange.condVar(object, condVar(object, 
                  scaled = TRUE))
            }
            else {
                vv <- .Call(merPredDcondVar, object@pp$ptr(), 
                  as.environment(rp))
                vv <- lapply(vv, "*", sigsqr)
            }
            for (i in names(ans)) {
                attr(ans[[i]], "postVar") <- vv[[i]]
            }
        }
        if (drop) 
            ans <- lapply(ans, function(el) {
                if (ncol(el) > 1) 
                  return(el)
                pv <- drop(attr(el, "postVar"))
                el <- drop(as.matrix(el))
                if (!is.null(pv)) 
                  attr(el, "postVar") <- pv
                el
            })
        class(ans) <- "ranef.mer"
    }
    ans
}
<bytecode: 0x105b0a630>
<environment: namespace:lme4>