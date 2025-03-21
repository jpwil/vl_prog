# this is pROC::ci.auc edited to provide bootstrapped standard errors

ci.auc_custom <- function(roc, conf.level = 0.95, method = c("delong", "bootstrap"), 
    boot.n = 2000, boot.stratified = TRUE, reuse.auc = TRUE, 
    progress = getOption("pROCProgress")$name, parallel = FALSE, 
    ...) {
    if (conf.level > 1 | conf.level < 0) 
        stop("conf.level must be within the interval [0,1].")
    if (pROC:::roc_utils_is_perfect_curve(roc)) {
        warning("ci.auc() of a ROC curve with AUC == 1 is always 1-1 and can be misleading.")
    }
    if (is.null(roc$auc) | !reuse.auc) 
        roc$auc <- auc(roc, ...)
    percent <- roc$percent
    oldauc <- roc$auc
    if (percent) {
        roc <- pROC:::roc_utils_unpercent(roc)
    }
    if (missing(method) | is.null(method)) {
        if (pROC:::has.partial.auc(roc)) {
            method <- "bootstrap"
        }
        else if ("smooth.roc" %in% class(roc)) {
            method <- "bootstrap"
        }
        else {
            method <- "delong"
        }
    }
    else {
        method <- match.arg(method, c("delong", "bootstrap"))
        if (pROC:::has.partial.auc(roc) && method == "delong") {
            stop("DeLong method is not supported for partial AUC. Use method=\"bootstrap\" instead.")
        }
        else if ("smooth.roc" %in% class(roc)) {
            stop("DeLong method is not supported for smoothed ROCs. Use method=\"bootstrap\" instead.")
        }
    }
    if (method == "delong") 
        ci <- pROC:::ci_auc_delong(roc, conf.level)
    else ci <- ci_auc_bootstrap_custom(roc, conf.level, boot.n, boot.stratified, 
        progress, parallel, ...)
    if (percent) {
        ci <- ci * 100
    }
    attr(ci, "conf.level") <- conf.level
    attr(ci, "boot.n") <- boot.n
    attr(ci, "boot.stratified") <- boot.stratified
    attr(ci, "auc") <- oldauc
    class(ci) <- c("ci.auc", "ci", class(ci))
    return(ci)
}


ci_auc_bootstrap_custom <- function (roc, conf.level, boot.n, boot.stratified, progress, 
    parallel, ...) {
    if (inherits(progress, "list")) 
        progress <- roc_utils_get_progress_bar(progress, title = "AUC confidence interval", 
            label = "Bootstrap in progress...", ...)
    if (boot.stratified) {
        aucs <- unlist(llply(1:boot.n, .fun = stratified.ci.auc, 
            roc = roc, .progress = progress, .parallel = parallel))
    }
    else {
        aucs <- unlist(llply(1:boot.n, .fun = nonstratified.ci.auc, 
            roc = roc, .progress = progress, .parallel = parallel))
    }
    if (sum(is.na(aucs)) > 0) {
        warning("NA value(s) produced during bootstrap were ignored.")
        aucs <- aucs[!is.na(aucs)]
    }
    return(quantile(aucs, 0.5), sd(aucs), sd(aucs))
}
