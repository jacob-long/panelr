#### wbMod class ##################################
## S4 declaration of wbMod superclass 
#' @title Within-Between Model (`wbm`) class
#' @details Models fit using [wbm()] return values of this class, which
#'  inherits from \code{\link[lme4]{merMod-class}}. 
#' @slot call_info A list of metadata about the arguments used.
#' @slot call The actual function call.
#' @slot summ The [jtools::summ()] object returned from calling it on the
#'  `merMod` object.
#' @slot summ_atts The attributes of the `summ` object.
#' @slot orig_data The data provided to the `data` argument in the function 
#'  call.
#' @export
setClass("wbm",
    # contains = "merMod",
    slots = c("call_info" = "list",
              "call" = "call",
              "summ" = "ANY",
              "summ_atts" = "list",
              "orig_data" = "ANY")
)

setClass("wblm", 
    contains = c("wbm", "lmerMod", "merMod"))

setClass("wbglm", 
    contains = c("wbm", "glmerMod", "merMod"))