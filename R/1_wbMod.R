#### wbMod class ##################################
## S4 declaration of wbMod superclass 
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