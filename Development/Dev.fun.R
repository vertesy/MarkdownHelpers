
#' ww.ttl_field.gg
#'
#' Internal function. Creates the string written into the PDF files "Title' (metadata) field.
#' @param plotname Name of the plot
#' @param creator String X in: "plotblabla by X". Defaults: "ggExpress".
#' @export
#' @examples ww.ttl_field.gg("/Users/myplot.jpg")

ww.ttl_field.gg <- function(plotname
                            , creator = unless.specified('b.scriptname', def = 'ggExpress')) {
  paste0(basename(plotname), " by " , creator)
}

