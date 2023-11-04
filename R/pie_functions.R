#' Title
#'
#' @return
#' @export
#'
#' @examples
defaults_pie <- function(...){
  
  list(
    ggplot2::coord_polar(theta = "y", ...),
    ggplot2::theme_void(),
    ggplot2::aes(x = 0) # hacky; grammar problem
  )
  
}

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
ggpie <- function(data){
  
  ggplot2::ggplot(data = data) + 
  defaults_pie()
  
}


# just aliasing to be nice to ourselves
# probably a better way 
# is doing more re-writing so that x is not a required aesthetic
#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
geom_wedge <- function(...){
  
  ggplot2::geom_bar(position = "fill", ...)
  
}


# some very preliminary and messy ideas for new Stat Wedge approach
# but wondering if this is worth it.  
# I think it could be if geom is used with ggplot() start point.
# StatWedge <- ggproto(`_class` = StatCount2, 
#                      `_inherit` = ggplot2::Stat,
#                      compute_group )
# 
# ggplot2::StatCount$compute_group %>% 
#   mutate(x)
# 
# stat_count

# geom_wedge <- function (mapping = NULL, data = NULL, geom = "bar", 
#                         position = "fill", 
#                         ..., width = NULL, na.rm = FALSE, 
#                         orientation = NA, show.legend = NA, 
#                         inherit.aes = TRUE) 
# {
#     params <- list2(na.rm = na.rm, orientation = orientation, 
#         width = width, ...)
#     layer(data = data, mapping = mapping, stat = StatCount, geom = geom, 
#         position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
#         params = params)
# }

