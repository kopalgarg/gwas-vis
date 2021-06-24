#' Essentially a wrapper for the stat_qq and stat_qq_line
#' functions in ggplot2
#' @param x numeric vector or p values
#' @param color color of abline
#' @param xlab
#' @param ylab
#' @param title
#' @return An QQ plot
#' @export
qq_plot <- function(x, color='blue', xlab=expression(Expected~~-log[10](italic(P))), ylab=expression(Observed~~-log[10](italic(P))), title){

  #- check for NAs NANs NULL and finite values
  x = x[!is.na(x) & !is.nan(x) & !is.null(x) & is.finite(x)]
  #- sort p-values in decreasing order
  data = data.frame(observed = sort(x, decreasing = T))

  p = ggplot(data, aes(sample = observed)) +
    stat_qq() + stat_qq_line(color=color, linetype="dashed") + pretty_plot() +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(title)

  p

}
