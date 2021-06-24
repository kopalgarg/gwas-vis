#' @param qfile path to ADMIXTURE output file (.Q file)
#' @param samples vector of samples with no header (in the same order)
#' @param racefile data frame of samples with known ancestry
#' @param n number of populations in ADMIXTURE file
#' @return An admixture plot
#' @export
admixture_plot <- function(qfile, samples, racefile, n){

   # input files
  qfile = fread(qfile)

  samples = as.data.frame(fread(samples, header = F))

  colnames(samples)='IID'

  racefile<- fread(racefile)

  df=cbind(qfile,samples)

  tbl=left_join(df, racefile)
  n = paste0('V',n)
  plot_data <- tbl %>%
    gather('pop', 'prob', V1:paste0('V',n))  %>%
    group_by(IID) %>%
    mutate(likely_assignment = pop[which.max(prob)],
           assingment_prob = max(prob)) %>%
    arrange(likely_assignment, desc(assingment_prob)) %>%
    ungroup() %>%
    mutate(IID = forcats::fct_inorder(factor(IID)))

  p1=ggplot(plot_data,  aes(IID, prob, fill = pop)) +
    geom_col(width = 2) +
    facet_grid(~race, scales = 'free', space = 'free') +theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()) + xlab('Individual') + ylab('Assignment Probability')+
    theme(legend.position = "none") +
    scale_fill_manual(values = jdb_palette("corona"))

  p1

}
