#' @param infile Path to p-value matrix
#' @param significance_threshold P-value of significance. Default: 10e-8
#' @param threshold_line_color Default: red
#' @param colors Default c('black', 'grey')
#' @param title Title for the plot
#' @return A manhattan plot
#' @export
manhattan_plot <- function(infile, significance_threshold = 10e-8, threshold_line_color = 'red', colors=c('black', 'grey'), title){

  in.dt <- data.table::fread(infile)
  ## conduct checks ##

  #- check column names

  nameVec = c('CHR','P', 'PVAL', 'PVALUE',
              'BP', 'POS', 'SNP','SNPID')
  nameVec = toupper(nameVec)
  colnames(in.dt) = toupper(colnames(in.dt))

  if(!any(colnames(in.dt) %in% nameVec)) stop(name.rules())
  colnames(in.dt)[which(colnames(in.dt) %in% c("CHR"))] <- 'CHR'
  colnames(in.dt)[which(colnames(in.dt) %in% c("P", 'PVAL', 'PVALUE'))] <- 'P'
  colnames(in.dt)[which(colnames(in.dt) %in% c("BP", "POS"))] <- 'BP'
  colnames(in.dt)[which(colnames(in.dt) %in% c("SNP", "SNPID"))] <- 'SNP'

  #- check for duplicated column names
  if (length(unique(colnames(in.dt))) < length(colnames(in.dt))) stop(col.duplicated())


  data <- in.dt %>%

    group_by(CHR) %>%
    summarise(chr_len=as.numeric(max(BP))) %>%

    mutate(tot=cumsum(chr_len)-chr_len) %>%
    subset(.,select=-chr_len) %>%

    left_join(in.dt, ., by=c("CHR"="CHR")) %>%

    arrange(CHR, BP) %>%
    mutate( BPcum=BP+tot)


  sub =data %>% group_by(CHR) %>% mutate(min =min(P)) %>% filter(min==P) %>% filter(-log10(min)>6 )
  axisdf <- data %>% group_by(CHR) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )


  p = ggplot(data, aes(x=BPcum, y=-log10(P))) +

    geom_point( aes(color=as.factor(CHR)), alpha=0.8, size=0.1) +
    scale_color_manual(values = rep(colors, 22 )) +
    scale_x_continuous( label = axisdf$CHR, breaks= axisdf$center ) +
    scale_y_continuous(expand = c(0, 0) ) +
    geom_hline(yintercept=-log10(10e-6), linetype="dashed",
               color = "blue", size=0.5)+
    geom_hline(yintercept=-log10(significance_threshold), linetype="dashed",
               color = threshold_line_color, size=0.5)+

    geom_text(size = 3,data = sub, aes(label=SNP, x=BPcum, y=-log10(P))) +
    xlab('CHR') +

    theme_bw() +
    theme(
      legend.position="none",
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    ggtitle(title)


  ## functions ##

  name.rules <- function(){
    writeLines("Column names should be as listed (not case-sensitive):
                  Chromosome can be 'CHR'
                  Position ID can be 'BP', 'POS'
                  P-value can be 'PVAL', 'PVALUE', 'P'
                  SNP ID can be 'SNP', 'SNPID'" )}

  col.duplicated <- function(){
    writeLines('Your table has duplicated column names')

  }

  p

}
