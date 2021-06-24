#' @param token ldlinkr token
#' @param range
#' @param build (hg38, hg37)
#' @infile path to association test results
#' @chr select chromosome
#' @pos select position
#' @population select population (EUR, EAS, SAS, AFR, AMR)
#' @return A zoom locus plot
#' @export
zoom_locus <- function(infile, token, range, chr, pos, population, build){

  data <- data.table::fread(infile)
  ## conduct checks ##
  nameVec = c('CHR','P', 'PVAL', 'PVALUE',
              'BP', 'POS', 'SNP', 'SNPID')
  nameVec = toupper(nameVec)
  colnames(data) = toupper(colnames(data))

  if(!any(colnames(data) %in% nameVec)) stop(name.rules())
  colnames(data)[which(colnames(data) %in% c("CHR"))] <- 'CHR'
  colnames(data)[which(colnames(data) %in% c("P", 'PVAL', 'PVALUE'))] <- 'P'
  colnames(data)[which(colnames(data) %in% c("BP", "POS"))] <- 'BP'
  colnames(data)[which(colnames(data) %in% c("SNP", "SNPID"))] <- 'SNP'

  #- check for duplicated column names
  if (length(unique(colnames(data))) < length(colnames(data))) stop(col.duplicated())


  token= token
  range = range

  dat.bmi.sel.region <- data %>% filter(CHR == chr, between(BP, pos - range, pos + range))

  if (build == 'hg38')
    ensembl = useMart("ensembl",dataset="hsapiens_gene_ensembl")
  if (build == 'hg37')
    ensembl = useMart(biomart="ENSEMBL_MART_ENSEMBL", host="grch37.ensembl.org", path="/biomart/martservice" ,dataset="hsapiens_gene_ensembl")


  out.bm.genes.region <- getBM(
    attributes = c('start_position','end_position','ensembl_gene_id','external_gene_name', 'gene_biotype'),
    filters = c('chromosome_name','start','end'),
    values = list(chr, pos - range, pos + range),
    mart = ensembl)

  LD_p=LDproxy(paste0('chr',chr, ':', pos), pop = pop, r2d = "r2", token = token)
  LD_p$Coord=as.character(LD_p$Coord)
  LD_p$BP=as.numeric(gsub(LD_p$Coord, pattern = paste0('chr',chr,':'), replacement = ''))
  merged=left_join(dat.bmi.sel.region,LD_p, by='BP') %>% filter(!is.na(pos))
  merged_main = merged %>% filter(BP==as.numeric(pos))



  p1 =ggplot(data = merged, aes(x=BP, y=-log10(P), color=R2)) + geom_point(size=2,alpha=0.7) +
    scale_color_gradientn(colors = jdb_palette("solar_basic"))+ pretty_plot() +
    geom_label(data=merged_main,size=4, aes(label=paste0(CHR,':',BP))) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(legend.position = "none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(legend.position = "top")


  out.bm.genes.region <- getBM(
    attributes = c('start_position','end_position','ensembl_gene_id','external_gene_name', 'gene_biotype'),
    filters = c('chromosome_name','start','end'),
    values = list(chr, pos+5000000, pos-5000000),
    mart = ensembl)

  plot.range=c(pos+range, pos-range)

  p2 <- ggplot(data = out.bm.genes.region) +
    geom_linerange(aes(x = external_gene_name, ymin = start_position, ymax = end_position, colour = gene_biotype, group = gene_biotype)) +
    coord_flip() + ylab("") +
    ylim(plot.range) +
    geom_text(aes(x = external_gene_name, y = start_position, label = external_gene_name, colour = gene_biotype), fontface = 2, alpha = I(0.7), hjust = "right", size= 2.5) +
    expand_limits(y=c(-1, 1)) +
    pretty_plot() +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    theme(legend.position = "bottom")

  p = grid.arrange(p1,p2)
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
