#' @param mds Path to MDS file with 'IID', C1-Cn columns
#' @param racefile Path to racefile with columns 'IID', 'race'
#' @return A PCA plot
#' @export
pca_plot <- function(mds, racefile){

  data<- fread(mds)
  racefile <- fread(racefile)

  #- check column names
  nameVec = c('IID','sample' ,'pop', 'race', 'population')
  nameVec = toupper(nameVec)
  colnames(racefile) = toupper(colnames(racefile))

  if(!any(colnames(racefile) %in% nameVec)) stop(name.rules())

  colnames(racefile)[which(colnames(racefile) %in% c("IID", "SAMPLE"))] <- 'IID'
  colnames(racefile)[which(colnames(racefile) %in% c("POP", "RACE" , 'POPULATION'))] <- 'POP'
  racefile = racefile %>% subset(., select= c('IID', 'POP'))
  data = data %>% subset(., select=c('C1', 'C2', 'IID'))

  datafile <- left_join(racefile, data)

  p = ggplot(data=datafile , aes(x=C1,y=C2,fill=POP,color=POP))+
    geom_point(alpha=0.5, size=2,shape=21) +
    scale_color_manual(values = c("AFR" = "#ff7f0e", "AMR" = "#2ca02c", "EAS" = '#d62728',
                                  'EUR' = '#1f77b4', 'SAS' = '#9467bd', 'UNK' = 'black'))+
    scale_fill_manual(values = c("AFR" = "#ff7f0e", "AMR" = "#2ca02c", "EAS" = '#d62728',
                                 'EUR' = '#1f77b4', 'SAS' = '#9467bd')) + pretty_plot() +
    geom_point(data=datafile , aes(x=C1,y=C2,fill=POP),alpha=0.7, size=1.5,shape=21) +
    xlab('PC1') + ylab('PC2')


  ## functions ##

  name.rules <- function(){
    writeLines("Column names should be as listed (not case-sensitive):
                   Individual ID can be 'IID' , 'sample'
                   Population can be 'race', 'pop', 'population'" )
  }

  p

}
