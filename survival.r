suppressPackageStartupMessages({
  require(dplyr)
  require(reshape2)
  require('survminer')
  require('survival')
})
extract.cox.summary = function(cox.summary){
  table = cbind(n.included = cox.summary$n,
                p.value = cox.summary$coefficients[,5,drop = F],
                cox.summary$conf.int[,c(1,3:4),drop = F])
  colnames(table)[2:3] = c('p.value','hazard.ratio')
  table = table%>%as.data.frame(stringsAsFactors = F)
  table$variable = rownames(table)
  table
}