suppressPackageStartupMessages({
  require(tidyverse)
#      require(Rfast)
})
source('/NAS/wg_zsx/data2/sxz/code/data_tools/vector.tools.r')
merge.df.list = function(df.list,col.names = sapply(df.list,colnames)){
	key = df.list[[1]] %>% rownames
  df = df.list %>% bind_cols %>% as.data.frame
  rownames(df) = key
  if(length(col.names) == ncol(df)){colnames(df) = col.names}
  df
}

sample.fpkm = function(cts.column, lengths, lib.sizes = sum(cts.column)){
    kilo.base = lengths/1e3
    million.reads = lib.sizes/1e6
    cts.column/(kilo.base*million.reads)
}

naive_fpkm = function(cts, lengths=1000, lib.sizes = colSums(cts)){
    kilo.base = lengths/1e3
    million.reads = lib.sizes/1e6
    cts/(kilo.base %*%t(million.reads))
}

naive_tpm = function(cts, lengths=1000, ...){
    kilo.base = lengths/1e3
    rpk = cts/kilo.base
    rpk_sums = colSums(rpk)/1e6
    t(t(rpk)/rpk_sums)
   }