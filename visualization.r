suppressPackageStartupMessages({
  require(tidyverse)
  require(Rtsne)
  require(cowplot)
  require(umap)
})
simple.umap = function (matrix, annotation = data.frame(row.names = colnames(matrix), 
    sample = colnames(matrix), label = "no label", stringsAsFactors = F), 
    scheme = "label", ..., set.seed = 42, 
    output.format = "plot"){
    set.seed(set.seed)
    
    intersected.samples = intersect(rownames(annotation)[!is.na(annotation[[scheme]])], 
        colnames(matrix))
    matrix = as.data.frame(matrix)[, intersected.samples]
    annotation = annotation[intersected.samples, ]
    matrix.umap = umap(matrix %>% t,...)$layout %>% as.data.frame %>% setNames(c('UMAP1','UMAP2'))
    res = cbind(annotation,matrix.umap[rownames(annotation),])

    res %>% ggplot(aes(x = UMAP1, y = UMAP2)) + 
    geom_point(aes_string(colour = scheme)) + 
    labs(colour = scheme) + theme_classic()
}
annotated.tsne = function(tsne, annotation){
  within(annotation,{
    tsneY1 = tsne$Y[,1]
    tsneY2 = tsne$Y[,2]
  })
}
plot.simple.tsne = function(matrix.tsne.annotation,scheme,collapse_after = 6,collapsed_label = 'collapsed'){
  y =matrix.tsne.annotation[[scheme]]# %>% as.character()
  matrix.tsne.annotation[[scheme]] = ifelse(y %in% (table(y) %>% sort(decreasing = T)%>% names %>% head(collapse_after)),y,collapsed_label)
		matrix.tsne.scheme.plot = matrix.tsne.annotation %>% ggplot(aes(x = tsneY1, 
        y = tsneY2)) + geom_point(aes(colour = matrix.tsne.annotation[[scheme]])) + 
        labs(colour = scheme)+ theme_classic()
		
		if(matrix.tsne.annotation[1,scheme,drop=T]=='no label'){
			matrix.tsne.scheme.plot = matrix.tsne.scheme.plot + theme(legend.position = "none")
		}
		matrix.tsne.scheme.plot
}
calculate_perplexity <- function(sample_number){
  if (sample_number >= 103) {
    perplexity = 34
  }
  else{
    perplexity = floor((sample_number -1)/3) %>% max(3)
  }
    perplexity
}
simple.tsne = function (matrix, 
                        annotation=data.frame(
                          row.names=colnames(matrix),
                          sample = colnames(matrix),
                          label='no label',
                          stringsAsFactors = F), 
                        scheme='label',
                        collapse_after = 6,collapsed_label = 'collapsed',
                        max_iter = 2500, perplexity = calculate_perplexity(nrow(annotation)), is_distance = F,pca = T,
    ...,tsne.data = NULL,set.seed = 42, output.format = 'plot') 
{
    set.seed(set.seed)
  if(is.null(tsne.data)){
    intersected.samples = intersect(rownames(annotation)[!is.na(annotation[[scheme]])],colnames(matrix))
    matrix = as.data.frame(matrix)[, intersected.samples]
    annotation = annotation[intersected.samples,]
    
    if(is_distance){
      matrix = matrix[intersected.samples,] %>% as.dist
      pca = F
    }
    matrix.tsne = Rtsne(matrix %>% t, max_iter = max_iter, perplexity = perplexity, is_distance=is_distance,pca = pca,
                        ...)
    matrix = NULL
    matrix.tsne.annotation = annotated.tsne(tsne = matrix.tsne, 
                                            annotation = annotation)
  }else{matrix.tsne.annotation = tsne.data}
    plot = plot.simple.tsne(matrix.tsne.annotation, scheme, collapse_after = collapse_after,collapsed_label = collapsed_label)
    if(output.format == 'plot'){output = plot}else{output = matrix.tsne.annotation}
    output
}

multiple.simple.tsne = function(matrix,annotation,schemes,collapse_after = 6,collapsed_label = 'collapsed',...){
  matrix.tsne.annotation = simple.tsne(matrix,annotation,schemes[[1]],...,output.format = 'data')
  simple.tsnes = schemes %>% 
    lapply(function(scheme){plot.simple.tsne(matrix.tsne.annotation, scheme, collapse_after = collapse_after,collapsed_label = collapsed_label)})
  plot_grid(plotlist = simple.tsnes, nrow = ceiling(length(schemes)/2),ncol = min(2,length(schemes)))
}
multi_roc = function (data, d_2class, m_score, grouping = "assignment", ipalette = NULL,...) 
{
    suppressPackageStartupMessages({
        require(tidyverse)
        require(plotROC)
    })
    if (!grouping %in% colnames(data)) {
        data[[grouping]] = "unassigned"
    }
    data = data[!is.na(data[[grouping]]), ]
    roc_plot = ggplot(data, aes_string(d = d_2class, m = m_score, 
        color = grouping)) + geom_roc(n.cuts = 0,...)
    if(!is.null(ipalette)){
        if(length(ipalette)==1){
            ipalette = rep(ipalette,data[[grouping]] %>% unique %>% length)
        }
        roc_plot = roc_plot + scale_color_manual(values = ipalette)
    }
    AUC = calc_auc(roc_plot)$AUC %>% round(2)
    roc_plot = roc_plot + annotate("text", x = 0.70, y = 0.25, 
        label = paste0("Median AUC : ", median(AUC),'\n(',min(AUC),' - ',max(AUC),')'))
    roc_plot
}
roc_auc = function(data, d_2class, m_score, colour='assignment'){
  suppressPackageStartupMessages({
    require(tidyverse)
    require(plotROC)
  })
  if(!colour %in% colnames(data)){
    data[[colour]] = 'unassigned'
  }
  data = data[!is.na(data[[colour]]),]
  roc_plot = ggplot(data, aes_string(d = d_2class, m = m_score, colour = colour )) + 
    geom_roc(n.cuts = 0)
  AUC = calc_auc(roc_plot)$AUC %>% round(2)
  names(AUC) = data[[colour]] %>% sort %>% unique()
  roc_plot = roc_plot+annotate('text',x = 0.75, y = 0.25, label = paste0(names(AUC),' AUC : ',AUC,collapse = '\n'))
  roc_plot
}
# kde2d.long = function(data, x, y, ...){
#     kde2d(x = data[[x]],y = data[[y]],...) %>% 
#     with({
#         colnames(z) = y
#         rownames(z) = x
#         z
#     })%>% melt(varname = c(x,y),value.name = 'z')
# }

# x = mc.gho.peaks.cv.keep.slop200window20.rpm.scheme[[1]] %>% pull('scheme_gho') %>% table %>% names %>% as.list %>% 
# lapply(FUN= function(class){
#     class.df = mc.gho.peaks.cv.keep.slop200window20.rpm.scheme[[1]][mc.gho.peaks.cv.keep.slop200window20.rpm.scheme[[1]][['scheme_gho']]==class,]
#     class.kde = class.df %>% 
#     kde2d.long('x','rpm',n=100,lims = c(range(mc.gho.peaks.cv.keep.slop200window20.rpm.scheme[[1]]$x),0,3)) %>% 
#     within({
#         z = z*rpm
#     })
#     class.kde[['scheme_gho']] = class
#     class.kde
# }) %>% bind_rows

# options(repr.plot.width=3, repr.plot.height=2)
# ggplot()+
# geom_point(data = x %>% arrange(z), 
#          aes(x = x, y = rpm, colour = scheme_gho, alpha = z))+
# # scale_fill_manual(values = c(hgg = '#B22222',lgg = '#DAA520',other.tumor = '#3CB371',non.tumor = '#4169E1'))+
# scale_alpha(range = c(0,0.1))+
# geom_line(data = mc.gho.peaks.cv.keep.slop200window20.rpm.scheme[[1]] %>% group_by(scheme_gho,x)%>% summarise(rpm = mean(rpm)),
#           aes(x = x , y = rpm, colour = scheme_gho),
#           alpha = 1,size = 1)+
# geom_line(data = mc.gho.peaks.cv.keep.slop200window20.rpm.scheme[[1]] %>% group_by(scheme_gho,x)%>% summarise(rpm = quantile(rpm,probs = 0.95)),
#           aes(x = x , y = rpm, colour = scheme_gho),
#           alpha = 1,size = 0.4)+
# geom_line(data = mc.gho.peaks.cv.keep.slop200window20.rpm.scheme[[1]] %>% group_by(scheme_gho,x)%>% summarise(rpm = quantile(rpm,probs = 0.05)),
#           aes(x = x , y = rpm, colour = scheme_gho),
#           alpha = 1,size = 0.4)+
# theme_clear()