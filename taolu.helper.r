# generic
small.round = function(x,digits=2,out.numeric=F){
    signx = ifelse(x>0,'','-')
    x=abs(x)
    power = floor(log10(x))
    di = round(x/(10^power),digits=2)
    notepower=ifelse(power<0,as.character(power),paste0('+',power))
    out = paste0(signx,ifelse(power<(-3),paste0(di,'e',notepower),sprintf('%.3f',x)))
    if(out.numeric){out=as.numeric(out)}
    out
}
keep.rownames=function (df, id.col = "rownames.2id") 
{
    df = as.data.frame(df)
    dfr = data.frame(stringsAsFactors = F,v1=rownames(df))%>%setNames(id.col)
    bind_cols(dfr,df)
}

write.txt = function(data,file){
    data = keep.rownames(data,'Symbol')
    write.table(data,file,quote=F,row.names = F,sep = '\t')
}
eps = function(filename,width,height,family='ArialMT'){
    setEPS()
    postscript(filename,width=width,height=height,family=family)
}

txtround=function(x,digits=3){
    lb = 0.1^digits
    if(x<lb){o=paste0('<',lb)}else{o=paste0('=',round(x,digits))}
    o
}

setlevel = function(x,new_levels=levels(x)){
    levels(x)=new_levels
    x
}
centerscale=function (x){
    pp = preProcess(x, method = c("center", "scale"))
    predict(pp, x)
}
center2median =function(d){sweep(d,1, apply(d,1,median,na.rm=T))}
center.scale = function(x){
    x=t(x)
    p=preProcess(x,method = c('center','scale'))
    t(predict(p,x))
}
# ploting
p2asterik = function(p,meter = c(ns = 1,'*'=0.05,'**'=0.01,'***'=0.001,'****'=0.0001)){
    names(meter)[max((1:length(meter))[meter>=p])]
}

plg = function(plotlist,...){
    plot_grid(plotlist = plotlist,...)
}
plg.commonlegend = function(plotlist,...){
    common.legend = get_legend(plotlist[[1]])
    plg(c(lapply(plotlist,function(plot){plot+theme(legend.position = 'none')}),list(legend=common.legend)),...)
}

plg.commonlegend2 = function(plotlist,...){
    common.legend = get_legend(plotlist[[1]])
    list(main=plg(lapply(plotlist,function(plot){plot+theme(legend.position = 'none')}),...),legend=common.legend)
}
ggblank = ggplot()+geom_blank()+theme_classic()
ggtitle.plot = function(title){
    ggplot()+ggtitle(title)+theme_void()
}
pct.bar = function(data,x,fill,fontsize=18,...){
    tab = table(data[[x]],data[[fill]])
    chisqp = chisq.test(tab)$p.value
    p.signif = p2asterik(chisqp)
    data =tab %>%as.data.frame%>% 
    group_by(Var1) %>% mutate(pct= prop.table(Freq) * 100) 
    ggplot(data,aes(Var1, pct, fill=Var2)) +
  geom_bar(stat="identity") + ylab("Percentage") +xlab(NULL)+
    geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%")),
  position=position_stack(vjust=0.5),...) +
  geom_signif(
    annotations = c(p.signif),
    y_position = 105, xmin = 0.5, xmax = length(unique(data$Var1))+0.5
  )+theme_bw()+theme(axis.title = element_text(size = fontsize),axis.text = element_text(size = fontsize),
                     legend.title = element_text(size = fontsize),legend.text = element_text(size = fontsize))
}
ggsignificance = function(lab,size=10){
    ggplot(data.frame(x=c(0,0,1,1),y=c(0,0.1,0.1,0)),aes(x,y))+geom_line()+
    annotate('text',label=lab,x = 0.5,y=0.15,size=size)+ylim(0,0.15+size*0.005)+coord_cartesian(clip='off')+theme_void()
}
pct.pie = function(data,x,fill,xlabels=unique(data[[x]])%>%sort,fill.pal = NULL,fontsize=12){
    tab = table(data[[x]], data[[fill]])
    chisqp = chisq.test(tab)$p.value
    p.signif = p2asterik(chisqp)
    sdata = data %>% df.split(x)
    xlabels = setNames(xlabels,names(sdata))
    plots = names(sdata)%>% lapply(function(sdata.name){
        idata = sdata[[sdata.name]]
        idata[[fill]] %>% table %>% as.data.frame %>% 
        within({pct = prop.table(Freq);pct.lab = paste0(round(pct*100,1),'%')})%>%
        ggplot(aes_string(x = "1", y = 'pct', fill = '.')) +
        geom_col() + geom_text(aes(label = pct.lab),position = position_stack(vjust = 0.5))+
        do.call(scale_fill_manual,fill.pal) +
        coord_polar(theta = "y")+theme_void()+theme(axis.title.x = element_text(size=12))+
        ylab(xlabels[sdata.name])
    })
    common.legend = get_legend(plots[[1]]);
    plots = lapply(plots,function(plot){plot+theme(legend.position = 'none')})
    plot_grid(plot_grid(ggsignificance(p.signif,fontsize/2),plg(plots,nrow=1),rel_heights=c(1,8),ncol=1),common.legend,rel_widths = c(4,1),nrow=1)
}

iqr.outliers = function(x,thres=1.5){
    thres = max(thres,1.5)
    x.iqr = IQR(x);x.show = between(x,median(x)-outlier.thres*x.iqr,median(x)+outlier.thres*x.iqr)
    x.show
}

grubbs.outliers = function(x,thres=0.05,two.sided = F){
    thres = min(thres,0.05)
    require(outliers)
    xi = data.frame(idx=1:length(x),x=x,p=1)[order(x),]
    x = xi$x;iseq=1:nrow(xi)
    for(j in 1:nrow(xi)){
        g = grubbs.test(x,type=10,two.sided = two.sided)
        gp = g$p.value
        s = ifelse(grepl('lowest',g$alternative),1,length(x))
        i = iseq[s]
        xi[i,'p']=gp
        if(gp>thres){break}
        x = x[-s]
        iseq = iseq[-s]
    }
    xi[order(xi$idx),'p',drop=T]>thres
}

corplot = function(data,x,y,color,x.lab=x,y.lab=y,color.lab=color,color.palette=NULL,
                   rm.outlier=T,outlier.function=grubbs.outliers,outlier.thres = 0.05){
    xv = data[[x]];yv=data[[y]];colorv=data[[color]]
    if(rm.outlier){
        x.show=outlier.function(xv,outlier.thres);
        y.show=outlier.function(yv,outlier.thres)
    data = data[x.show&y.show,];xv = data[[x]];yv=data[[y]];colorv=data[[color]]}
    if(is.null(color.palette)){color.palette=rep('black',length(unique(colorv)))}
    lms = lm(yv~xv)%>%summary
    p.value =lms$coefficients['xv','Pr(>|t|)'] ;beta=lms$coefficients['xv','Estimate'];rho=lms$r.squared^0.5
    stat.x = ifelse(beta>0,min(xv),max(xv));stat.y=max(yv);stat.hjust = ifelse(beta>0,0,1)
    if(is.finite(p.value)&is.finite(rho)){
    stats = sprintf('R%s,p%s', txtround(rho),txtround(p.value))
    p = ggplot(data,aes_string(x=x,y=y))+geom_point(aes_string(color=color),size=0.4)+
    geom_smooth(method = "lm", se = T)+
        scale_color_manual(name=color.lab,values=color.palette)+
        annotate('text',x = stat.x,y=stat.y,label=stats,hjust=stat.hjust)+
    ylab(y.lab)+coord_cartesian(clip='off')+xlab(x.lab)+theme_classic()+theme(legend.position='right')
    p$p.value = p.value
    p$r = rho}else{
        stats = sprintf('R%s,p%s', txtround(0),txtround(1))
        p=ggplot(data,aes_string(x=x,y=y))+geom_point(aes_string(color=color),size=0.4)+
            scale_color_manual(name=color.lab,values=color.palette)+
            annotate('text',x = stat.x,y=stat.y,label=stats,hjust=stat.hjust)+
        ylab(y.lab)+coord_cartesian(clip='off')+xlab(x.lab)+theme_classic()+theme(legend.position='right')
        p$p.value = 1;p$r = 0
    }
    p
}
corhtmap = function(rna,res,htmap.title=NULL,silent = T,...){
    multivariate.cox.pass.sig = cbind(t(rna),res)%>%cor_mat()
    rna.rownames = rownames(rna);res.colnames=colnames(res)
    multivariate.cox.pass.sigR = make.rownames(multivariate.cox.pass.sig,'rowname',F)[res.colnames,rna.rownames]%>%as.matrix
    multivariate.cox.pass.sigp = make.rownames(cor_get_pval(multivariate.cox.pass.sig),'rowname',F)[res.colnames,rna.rownames]%>%as.matrix
    multivariate.cox.pass.sigp = ifelse(is.na(multivariate.cox.pass.sigR),1,multivariate.cox.pass.sigp)
    multivariate.cox.pass.sigpa = ifelse((multivariate.cox.pass.sigp)<0.05,
                                 ifelse((multivariate.cox.pass.sigp)<0.01,
                                 '**','*'),'')
    multivariate.cox.pass.sigR = ifelse(is.na(multivariate.cox.pass.sigR),0,multivariate.cox.pass.sigR)
    
    lasso.corhtmap = multivariate.cox.pass.sigR %>% 
    pheatmap(display_numbers = multivariate.cox.pass.sigpa, main =htmap.title, silent = silent,...)
}

combineclusters = function(raw.cluster,grouping=as.list(levels(cluster))%>%setNames(levels(cluster))){
    ref = data.frame(stringsAsFactors = F,raw.cluster=unlist(grouping),new.cluster=rep(names(grouping),sapply(grouping,length)))
    vlookup(raw.cluster,ref,'raw.cluster','new.cluster')%>%factor(names(grouping))
}
ccp.withinRatio = function(ccp){sapply(ccp[-1],function(ccp){with(ccp,{
    rownames(consensusMatrix)=names(consensusClass)
    colnames(consensusMatrix)=names(consensusClass)
    consensusMatrix.long = as.data.frame(consensusMatrix) %>% keep.rownames('barcode0')%>%
    melt(id.vars = 'barcode0',value.name = 'ci',variable.name = 'barcode1')%>%
    within({
        ccp0 = consensusClass[barcode0]
        ccp1 = consensusClass[barcode1]
        position = ifelse(ccp0==ccp1,'within','without')
    })
    mean(with(consensusMatrix.long,ci[position=='within']))/mean(with(consensusMatrix.long,ci[position=='without']))
})})}

ccp.size.Min2med = function(ccp){sapply(ccp[-1],function(ccp){with(ccp,{
    ct=table(consensusClass);ctd = min(ct)/median(ct)
})})}
ccp.size.Min2Max = function(ccp){sapply(ccp[-1],function(ccp){with(ccp,{
    ct=table(consensusClass);ctd = min(ct)/max(ct)
})})}

ccp.size.Mad = function(ccp){sapply(ccp[-1],function(ccp){with(ccp,{
    ct=table(consensusClass);ctd = mean(abs(ct-median(ct)))
})})}

ccp.CDF=function(ccps,breaks=100,normalizer = ccp.size.Min2med){
    ml=c(list(matrix(0)),lapply(ccps[-1],function(ccp){with(ccp,ml)}))
  par(mfrow=c(2,3))
    #plot CDF distribution
  plot(c(0),xlim=c(0,1),ylim=c(0,1),col="white",bg="white",xlab="consensus index",ylab="CDF",main="consensus CDF", las=2)
  k=length(ml)
  this_colors = rainbow(k-1)
  areaK = c()
  for (i in 2:length(ml)){
    v=triangle(ml[[i]],mode=1)

    #empirical CDF distribution. default number of breaks is 100    
    h = hist(v, plot=FALSE, breaks=seq(0,1,by=1/breaks))
    h$counts = cumsum(h$counts)/sum(h$counts)

    #calculate area under CDF curve, by histogram method.
    thisArea=0
    for (bi in 1:(length(h$breaks)-1)){
       thisArea = thisArea + h$counts[bi]*(h$breaks[bi+1]-h$breaks[bi]) #increment by height by width
       bi = bi + 1
    }
    areaK = c(areaK,thisArea)
    lines(h$mids,h$counts,col=this_colors[i-1],lwd=2,type='l')
  }
  legend(0.6,0.4,legend=paste(rep("k=",k-1),seq(2,k,by=1),sep=""),fill=this_colors)

  #plot area under CDF change.
  deltaK=areaK[1] #initial auc at k=2
  for(i in 2:(length(areaK))){
    #proportional increase relative to prior K.
    deltaK = c(deltaK,( areaK[i] - areaK[i-1])/areaK[i-1])
  }
  plot(1+(1:length(deltaK)),y=deltaK,xlab="k",ylab="relative change in area under CDF curve",main="Delta area",type="b")
    withinRatios = ccp.withinRatio(ccps)
    withinRatios.delta = withinRatios-c(1,withinRatios[-length(withinRatios)])
    normalize.factor = normalizer(ccps)
    Normalize.withinRatios = withinRatios*normalize.factor
    plot(1+(1:length(deltaK)),y=withinRatios,xlab="k",ylab="Ratio",main="Mean Consensus Index within Clusters/between",type="b")
    plot(1+(1:length(deltaK)),y=withinRatios.delta,xlab="k",ylab="Delta Ratio",main="Consensus Index within Clusters/between (Delta)",type="b")
    plot(1+(1:length(deltaK)),y=Normalize.withinRatios,xlab="k",ylab="Normalized Ratio",main="Consensus Index within Clusters/between (Normalized)",type="b")
}

triangle = function(m,mode=1){
  #mode=1 for CDF, vector of lower triangle.
  #mode==3 for full matrix.
  #mode==2 for calcICL; nonredundant half matrix coun
  #mode!=1 for summary 
  n=dim(m)[1]
  nm = matrix(0,ncol=n,nrow=n)
  fm = m


  nm[upper.tri(nm)] = m[upper.tri(m)] #only upper half
  
  fm = t(nm)+nm
  diag(fm) = diag(m)
  
  nm=fm
  nm[upper.tri(nm)] = NA
  diag(nm) = NA
  vm = m[lower.tri(nm)]
  
  if(mode==1){
    return(vm) #vector 		
  }else if(mode==3){
    return(fm) #return full matrix
  }else if(mode == 2){
    return(nm) #returns lower triangle and no diagonal. no double counts.
  }
  
}
    
ccp.htmap = function(ccp,ccp.grouping=NULL,ccp.palette=NULL,matrix.col='blue'){
    suppressPackageStartupMessages({require(ComplexHeatmap);require(circlize)})
    with(ccp,{
    consensusClass = paste0('cluster',consensusClass)
    if(!is.null(ccp.grouping)){consensusClass=combineclusters(consensusClass,ccp.grouping)}
    if(is.null(ccp.palette)){ccp.palette = clrs[[3]]%>%setNames(paste0('cluster',1:length(clrs[[3]])))}
    colannot = columnAnnotation(
        consensusClass = consensusClass,
        col=list(consensusClass = ccp.palette),
        na_col='white',annotation_height=unit(0.5,'cm'),show_legend=T
    )
    cr = colorRamp2(c(0, 1), c("white", matrix.col))
    htmap = Heatmap(matrix = consensusMatrix,col=cr,name = 'Consensus\nIndex',
                    heatmap_height = unit(10,'cm'),heatmap_width = unit(10,'cm'),
                    cluster_columns = consensusTree,cluster_rows = consensusTree,
            top_annotation = colannot)
    htmap
})}
# options(repr.plot.height=5,repr.plot.width=18)
# mat.c2med.TP.decide3 %>% ccp.htmap(ccp.grouping,ccp.palette)

# functional analysis
myenrich=function (entrezIDs, DB, universe = NULL) 
{
    enrich = enricher(gene = entrezIDs, TERM2GENE = DB[1:2],qvalueCutoff = 1,pvalueCutoff = 1, maxGSSize = 5000,
        universe = universe)
    enrich@result = within(enrich@result, {
        Description = vlookup(ID, DB, "annotID", "annotTerm")
    })
    enrich
}

mygse=function (named_x, DB) 
{
    gse = GSEA(gene = named_x, TERM2GENE = DB[1:2],maxGSSize = 5000,pvalueCutoff = 1)
    gse@result = within(gse@result, {
        Description = vlookup(ID, DB, "annotID", "annotTerm")
    })
    gse
}

annotDB2pub = function(db){
    strsplit(db,'_')%>%sapply(function(segs){
        if(length(segs)>1){segs = segs[-1]}
        paste0(segs,collapse=' ')
    })
}

annotTerm2pub2 =function(term){
    strsplit(term,'_')%>%sapply(function(segs){
        segs = segs[-1]
        segs2 = segs%>%tolower %>% (Hmisc::capitalize)
        segs = case_when(segs %in% c('AND')~tolower(segs),nchar(segs)>3~segs2,TRUE~segs)
        paste0(segs,collapse=' ')
    })
}

description2pub = function(obj){
    if(nrow(obj@result)>0){
        obj@result=within(obj@result,{Description = annotTerm2pub(Description)})
    }
    obj
}
computeGeneRatio = function(GeneRatio){
    strsplit(GeneRatio,'/')%>%sapply(function(x){x=as.numeric(x);return(x[1]/x[2])})
}

computeOddsRatio = function(gr,br){
    paste0(gr,'/',br)%>%strsplit('/')%>%
    sapply(function(x){
        x=as.numeric(x);
        a = x[1];b=x[2]-x[1];c=x[3];d=x[4]-x[3]
        a*d/(b*c)
    })
}

addOddsRatio = function(obj){
    if (nrow(obj@result) > 0) {
        obj@result = within(obj@result, {
            OddsRatio = mapply(computeOddsRatio,GeneRatio,BgRatio)
            log2OddsRatio = log2(OddsRatio)
        })
    }
    obj
}
filterORA = function(ORA,...){
    ORA@result = ORA@result %>% filter(...)
    ORA
}

OR.dotplot = function(ORA,showCategory=10,title='',ordered.by = 'OddsRatio'){
    tab = addOddsRatio(ORA)@result
    tab = tab[order(tab[[ordered.by]],decreasing = T),]%>% 
    filter(!duplicated(Description))%>% head(showCategory)%>% arrange_at(ordered.by)
    tab[['Description']]=factor(tab[['Description']],as.character(tab[['Description']]))
    ggplot(tab,aes_string(x='Description',y=ordered.by,color='p.adjust',size="Count"))+geom_point()+
    coord_flip(clip='off')+ggtitle(title)+
    scale_color_gradient(name = 'p.adjust',guide=guide_colourbar(reverse = TRUE),low = 'red',high='blue')+theme_bw()
}

maxn=function(x=NULL,...){
    if(is.null(x)){return(NULL)}else{return(max(x,...))}
}
gsva.hbar = function(data, palette = c(up = "#e69f00", down = "#56b4e9")){
    data = within(data, {
        txt.pos = ifelse(t < 0, 0.5, -0.5)
        direction = ifelse(t < 0, "down", "up")
    })
    ylims = with(data,{
        plens = split(strwidth(as.character(pathway), font = 2, units = 'in')*5,ifelse(t>0,'left','right'))
        tlens = split(abs(t)*1.05,ifelse(t<0,'left','right'))
#         list(left=(-1)*min(maxn(plens$left),maxn(tlens$left)*1.8),right=min(maxn(plens$right),maxn(tlens$right)*1.8))
        list(left=(-1)*max(c(plens$left,tlens$left)),right=max(c(plens$right,tlens$right)))
    })
    ggplot(data) + geom_bar(aes_string(x = "pathway", y = "t", 
        fill = "direction"), stat = "identity") + ylab("t values of differential\nGSVA scores") + 
        geom_text(data = data %>% filter(t > 0), mapping = aes(y = txt.pos, 
            x = pathway, label = pathway), hjust = 1,size=3) + geom_text(data = data %>% 
        filter(t < 0), mapping = aes(y = txt.pos, x = pathway, 
        label = pathway), hjust = 0,size=3)+ ylim(ylims$left,ylims$right) + coord_flip(clip = "off") + 
        scale_fill_manual(values = palette) + theme_classic() + 
        theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), 
            legend.position = "none", axis.text.y = element_blank(), 
            axis.line.y = element_blank())
}
coef2coxsummary = function(coef,nz=T){
    if(nz){coef=coef[coef!=0]}
    o = coef%>% sort(decreasing = F)%>% as.data.frame %>%
    setNames('beta')%>% keep.rownames('variable')
    o
}
beta.hbar = function(beta, palette = c(up = "#e69f00", down = "#56b4e9"),txt.size=4){
    beta = beta[beta!=0]
    lab.space = min(abs(beta),0.02)
    data = beta %>% sort(decreasing = F)%>% as.data.frame %>%
    setNames('beta')%>% keep.rownames('variable') %>%
    within({
        variable = factor(variable,unique(variable))
        txt.pos = ifelse(beta<0, lab.space, -lab.space)
        direction = ifelse(beta<0, "down", "up")
    })
    ylims = with(data,{
        plens = split(strwidth(as.character(variable), font = 2, units = 'in'),ifelse(beta>0,'left','right'))
        tlens = split(abs(beta)*1.05,ifelse(beta<0,'left','right'))
#         list(left=(-1)*min(maxn(plens$left),maxn(tlens$left)*1.8),right=min(maxn(plens$right),maxn(tlens$right)*1.8))
#         list(left=(-1)*max(c(plens$left,tlens$left)),right=max(c(plens$right,tlens$right)))
        list(left=(-1)*max(c(tlens$left,0)),right=max(c(tlens$right,0)))
    })
    ggplot(data) + geom_bar(aes_string(x = "variable", y = "beta", 
        fill = "direction"), stat = "identity") + ylab("Model betaficients") + 
        geom_text(data = data %>% filter(beta > 0), mapping = aes(y = txt.pos, 
            x = variable, label = variable), hjust = 1,size=txt.size) + geom_text(data = data %>% 
        filter(beta<0), mapping = aes(y = txt.pos, x = variable, 
        label = variable), hjust = 0,size=txt.size)+ #ylim(ylims$left,ylims$right) + 
    coord_flip(clip = "off") + 
        scale_fill_manual(values = palette) + theme_classic() + 
        theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), 
            legend.position = "none", axis.text.y = element_blank(), 
            axis.line.y = element_blank())
}
annotTerm2pub = function (term) 
{
    strsplit(term, "_") %>% sapply(function(segs) {
        segs = segs[-1]
        Hmisc::capitalize(paste0(tolower(segs), collapse = " "))
    })
}
# differential expression
shen.limmatrendF = function(m,g){
    DMR.classes=g;DMR.Data=m
    Des <- model.matrix(~0 + DMR.classes)
      colnames(Des) <- levels(factor(DMR.classes))

      LimmaFit <- lmFit(DMR.Data, Des)%>%
        eBayes(., trend = TRUE)%>%
        topTable(., number = nrow(DMR.Data))

      LimmaFit <- LimmaFit%>%.[order(.$adj.P.Val),]
    LimmaFit
}

shen.limmatrendT = function(m,g){
    DMR.classes=paste0('g',factor(g)%>%as.numeric);DMR.Data=m
    Des <- model.matrix(~0 + DMR.classes)
      colnames(Des) <- levels(factor(DMR.classes))
    cm = makeContrasts(gc = g2-g1, levels=Des)
      LimmaFit <- lmFit(DMR.Data, Des)%>%
    contrasts.fit(cm)%>%
        eBayes(.)
      LimmaFit <- topTable(LimmaFit, "gc", number=Inf)%>%.[order(.$adj.P.Val),]
    LimmaFit
}

gsva.limma = function(gsva.mat,g){
    DMR.classes = paste0("g", factor(g) %>% as.numeric)
    DMR.Data = gsva.mat
    Des <- model.matrix(~0 + DMR.classes)
    colnames(Des) <- levels(factor(DMR.classes))
    cm = makeContrasts(gc = g2 - g1, levels = Des)
    LimmaFit <- lmFit(DMR.Data, Des) %>% contrasts.fit(cm) %>% 
        eBayes(.)
    LimmaFit <- topTable(LimmaFit, "gc", number = Inf) %>% .[order(.$adj.P.Val), 
        ]
    LimmaFit
}

tipTable = function(topTable,n=10){
    topTable=topTable %>% arrange(adj.P.Val)
    bind_rows(topTable%>%filter(logFC>0)%>%head(n),topTable%>%filter(logFC<0)%>%head(n))
}

# survival analysis
nz.coefs = function(fit){
    co = coef(fit,s=fit$lambda.min)%>%as.matrix
    co[co[,1]!=0,1]
}

mycoxph = function(data,x.names,time.name='OS',status.name='status'){
    formula = sprintf('Surv(%s, %s) ~ %s',time.name,status.name,paste0(x.names,collapse = '+'))
    fit = coxph(formula%>%as.formula, data = data);fit$data = data
    fit
}
# mycoxnet = function(data,x.names){
#     glmnet(x=data[,x.names],  y=with(data,Surv(time = OS,event = vital_status)), family = "cox")
# }
# mycoxnet=function(data, x.names){
#     cv.glmnet(x = data[, x.names], y = with(data, Surv(time = OS, 
#         event = vital_status)), family = "cox", type.measure = "C")
# }
stat2p = function(Est,u,l){
#     https://www.bmj.com/content/343/bmj.d2304
#     If the upper and lower limits of a 95% CI are u and l respectively:
#     For a ratio measure, such as a risk ratio, the above formulas should be 
#     used with the estimate Est and the confidence limits on the log scale 
#     (eg, the log risk ratio and its CI).
#     Est = log(0.81) = −0.211
#     l = log(0.70) = −0.357, u = log (0.94) = −0.062
#     1 calculate the standard error: SE = (u − l)/(2×1.96)
    SE = (u - l)/(2*1.96)
#     2 calculate the test statistic: z = Est/SE
    z = Est/SE
#     3 calculate the P value2: P = exp(−0.717×z − 0.416×z2).
    exp(-0.717*z - 0.416*(z^2))
}
mycph = function(data,x.names,time.name='OS',status.name='status',time.inc=365){
    formula = sprintf('Surv(%s, %s) ~ %s',time.name,status.name,paste0(x.names,collapse = '+'))
    fit = cph(formula%>%as.formula, data = data, x=T, y=T, surv=T, time.inc=time.inc);fit$data = data
    fit
}
score.histogram = function(data,score,grouping,cutoff,palette,hjust=0,y=10,yend=5,nbin=ceiling(nrow(data)/8)){
    data %>% 
    gghistogram(x=score,color=grouping,fill=grouping,bins = nbin)+
        scale_color_manual(name =score,values = palette)+
        scale_fill_manual(name =score,values = palette)+
        annotate("segment", x =cutoff, xend =cutoff, y =y, yend = yend,
                 arrow = arrow( length = unit(.01,"npc")))+coord_cartesian(clip='off')+
        annotate("label", x =cutoff, y =y, label=paste0('cutoff = ',round(cutoff,3)),hjust=hjust)+
        theme(legend.position = 'right')
}
survival.tcga = function(data,variables,plot=T,legend.position='right',surv.median.line='v',xlims=NULL,medOS.x=NULL,medOS.y=1,
#                                break.time.by = 360,#xlim = c(0,1000),xtickslab.rt = 45,
                               ...
                              ){
   data = na.omit(data[,c('OS','vital_status',variables)])
    RHS = paste0(variables,collapse = '+')
    surv.formula = sprintf('Surv(OS, vital_status) ~ %s,data = data',RHS)
    coxfit = parse(text = sprintf('coxph(%s)',surv.formula) ) %>% eval
    if(plot){
        survfit =  parse(text = sprintf('survfit(%s)',surv.formula) ) %>% eval
        medOS = survfit %>% surv_median 
        medOS.legend =medOS %>% with({
            paste0('Median OS Months (95% CI)','\n',
                   paste0(extract.between(strata,'=','$'),': ',round(median/30,1),
                          ' (',round(lower/30,1),'-',round(upper/30,1),')',collapse='\n'))
        })
        max.medOS = min(max(medOS$median,na.rm = T),3*min(medOS$median,na.rm = T));
        if(is.null(xlims)){xlims=c(0,2*max.medOS)}
        if(is.null(medOS.x)){medOS.x = xlims[2]/2}
    plot = ggsurvplot(fit = survfit, 
                        data =data,
                        pval = TRUE,surv.median.line=surv.median.line,xlim = xlims,
#                         break.time.by = break.time.by,#xtickslab.rt = xtickslab.rt,
                       ...
                      )
        
        plot=within(plot,{
            if(surv.median.line!='none'){plot = plot+annotate('text',x=medOS.x,y=medOS.y,label=medOS.legend,hjust=0,vjust=1)}
            plot=plot+theme(legend.position = legend.position)
        })
    out = list(plot = plot, data.subset = data,survfit=survfit,medOS=medOS)
    }else{
        out = list(data.subset = data)
    }    
}

survplot.save = function(plot,filename){
    ggsave(filename,
       plot = print(plot),
       width = 4.5+0.1*(plot$data.survplot$strata%>% levels %>% nchar %>% max),height = 4)
    filename
}
plot.timeROC125=function(roc.obj){
    plot(roc.obj, time=1 * 365, col = "red", title = FALSE)  
    plot(roc.obj, time=2 * 365, add=TRUE, col="blue") 
    plot(roc.obj, time=3 * 365, add=TRUE, col="green") 
    legend("right",sprintf('%s (AUC=%.3f)',c("1 Years" ,"2 Years", "3 Years"),roc.obj$AUC),
           col=c("red", "blue", "green"), lty=1, lwd=2)
}
timeROC2tidy=function(roc.obj){
    roc.obj %>% with({
    timepoint.levels = colnames(TP)
    data.frame(stringsAsFactors = F,TP,idx=1:nrow(TP),tab='TP')%>% 
    rbind(data.frame(stringsAsFactors = F,FP,idx=1:nrow(FP),tab='FP'))%>%
    setNames(c(timepoint.levels,'idx','tab'))%>%
    melt(id.vars = c('idx','tab'),value.name='v',variable.name = 'timepoint')%>%
    dcast(idx+timepoint~tab,mean,value.var = 'v')%>%
    within({timepoint=factor(timepoint,timepoint.levels);
            time.point.lab = names(times)[as.numeric(timepoint)];
            AUC=roc.obj$AUC[timepoint]})})
}

ggplot.multi_timeROC = function(tidy,labels = unique(tidy$variable),title=NULL,legend.position = c(0.78, 0.25),palette=1){
    tidy$variable = factor(tidy$variable,unique(tidy$variable))
    if(is.null(title)){title=tidy$time.point.lab[1]}else{title=paste0(title,': ',tidy$time.point.lab[1])}
    auc = tidy %>% group_by(variable) %>% summarise(auc = AUC[1])%>%with(setNames(auc,variable))
    ggplot(tidy)+geom_line(aes(x=FP,y=TP,group=variable,color=variable),size=1)+xlab('1-Specificity')+ylab('Sensitivity')+
    scale_color_manual(name=NULL,values = brewer_pal('qual',palette = palette,direction = -1)(length(auc))[1:length(auc)],
                       labels=sprintf('%s (AUC=%.3f)',labels,auc))+
    theme_classic()+theme(legend.position = legend.position,axis.title = element_text(size=13),
                          axis.text = element_text(size=10),legend.text = element_text(size=7.5),
                          legend.title = element_text(size=13))+ggtitle(title)
}
compute_C_index = function(fit){
    set.seed(1024)
    # Get the Dxy
    v <- rms::validate(fit, dxy = TRUE, B = 1000)
    Dxy = v[rownames(v) == "Dxy", colnames(v) == "index.corrected"]
    orig_Dxy = v[rownames(v) == "Dxy", colnames(v) == "index.orig"]

    # The c-statistic according to Dxy=2(c-0.5)
    c(bias_corrected_c_index=abs(Dxy)/2 + 0.5,
    orig_c_index=abs(orig_Dxy)/2 + 0.5)
}
plot.cal3 = function(rms,time.point.palette=c('#d15346','#229e7c','#22b9d1')){
    with(rms,{plot(cal1, lwd = 2, lty = 1, errbar.col = time.point.palette[1],
        xlab = "Predicted Probability Overall Survival", ylab = "Actual Overall Survival",
        col = time.point.palette[1], subtitles = FALSE, xlim = c(0, 1), ylim = c(0, 1), main = "Calibrate plot")
    lines(cal1[, c("mean.predicted", "KM")], type = "l", lwd = 2, col = time.point.palette[1])
    abline(0, 1, lty = 3, lwd = 2, col = 'gray')
    plot(cal2, lwd = 2, lty = 1, errbar.col = time.point.palette[2],add=T,
        xlab = "Predicted Probability Overall Survival", ylab = "Actual Overall Survival",
        col =time.point.palette[2])
    lines(cal2[, c("mean.predicted", "KM")], type = "l", lwd = 2, col = time.point.palette[2])
    plot(cal3, lwd = 2, lty = 1, errbar.col = time.point.palette[3],add=T,
        xlab = "Predicted Probability Overall Survival", ylab = "Actual Overall Survival",
        col =time.point.palette[3])
    lines(cal3[, c("mean.predicted", "KM")], type = "l", lwd = 2, col = time.point.palette[3])
    legend(x=0.6,y=0.25,legend=names(time.points),bty='n',
           col=c(time.point.palette[1],time.point.palette[2],time.point.palette[3]),lty=rep(1,3),lwd=2,y.intersp = 1.2)})
}
# misc
gidx = function(pos,chr,g,gcs = g %>% as.numeric %>% cumsum%>%setNames(names(g))){
    gcs[chr]-g[chr]+pos
}

read.gct = function(gct){
    read.table(gct,header = T,sep = '\t',skip = 2,stringsAsFactors = F)[-2]%>%make.rownames('NAME',F)%>%t
}

estimateTumorPurity = function(EstimateScore){
#     https://www.nature.com/articles/ncomms3612
    i = 0.6049872018+0.0001467884*EstimateScore
    ifelse(between(i,0,pi/2),cos(i),ifelse(i<0,1,0))
}

write.txt=function (data, file) 
{
    data = keep.rownames(data, "Symbol")
    write.table(data, file, quote = F, row.names = F, sep = "\t")
}
#wgcna
wgcna.setup = function(mat,powers = c(c(1:10), seq(from = 12, to=20, by=2)) ){
    within(as.list(environment()),{
        WGCNA_matrix = t(mat)

    datExpr0 = as.data.frame(WGCNA_matrix) 

    gsg = goodSamplesGenes(datExpr0, verbose = 3)

    datExpr = datExpr0[gsg$goodSamples, gsg$goodGenes]
    nGenes = ncol(datExpr)
    sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)})
}

plot.wgcna.sft = function(wgcna,cex = 0.9){
    with(sft,{
        par(mfrow = c(1,2))
        cex1 = cex
        col1 = rep("black",length(powers));col1[powers==sft$powerEstimate]='red'
        plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2], 
             xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",
             type="n",main = paste("Scale independence"))
        text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
             labels=powers,cex=cex1,col=col1) 
        abline(h=0.90,col="red")

        plot(sft$fitIndices[,1], sft$fitIndices[,5], 
             xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n", main = paste("Mean connectivity"))
        text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col=col1)
    })
}

wgcna.genetree1 = function(wgcna,minModuleSize = round(wgcna$nGenes/20),deepSplit = 4){
    within(wgcna,{
        
        softPower = sft$powerEstimate
    # 共表达相似性和邻接性
    adjacency = adjacency(datExpr, power = softPower)
    # 为了减小噪声和虚假关联的影响，我们将邻接变换为拓扑重叠矩阵，并计算相应的不相似度。
    TOM = TOMsimilarity(adjacency)
    dissTOM = 1-TOM

    # 现在使用层次聚类来产生基因的层次聚类树(树状图)。
    geneTree = hclust(as.dist(dissTOM), method = "average")

    # 模块识别使用动态树切割，这里敏感度deepSplit我设置了1，需要根据自己数据进行调试，默认是2
    dynamicMods = cutreeDynamic(dendro = geneTree, distM = dissTOM, 
    deepSplit = deepSplit, pamRespectsDendro = FALSE, minClusterSize = minModuleSize) 
    
    dynamicColors = labels2colors(dynamicMods)
    # colorSeq = colorRampPalette(brewer.pal(8, "Set2"))(length(unique(dynamicMods)))
    print(table(dynamicColors))
    # 量化各个模块的共表达相似性，根据模块间的相关性合并表达模式相似的模块。Calculate eigengenes
    MEList = moduleEigengenes(datExpr, colors = dynamicColors)

    MEs = MEList$eigengenes

    #  Calculate dissimilarity of module eigengenes
    MEDiss = 1-cor(MEs)
    #  Cluster module eigengenes
    METree = hclust(as.dist(MEDiss), method = "average")})
}

plot.genetree1=function(wgcna.genetree1,MEDissThres = 0.4){
    with(wgcna.genetree1,{
    # 这里选择切割高度0.3，即模块之间相关性达到0.7进行合并，这个系数也要根据自己的数据进行更改，常见的是0.25（Fig.4）。
    plot(METree, main = "Clustering of module eigengenes", xlab = "", sub = "")
    # 在树状图中加入切割线
    abline(h=MEDissThres, col = "red")
    invisible(MEDissThres)})
}

wgcna.mergeME = function(wgcna.genetree1, MEDissThres=0.4){# 调用自动归并函数
within(wgcna.genetree1,{
    merge = mergeCloseModules(datExpr, dynamicColors, cutHeight = MEDissThres, verbose = 3)
    # 合并模块的颜色
    mergedColors = merge$colors
    print(table(mergedColors))
    # 合并后新模块
    mergedMEs = merge$newMEs
    mergedMEgenes = split(colnames(datExpr),mergedColors)
    names(mergedMEgenes) = paste0('ME',names(mergedMEgenes))
})}
wgcna.kme = function(wgcna.merged){
    within(wgcna.merged,{
        datKME=signedKME(datExpr, mergedMEs, outputColumnName="ME")

        datKME.long = datKME %>% keep.rownames('matID') %>% 
        melt(id.vars = 'matID',value.name = 'MM',variable.name = 'module')%>%
        within({absMM=abs(MM)})
    })
}

wgcna.traitcor = function(wgcna.kme,datTraits){
    within(wgcna.kme,{
        datTraits=datTraits

        # 明确基因和样本数
        nSamples = nrow(datTraits)

        moduleTraitCor = cor(mergedMEs[rownames(datTraits),], datTraits, use = "p")
        moduleTraitPvalue = corPvalueStudent(moduleTraitCor, nSamples)

        # 通过p值显示相关性
        textMatrix = paste(signif(moduleTraitCor, 2), "\n(", signif(moduleTraitPvalue, 1), ")", sep = "")
        dim(textMatrix) = dim(moduleTraitCor)
        
        moduleTraitCor.long = moduleTraitCor %>% keep.rownames('module')%>% 
        melt(id.vars = 'module',value.name = 'rho',variable.name = 'trait')
        
        geneTraitSignificance = cor(datExpr[rownames(datTraits),], datTraits, use = "p")
        GSPvalue = as.data.frame(corPvalueStudent(geneTraitSignificance, nSamples))
    })
}
plot.mergedME = function(wgcna.merged,...){
    with(wgcna.merged,{plotDendroAndColors(geneTree, cbind(dynamicColors, mergedColors),
    c("Dynamic Tree Cut", "Merged dynamic"), dendroLabels = FALSE, 
    hang = 0.03, addGuide = TRUE, guideHang = 0.05,...)})
}
wgcna.metrait.htmap = function(wgcna.metrait){
    with(wgcna.metrait,{
        par(mar = c(6, 8.5, 3, 3))
        # 通过热图显示相关性
        labeledHeatmap(Matrix = moduleTraitCor,
                       xLabels = names(datTraits), yLabels = names(mergedMEs), ySymbols = names(mergedMEs), colorLabels = FALSE, 
                       colors = blueWhiteRed(50), textMatrix = textMatrix, setStdMargins = FALSE, 
                       cex.text = 0.8, zlim = c(-1,1), main = paste("Module-trait relationships"))
        # 想改变热图配色可以更改 colors = greenWhiteRed(50)
    })
}
scatter.wgcna.mmgs = function(wgcna.metrait,show_trait = 1,show_module='MEgrey'){
    with(wgcna.metrait,{
        p = datKME.long %>% filter(module==show_module) %>% within({
            GS = geneTraitSignificance[matID,show_trait];absGS = abs(GS)
        })%>%(function(data){ ggscatter(data,x='absMM',y='absGS',shape=1)+
        xlab(sprintf('|Module Membership in %s|',show_module))+
        ylab(sprintf('|Gene Significance in %s|',show_trait))})
        p$show_trait=show_trait;p$show_module=show_module
        p
    })
}

#在R4里面，class(matrix)得出c('matrix','array')导致报错，这里稍微改一下

for(prr.script in list.files('/NAS/wg_zsx/bin/pRRophetic/R',full.names=T)){source(prr.script)}

pRRopheticPredict4=function (testMatrix, drug, tissueType = "all", batchCorrect = "eb", 
    powerTransformPhenotype = TRUE, removeLowVaryingGenes = 0.2, 
    minNumSamples = 10, selection = -1, printOutput = TRUE, removeLowVaringGenesFrom = "homogenizeData", 
    dataset = "cgp2014") 
{
    cgpTrainData <- getCGPinfo(drug, tissueType, dataset)
    predictedPtype <- calcPhenotype4(cgpTrainData$trainDataOrd, 
        cgpTrainData$ic50sOrd, testMatrix, batchCorrect = batchCorrect, 
        powerTransformPhenotype = powerTransformPhenotype, removeLowVaryingGenes = removeLowVaryingGenes, 
        minNumSamples = minNumSamples, selection = selection, 
        printOutput = printOutput, removeLowVaringGenesFrom = removeLowVaringGenesFrom)
    return(predictedPtype)
}
ComBat=sva::ComBat
powerTransform = car::powerTransform
linearRidge=ridge::linearRidge
calcPhenotype4= function (trainingExprData, trainingPtype, testExprData, batchCorrect = "eb", 
    powerTransformPhenotype = TRUE, removeLowVaryingGenes = 0.2, 
    minNumSamples = 10, selection = -1, printOutput = TRUE, removeLowVaringGenesFrom = "homogenizeData") 
{
    if (class(testExprData)[1] != "matrix") 
        stop("ERROR: \"testExprData\" must be a matrix.")
    if (class(trainingExprData)[1] != "matrix") 
        stop("ERROR: \"trainingExprData\" must be a matrix.")
    if (class(trainingPtype) != "numeric") 
        stop("ERROR: \"trainingPtype\" must be a numeric vector.")
    if (ncol(trainingExprData) != length(trainingPtype)) 
        stop("The training phenotype must be of the same length as the number of columns of the training expressin matrix.")
    if ((ncol(trainingExprData) < minNumSamples) || (ncol(testExprData) < 
        minNumSamples)) {
        stop(paste("There are less than", minNumSamples, "samples in your test or training set. It is strongly recommended that you use larger numbers of samples in order to (a) correct for batch effects and (b) fit a reliable model. To supress this message, change the \"minNumSamples\" parameter to this function."))
    }
    homData <- homogenizeData(testExprData, trainingExprData, 
        batchCorrect = batchCorrect, selection = selection, printOutput = printOutput)
    if (!(removeLowVaringGenesFrom %in% c("homogenizeData", "rawData"))) {
        stop("\"removeLowVaringGenesFrom\" must be one of \"homogenizeData\", \"rawData\"")
    }
    keepRows <- seq(1:nrow(homData$train))
    if (removeLowVaryingGenes > 0 && removeLowVaryingGenes < 
        1) {
        if (removeLowVaringGenesFrom == "homogenizeData") {
            keepRows <- doVariableSelection(cbind(homData$test, 
                homData$train), removeLowVaryingGenes = removeLowVaryingGenes)
            numberGenesRemoved <- nrow(homData$test) - length(keepRows)
            if (printOutput) 
                cat(paste("\n", numberGenesRemoved, "low variabilty genes filtered."))
        }
        else if (removeLowVaringGenesFrom == "rawData") {
            evaluabeGenes <- rownames(homData$test)
            keepRowsTrain <- doVariableSelection(trainingExprData[evaluabeGenes, 
                ], removeLowVaryingGenes = removeLowVaryingGenes)
            keepRowsTest <- doVariableSelection(testExprData[evaluabeGenes, 
                ], removeLowVaryingGenes = removeLowVaryingGenes)
            keepRows <- intersect(keepRowsTrain, keepRowsTest)
            numberGenesRemoved <- nrow(homData$test) - length(keepRows)
            if (printOutput) 
                cat(paste("\n", numberGenesRemoved, "low variabilty genes filtered."))
        }
    }
    offset = 0
    if (powerTransformPhenotype) {
        if (min(trainingPtype) < 0) {
            offset <- -min(trainingPtype) + 1
            trainingPtype <- trainingPtype + offset
        }
        transForm <- powerTransform(trainingPtype)[[6]]
        trainingPtype <- trainingPtype^transForm
    }
    if (printOutput) 
        cat("\nFitting Ridge Regression model... ")
    trainFrame <- data.frame(Resp = trainingPtype, t(homData$train[keepRows, 
        ]))
    rrModel <- linearRidge(Resp ~ ., data = trainFrame)
    if (printOutput) 
        cat("Done\n\nCalculating predicted phenotype...")
    totBeta <- sum(abs(coef(rrModel)))
    eachBeta <- abs(coef(rrModel))
    eachContribution <- eachBeta/totBeta
    if (class(homData$test)[1] == "numeric") {
        n <- names(homData$test)
        homData$test <- matrix(homData$test, ncol = 1)
        rownames(homData$test) <- n
        testFrame <- data.frame(t(homData$test[keepRows, ]))
        preds <- predict(rrModel, newdata = rbind(testFrame, 
            testFrame))[1]
    }
    else {
        testFrame <- data.frame(t(homData$test[keepRows, ]))
        preds <- predict(rrModel, newdata = testFrame)
    }
    if (powerTransformPhenotype) {
        preds <- preds^(1/transForm)
        preds <- preds - offset
    }
    if (printOutput) 
        cat("Done\n\n")
    return(preds)
}