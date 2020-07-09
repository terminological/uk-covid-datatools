#' Survival data procesing
#' @export
SurvivalProcessingPipeline = R6::R6Class("SurvivalProcessingPipeline", inherit=PassthroughFilesystemCache, public = list(

    #' @description generate survival data and right censored data from 
  #' 
  #' @param path - path to the ff100 file
  
  #' @return data set in survival format including status, time, left, right, ageCat columns 
  generateSurvivalData = function(df,
                                  idVar,
                                  startDateVar, 
                                  endDateExpr, 
                                  statusExpr = if_else(is.na({{endDateExpr}}),0,1), 
                                  ageVar = "age", 
                                  censoredDateExpr = max({{endDateExpr}},na.rm = TRUE),
                                  ageBreaks = c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf),
                                  ageLabels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'),
                                  ageReferenceCat = NA,
                                  statusLabels = c("censored","died")
  ) {
    idVar = ensym(idVar) 
    startDateVar = ensym(startDateVar) 
    endDateExpr = enexpr(endDateExpr)
    statusExpr = enexpr(statusExpr)
    ageVar = ensym(ageVar)
    censoredDateExpr = enexpr(censoredDateExpr)
    
    out = df %>% 
      filter(!is.na(!!startDateVar)) %>% 
      mutate(
        startDate = as.Date(!!startDateVar),
        endDate = as.Date(!!endDateExpr),
        id = !!idVar,
        status = !!statusExpr
      ) %>% mutate(
        endDate = if_else(status==0, as.Date(!!censoredDateExpr+0.001), endDate+0.001),
        time = as.numeric(endDate - startDate), # zero times causes issues
        ageCat = cut(!!ageVar,breaks = ageBreaks, labels = ageLabels, ordered_result = TRUE, include.lowest = TRUE),
        status = factor(status, labels=statusLabels)
      ) #%>% mutate(
        #time = time+0.1 # if_else(time==0,0.5,time)
      #)
    
    if (!is.na(ageReferenceCat)) {
      out = out %>% mutate(
        ageCat = relevel(factor(as.character(ageCat),ordered=FALSE), ref = ageReferenceCat)
      )
    }
    
    # first instance of startDate by case
    out = out %>% group_by(id) %>% arrange(startDate) %>% filter(
        row_number() == 1
      )
    # first instance of endDate by case
    out = out %>% arrange(endDate) %>% filter(
        row_number() == 1
      ) 
    out = out %>% filter(
        startDate <= endDate
      ) 
    
    return(out %>% as.data.frame())
  },
  
  getMaxDate = function(df, dateCols) {
    as.Date(max(sapply(dateCols, function(d) max(df[[d]], na.rm=TRUE))),origin=as.Date("1970-01-01"))
  },
  
  
  #' @description generate transition matrix with enquoted tidy column sepecifications
  #' 
  #' @param ... a set of mappings
  
  #' @return a transistion matrix
  transitionMatrix = function(...) {
    dots=enexprs(...)
    states = unique(c(names(dots),sapply(dots, as_label)))
    transMatrix = matrix(rep(NA,length(states)*length(states)),nrow=length(states),dimnames = list(states,states))
    for (i in 1:length(dots)) {
      from = names(dots)[i]
      to = sapply(dots, as_label)[[i]]
      transMatrix[from,to]=i
    }
    return(transMatrix)
  },
  
  #' @description generate survival data and right censored data from a multistate model
  #' 
  #' @param path - path to the ff100 file
  
  #' @return data set in survival format including status, time, left, right, ageCat columns 
  generateMultistateSurvivalData = function(df, 
                idVar,
                transMatrix, 
                ageVar = "age", 
                censoringDateExpr = getMaxDate(df, dateCols), 
                startDateCol=names(allowableTransitions)[1],
                ageBreaks = c(-Inf,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,Inf),
                ageLabels = c('0-4','5-9','10-14','15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'),
                ageReferenceCat = NA,
                ...) {
    
    ageVar = ensym(ageVar)
    idVar=ensym(idVar)
    censoringDateExpr = enexpr(censoringDateExpr)
    keep = df %>% groups() %>% as.character()
    df = df %>% ungroup()
    dots = enexprs(...)
    dateCols = names(dots)
    df = df %>% mutate(..., id=!!idVar)
    df = df %>% mutate(censorDate = as.Date(!!censoringDateExpr))
    
    for(dateCol in dateCols) {
      df = df %>% group_by(id) %>% arrange(!!as.symbol(dateCol)) %>% filter(row_number() == 1)
    }
    
    df = df %>% mutate(
      ageCat = cut(!!ageVar,breaks = ageBreaks, labels = ageLabels, ordered_result = TRUE, include.lowest = TRUE)
    )
    if (!is.na(ageReferenceCat)) {
      out = out %>% mutate(
        ageCat = relevel(factor(as.character(ageCat),ordered=FALSE), ref = ageReferenceCat)
      )
    }
    
    addData = df %>% select(c("id",keep,"ageCat"))
    
    df = df %>% select(c("id",dateCols,startDateCol,"censorDate"))
    statusCols = paste0(dateCols,"_status")
    timeCols = paste0(dateCols,"_time")
    
    df[statusCols] = if_else(is.na(df[dateCols]),0,1)
    df[dateCols] = if_else(is.na(df[dateCols]), rep(as.numeric(df$censorDate),length(dateCols)), unlist(df[dateCols]))
    
    # add noise to outcomes to prevent zero values
    df[dateCols] = df[dateCols] + matrix(rep(seq(0,0.001,length.out = length(dateCols)),nrow(df)),ncol = 3,byrow = TRUE)
    
    df[timeCols] = unlist(df[dateCols]) - rep(as.numeric(unlist(df[startDateCol])),length(dateCols))
    df = df %>% select(c("id",timeCols,statusCols)) %>% as.data.frame()
    
    #transMatrix = unname(transMatrix)
    time=df %>% select(timeCols) %>% as.matrix()
    status= df %>% select(statusCols) %>% as.matrix()
    trans=transMatrix
    start=list(state=1,time=0)
    id=df %>% pull(id)
    
    out = mstate::msprep(time=time,status=status,trans=trans,start=start,id=id)
    out = out %>% left_join(addData, by="id") #%>% mutate(
      #time = time+0.5,#if_else(time ==0, 0.5,time),
      #Tstop = Tstop+0.5#if_else(time ==0, Tstop+0.5,Tstop)
    #)
    class(out) = c(class(out),"msdata")
    return(out)
  },
  
  standardModels = function(items = c("weibull","gamma","lognorm")) {
    tmp = list(
      weibull = list(name = "weibull", start = list(shape =1 , scale=10)),
      gamma = list(name = "gamma", start = list(shape =1 , rate=0.1)),
      lognorm = list(name = "lnorm", start = list(meanlog =2 , sdlog=1)),
      exp = list(name = "exp", start = list(rate=0.5)),
      norm = list(name = "norm", start = list(mean = 1, sd = 1))
    )
    return(tmp[names(tmp) %in% items])
  },
  
  #' @description for each ageCat fit a set of models and return a data fram with the parameters.
  #' 
  #' @param survivalDf - a data frame containing an ageCat, and a left and right column with min and max times of individual events, If event is censored then right is NA
  #' @param models -  a list of models that you want to fit - e.g. lnorm, norm, etc...
  
  #' @return an datafram of ageCat, model, param(eterName), value, low_ci, high_ci
  fitModelsByAge = function(survivalDf, ageVar = "ageCat", models = self$standardModels(),...) {
    
    
    ageVar = ensym(ageVar)
    isCensored = length(levels(survivalDf$status)) > 1
    censoredFac = min(as.integer(survivalDf$status))
    
    #browser()
    censoredDf = survivalDf %>% mutate(
      #time = if_else(time==0, 0.5, time)
    #) %>% mutate(
      left=time,
      right=ifelse(isCensored & as.integer(status)==censoredFac, NA, time)
    )
    
    modelNames = unlist(sapply(models, "[", "name"))
    
    out = censoredDf %>% ungroup() %>% group_by(!!ageVar) %>% group_modify( function(d,g,...) {
      
      tmp = data.frame()
      for (m in models) {
        dist = suppressWarnings({
          tryCatch(fitdistrplus::fitdistcens(d %>% as.data.frame(), m$name, start = m$start, ...), error = function(e) NULL)
        })
        #browser()
        if (!is.null(dist)) {
          #browser()
          dist2 = suppressWarnings(fitdistrplus::bootdistcens(dist,silent = TRUE,niter = 100,ncpus = 3))
          
          tmp = tmp %>% bind_rows(tibble(
            n=dist$n,
            aic=dist$aic,
            bic=dist$bic,
            loglik=dist$loglik,
            model = m$name,
            param = names(dist$estimate),
            value = dist$estimate,
            low_ci = matrix(dist2$CI,ncol=3)[,2],
            high_ci = matrix(dist2$CI,ncol=3)[,3],
          ))
        }
      }
      return(tmp)
  
    })
    
    return(out)
  },
  
  doPdf = function(model, paramNames, params, timepoints, ...) {
    names(params) = paramNames
    params$x=timepoints
    pred = do.call(paste0("d",model),params)
    return(pred)
  },
  
  doCdf = function(model, paramNames, params, timepoints, ...) {
    names(params) = paramNames
    params$q=timepoints
    # browser()
    pred = do.call(paste0("p",model),params)
    return(pred)
  },
  
  doMedian = function(model, paramNames, params, ...) {
    suppressWarnings({names(params) = paramNames})
    params$p=0.5 #c(0.025,0.05,0.25,0.5,0.75,0.95,0.975)
    pred = do.call(paste0("q",model),params)
    return(pred)
  },
  
  doEstimate = function(d, model1, timepoints , ...) {
    out = tibble(
      days = timepoints,
      pdf = self$doPdf(model1, d$param, d$value, timepoints, ...),
      pdf_lo = self$doPdf(model1, d$param, d$low_ci, timepoints, ...),
      pdf_hi = self$doPdf(model1, d$param, d$high_ci, timepoints, ...),
      cdf = self$doCdf(model1, d$param, d$value, timepoints, ...),
      cdf_lo = self$doCdf(model1, d$param, d$low_ci, timepoints, ...),
      cdf_hi = self$doCdf(model1, d$param, d$high_ci, timepoints, ...),
      median = self$doMedian(model1, d$param, d$value, timepoints, ...),
      median_lo = self$doMedian(model1, d$param, d$low_ci, timepoints, ...),
      median_hi = self$doMedian(model1, d$param, d$high_ci, timepoints, ...)
    )
    return(out)
    
  },
  
  #' @description for a set of model parameters, calculate the distributions and median 
  #' 
  #' @param modelParameterDf - a data frame set of parameters for a range of models as param, value columns
  #' @param days - the time frame of the prediction
  
  #' @return an dataframe of ageCat and time varying surfaces, including pdfs, cdfs and medians, with high and low estimates 
  createSurvivalSurfaces = function(modelParameterDf, days=30, timepoints = seq(0.1,days,length.out = 100)) {
    # smooth model parameters over age groups
    out_sm = modelParameterDf %>% ungroup() %>% group_by(model,param) %>% group_modify(function(d,g,...) {
      return(tibble(
        ageCat = d$ageCat,
        n = d$n,
        value = ksmooth(1:length(d$value), d$value, n.points = length(d$value), bandwidth=2)$y,
        low_ci = ksmooth(1:length(d$value), d$low_ci, n.points = length(d$value), bandwidth=2)$y,
        high_ci = ksmooth(1:length(d$value), d$high_ci, n.points = length(d$value), bandwidth=2)$y
      ))
    })
    
    
    
    # calculatate surfaces using doEstimate
    tmp2 = out_sm %>% ungroup() %>% group_by(ageCat, n, model) %>% group_modify(function (d,g,...) {
      est = suppressWarnings(self$doEstimate(d, model = g$model[1], timepoints = timepoints, ...))
    })
    
    tmp2 = tmp2 %>% group_by(ageCat,model) %>% arrange(days) %>% mutate(
      p = cdf-lag(cdf,default=0),
      p_lo = cdf_lo-lag(cdf_lo,default=0),
      p_hi = cdf_hi-lag(cdf_hi,default=0)
    ) %>% ungroup()
    
    return(tmp2)
  },
  
  #' @description plot a survival surface as a CDF
  #' 
  #' @param surfacesDf - an dataframe of ageCat and time varying surfaces, including pdfs, cdfs and medians, with high and low estimates 
  #' @param distr - a single distribution to plot
  
  #' @return a plot of the survival surface (1-cdf)
  plotSurvivalSurface = function(surfacesDf, distr) {
    
    surf1 = ggplot(surfacesDf %>% filter(model==distr),
                   aes(x=days,y=as.integer(ageCat),z=1-cdf, fill=1-cdf))+geom_tile()+scale_fill_gradient2(high="red",mid="yellow",low="green", midpoint=0.5, guide="none", limits=c(0,1))+xlab("days")+ylab("age")+
      metR::geom_contour2(colour="black", breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+scale_y_continuous(
        breaks = 4:17,
        labels = c('15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))+
      metR::geom_contour2(colour="blue", breaks=c(0.5), size=1.5)+
      metR::geom_text_contour(breaks=c(0.1,0.3,0.5,0.7,0.9),stroke=0.2)+guides(fill="none")
      # theme(axis.title=element_blank(),
      #       axis.text.y=element_blank())+guides(fill="none")
    
    
    surf_lo = ggplot(surfacesDf %>% filter(model==distr),
                     aes(x=days,y=as.integer(ageCat),z=1-cdf_hi, fill=1-cdf_hi))+geom_tile()+scale_fill_gradient2(high="red",mid="yellow",low="green", midpoint=0.5, guide="none", limits=c(0,1))+xlab("days")+
      metR::geom_contour2(colour="black", breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+scale_y_continuous(
        breaks = 4:17,
        labels = c('15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))+
      metR::geom_contour2(colour="blue", breaks=c(0.5), size=1.5)+
      theme(axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_blank())+guides(fill="none")
    
    surf_hi = ggplot(surfacesDf %>% filter(model==distr),
                     aes(x=days,y=as.integer(ageCat),z=1-cdf_lo, fill=1-cdf_lo))+geom_tile()+scale_fill_gradient2(high="red",mid="yellow",low="green", midpoint=0.5, guide="none", limits=c(0,1))+xlab("days")+
      metR::geom_contour2(colour="black", breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))+scale_y_continuous(
        breaks = 4:17,
        labels = c('15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))+
      metR::geom_contour2(colour="blue", breaks=c(0.5), size=1.5)+
      theme(axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_blank()
      )+guides(fill="none")
    
    #surf = Nplot + surf1 + (surf_lo / surf_hi) + patchwork::plot_annotation(tag_levels = 'A') + patchwork::plot_layout(ncol=3,widths = c(0.3,2,1))
    surf = surf1 + (surf_lo / surf_hi) + patchwork::plot_annotation(tag_levels = 'A') + patchwork::plot_layout(ncol=2,widths = c(2,1))
    return(surf)
  },
  
  
  #' @description plot a survival surface as a PDF
  #' 
  #' @param surfacesDf - an dataframe of ageCat and time varying surfaces, including pdfs, cdfs and medians, with high and low estimates 
  #' @param distr - a single distribution to plot
  
  #' @return a plot of the survival surface as pdf
  plotIncidenceSurface = function(surfacesDf, distr) {
    surf1 = ggplot(surfacesDf %>% filter(model==distr),
                   aes(x=days,y=as.integer(ageCat),z=pdf, fill=pdf))+geom_tile()+scale_fill_gradient(high="black",low="white", guide = "none")+xlab("days")+ylab("age")+
      metR::geom_contour2(colour="blue")+
      scale_y_continuous(
        breaks = 4:17,
        labels = c('15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))+
      metR::geom_text_contour(stroke=0.2)+
      guides(fill="none")
    
    surf_lo = ggplot(surfacesDf %>% filter(model==distr),
                     aes(x=days,y=as.integer(ageCat),z=pdf_hi, fill=pdf_hi))+geom_tile()+scale_fill_gradient(high="black",low="white", guide = "none")+
      metR::geom_contour2(colour="blue")+
      scale_y_continuous(
        breaks = 4:17,
        labels = c('15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))+
      theme(axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_blank())+
      guides(fill="none")
    
    surf_hi = ggplot(surfacesDf %>% filter(model==distr),
                     aes(x=days,y=as.integer(ageCat),z=pdf_lo, fill=pdf_lo))+geom_tile()+scale_fill_gradient(high="black",low="white", guide = "none")+
      metR::geom_contour2(colour="blue")+
      scale_y_continuous(
        breaks = 4:17,
        labels = c('15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))+
      theme(axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.text.x=element_blank()
      )+
      guides(fill="none")
    
    surf = surf1 + (surf_lo / surf_hi) + patchwork::plot_annotation(tag_levels = 'A') + patchwork::plot_layout(ncol=2,widths = c(2,1))
    return(surf)
  },
  
  
  plotProbabilityMatrix = function(surfacesDf, distr, pExpr = "p" ) {
    pExpr = enexpr(pExpr)
    ggplot(surfacesDf %>% filter(model==distr) %>% mutate(tmp_p = !!pExpr),
        aes(x=days,y=as.integer(ageCat),z=tmp_p, fill=tmp_p, label=sprintf("%1.2f",tmp_p),colour=tmp_p))+
        geom_tile(colour="white")+
        geom_text(angle=90, size=2, colour="black")+
        scale_fill_gradientn(colours=c("white","green","yellow","orange","red"), breaks=c(0,0.005,0.025,0.075,0.125,1))+xlab("days")+ylab("age")+
        scale_y_continuous(
          breaks = 4:17,
          labels = c('15-19','20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80+'))+
        guides(fill="none", colour="none")
    
  }
))