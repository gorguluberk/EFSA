prepare_data <- function(dataset,label){
	lenseries=ncol(dataset)
	nofdata=nrow(dataset)
	nofclass=length(unique(label))
	
	times=rep(c(1:lenseries),nofdata)
	cls=matrix(label,nrow=nofdata,ncol=lenseries)
    tid=sort(rep(c(1:nofdata),lenseries))
    dataobs=data.table(tid=tid,times=times,obs=c(t(dataset)),cls=factor(c(t(cls))))
    classes=unique(label)
	
	return(list("data" = dataobs,"class"=classes))
	
}