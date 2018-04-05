generate_shapelets<-function(train_data,train_label,nofsample=50){
	difftrain_data=t(apply(train_data,1,diff))
	doubdifftrain_data = t(apply(difftrain_data,1,diff))
	train_dict = prepare_data(train_data,train_label)
	trainobs = train_dict$data
	classes = train_dict$class
	difftrain_dict = prepare_data(difftrain_data,train_label)
	difftrainobs = difftrain_dict$data
	doubdifftrain_dict = prepare_data(doubdifftrain_data,train_label)
	doubdifftrainobs = doubdifftrain_dict$data
	
	first=TRUE
	nsample=1
	for(repl in 1:nofsample){
		sec=unlist(lapply(classes,function(x) { ind=which(train_label==x);
			if(length(ind)<=nsample) { ind }	else {
				sample(ind,nsample);
		}}))
		inputtrain=as.data.frame(trainobs[tid %in% sec,c(2:4),with=F])
		diffinputtrain=as.data.frame(difftrainobs[tid %in% sec,c(2:4),with=F])
		doubdiffinputtrain=as.data.frame(doubdifftrainobs[tid %in% sec,c(2:4),with=F])

		fit=randomForest(inputtrain[,c(1,3)],inputtrain$obs,ntree=1,replace=F,sampsize=nrow(inputtrain))
		difffit=randomForest(diffinputtrain[,c(1,3)],diffinputtrain$obs,ntree=1,replace=F,sampsize=nrow(diffinputtrain))
		doubdifffit=randomForest(doubdiffinputtrain[,c(1,3)],doubdiffinputtrain$obs,ntree=1,replace=F,sampsize=nrow(doubdiffinputtrain))

		doubdifftrain_terminal=attr(predict(doubdifffit, as.matrix(doubdiffinputtrain[,c(1,3)]),nodes=TRUE), "nodes")
		difftrain_terminal=attr(predict(difffit, as.matrix(diffinputtrain[,c(1,3)]),nodes=TRUE), "nodes")
		train_terminal=attr(predict(fit, as.matrix(inputtrain[,c(1,3)]),nodes=TRUE), "nodes")

		inputtrain=data.table(inputtrain)
		inputtrain[,TerminalNodes:=train_terminal]
		newinput=inputtrain[,list(Start=min(times),End=max(times),Level=mean(obs),Times=times),by=list(TerminalNodes,cls)]#cls by
		diffinputtrain=data.table(diffinputtrain)
		diffinputtrain[,TerminalNodes:=difftrain_terminal]

		doubdiffinputtrain=data.table(doubdiffinputtrain)
		doubdiffinputtrain[,TerminalNodes:=doubdifftrain_terminal]

		adjust=diffinputtrain[times == max(diffinputtrain$times)]
		adjust$times=adjust$times+1
		diffinputtrain=rbind(diffinputtrain,adjust)
		diffnewinput=diffinputtrain[,list(DStart=min(times),DEnd=max(times),DLevel=mean(obs),Times=times),by=list(TerminalNodes,cls)]

		doubadjust=doubdiffinputtrain[times == max(doubdiffinputtrain$times)]
		doubadjust$times=doubadjust$times+1
		doubdiffinputtrain=rbind(doubdiffinputtrain,doubadjust)
		doubadjust$times=doubadjust$times+1
		doubdiffinputtrain=rbind(doubdiffinputtrain,doubadjust)
		doubdiffnewinput=doubdiffinputtrain[,list(DStart=min(times),DEnd=max(times),DLevel=mean(obs),Times=times),by=list(TerminalNodes,cls)]

		fulldata=merge(newinput,diffnewinput,by=c("cls","Times"),all=T)
		fulldata=merge(fulldata,doubdiffnewinput,by=c("cls","Times"),all=T)   
		fulldata=fulldata[,by=list('cls','Times')]
		fulldata = fulldata[,list(Class=cls, Times=Times, TerminalNodes = TerminalNodes.x,Start=Start, End=End,Level=Level,Trial = mean(DLevel.x),DStart=DStart.x,DEnd=DEnd.x,Curvature=mean(DLevel.y),Length=End-Start+1,Consta=(Start-(Start+End)/2)+(Times-Start),DConsta=(DStart.x-(DStart.x+DEnd.x)/2)+(Times-DStart.x)),by=list(TerminalNodes.x)]

		fulldata[,a:=Curvature/2,by=list(TerminalNodes,Class,Times)]
		fulldata[,b:=Trial-a*(Length+1),by=list(TerminalNodes,Class,Times)]
		fulldata[,c:=Level-a*(Length+1)*(2*Length+1)/6-b*(Length+1)/2,by=list(TerminalNodes,Class,Times)]
		fulldata[,RealLevel:=a*(Times-Start+1)*(Times-Start+1)+b*(Times-Start+1)+c,by=list(TerminalNodes,Class,Times)]

		if(first){
		completefulldata=fulldata

		first=FALSE	
		} else {
		completefulldata=rbind(completefulldata,fulldata)

		}
	}
	tempfulldata=completefulldata
	tempfulldata=tempfulldata[,by=list(Class,Level,Start,End)]
	tempfulldata[,Len:=End-Start+1]
	tempfulldata=tempfulldata[Len > 5]
	tempfulldata=tempfulldata[Len < 0.75*max(inputtrain$times)]
	if(nrow(tempfulldata)<1){
	  next
	}
	shapelet_temp = tempfulldata[,list(Len=Len),by=list(Class,Start,End)]
	
	if(nrow(shapelet_temp)<1){
	  next
	}
	shapelet_features=shapelet_temp[,list(Len=.N),by=list(Start,End,Class)]
	maxLen=max(shapelet_features$Len)
	minLen=min(shapelet_features$Len)
	meanLen=mean(shapelet_features$Len)
	shapeletLength =shapelet_features$Len
	shapeletNum = nrow(shapelet_features)
	
	
	shapelet = matrix(-999,shapeletNum,maxLen)
	
	shapeletId=rep(c(1:shapeletNum),shapeletLength)
	
	timeId = unlist(lapply(c(1:shapeletNum),function(x){
	  c(1:(shapeletLength[x]))
	}))
	temp=NULL
	shapelet[cbind(shapeletId,timeId)]=tempfulldata$RealLevel
	return(list("shapelets"=shapelet,"shapelet_length"= shapeletLength))
}




