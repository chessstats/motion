
	pparse <- function() {
		require(stringr)
		i<<-1

		mat<-matrix(,nrow=200,ncol=5)

		for(file in list.files(path="../r/hist",pattern="\\.txt$")) {
			m<-str_match(file,"^(..)(..)")
			year<-m[1,2]
			month<-m[1,3]

			d<-paste(year,month,sep="_")

			if(TRUE){

				cat("processing",year,"\n")

				mat[i,1]<-d
				
				path=paste("../r/hist",file,sep="/")
				cat(i,path,"\n")

				p<-read.table(path)

				rtg<-p[complete.cases(p$rtg),"rtg"]

				to2600<-length(rtg[(rtg>=2000)&(rtg<2600)])
				from2600<-length(rtg[(rtg>=2600)])

				avgr<-mean(rtg)
				
				mat[i,2]=avgr
				mat[i,3]=to2600
				mat[i,4]=from2600

				rath<-from2600/(to2600+from2600)
				mat[i,5]=rath

				cat("avgr",avgr,"freq 2000-2600",to2600,"freq >2600",from2600,"rath",rath,"\n")

				i<<-i+1
				if(i>200){
					break
				}

			}
		}

		dt<-as.data.frame(mat[1:i,])
		colnames(dt)<-c("date","avgr","freq 2000-2600","freq >2600","ratio >2600")
		dt<-dt[complete.cases(dt$date),]
		dt<-dt[order(dt$date),]

		print(dt)

		write.table(dt,"histavgr.txt")
		
		cat("done","\n")

	}


	pparse()