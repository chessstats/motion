



	pparse <- function() {
		root<-"../R/hist"

		dir.create("pngs")

		require(stringr)
		i<<-1

		mat<-matrix(,nrow=200,20)

		for(file in list.files(path=root,pattern="\\.txt$")) {
			m<-str_match(file,"^(....)")
			d<-m[1,2]

			mat[i,1]<-d
			
			path=paste(root,file,sep="/")
			cat(i,path,"\n")

			p<-read.table(path)

			p<-p[complete.cases(p$born),]
			pw<-p[grep("w",p$flag),]
			pm<-p[grep("w",p$flag,invert=TRUE),]

			born<-p[,"born"]
			bornw<-pw[,"born"]
			bornm<-pm[,"born"]

			numb<-length(born)
			numbw<-length(bornw)
			numbm<-length(bornm)

			if((numb>100)&(numbw>0)) {

				age<-2016-born
				age<-age[(age>10)&(age<100)]

				agew<-2016-bornw
				agew<-agew[(agew>10)&(agew<100)]

				agem<-2016-bornm
				agem<-agem[(agem>10)&(agem<100)]

				minb<-min(age)
				maxb<-max(age)
				avga<-mean(age)

				minbw<-min(agew)
				maxbw<-max(agew)
				avgaw<-mean(agew)

				minbm<-min(agem)
				maxbm<-max(agem)
				avgam<-mean(agem)

				parf<-numbw/numb

				cat("parf",parf,"numb",numb,"mina",minb,"maxa",maxb,"avga",avga,"numbw",numbw,"minaw",minbw,"maxaw",maxbw,"avgaw",avgaw,"numbm",numbm,"minam",minbm,"maxam",maxbm,"avgam",avgam,"\n")

				pngname=paste("pngs/a",d,".png",sep="")

				png(pngname)

				plot(density(age),main=paste("Date 20",substr(d,1,2),".",substr(d,3,4),sep=""),xlim=c(10,80),ylim=c(0,0.05),lty=2)
				lines(density(agew),col="red",lwd=3)
				lines(density(agem),col="blue",lwd=3)

				graphics.off()

			}

			i<<-i+1
			if(i>100){
				break
			}
		}

		dt<-as.data.frame(mat[1:i,])
		colnames(dt)<-c("date")
		dt<-dt[complete.cases(dt$date),]

		dt[["date2"]]=gsub("^0","",dt$date)
		dt[["year"]]=round(strtoi(dt$date2)/100)*100+(strtoi(dt$date2)-round(strtoi(dt$date2)/100)*100)*(100/12)

		dt<-dt[order(dt$year),]

		write.table(dt,"gdhist.txt")
		cat("done","\n")

	}


	pparse()