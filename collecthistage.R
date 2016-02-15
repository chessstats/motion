



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

			born<-p[,"born"]

			numb<-length(born)

			if(numb>100) {

				age<-2016-born
				age<-age[(age>10)&(age<100)]

				minb<-min(age)
				maxb<-max(age)
				avga<-mean(age)

				cat("numb",numb,"mina",minb,"maxa",maxb,"avga",avga,"\n")

				pngname=paste("pngs/a",d,".png",sep="")

				png(pngname)

				plot(density(age),main=paste("Date 20",substr(d,1,2),".",substr(d,3,4),sep=""),xlim=c(10,80))

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