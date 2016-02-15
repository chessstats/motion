



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

			p<-p[complete.cases(p$rtg),]

			rtg<-p[,"rtg"]
			flag<-p[,"flag"]

			numr<-length(rtg)
			minr<-min(rtg)
			maxr<-max(rtg)
			avgr<-mean(rtg)

			nrf<-length(grep("w",flag))
			parf<-nrf/numr

			cat("numr",numr,"minr",minr,"maxr",maxr,"avgr",avgr,"nf",nrf,"parf",parf,"\n")

			rtg<-rtg[rtg>2000]

			brks=c(2000,2100,2200,2300,2400,2500,2600,2700,2800,2900)

			pngname=paste("pngs/h",d,".png",sep="")

			png(pngname)

			h<-hist(rtg,breaks=brks)

			plot(density(rtg))

			graphics.off()

			print(h$density)

			i<<-i+1
			if(i>200){
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