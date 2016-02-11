



	pparse <- function() {
		root<-"../R/hist"

		require(stringr)
		i<<-1

		load("players.dat")
		fideids<-as.character(names(players))
		pnames<-as.character(players)
		#cat("ids",fideids,"names",pnames)
		
		l<-length(players)
		cat("number of players",l,"\n")

		mat<-matrix(,nrow=200,ncol=(l+1))

		for(file in list.files(path=root,pattern="\\.txt$")) {
			m<-str_match(file,"^(....)")
			d<-m[1,2]

			mat[i,1]<-d
			
			path=paste(root,file,sep="/")
			cat(i,path,"\n")

			p<-read.table(path)

			rtgs<-p[,"rtg"]
			names(rtgs)<-as.character(p[,"fideid"])

			print(rtgs[1:5])

			for(j in 1:l) {
				fideid=fideids[j]
				#cat("looking up",name,"\n")
				if(fideid %in% names(rtgs)){
					#cat(players[j],"=",rtg,"\n")
					mat[i,j+1]=rtgs[[fideid]]
				}
			}

			i<<-i+1
			if(i>200){
				break
			}
		}

		dt<-as.data.frame(mat[1:i,])
		colnames(dt)<-c("date",fideids)
		dt<-dt[complete.cases(dt$date),]
		dt<-dt[order(dt$date),]

		dt[["date2"]]=gsub("^0","",dt$date)
		dt[["year"]]=round(strtoi(dt$date2)/100)*100+(strtoi(dt$date2)-round(strtoi(dt$date2)/100)*100)*(100/12)

		dt<-dt[order(dt$year),]

		write.table(dt,"ratinghist.txt")
		cat("done","\n")

	}


	pparse()