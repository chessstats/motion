



pparse <- function() {
	root="../R/hist"
	collisions<-0

	require(stringr)
	i<<-1

	players<-list()

	for(file in list.files(path=root,pattern="\\.txt$")) {
		m<-str_match(file,"^(....)")
		d<-m[1,2]
		
		path=paste(root,file,sep="/")
		cat(i,path,"\n")

		p<-read.table(path)

		p<-p[complete.cases(p$rtg),]
		p<-p[complete.cases(p$name),]
		p<-p[complete.cases(p$fideid),]
		
		p<-p[order(-p$rtg),]

		for(j in 1:100) {
			name=as.character(p[j,"name"])
			fideid=as.character(p[j,"fideid"])
			if(fideid %in% names(players)){
				oldname<-players[[fideid]]
				if(oldname!=name){
					cat("collision, old:",oldname,"new:",name,"\n");
					answer<-readline(prompt="Accept new name? ")
					collisions<-collisions+1
					if(answer=="y"){
						players[[fideid]]<-name
						cat("name changed to",name,"\n")
					}
				}
			}
			else{
				players[[fideid]]<-name
			}
		}

		cat("num players",length(players),"num collisions",collisions,"\n")

		i<<-i+1
		if(i>200){
			break
		}
	}

	players<-players[order(as.character(players))]
	print("saving players")
	save(players,file="players.dat")

}


pparse()