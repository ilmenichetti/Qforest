
#load the command to create the palette
#library(viridis)
# not needed any longer since I embedded the resulting palette


#pause function for plots
pause = function(){
  if (interactive())
  {
    invisible(readline(prompt = "Press <Enter> to continue..."))
  }
  else
  {
    cat("Press <Enter> to continue...")
    invisible(readLines(file("stdin"), 1))
  }
}


###AG simulation plot function
###############################
plot.AG.simulation<-function(AG.simulation, management.plan, input.file){

  #palette<-rev(viridis(6)[1:5])
  palette<-c("#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", "#46337EFF", "#440154FF") #palette based on the package "viridis" but embedded
  
  if(input.file==F){
    
    
    plot(AG.simulation$BA, type="l", xlab="year", ylab="Basal area", ylim=c(0, max(AG.simulation$BA, na.rm=T)*1.05))
    year_seq<-seq(1:management.plan$gm)
    lines(year_seq[1:management.plan$g1], AG.simulation$BA[1:management.plan$g1], col=palette[1])
    abline(v=management.plan$g1, lty=2, col=palette[1])
    abline(v=management.plan$g2, lty=2, col=palette[2])
    lines(year_seq[management.plan$g1:management.plan$g2], AG.simulation$BA[management.plan$g1:management.plan$g2], col=palette[2])
    abline(v=management.plan$g3, lty=2, col=palette[3])
    lines(year_seq[management.plan$g2:management.plan$g3], AG.simulation$BA[management.plan$g2:management.plan$g3], col=palette[3])
    abline(v=management.plan$g4, lty=2, col=palette[4])
    lines(year_seq[management.plan$g3:management.plan$g4], AG.simulation$BA[management.plan$g3:management.plan$g4], col=palette[4])
    lines(year_seq[management.plan$g4:management.plan$gm], AG.simulation$BA[management.plan$g4:management.plan$gm], col=palette[5])
    
    pause()
    
    plot(AG.simulation$BAu, type="l", xlab="year", ylab="Basal area under bark", ylim=c(0, max(AG.simulation$BA, na.rm=T)*1.05))
    year_seq<-seq(1:management.plan$gm)
    lines(year_seq[1:management.plan$g1], AG.simulation$BAu[1:management.plan$g1], col=palette[1])
    abline(v=management.plan$g1, lty=2, col=palette[1])
    abline(v=management.plan$g2, lty=2, col=palette[2])
    lines(year_seq[management.plan$g1:management.plan$g2], AG.simulation$BAu[management.plan$g1:management.plan$g2], col=palette[2])
    abline(v=management.plan$g3, lty=2, col=palette[3])
    lines(year_seq[management.plan$g2:management.plan$g3], AG.simulation$BAu[management.plan$g2:management.plan$g3], col=palette[3])
    abline(v=management.plan$g4, lty=2, col=palette[4])
    lines(year_seq[management.plan$g3:management.plan$g4], AG.simulation$BAu[management.plan$g3:management.plan$g4], col=palette[4])
    lines(year_seq[management.plan$g4:management.plan$gm], AG.simulation$BAu[management.plan$g4:management.plan$gm], col=palette[5])
    
    pause()
    
    plot(AG.simulation$N, type="l", xlab="year", ylab="Number of trees", ylim=c(0, max(AG.simulation$N, na.rm=T)*1.05))
    year_seq<-seq(1:management.plan$gm)
    lines(year_seq[1:management.plan$g1], AG.simulation$N[1:management.plan$g1], col=palette[1])
    abline(v=management.plan$g1, lty=2, col=palette[1])
    abline(v=management.plan$g2, lty=2, col=palette[2])
    lines(year_seq[management.plan$g1:management.plan$g2], AG.simulation$N[management.plan$g1:management.plan$g2], col=palette[2])
    abline(v=management.plan$g3, lty=2, col=palette[3])
    lines(year_seq[management.plan$g2:management.plan$g3], AG.simulation$N[management.plan$g2:management.plan$g3], col=palette[3])
    abline(v=management.plan$g4, lty=2, col=palette[4])
    lines(year_seq[management.plan$g3:management.plan$g4], AG.simulation$N[management.plan$g3:management.plan$g4], col=palette[4])
    lines(year_seq[management.plan$g4:management.plan$gm], AG.simulation$N[management.plan$g4:management.plan$gm], col=palette[5])
    
    pause()
    
    plot(AG.simulation$d, type="l", xlab="year", ylab="Mean diameter (cm)", ylim=c(0, max(AG.simulation$d, na.rm=T)*1.05))
    year_seq<-seq(1:management.plan$gm)
    lines(year_seq[1:management.plan$g1], AG.simulation$d[1:management.plan$g1], col=palette[1])
    abline(v=management.plan$g1, lty=2, col=palette[1])
    abline(v=management.plan$g2, lty=2, col=palette[2])
    lines(year_seq[management.plan$g1:management.plan$g2], AG.simulation$d[management.plan$g1:management.plan$g2], col=palette[2])
    abline(v=management.plan$g3, lty=2, col=palette[3])
    lines(year_seq[management.plan$g2:management.plan$g3], AG.simulation$d[management.plan$g2:management.plan$g3], col=palette[3])
    abline(v=management.plan$g4, lty=2, col=palette[4])
    lines(year_seq[management.plan$g3:management.plan$g4], AG.simulation$d[management.plan$g3:management.plan$g4], col=palette[4])
    lines(year_seq[management.plan$g4:management.plan$gm], AG.simulation$d[management.plan$g4:management.plan$gm], col=palette[5])
    
  }else{
  
      png(file="AG.simulation.png", width=2500, height=2500, res=300)
      par(mfrow=c(2,2))
      
      plot(AG.simulation$BA, type="l", xlab="year", ylab="Basal area", ylim=c(0, max(AG.simulation$BA, na.rm=T)*1.05))
      year_seq<-seq(1:management.plan$gm)
      lines(year_seq[1:management.plan$g1], AG.simulation$BA[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], AG.simulation$BA[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], AG.simulation$BA[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], AG.simulation$BA[management.plan$g3:management.plan$g4], col=palette[4])
      lines(year_seq[management.plan$g4:management.plan$gm], AG.simulation$BA[management.plan$g4:management.plan$gm], col=palette[5])
      
      plot(AG.simulation$BAu, type="l", xlab="year", ylab="Basal area under bark", ylim=c(0, max(AG.simulation$BA, na.rm=T)*1.05))
      year_seq<-seq(1:management.plan$gm)
      lines(year_seq[1:management.plan$g1], AG.simulation$BAu[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], AG.simulation$BAu[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], AG.simulation$BAu[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], AG.simulation$BAu[management.plan$g3:management.plan$g4], col=palette[4])
      lines(year_seq[management.plan$g4:management.plan$gm], AG.simulation$BAu[management.plan$g4:management.plan$gm], col=palette[5])
      
      plot(AG.simulation$N, type="l", xlab="year", ylab="Number of trees", ylim=c(0, max(AG.simulation$N, na.rm=T)*1.05))
      year_seq<-seq(1:management.plan$gm)
      lines(year_seq[1:management.plan$g1], AG.simulation$N[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], AG.simulation$N[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], AG.simulation$N[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], AG.simulation$N[management.plan$g3:management.plan$g4], col=palette[4])
      lines(year_seq[management.plan$g4:management.plan$gm], AG.simulation$N[management.plan$g4:management.plan$gm], col=palette[5])
      
      plot(AG.simulation$d, type="l", xlab="year", ylab="Mean diameter (cm)", ylim=c(0, max(AG.simulation$d, na.rm=T)*1.05))
      year_seq<-seq(1:management.plan$gm)
      lines(year_seq[1:management.plan$g1], AG.simulation$d[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], AG.simulation$d[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], AG.simulation$d[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], AG.simulation$d[management.plan$g3:management.plan$g4], col=palette[4])
      lines(year_seq[management.plan$g4:management.plan$gm], AG.simulation$d[management.plan$g4:management.plan$gm], col=palette[5])
      
      dev.off()
  
      }
    }

###C partitions plot function
###############################
plot.C.partitions<-function(C.partitions, management.plan, input.file){
  
  names<-colnames(C.partitions)
  names_titles<-c( "Needles","Branches","Stem V","Stem B","Fine root","Root >5cm", "Root <5cm", "Stump")
  max_plot<-max(C.partitions, na.rm=T)
  #palette<-rev(viridis(6)[1:5])
  palette<-c("#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", "#46337EFF", "#440154FF") #palette based on the package "viridis" but embedded
  
  
  if(input.file==F){

    for(i in 1:length(names)){
      
      object<-paste("C.partitions$",names[i], sep="")
      
      plot(eval(parse(text=object)), type="l", xlab="year", ylab=expression(paste("(Kg ha"^-1,")")), ylim=c(0, max_plot*1.05), main=names_titles[i])
      year_seq<-seq(1:management.plan$gm)
      lines(year_seq[1:management.plan$g1], eval(parse(text=object))[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], eval(parse(text=object))[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], eval(parse(text=object))[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], eval(parse(text=object))[management.plan$g3:management.plan$g4], col=palette[4])
      lines(year_seq[management.plan$g4:management.plan$gm], eval(parse(text=object))[management.plan$g4:management.plan$gm], col=palette[5])
      
      pause()
      
    }
    
  }else{
    
    png(file="C.partitions.simulations.png", width=3000, height=3000, res=300)

    #find the amount of panels for the plot
    panel_size<-ceiling(sqrt(length(names)))
    
    par(mfrow=c(panel_size,panel_size))
    
    for(i in 1:length(names)){
      
      object<-paste("C.partitions$",names[i], sep="")
      
      plot(eval(parse(text=object)), type="l", xlab="year", ylab=expression(paste("(Kg ha"^-1,")")), ylim=c(0, max_plot*1.05), main=names_titles[i])
      year_seq<-seq(1:management.plan$gm)
      lines(year_seq[1:management.plan$g1], eval(parse(text=object))[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], eval(parse(text=object))[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], eval(parse(text=object))[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], eval(parse(text=object))[management.plan$g3:management.plan$g4], col=palette[4])
      lines(year_seq[management.plan$g4:management.plan$gm], eval(parse(text=object))[management.plan$g4:management.plan$gm], col=palette[5])
      
    }
    
    dev.off()
    
  }
}

###Litter plot function
###############################
plot.Litter<-function(Litter, management.plan, input.file){
  
  names<-colnames(Litter)
  names_titles<-c( "Litter from fine roots", "Litter from branches")
  max_plot<-max(Litter, na.rm=T)
  #palette<-rev(viridis(6)[1:5])
  palette<-c("#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", "#46337EFF", "#440154FF") #palette based on the package "viridis" but embedded
  
  
  if(input.file==F){
    
    for(i in 1:length(names)){
      
      object<-paste("Litter$",names[i], sep="")
      
      plot(eval(parse(text=object)), type="l", xlab="year", ylab=expression(paste("(Kg ha"^-1,")")), ylim=c(0, max_plot*1.05), main=names_titles[i])
      year_seq<-seq(1:management.plan$gm)
      lines(year_seq[1:management.plan$g1], eval(parse(text=object))[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], eval(parse(text=object))[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], eval(parse(text=object))[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], eval(parse(text=object))[management.plan$g3:management.plan$g4], col=palette[4])
      lines(year_seq[management.plan$g4:management.plan$gm], eval(parse(text=object))[management.plan$g4:management.plan$gm], col=palette[5])
      
      pause()
      
    }
    
  }else{
    
    png(file="Litter.simulations.png", width=3000, height=1500, res=300)

    par(mfrow=c(1,2))
    
    for(i in 1:length(names)){
      
      object<-paste("Litter$",names[i], sep="")
      
      plot(eval(parse(text=object)), type="l", xlab="year", ylab=expression(paste("(Kg ha"^-1,")")), ylim=c(0, max_plot*1.05), main=names_titles[i])
      year_seq<-seq(1:management.plan$gm)
      lines(year_seq[1:management.plan$g1], eval(parse(text=object))[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], eval(parse(text=object))[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], eval(parse(text=object))[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], eval(parse(text=object))[management.plan$g3:management.plan$g4], col=palette[4])
      lines(year_seq[management.plan$g4:management.plan$gm], eval(parse(text=object))[management.plan$g4:management.plan$gm], col=palette[5])
      
    }
    
    dev.off()
    
  }
}

###Harvest residuals plot function
###############################
plot.Harvest.residuals<-function(Harvest.residuals, input.file){
  
  names<-colnames(Harvest.residuals)
  names_titles<-c("G needles", "G branches","G stemV","G stemB","G root >5cm","G root <5cm", "G stump")
  max_plot<-max(Harvest.residuals, na.rm=T)
  
  
  if(input.file==F){
    
    for(i in 1:length(names)){
      
      object<-paste("Harvest.residuals$",names[i], sep="")
      
      barplot(eval(parse(text=object)), xlab="year", ylab=expression(paste("(Kg ha"^-1,")")), ylim=c(0, max_plot*1.05), main=names_titles[i])
       
      pause()
      
    }
    
  }else{
    
    png(file="Harvest.residuals.simulations.png", width=3000, height=3000, res=300)

    #find the amount of panels for the plot
    panel_size<-ceiling(sqrt(length(names)))
    
    par(mfrow=c(panel_size,panel_size))
    
    for(i in 1:length(names)){
      
      object<-paste("Harvest.residuals$",names[i], sep="")
      
      barplot(eval(parse(text=object)), xlab="year", ylab=expression(paste("(Kg ha"^-1,")")), ylim=c(0, max_plot*1.05), main=names_titles[i])
      
    }
    
    dev.off()
    
  }
}

###SOC remaining plot function
###############################
plot.remaining.mass<-function(Remaining.mass.OM, management.plan, input.file, timestep){
  
  names<-colnames(Remaining.mass.OM)
  names_titles<-c("FC needles", "FC branches", "GC needles", "GC branches", "GC stemV",   
                  "GC stemB",  "GC stump" ,  "GC root1", "GC root2")
  max_plot<-max(Remaining.mass.OM, na.rm=T)
  #palette<-rev(viridis(6)[1:5])
  palette<-c("#4AC16DFF", "#1FA187FF", "#277F8EFF", "#365C8DFF", "#46337EFF", "#440154FF") #palette based on the package "viridis" but embedded
  
  
  if(input.file==F){
    
    for(i in 1:length(names)){
      
      object<-paste("Remaining.mass.OM$",names[i], sep="")
      
      year_seq<-seq(1:(tail(timestep,1)))
      plot(year_seq, eval(parse(text=object)), type="l", xlab="year", ylab=expression(paste("(Kg ha"^-1,")")), ylim=c(0, max_plot*1.05), main=names_titles[i], col="darkorange")
      lines(year_seq[1:management.plan$g1], eval(parse(text=object))[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], eval(parse(text=object))[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], eval(parse(text=object))[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], eval(parse(text=object))[management.plan$g3:management.plan$g4], col=palette[4])
      abline(v=management.plan$gm, lty=2, col=palette[5])
      lines(year_seq[management.plan$g4:management.plan$gm], eval(parse(text=object))[management.plan$g4:management.plan$gm], col=palette[5])
      lines(year_seq[management.plan$gm:tail(year_seq,1)], eval(parse(text=object))[management.plan$gm:tail(year_seq,1)], col=palette[6])
      
      pause()
      
    }
    
  }else{
    
    png(file="SOC.simulations.png", width=3500, height=3500, res=300)
    
    #find the amount of panels for the plot
    panel_size<-ceiling(sqrt(length(names)))
    
    par(mfrow=c(panel_size,panel_size))
    
    for(i in 1:length(names)){
      
      object<-paste("Remaining.mass.OM$",names[i], sep="")
      
      year_seq<-seq(1:(tail(timestep,1)))
      plot(year_seq, eval(parse(text=object)), type="l", xlab="year", ylab=expression(paste("(Kg ha"^-1,")")), ylim=c(0, max_plot*1.05), main=names_titles[i], col="darkorange")
      lines(year_seq[1:management.plan$g1], eval(parse(text=object))[1:management.plan$g1], col=palette[1])
      abline(v=management.plan$g1, lty=2, col=palette[1])
      abline(v=management.plan$g2, lty=2, col=palette[2])
      lines(year_seq[management.plan$g1:management.plan$g2], eval(parse(text=object))[management.plan$g1:management.plan$g2], col=palette[2])
      abline(v=management.plan$g3, lty=2, col=palette[3])
      lines(year_seq[management.plan$g2:management.plan$g3], eval(parse(text=object))[management.plan$g2:management.plan$g3], col=palette[3])
      abline(v=management.plan$g4, lty=2, col=palette[4])
      lines(year_seq[management.plan$g3:management.plan$g4], eval(parse(text=object))[management.plan$g3:management.plan$g4], col=palette[4])
      abline(v=management.plan$gm, lty=2, col=palette[5])
      lines(year_seq[management.plan$g4:management.plan$gm], eval(parse(text=object))[management.plan$g4:management.plan$gm], col=palette[5])
      lines(year_seq[management.plan$gm:tail(year_seq,1)], eval(parse(text=object))[management.plan$gm:tail(year_seq,1)], col=palette[6])
      
    }
    
    dev.off()
    
  }
}
