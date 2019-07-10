


# declaring the parameters needed for the simulations
# all the more interesting parameters are kept in a separate data frame
# to offer flexibility in the calibration
parameters<-data.frame(  
  
  #fine root:needle litter
  FRNL=1,
  
  #fine root:needle biomass
  FRNB=0.33,
  
  #C content average
  fc=0.5,
  
  #parameters for the Q decomposition model
  beta=7,
  eta_11=0.36,
  q0=1,
  e0=0.25,
  
  #time to colonize different litter types
  tmax_b=1, #for needles and roots
  tmax_g=13, #for branches
  tmax_st=34, #for stems
  tmax_stmp=34, #for stumps
  tmax_r1=20, # for roots >5 cm
  tmax_r2=13 #for roots <5 cm
  
  
)




#main function for management plan selection
management.plan<-function(SI, Lat){
  
### Self-contained declaration of matrices for the management plans
# this is in order to skip the reading of the specific csv and make the function more general

gm<-c(141, 141, 141, 141, 126, 121, 111, 106, 101, 95, 85, 80, 75, 70)

s1_vec<-c(0.75, 0.75, 0.75, 0.75, 0.72, 0.69, 0.71, 0.73, 0.76, 0.75, 0.72, 0.73, 0.76, 0.76)
s2_vec<-c(0.71, 0.71, 0.71, 0.71, 0.74, 0.72, 0.73, 0.73, 0.73, 0.74, 0.73, 0.67, 0.71, 0.69)
s3_vec<-c(1, 1, 1, 1, 1, 0.86, 0.84, 0.71, 0.74, 0.74, 0.76, 0.72, 0.74, 0.74)
s4_vec<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0.75, 0.72, 0.76, 0.77, 0.74)
s<-as.data.frame(cbind(s1_vec,s2_vec,s3_vec,s4_vec))

g1_vec<-c(75, 75, 75, 75, 62, 55, 47, 37, 33, 30, 27, 25, 24, 21)
g2_vec<-c(100, 100, 100, 100, 82, 76, 65, 53, 48, 43, 36, 35, 35, 30)
g3_vec<-c(140, 140, 140, 140, 125, 91, 85, 71, 65, 55, 45, 46, 45, 39)
g4_vec<-c(140, 140, 140, 140, 125, 120, 110, 105, 100, 70, 57, 59, 57, 47)
g<-as.data.frame(cbind(g1_vec,g2_vec,g3_vec,g4_vec))

H0<-c(12, 12, 12, 12, 12, 12, 12, 11, 11, 11, 11, 11, 11, 11)
gm<-c(141, 141, 141, 141, 126, 121, 111, 106, 101, 95, 85, 80, 75, 70)


#selection id
ID<-((SI-10)/2)+1

### Select the values from the specific site

#Year when start diameter at breast hight is measured  
T.=H0[ID]#was T


#Year of final felling
gm=gm[ID]
#Thinnings
g1=as.numeric(g[ID,1])
g2=as.numeric(g[ID,2])
g3=as.numeric(g[ID,3])
g4=as.numeric(g[ID,4])
s1=as.numeric(s[ID,1])
s2=as.numeric(s[ID,2])
s3=as.numeric(s[ID,3])
s4=as.numeric(s[ID,4])


thinnings<-data.frame(g1, g2, g3, g4, gm)
 results<-data.frame(ID, T., thinnings)

 return(results)
}




#main function for basal area simulation
Sim.aboveground<-function(SI, Lat, management.plan){
  
### Self-contained declaration of matrices for the management plans
# this is in order to skip the reading of the specific csv and make the function more general

An<-c(19.2, 21.2, 23.2, 25, 26.9, 28.5, 30.1, 31.8, 33.3, 35, 36.5, 38.1, 39.6, 41.3)
b<-c(0.219, 0.221, 0.226, 0.224, 0.235, 0.247, 0.244, 0.249, 0.254, 0.266, 0.263, 0.255, 0.272, 0.28)
N0<-c(1250, 1250, 1250, 1250, 1400, 1600, 1800, 2000, 2200, 2300, 2500, 2700, 2900, 3000)
#H0<-c(12, 12, 12, 12, 12, 12, 12, 11, 11, 11, 11, 11, 11, 11)
BA0<-c(21.4, 21.4, 21.4, 21.4, 22.1, 23.1, 23.9, 22, 22.5, 22.6, 23, 23.4, 23.7, 23.7)
#gm<-c(141, 141, 141, 141, 126, 121, 111, 106, 101, 95, 85, 80, 75, 70)
#management_plan<-as.data.frame(cbind(An, b, N0, H0, BA0, gm))

s1_vec<-c(0.75, 0.75, 0.75, 0.75, 0.72, 0.69, 0.71, 0.73, 0.76, 0.75, 0.72, 0.73, 0.76, 0.76)
s2_vec<-c(0.71, 0.71, 0.71, 0.71, 0.74, 0.72, 0.73, 0.73, 0.73, 0.74, 0.73, 0.67, 0.71, 0.69)
s3_vec<-c(1, 1, 1, 1, 1, 0.86, 0.84, 0.71, 0.74, 0.74, 0.76, 0.72, 0.74, 0.74)
s4_vec<-c(1, 1, 1, 1, 1, 1, 1, 1, 1, 0.75, 0.72, 0.76, 0.77, 0.74)
s<-as.data.frame(cbind(s1_vec,s2_vec,s3_vec,s4_vec))


g1_vec<-c(75, 75, 75, 75, 62, 55, 47, 37, 33, 30, 27, 25, 24, 21)
g2_vec<-c(100, 100, 100, 100, 82, 76, 65, 53, 48, 43, 36, 35, 35, 30)
g3_vec<-c(140, 140, 140, 140, 125, 91, 85, 71, 65, 55, 45, 46, 45, 39)
g4_vec<-c(140, 140, 140, 140, 125, 120, 110, 105, 100, 70, 57, 59, 57, 47)
g<-as.data.frame(cbind(g1_vec,g2_vec,g3_vec,g4_vec))


#selection id
ID<-management.plan$ID

###select the initial parameters for the site from the management plant data
#declare a few vectors where to store the simulated values
N<-c()
BA<-c()
BAu<-c()
d<-c()

### Select the values from the specific site

#Year when start diameter at breast hight is measured  
T.=management.plan$T.#was T

#Number of trees at start year 
N[1]=N0[ID] 

#Year of final felling
gm=management.plan$gm
#Thinnings
g1=management.plan$g1
g2=management.plan$g2
g3=management.plan$g3
g4=management.plan$g4
s1=as.numeric(s[ID,1])
s2=as.numeric(s[ID,2])
s3=as.numeric(s[ID,3])
s4=as.numeric(s[ID,4])

An=An[ID]

### Growth function for spruce

#defining the function for aboveground development to be used in the following formula
HN<-function(time, An, T.){
if(Lat>60){
  HN<-1.3+An*(1-exp(-(0.0019358+4.1*(10^-5)*(10*An)^1.0105)*(time-T.)))^(1/(1-0.43279+8*10^-6*(10*An)^1.8008))
  }else{
  HN<-1.3+An*(1-exp(-(0.042624-7.1145*(10^-5)*(10*An)^1.0068)*(time-T.)))^(1/(1-0.15933-3.7*10^-6*(10*An)^-3.156))
  }
 return(HN) 
}
  

######Basal area development

########################
##Initial basal area
########################
BA[T.-1]<-N[1]*10*10^-4
#initial basal area at first thinning
BA[g1]<-BA0[ID] 
#Relative growth rate
k_BA<-log(BA[g1]/BA[T.-1])*(1/(g1-T.+1)) #considers the first thinning

################################################
## Basal area increment from start to first thinning
timestep_1<-seq(from=T., to=g1)
BA[timestep_1]<-BA[T.-1]*exp(k_BA*(timestep_1-T.+1))
#Number of trees
N[timestep_1]<-N[1] 
#Recalculation of basal area over bark to basal area under bark
BAu[g1]<-s1*BA[g1]*(1+0.4508*((sqrt((100*BA[g1])/(pi*N[g1])))^(-b[ID]))*BA[g1]^-0.281*(g1-1-T.)^0.125)^-1
#Basal area at first thinning
BA[g1]<-s1*BA[g1]
#Update number of trees at first thinning
N[g1]<-N[g1]*s1

## Basal area increment from first to second thinning
timestep_1<-seq(from=(g1+1), to=g2)
#Number of trees
N[timestep_1]<-N[g1]

#loop for area under the bark, between thinning 1 and thinning 2
for(i in 1:(length(timestep_1))){
BAu[timestep_1[i]]<-BAu[timestep_1[i]-1]+
  0.817*(BAu[timestep_1[i]-1])^0.25*(10*HN(time=timestep_1[i]-1, An=An, T.= T.))^0.334*(timestep_1[i]-1-T.)^-0.748
BA[timestep_1[i]]<-BAu[timestep_1[i]]*(1+0.4508*(sqrt((100*BAu[timestep_1[i]])/(pi*N[g1])))^(-b[ID])*BA[g1]^-0.281*(timestep_1[i]-1-T.)^0.125)
}

##Basal area at second thinning
BA[g2]<-s1*BA[g2]
#Basal area under bark at second thinning
BAu[g2]<-s1*BAu[g2]
#Number of trees at second thinning
N[g2]<-N[g2]*s1

################################################
## Basal area increment from second to third thinning
timestep_2<-seq(from=g2+1, to=g3)
N[timestep_2]<-N[g2] 

#loop for area under the bark, between thinning 1 and thinning 2
for(i in 1:(length(timestep_2))){
  BAu[timestep_2[i]]<-BAu[timestep_2[i]-1]+0.817*(BAu[timestep_2[i]-1])^0.25*(10*HN(time=timestep_2[i]-1, An=An, T.= T.))^0.334*(timestep_2[i]-1-T.)^-0.748
  BA[timestep_2[i]]<-BAu[timestep_2[i]]*(1+0.4508*(sqrt((100*BAu[timestep_2[i]])/(pi*N[g1])))^(-b[ID])*BA[g1]^-0.281*(timestep_2[i]-T.)^0.125)
}

#Basal area at second thinning
BA[g3]<-s1*BA[g3]
#Basal area under bark at second thinning
BAu[g3]<-s1*BAu[g3]
#Number of trees at second thinning
N[g3]<-N[g3]*s1


################################################
## Basal area increment from third to fourth thinning
timestep_3<-seq(from=g3+1, to=g4)
N[timestep_3]<-N[g3] 

#loop for area under the bark, between thinning 1 and thinning 2
for(i in 1:(length(timestep_3))){
  BAu[timestep_3[i]]<-BAu[timestep_3[i]-1]+0.817*(BAu[timestep_3[i]-1])^0.25*(10*HN(time=timestep_3[i]-1, An=An, T.= T.))^0.334*(timestep_3[i]-1-T.)^-0.748
  BA[timestep_3[i]]<-BAu[timestep_3[i]]*(1+0.4508*(sqrt((100*BAu[timestep_3[i]])/(pi*N[g1])))^(-b[ID])*BA[g1]^-0.281*(timestep_3[i]-T.)^0.125)
}

#Basal area at second thinning
BA[g4]<-s1*BA[g4]

#Basal area under bark at second thinning
BAu[g4]<-s1*BAu[g4]

#Number of trees at second thinning
N[g4]<-N[g4]*s1


################################################
## Basal area increment from fourth thinning to final felling
timestep_4<-seq(from=g4+1, to=gm)
N[timestep_4]<-N[g4] 

#loop for area under the bark, between thinning 1 and thinning 2
for(i in 1:(length(timestep_4))){
  BAu[timestep_4[i]]<-BAu[timestep_4[i]-1]+0.817*(BAu[timestep_4[i]-1])^0.25*(10*HN(time=timestep_4[i]-1, An=An, T.= T.))^0.334*(timestep_4[i]-1-T.)^-0.748
  BA[timestep_4[i]]<-BAu[timestep_4[i]]*(1+0.4508*(sqrt((100*BAu[timestep_4[i]])/(pi*N[g1])))^(-b[ID])*BA[g1]^-0.281*(timestep_4[i]-T.)^0.125)
}

d=2*100*sqrt((1/pi)*BA/N)

sim<-data.frame(BA, N, BAu, d)
return(sim)

}





### main function for carbon partitioning
#Note: contains some parameters that should be included in the uncertainty estimation
C.partitioning<-function(AG.simulation, parameters){

  #fine root:needle litter
  FRNL=parameters$FRNL

  #fine root:needle biomass
  FRNB=parameters$FRNB
  
  #C content average
  fc=parameters$fc
  
  Needles<-fc*AG.simulation$N*exp(-1.9602+7.8171*(AG.simulation$d/(AG.simulation$d+12)))
  
  Branches<-fc*AG.simulation$N*exp(-1.2804+8.5242*(AG.simulation$d/(AG.simulation$d+13)))-Needles
  
  StemV<-fc*AG.simulation$N*exp(-2.2471+11.4873*(AG.simulation$d/(AG.simulation$d+14)))
  StemB<-fc*AG.simulation$N*exp(-3.3912+9.8364*(AG.simulation$d/(AG.simulation$d+15)))
  
  Stump<-fc*AG.simulation$N*exp(-3.3645+10.6685*(AG.simulation$d/(AG.simulation$d+17)))
  
  Fine.root<-FRNB*Needles
  
  #Root >5cm
  Root.above.5cm<-fc*AG.simulation$N*exp(-6.3851+13.3703*(AG.simulation$d/(AG.simulation$d+8)))
  #Root <5cm
  Root.below.5cm<-1.11*fc*AG.simulation$N*exp(-2.5706+7.6283*(AG.simulation$d/(AG.simulation$d+12)))
  
  results<-data.frame(Needles, Branches, StemV, StemB, Fine.root, Root.above.5cm, Root.below.5cm,Stump)
  return(results)
  
  }




## main function for litter production
#Note: contains some parameters that should be included in the uncertainty estimation
Litter.production<-function(C.partitions, Lat, parameters){
  
  FRNL=parameters$FRNL
  
  f_needles=0.489-0.0063*Lat  
  fb_needles=0.514-0.0067*Lat
  f_branches=0.2*f_needles
  
  #needles and fine root litter
  # was "Fneedlfr" in the original Mathcad file
  Litter_fine.roots.needles<-f_needles*C.partitions$Needles*(1+FRNL)
  
  
  #branch litter
  # was "Fbranches" in the original Mathcad file
  Litter_branches<-f_branches*C.partitions$Branches
  
  results<-data.frame(Litter_fine.roots.needles, Litter_branches)
  return(results)
  
  
  }




### main function for carbon partitioning
#Note: contains some parameters that should be included in the uncertainty estimation
#in particular the harvest management plan
Harvest.residuals<-function(C.partitions, management.plan, parameters){
  
  #residuals left at the site after harvesting
  # this needs to be modified depending on the residual management plan
  u1_needles=1
  u1_branches=1
  u1_root1=1
  u1_root2=1
  u1_stump=1
  u1_stem=1
  
  u2_needles=1
  u2_branches=1
  u2_root1=1
  u2_root2=1
  u2_stump=1
  u2_stem=1
  
  u3_needles=1
  u3_branches=1
  u3_root1=1
  u3_root2=1
  u3_stump=1
  u3_stem=1
  
  u4_needles=1
  u4_branches=1
  u4_root1=1
  u4_root2=1
  u4_stump=1
  u4_stem=1

  ugm_needles=1
  ugm_branches=1
  ugm_root1=1
  ugm_root2=1
  ugm_stump=1
  ugm_stem=1
  
  #thinnings
  g1<-management.plan$g1
  g2<-management.plan$g2
  g3<-management.plan$g3
  g4<-management.plan$g4
  gm<-management.plan$gm
  
  #declaring open vectors
  Gneedl<-c()
  Gbranches<-c()
  GstemV<-c()
  Groot1<-c()
  Groot2<-c()
  GstemB<-c()
  Gstump<-c()
  
  #first thinning
  timestep1<-seq(from=management.plan$T., to=g1-1)
  Gneedl[g1]=u1_needles*(1+parameters$FRNB)*(C.partitions$Needles[g1-1]-C.partitions$Needles[g1])
  Gbranches[g1]=u1_branches*(C.partitions$Branches[g1-1]-C.partitions$Branches[g1])
  GstemV[g1]=u1_stem*(C.partitions$StemV[g1-1]-C.partitions$StemV[g1])
  Groot1[g1]=u1_root1*(C.partitions$Root.above.5cm[g1-1]-C.partitions$Root.above.5cm[g1])
  Groot2[g1]=u1_root2*(C.partitions$Root.below.5cm[g1-1]-C.partitions$Root.below.5cm[g1])
  GstemB[g1]=u1_stem*(C.partitions$StemB[g1-1]-C.partitions$StemB[g1])
  Gstump[g1]=u1_stump*(C.partitions$Stump[g1-1]-C.partitions$Stump[g1])
  
  #second thinning
  timestep2<-seq(from=g1+1, to=g2-1)
  Gneedl[g2]=u2_needles*(1+parameters$FRNB)*(C.partitions$Needles[g2-1]-C.partitions$Needles[g2])
  Gbranches[g2]=u2_branches*(C.partitions$Branches[g2-1]-C.partitions$Branches[g2])
  GstemV[g2]=u2_stem*(C.partitions$StemV[g2-1]-C.partitions$StemV[g2])
  Groot1[g2]=u2_root1*(C.partitions$Root.above.5cm[g2-1]-C.partitions$Root.above.5cm[g2])
  Groot2[g2]=u2_root2*(C.partitions$Root.below.5cm[g2-1]-C.partitions$Root.below.5cm[g2])
  GstemB[g2]=u2_stem*(C.partitions$StemB[g2-1]-C.partitions$StemB[g2])
  Gstump[g2]=u2_stump*(C.partitions$Stump[g2-1]-C.partitions$Stump[g2])
  
  #third thinning
  timestep3<-seq(from=g2+1, to=g3-1)
  Gneedl[g3]=u3_needles*(1+parameters$FRNB)*(C.partitions$Needles[g3-1]-C.partitions$Needles[g3])
  Gbranches[g3]=u3_branches*(C.partitions$Branches[g3-1]-C.partitions$Branches[g3])
  GstemV[g3]=u3_stem*(C.partitions$StemV[g3-1]-C.partitions$StemV[g3])
  Groot1[g3]=u3_root1*(C.partitions$Root.above.5cm[g3-1]-C.partitions$Root.above.5cm[g3])
  Groot2[g3]=u3_root2*(C.partitions$Root.below.5cm[g3-1]-C.partitions$Root.below.5cm[g3])
  GstemB[g3]=u3_stem*(C.partitions$StemB[g3-1]-C.partitions$StemB[g3])
  Gstump[g3]=u3_stump*(C.partitions$Stump[g3-1]-C.partitions$Stump[g3])
  
  #fourth thinning
  timestep4<-seq(from=g3+1, to=g4-1)
  Gneedl[g4]=u4_needles*(1+parameters$FRNB)*(C.partitions$Needles[g4-1]-C.partitions$Needles[g4])
  Gbranches[g4]=u4_branches*(C.partitions$Branches[g4-1]-C.partitions$Branches[g4])
  GstemV[g4]=u4_stem*(C.partitions$StemV[g4-1]-C.partitions$StemV[g4])
  Groot1[g4]=u4_root1*(C.partitions$Root.above.5cm[g4-1]-C.partitions$Root.above.5cm[g4])
  Groot2[g4]=u4_root2*(C.partitions$Root.below.5cm[g4-1]-C.partitions$Root.below.5cm[g4])
  GstemB[g4]=u4_stem*(C.partitions$StemB[g4-1]-C.partitions$StemB[g4])
  Gstump[g4]=u4_stump*(C.partitions$Stump[g4-1]-C.partitions$Stump[g4])
  
  #final felling 
  timestep5<-seq(from=g4+1, to=gm-1)
  Gneedl[gm]=ugm_needles*(1+parameters$FRNB)*(C.partitions$Needles[gm])
  Gbranches[gm]=ugm_branches*(C.partitions$Branches[gm])
  GstemV[gm]=ugm_stem*(C.partitions$StemV[gm])
  Groot1[gm]=ugm_root1*(C.partitions$Root.above.5cm[gm])
  Groot2[gm]=ugm_root2*(C.partitions$Root.below.5cm[gm])
  GstemB[gm]=ugm_stem*(C.partitions$StemB[gm])
  Gstump[gm]=ugm_stump*(C.partitions$Stump[gm])

  #Left at site at harvesting at the end of the rotation period 
  Gneedl[gm-1]=0
  Gbranches[gm-1]=0
  GstemV[gm-1]=0
  Groot1[gm-1]=0
  Groot2[gm-1]=0
  GstemB[gm-1]=0
  Gstump[gm-1]=0
  
  #Left at site at harvesting at the start of the rotation period 
  Gneedl[1]=Gneedl[gm]
  Gbranches[1]=Gbranches[gm]
  GstemV[1]=GstemV[gm]
  Groot1[1]=Groot1[gm]
  Groot2[1]=Groot2[gm]
  GstemB[1]=GstemB[gm]
  Gstump[1]=Gstump[gm]
  
  Groot.above.5cm<-Groot1
  Groot.below.5cm<-Groot2
  Gneedles<-Gneedl
  
  results<-data.frame(Gneedles, Gbranches, GstemV, GstemB, Groot.above.5cm, Groot.below.5cm, Gstump)
  return(results)
  
  }





# defining the decomposition function to be used in all the following calculations
# the function is based on the Q system of assumptions, and it is used to calculate G (remaining mass)
Remaining.OM<-function(Lat, parameters, tmax, timestep){
  
  # calculating microbial efficiency based on latitude
  u0=(0.0855+0.0157*(50.6-0.768*Lat))
  
  beta=parameters$beta
  eta_11=parameters$eta_11
  q0=parameters$q0
  e0=parameters$e0

  fc=parameters$fc
  
  # #Fraction of litter colonised, function internal to the decomposition module
  # A<-function(timesetp, tmax){
  #   for(i in 1:length(timestep)){
  #     if(timestep[i]<tmax){
  #       A[i]=1-(1-(timestep/tmax)^2)} else 
  #       {A[i]=1}
  #     }
  #   return(A)
  #   }
  # 
  # #rate of fraction colonised, function internal to the decomposition module
  # dA<-function(timestep, tmax){
  #   for(i in 1:length(timestep)){
  #     if(timestep[i]<tmax){
  #       dA[i]=((2/tmax)*(1-(timestep/tmax)^2))/(1+(1/tmax))} else 
  #       {dA[i]=0}
  #     }
  #   return(dA)
  #   }
  # 
  
  #to simplify the following equations
  alpha=fc*beta*eta_11*u0*q0^beta
  
  zeta=(1-e0)/(beta*eta_11*e0)
  
  G<-c()
  #for loop running for each year (timestep vector)
  for (i in 1:length(timestep)){
    if(timestep[i]<tmax){
      G[i]=((2/tmax)*(1/(alpha*(1-zeta)))*((1+alpha*timestep[i])^(1-zeta)-(1-(timestep[i]/tmax)))+
          ((2/tmax^2)*(1/(alpha^2*(1-zeta)*(2-zeta)))*(1-(1+alpha*timestep[i])^(2-zeta)))+
          (1-(timestep[i]/tmax)^2))
    }else{
      G[i]=(2/tmax)*(1/(alpha*(1-zeta)))*(1+alpha*timestep[i])^(1-zeta)+
        ((2/(tmax^2))*(1/(alpha^2*(1-zeta)*(2-zeta)))*(((1+alpha*(timestep[i]-tmax))^(2-zeta))-((1+alpha*timestep[i])^(2-zeta))))
    }
  }
  return(G)
  
}



#remaining mass of all the cohorts
Remaining.mass.OM<-function( Litter, Harvest.residuals, Lat, parameters, timestep){
  
  #calculates the proportion of remaining C for a certain material
  Needles_fraction<-Remaining.OM(Lat, parameters, tmax=parameters$tmax_b, timestep)
  Branches_fraction<-Remaining.OM(Lat, parameters, tmax=parameters$tmax_g, timestep)
  Stem_fraction<-Remaining.OM(Lat, parameters, tmax=parameters$tmax_st, timestep)
  Stump_fraction<-Remaining.OM(Lat, parameters, tmax=parameters$tmax_stmp, timestep)
  Root1_fraction<-Remaining.OM(Lat, parameters, tmax=parameters$tmax_r1, timestep)
  Root2_fraction<-Remaining.OM(Lat, parameters, tmax=parameters$tmax_r2, timestep)
  
  
#### Soil C from litter inputs from treees
  
  ### needles
  # approximate an integration numerically through a matrix
  # every year some C is added and decays according to the proportions calculated above
  FC_needles_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      FC_needles_annual<-Litter$Litter_fine.roots.needles[i]*Needles_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      FC_needles_annual_long<-c(years_before, FC_needles_annual)[1:length(timestep)]
      FC_needles_matrix[,i]<-FC_needles_annual_long
    }
  # now calculate the marginal sum of the matrix
    FC_needles<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      FC_needles[i]<-sum(FC_needles_matrix[i,], na.rm = T)  
    }
    
  ### branches
  # approximate an integration numerically through a matrix
  # every year some C is added and decays according to the proportions calculated above
    FC_branches_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      FC_branches_annual<-Litter$Litter_branches[i]*Branches_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      FC_branches_annual_long<-c(years_before, FC_branches_annual)[1:length(timestep)]
      FC_branches_matrix[,i]<-FC_branches_annual_long
    }
    # now calculate the marginal sum of the matrix
    FC_branches<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      FC_branches[i]<-sum(FC_branches_matrix[i,], na.rm = T)  
    }
    
#### Soil C from harvesting residues
    
    ### needles
    # approximate an integration numerically through a matrix
    # every year some C is added and decays according to the proportions calculated above
    GC_needles_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_needles_annual<-Harvest.residuals$Gneedles[i]*Needles_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      GC_needles_annual_long<-c(years_before, GC_needles_annual)[1:length(timestep)]
      GC_needles_matrix[,i]<-GC_needles_annual_long
    }
    # now calculate the marginal sum of the matrix
    GC_needles<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_needles[i]<-sum(GC_needles_matrix[i,], na.rm = T)  
    }
    
    ### branches
    # approximate an integration numerically through a matrix
    # every year some C is added and decays according to the proportions calculated above
    GC_branches_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_branches_annual<-Harvest.residuals$Gbranches[i]*Branches_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      GC_branches_annual_long<-c(years_before, GC_branches_annual)[1:length(timestep)]
      GC_branches_matrix[,i]<-GC_branches_annual_long
    }
    # now calculate the marginal sum of the matrix
    GC_branches<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_branches[i]<-sum(GC_branches_matrix[i,], na.rm = T)  
    }
    
    ### stemV
    # approximate an integration numerically through a matrix
    # every year some C is added and decays according to the proportions calculated above
    GC_stemV_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_stemV_annual<-Harvest.residuals$GstemV[i]*Stem_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      GC_stemV_annual_long<-c(years_before, GC_stemV_annual)[1:length(timestep)]
      GC_stemV_matrix[,i]<-GC_stemV_annual_long
    }
    # now calculate the marginal sum of the matrix
    GC_stemV<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_stemV[i]<-sum(GC_stemV_matrix[i,], na.rm = T)  
    }
    
    ### stemB
    # approximate an integration numerically through a matrix
    # every year some C is added and decays according to the proportions calculated above
    GC_stemB_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_stemB_annual<-Harvest.residuals$GstemB[i]*Stem_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      GC_stemB_annual_long<-c(years_before, GC_stemB_annual)[1:length(timestep)]
      GC_stemB_matrix[,i]<-GC_stemB_annual_long
    }
    # now calculate the marginal sum of the matrix
    GC_stemB<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_stemB[i]<-sum(GC_stemB_matrix[i,], na.rm = T)  
    }
    
    ### stump
    # approximate an integration numerically through a matrix
    # every year some C is added and decays according to the proportions calculated above
    GC_stump_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_stump_annual<-Harvest.residuals$Gstump[i]*Stump_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      GC_stump_annual_long<-c(years_before, GC_stump_annual)[1:length(timestep)]
      GC_stump_matrix[,i]<-GC_stump_annual_long
    }
    # now calculate the marginal sum of the matrix
    GC_stump<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_stump[i]<-sum(GC_stump_matrix[i,], na.rm = T)  
    }
    
    
    ### root1
    # approximate an integration numerically through a matrix
    # every year some C is added and decays according to the proportions calculated above
    GC_root1_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_root1_annual<-Harvest.residuals$Groot.above.5cm[i]*Root1_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      GC_root1_annual_long<-c(years_before, GC_root1_annual)[1:length(timestep)]
      GC_root1_matrix[,i]<-GC_root1_annual_long
    }
    # now calculate the marginal sum of the matrix
    GC_root1<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_root1[i]<-sum(GC_root1_matrix[i,], na.rm = T)  
    }
    
    ### root2
    # approximate an integration numerically through a matrix
    # every year some C is added and decays according to the proportions calculated above
    GC_root2_matrix<-mat.or.vec(length(timestep), length(timestep))
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_root2_annual<-Harvest.residuals$Groot.below.5cm[i]*Root2_fraction
      years_before<-rep(NA, i-1) #create a vector of NAs to fill the years before the actual litter input
      GC_root2_annual_long<-c(years_before, GC_root2_annual)[1:length(timestep)]
      GC_root2_matrix[,i]<-GC_root2_annual_long
    }
    # now calculate the marginal sum of the matrix
    GC_root2<-c()
    for(i in 1:length(timestep)){ # the loop produces a square matrix of size=length(timestep)
      GC_root2[i]<-sum(GC_root2_matrix[i,], na.rm = T)  
    }
    
    results<-data.frame(FC_needles, FC_branches, GC_needles, GC_branches, GC_stemV, GC_stemB, GC_stump, GC_root1, GC_root2)
    
    return(results)
}



