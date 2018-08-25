# Aggregation by 2 with +150% change percentage for out-of-control shifts Poisson


library(doSNOW)
library(foreach)

set.seed(19190)


code=function(number, comm, new.within, within, between){
  library(lattice)
  library(latticeExtra)
  library(operator.tools)
  library(gtools)
  library(network)
  library(rmutil)
  # Network Count Data Form Only
  # Change drive path according to the location of the function file
  setwd("C:/Users/jmz167/Desktop")
  source("Simulation Functions.R")
  # within community and within community P values
  settings=data.gen(number, comm, new.within, within, between)
  s.tilda.work=detection.timepoint=agg.s.tilda.work=agg.detection.timepoint=shift.point2=rep(NA, 1000)
  # random shift point between 225 and 275
  # Number of simulations
  for(t in 1:2000){
    # shift point is random for each of the 1000 simulation instances
    if(t<51){
      shift.point=841
    } else if(t<101){
      shift.point=842
    } else if(t<151){
      shift.point=843
    } else if(t<201){
      shift.point=844
    } else if(t<251){
      shift.point=845
    } else if(t<301){
      shift.point=846
    } else if(t<351){
      shift.point=847
    } else if(t<401){
      shift.point=848
    } else if(t<451){
      shift.point=849
    } else if(t<501){
      shift.point=850
    } else if(t<551){
      shift.point=851
    } else if(t<601){
      shift.point=852
    } else if(t<651){
      shift.point=853
    } else if(t<701){
      shift.point=854
    } else if(t<751){
      shift.point=855
    } else if(t<801){
      shift.point=856
    } else if(t<851){
      shift.point=857
    } else if(t<901){
      shift.point=858
    } else if(t<951){
      shift.point=859
    } else if(t<1001){
      shift.point=860}
    # While loop check if the data sequence reaches at the the time point of shift without false alarm. Else regenerate data sequence
    w.loop.indi=agg.w.loop.indi=0
    while(w.loop.indi==0 & agg.w.loop.indi==0){
      degree=fst=snd=agg.degree=agg.fst=agg.snd=NULL
      m.tilda=m.tilda2=m.tilda3=rep(NA, 860-20)
      agg.m.tilda=agg.m.tilda2=agg.m.tilda3=rep(NA, (860)/2-20)
      s.tilda=s.tilda2=s.tilda3=rep(NA, 860-40)
      agg.s.tilda=agg.s.tilda2=agg.s.tilda3=rep(NA, (860)/2-40)
      agg.e=agg.e.bernoulli=list()
      # This specifies aggregation level in the simulation
      agg.index=seq(2, 860, by=2)
      for(i in 1:(860)){
        if(i < shift.point){data=sequential(number=settings[[1]], lambda=settings[[2]])} else{data=sequential(number=settings[[1]], lambda=settings[[3]])}
        # saving both count and binary adjacency matrices
        agg.e[[i]]=data[[1]]
        agg.e.bernoulli[[i]]=data[[3]]
        # Simulations according to the aggregation level specified
        if(i %in% agg.index){
          agg.data=sequential2(agg.e[(i-1):i], agg.e.bernoulli[(i-1):i])
          # Pulling specifically generated network count data, for network binary data, parameter passed to the custom "stat" function will be the 3rd and 4th objects from the sequential2 output instead of the 1st and 2nd object
          agg.data2=stat(n=number, e=agg.data[[1]], network=agg.data[[2]])
          agg.degree=cbind(agg.degree,agg.data2[[1]])
          agg.fst=cbind(agg.fst,agg.data2[[2]])
          agg.snd=cbind(agg.snd,agg.data2[[3]])
          if(i > 40){
            agg.std=standardization(agg.degree[,(i/2-20):(i/2)],agg.fst[,(i/2-20):(i/2)],agg.snd[,(i/2-20):(i/2)], n=number)
            agg.m.tilda[i/2-20]=agg.std[[1]]
            agg.m.tilda2[i/2-20]=agg.std[[2]]
            agg.m.tilda3[i/2-20]=agg.std[[3]]
          }
          if(i > 80){
            agg.priebe=standardization2(agg.m.tilda[(i/2-40):(i/2-20)], agg.m.tilda2[(i/2-40):(i/2-20)], agg.m.tilda3[(i/2-40):(i/2-20)])
            agg.s.tilda[i/2-40]=agg.priebe[[1]]
            agg.s.tilda2[i/2-40]=agg.priebe[[2]]
            agg.s.tilda3[i/2-40]=agg.priebe[[3]]
          }
        }
        # simulation without any aggregation, this portion of code is only used in the case where W=2, for the codes corresponding to higher aggregation levels, this portion of code and its 
        # output are ignored from the analysis and plot output.
        data2=stat(n=number, e=data[[1]], network=data[[2]])
        degree=cbind(degree,data2[[1]])
        fst=cbind(fst,data2[[2]])
        snd=cbind(snd,data2[[3]])
        if(i > 20){
          std=standardization(degree[,(i-20):i],fst[,(i-20):i],snd[,(i-20):i], n=number)
          m.tilda[i-20]=std[[1]]
          m.tilda2[i-20]=std[[2]]
          m.tilda3[i-20]=std[[3]]
        }
        if(i >40){
          priebe=standardization2(m.tilda[(i-40):(i-20)], m.tilda2[(i-40):(i-20)], m.tilda3[(i-40):(i-20)])
          s.tilda[i-40]=priebe[[1]]
          s.tilda2[i-40]=priebe[[2]]
          s.tilda3[i-40]=priebe[[3]]
        }
        if(length(which(s.tilda[1:(shift.point-41)]>4))==0 && length(which(s.tilda2[1:(shift.point-41)]>4))==0 && length(which(s.tilda3[1:(shift.point-41)]>4))==0){
          w.loop.indi=1
        }
        if(check.integer(shift.point/2-40)==T){
          if(length(which(agg.s.tilda[1:(shift.point/2-41)]>4))==0 && length(which(agg.s.tilda2[1:(shift.point/2-41)]>4))==0 && length(which(agg.s.tilda3[1:(shift.point/2-41)]>4))==0){
            agg.w.loop.indi=1
          } else if(length(which(agg.s.tilda[1:floor(shift.point/2-40)]>4))==0 && length(which(agg.s.tilda2[1:floor(shift.point/2-40)]>4))==0 && length(which(agg.s.tilda3[1:floor(shift.point/2-40)]>4))==0){
            agg.w.loop.indi=1
          }
        }
      }
    }
    # after the while loop, report detection success and detection time point
    detection.timepoint[t]=0
    s.tilda.work[t]=0
    if(length(which(s.tilda[1:(shift.point-41)]>4))==0 && length(which(s.tilda[(shift.point-40):(860-40)]>4))>=1){
      s.tilda.work[t]=1
      detection.timepoint[t]=which(s.tilda>4)[1]
    }else if(length(which(s.tilda2[1:(shift.point-41)]>4))==0 && length(which(s.tilda2[(shift.point-40):(860-40)]>4))>=1){
      s.tilda.work[t]=1
      detection.timepoint[t]=which(s.tilda2>4)[1]
    }else if(length(which(s.tilda3[1:(shift.point-41)]>4))==0 && length(which(s.tilda3[(shift.point-40):(860-40)]>4))>=1){
      s.tilda.work[t]=1
      detection.timepoint[t]=which(s.tilda3>4)[1]
    }
    agg.detection.timepoint[t]=0
    agg.s.tilda.work[t]=0
    # Check for the shift time of the anomaly, whether if it is within an aggregation period, or at the end of an aggregation period. This information is used to generate Figure 10 in the paper.
    if(check.integer(shift.point/2-40)==T){
      if(length(which(agg.s.tilda[1:(shift.point/2-41)]>4))==0 && length(which(agg.s.tilda[(shift.point/2-40):((860)/2-40)]>4))>=1){
        agg.s.tilda.work[t]=1
        agg.detection.timepoint[t]=which(agg.s.tilda>4)[1]
      }else if(length(which(agg.s.tilda2[1:(shift.point/2-41)]>4))==0 && length(which(agg.s.tilda2[(shift.point/2-40):((860)/2-40)]>4))>=1){
        agg.s.tilda.work[t]=1
        agg.detection.timepoint[t]=which(agg.s.tilda2>4)[1]
      }else if(length(which(agg.s.tilda3[1:(shift.point/2-41)]>4))==0 && length(which(agg.s.tilda3[(shift.point/2-40):((860)/2-40)]>4))>=1){
        agg.s.tilda.work[t]=1
        agg.detection.timepoint[t]=which(agg.s.tilda3>4)[1]
      }
    } else {
      if(length(which(agg.s.tilda[1:floor(shift.point/2-40)]>4))==0 && length(which(agg.s.tilda[ceiling(shift.point/2-40):((860)/2-40)]>4))>=1){
        agg.s.tilda.work[t]=1
        agg.detection.timepoint[t]=which(agg.s.tilda>4)[1]
      }else if(length(which(agg.s.tilda2[1:floor(shift.point/2-40)]>4))==0 && length(which(agg.s.tilda2[ceiling(shift.point/2-40):((860)/2-40)]>4))>=1){
        agg.s.tilda.work[t]=1
        agg.detection.timepoint[t]=which(agg.s.tilda2>4)[1]
      }else if(length(which(agg.s.tilda3[1:floor(shift.point/2-40)]>4))==0 && length(which(agg.s.tilda3[ceiling(shift.point/2-40):((860)/2-40)]>4))>=1){
        agg.s.tilda.work[t]=1
        agg.detection.timepoint[t]=which(agg.s.tilda3>4)[1]
      }
    }
  }
  # Simulation result output
  out=rbind(shift.point2, s.tilda.work, detection.timepoint, agg.s.tilda.work, agg.detection.timepoint)
  # proc.time() - ptm
  return(out)
}

# Simulation Parameter Settings. First number refers to number of nodes in the network, second number refers to the number of communities in the network, first number refers to the 
# propensity of communication among the nodes within the anomalous subnetwork in the presence of the anomaly, forth number refers to the baseline propensity of commuication among the 
# nodes within the each community, fifth number refers to the baseline propoensity of communication among the amongs across different communities.
combo=rbind(c(20, 2, 0.3, 0.2, 0.1), c(20, 2, 0.75, 0.5, 0.25), c(20, 2, 1.5, 1, 0.5), c(20, 2, 2.25, 1.5, 0.75), c(20, 2, 3, 2, 1), c(20, 2, 3.75, 2.5, 1.25), c(20, 2, 4.5, 3, 1.5),
            c(20, 2, 6, 4, 2), c(20, 2, 7.5, 5, 2.5))


# Leveraging parallel computing to perform multiple simulations per parameter setting combinations
cl2=makeCluster(9, type = "SOCK")
registerDoSNOW(cl2)
getDoParWorkers()
getDoParName()
getDoParVersion()
output=foreach(i = 1:9, .combine=rbind) %dopar% {code(number=combo[i,1], comm=combo[i,2], new.within=combo[i,3], within=combo[i,4], between=combo[i,5])}
output2=output
save(output, output2, file = "v 20 aggregation 2 +50per 2comm poisson.RData")
stopCluster(cl2)




