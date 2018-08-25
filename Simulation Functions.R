# DCSBM Model Functions

# set.seed(11567)
# Function to generate adjacency matrix using provided probability matrix
generate.network = function(n, p){
  # Make the adjacency matrix symmetric.
  networkmat=matrix(data=NA,nrow=n,ncol=n)
  for(i in 1:n){
    for(j in 1:n){
      if (i <= j){
        networkmat[j, i]=networkmat[i, j]=rpois(1,p[i,j])
      }
    }
  }
  return(networkmat)
}


# Convert network counting data to binary network data
sequential=function(number, lambda){
  e=generate.network(number, lambda)
  e.bernoulli=e
  e.bernoulli[e.bernoulli>0]=1
  network=network(e,directed=F)
  network.bernoulli=network(e.bernoulli,directed=F)
  return(list(e, network, e.bernoulli, network.bernoulli))
}


# Aggregate simulated network data 
sequential2=function(e, e.bernoulli){
  aggregate.e=Reduce('+', e)
  aggregate.e.bernoulli=Reduce('+', e.bernoulli)
  aggregate.e.bernoulli[aggregate.e.bernoulli>0]=1
  aggregate.network=network(aggregate.e,directed=F)
  aggregate.network.bernoulli=network(aggregate.e.bernoulli,directed=F)
  return(list(aggregate.e, aggregate.network, aggregate.e.bernoulli, aggregate.network.bernoulli))
}

check.integer <- function(N){
  !grepl("[^[:digit:]]", format(N,  digits = 20, scientific = FALSE))
}


# Function that returns zero, 1st, and 2nd neighborhoods for all nodes in the network
neighborhoodk = function(nodes, network, e){
  degree=rep(0,nodes)
  first.neighbor=rep(0,nodes)
  second.neighbor=rep(0,nodes)
  for (i in 1:nodes){
    # degree of a node
    degree[i]=sum(e[i,])
    if(degree[i]==0){
      first.neighbor[i]=second.neighbor[i]=0
    }else{
      verticies=get.neighborhood(network, i, type="combined")
      # using neighborhood2 function to calculate additive edges for the degree
      # adding these two together to get first neighborhood
      first.neighbor[i] = degree[i] + neighborhood2(verticies, e, i)
      # using neighborhood3 function to calculate additive edges for the first neighborhood
      # adding these two together to get second neighborhood
      second.neighbor[i] = neighborhood3(verticies, e, i, network) + first.neighbor[i]
    }
  }
  # returns 3 vectors in a list; each vector contains the degrees, first, and second neighborhood edges for all nodes in the network
  neighborhoods=list(degree, first.neighbor, second.neighbor)
  return(neighborhoods)
}

# Function to get the 1st neighborhood edges; i.e. connections among all the nodes that are in a specific node's degree (personal) network
# additive to degree
neighborhood2 = function(verticies, e, i){
  l=length(verticies)
  delta=rep(0,l)
  for (j in 1:l){
    delta[j]=sum(e[verticies[j], verticies[-c(1:j)]])
  }
  return(sum(delta))
}


# Function to get 2nd neighborhood edges; i.e. connections among all the nodes that are in the second neighborhood
# of a specific node's degree (personal) network; as well as any connections among the second degree nodes themselves
# additive to first neighborhood
neighborhood3 = function(verticies, e, i, network){
  prev.second.verticies=NULL
  l=length(verticies)
  delta=rep(0,l)
  for (j in 1:l){
    second.verticies=get.neighborhood(network, verticies[j], type="combined")
    second.verticies=second.verticies[second.verticies %!in% i]
    second.verticies=second.verticies[second.verticies %!in% verticies]
    second.verticies=second.verticies[second.verticies %!in% prev.second.verticies]
    if(length(second.verticies)==0){delta[j]=sum(e[verticies[j], prev.second.verticies])}else{
      l2=length(second.verticies)
      delta2=rep(0,l2)
      for (k in 1:l2){
        delta2[k]=sum(e[second.verticies[k], second.verticies[-c(1:k)]])+sum(e[second.verticies[k], verticies[j]])+sum(e[second.verticies[k], prev.second.verticies])
      }
      delta[j]=sum(delta2)
    }
    prev.second.verticies=c(prev.second.verticies, second.verticies)
  }
  return(sum(delta))
}

# Function to save all the statistics calculcated from the custom fucntion named "neightborhoodk" and saving the statistics to 3 different vectors respectively
stat=function(n, e, network){
  info=neighborhoodk(nodes=n, network, e)
  degree=as.vector(info[[1]])
  fst=as.vector(info[[2]])
  snd=as.vector(info[[3]])
  return(list(degree,fst,snd))
}


# Calculcates the first set of standardized values using Priebe's method
standardization=function(degree,fst,snd, n){
  mu.hat1=mu.hat2=mu.hat3=sd1=sd2=sd3=psi.tilda=psi.tilda2=psi.tilda3=rep(0, n)
  new1=degree[,21]
  new2=fst[,21]
  new3=snd[,21]
  degree=degree[,1:20]
  fst=fst[,1:20]
  snd=snd[,1:20]
  for(u in 1:n){
    mu.hat1[u]=mean(degree[u,])
    mu.hat2[u]=mean(fst[u,])
    mu.hat3[u]=mean(snd[u,])
    sd1[u]=max(sd(degree[u,]), 1)
    sd2[u]=max(sd(fst[u,]), 1)
    sd3[u]=max(sd(snd[u,]), 1)
  }
  for(y in 1:n){
    psi.tilda[y]=(new1[y]-mu.hat1[y])/sd1[y]
    psi.tilda2[y]=(new2[y]-mu.hat2[y])/sd2[y]
    psi.tilda3[y]=(new3[y]-mu.hat3[y])/sd3[y]
  }
  m.tilda=max(psi.tilda)
  m.tilda2=max(psi.tilda2)
  m.tilda3=max(psi.tilda3)
  return(list(m.tilda, m.tilda2, m.tilda3))
}

# Calculcate the second set of standardized values using Priebe's method
standardization2=function(m.tilda,m.tilda2,m.tilda3){
  m.tilda.mean=mean(m.tilda[1:20])
  m.tilda.sd=max(sd(m.tilda[1:20]), 1)
  s.tilda=(m.tilda[21]-m.tilda.mean)/m.tilda.sd
  m.tilda.mean2=mean(m.tilda2[1:20])
  m.tilda.sd2=max(sd(m.tilda2[1:20]), 1)
  s.tilda2=(m.tilda2[21]-m.tilda.mean2)/m.tilda.sd2
  m.tilda.mean3=mean(m.tilda3[1:20])
  m.tilda.sd3=max(sd(m.tilda3[1:20]), 1)
  s.tilda3=(m.tilda3[21]-m.tilda.mean3)/m.tilda.sd3
  return(list(s.tilda, s.tilda2, s.tilda3))
}


# Simulation function, generate simulation results from 1 round of simulation. This portion of code contain references to most of the custom functions above
data.gen=function(number, comm, new.within, within, between){
  # theta=runif(number, 1-delta.cu[1], 1+delta.cu[1])
  # pi vectors for community label, in matrix form, each row contains a pi vector of length equal to the number of communities
  pi.matrix=matrix(data=NA, nrow=number, ncol=comm)
  people=sample(1:number, number, replace=F)
  for(w in 1:comm){
    a=sample(people, number/comm, replace=F)
    pi.matrix[a,w]=1
    people=setdiff(people, a)
  }
  pi.matrix[is.na(pi.matrix)]=0
  # These following randomizations DO change for each simulation loop. The following codes are for generating the
  # community label for each nodes, ~ multinomial (pi vector); separating the different communities into clusters
  # in terms of nodes within that community; the large dimension P mattrix (#nodes * # nodes) used to generate the 
  # adjacency matrix, the P matrix here changes in each loop depending on the nodes' community labels; Finally the 
  # lambda matrix.
  # community labels for nodes
  comm.label=rep(NA, number)
  for(j in 1:number){
    comm.label[j]=sample(c(1:comm), size=1, prob=pi.matrix[j,])
  }
  comm.label=as.data.frame(cbind(c(1:number),comm.label))
  # comm.label=comm.label[order(comm.label[, "comm.label"]),]
  # community clusters and thetas generated according to the constraint
  cluster=list()
  theta.cluster=list()
  theta=rep(NA,number)
  for(g in 1:comm){
    cluster[[g]]=comm.label[which(comm.label$comm.label==g),]
    rownames(cluster[[g]])=NULL
    # Theta parameter
    theta.cluster[[g]]=rpareto(nrow(cluster[[g]]), 1, 3)
    
    # Normalize theta according to the constraint, number of nodes*10
    theta.cluster[[g]]=(theta.cluster[[g]]/sum(theta.cluster[[g]])*nrow(cluster[[g]]))
    
    # Normalize theta according to the constraint, 1
    # theta.cluster[[g]]=theta.cluster[[g]]/sum(theta.cluster[[g]])
    theta[comm.label$comm.label==g]=theta.cluster[[g]]
  }
  # P matrix
  P=matrix(data=rep(between,number^2),ncol=number,nrow=number,byrow=T)
  for(y in 1:comm){
    perm=permutations(length(cluster[[y]]$V1), 2, v=cluster[[y]]$V1)
    P[apply(perm,1,function(x) x[1]), apply(perm,1,function(x) x[2])]=within
  }
  new.P=P
  # random choose a community that contain the localized outbreak 
  new.comm=1
  new.perm=permutations(length(cluster[[new.comm]]$V1), 2, v=cluster[[new.comm]]$V1)
  new.P[apply(new.perm,1,function(x) x[1]), apply(new.perm,1,function(x) x[2])]=new.within
  
  diag(P)=0
  diag(new.P)=0
  # lambda matrix
  lambda=matrix(data=NA,nrow=number,ncol=number)
  for(k in 1:number){
    lambda[k,]=theta[k]*theta
  }
  lambda2=lambda
  lambda=lambda*P
  new.lambda=lambda2*new.P
  # These also change from loop instant to loop instant, the following codes generate t=T adjacency matrices based on the 
  # same lambda matrix, the adjacency matrices represent the network graphs over time.
  # t specifies the number of simulations to perform
  # Generating the same number of adjacency matrices as the previously specified number of time points; 
  # Adjacency matrices from the shift point and onwards are generated from the p.new (shift probability matrix)
  #number of time points per simulation
  return(list(number, lambda, new.lambda))
}
