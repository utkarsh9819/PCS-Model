
library(PCSinR)
library(readr)
library(ggplot2)
d1<-read.csv("D1.csv", header = TRUE, sep = ",")
d2<-read.csv("D2.csv", header = TRUE, sep = ",")
d3<-read.csv("D3.csv", header = TRUE, sep = ",")
d4<-read.csv("D4.csv", header = TRUE, sep = ",")
d5<-read.csv("D5.csv", header = TRUE, sep = ",")
d6<-read.csv("D6.csv", header = TRUE, sep = ",")
d7<-read.csv("D7.csv", header = TRUE, sep = ",")
d8<-read.csv("D8.csv", header = TRUE, sep = ",")

car_data <- list (d1, d2, d3, d4, d5, d6, d7, d8)


model<-function(data){               #creating the model
  node_names <- c("source_node", rownames(data), "Hatsdun", "Kaiwa", "Dasuka", "Nabusi" )
  nodes<-17
  # Defining interconnection matrix
  
  interconnection_matrix <- matrix(rep(0, 17^2), nrow=nodes)
  rownames(interconnection_matrix) <- node_names
  colnames(interconnection_matrix) <- node_names
  
  for (i in 1:12){
    interconnection_matrix[1, 1+i] <- data$Weight[i]
  }
  
  sourceNodeCueW <- interconnection_matrix[1,2:13]
  sourceNodeCueW = (sourceNodeCueW) * 1 ** 1.9
  interconnection_matrix[1,2:13] = sourceNodeCueW 
  
  cueOptionScale = 0.01
  
  for(i in 1:4){
    
    for(j in 1:12){
      interconnection_matrix[j+1, i+13] <- data[j, i+2] * cueOptionScale 
      
      
    }
  }
  
  
  for(i in 1:3){
    for (j in i:4){
      
      interconnection_matrix[i+13, j+13] = -0.2
    }
  }
  
  interconnection_matrix <- interconnection_matrix +
    t(upper.tri(interconnection_matrix) * interconnection_matrix)
  interconnection_matrix <- matrix(interconnection_matrix, nrow=17)
  diag(interconnection_matrix) <- 0 
  
  result_final=PCS_run_from_interconnections(interconnection_matrix, convergence_criteria=c(PCS_convergence_McCandR),
                                             convergence_names=NULL)
  return(result_final)
}


n_iteration<-rep(0, length(car_data))
node_v<-list(rep(0, length(car_data)))
cue_final<-list(rep(0, 12))


for (i in 1:8){
  dp<-data.frame(car_data[i])
  decision<-model(dp)
  n_iteration[i]<-decision$convergence
  final<-list(rep(0, length(4)))
  cf<-list(rep(0, length(12)))
  
    cf<-as.list(decision$iterations[nrow(decision$iterations),4:15])

    final<-as.list(decision$iterations[nrow(decision$iterations),16:19])

  node_v[[i]]<-final
  cue_final[[i]]<-cf
  
}

cue_final<- as.data.frame(do.call(rbind, cue_final))

 node_v<- as.data.frame(do.call(rbind, node_v))
 
 result <- cbind(node_v, cue_final)

 
 result["iterations"]<-n_iteration
 colnames(result)<-c("Hatsdun", "Kaiwa", "Dasuka", "Nabusi", rownames(d1), "Iterations")
 result["trial"]<-c(1,2,3,4,5,6,7,8)
 result1<-result[-7,]
 ggplot(data=result1, aes(x=trial, y=Iterations))+
   geom_point()+
   geom_line()
 
 
 
 
 
  result_8<-model(d8)
 
traj<-result_8$iterations[, 4:15] 
last<-(decision$iterations[nrow(decision$iterations),4:15])
last<-(last*100)
last<- data.frame(last)
colnames(last)<- c(rownames(d1))

last<-t(last)
n<-c(rownames(d1))
last<- data.frame(last)

last$new_column<-n
colnames(last)<-c("Final_Node_Activation", "Cue_number")

 ggplot(data=last,aes(x = as.numeric(Cue_number), y =Final_Node_Activation))+
   geom_point()+
   ylim(-20, 70)+
   geom_line()+
   scale_x_continuous(limits = c(0, 13),  # Range limits
                      breaks = seq(0, 13, by = 1))+
   labs(x = "Cue Number", y = "Final Activation") 
 
 
 

 

 
 
 
