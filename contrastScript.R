#set up matrix of all 64 combinations of levels,
##Also record their mod 2 of the linear contrast of confounding Interactions
orthoMatrix = matrix(0,nrow = 64,ncol = 9)
colnames(orthoMatrix) = c("f","e","d","c","b","a","L1","L2","L3")

rowCount= 1
for(f in c(0,1)){
  for(e in c(0,1)){
    for(d in c(0,1)){
      for(c in c(0,1)){
        for(b in c(0,1)){
          for(a in c(0,1)){
            orthoMatrix[rowCount,1:6] = c(f,e,d,c,b,a)
            
            #Check linear contrast mod 2
            orthoMatrix[rowCount,7] = (a+b+c+d)%%2
            orthoMatrix[rowCount,8] = (a+c+e)%%2
            orthoMatrix[rowCount,9] = (a+b+e+f)%%2
            rowCount = rowCount +1
          }
        }
      }
    }
  }
}

#block 1
block1 = orthoMatrix[which(orthoMatrix[,7]==0 &
                             orthoMatrix[,8]==0&
                             orthoMatrix[,9]==0),]
block1

#block 2
block2 = orthoMatrix[which(orthoMatrix[,7]==1 &
                             orthoMatrix[,8]==0&
                             orthoMatrix[,9]==0),]
block2

#block 3
block3 = orthoMatrix[which(orthoMatrix[,7]==0 &
                             orthoMatrix[,8]==1&
                             orthoMatrix[,9]==0),]
block3

#block 4
block4 = orthoMatrix[which(orthoMatrix[,7]==0 &
                             orthoMatrix[,8]==0&
                             orthoMatrix[,9]==1),]
block4

#block 5
block5 = orthoMatrix[which(orthoMatrix[,7]==1 &
                             orthoMatrix[,8]==1&
                             orthoMatrix[,9]==0),]
block5

#block 6
block6 = orthoMatrix[which(orthoMatrix[,7]==1 &
                             orthoMatrix[,8]==0&
                             orthoMatrix[,9]==1),]
block6

#block 7
block7 = orthoMatrix[which(orthoMatrix[,7]==0 &
                             orthoMatrix[,8]==1&
                             orthoMatrix[,9]==1),]
block7

#block 8
block8 = orthoMatrix[which(orthoMatrix[,7]==1 &
                             orthoMatrix[,8]==1&
                             orthoMatrix[,9]==1),]
block8