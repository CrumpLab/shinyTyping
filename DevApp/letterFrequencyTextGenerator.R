#RANDOM
numReps=500
oneNine=rep(seq(1,9,1),numReps)
oneNine=sample(oneNine)

paragraph=""
  for(i in oneNine)
  {  
    word=""
    for(j in 1:i)
    {
      newLet=sample(letters)[1]
      word=paste(word,newLet,sep="")
    }
    paragraph=paste(paragraph,word,sep=" ")
  }

write(paragraph,file="random.txt")

#UNIGRAM FREQUENCIES
library(data.table)
letter_freqs <- fread("ngrams1.csv",integer64="numeric")
letter_freqs[letter_freqs==0]<-1
letter_freqs=letter_freqs[order(letter_freqs$gram),]
letter_probabilities_uni <- apply(letter_freqs[,12:(12+44)],2,function(x){x/sum(x)})
paragraph=""
oneNine=sample(oneNine)
# i = word length, j = let position

for(i in oneNine)
{  
  word=""
  for(j in 1:i)
  {  # change here
    currentPosn=paste(i,"/",j,":",j,sep="")
    probs=letter_probabilities_uni[,which(colnames(letter_probabilities_uni)==currentPosn)]
    newLet=sample(letters,size=1,prob=probs)
    word=paste(word,newLet,sep="")
  }
  paragraph=paste(paragraph,word,sep=" ")
}

write(paragraph,file="unigram.txt")

#BIGRAM FREQUENCIES

#######
get_prob<- function(df) {apply(df,2,function(x){x/sum(x)})}
get_entropies <- function(df){apply(df,2,function(x){-1*sum(x*log2(x))})}

# GET LETTER POSITION > 1
# read in n-gram tsv and clean up
gram_2 <- read.table('2-gram.txt',header=TRUE,sep="\t")
colnames(gram_2)<- scan(file="2-gram.txt",what="text",nlines=1,sep="\t")

# fix NA level
levels(gram_2$`2-gram`)<-c(levels(gram_2$`2-gram`),as.character("NA"))
gram_2[is.na(gram_2$`2-gram`),]$`2-gram` = as.character("NA")


# find and replace missing combos with 0 
allLet<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
allCombos<-c()
for (i in 1:length(allLet)){
  for(j in 1:length(allLet)){
    allCombos<-c(allCombos,paste(allLet[i],allLet[j],sep=""))
  }
}

missing<-allCombos[!allCombos%in%gram_2$`2-gram`]
missing<-cbind(missing,matrix(0,nrow = length(missing), ncol = ncol(gram_2)-1))
colnames(missing)<-colnames(gram_2)
gram_2<-rbind(gram_2,missing)

# change 0s to 1s
gram_2[gram_2 == 0] <- 1

#split bigrams into letter 1 & 2
letterz <- data.frame(do.call('rbind', strsplit(as.character(gram_2$`2-gram`),'',fixed=TRUE)))
colnames(letterz)<-c('n-1','n')
names(gram_2)[names(gram_2) == '2-gram'] <- 'bigram'
gram_2<-cbind(letterz,gram_2)

#remove unnecessary columns
gram_2<-gram_2[,-4:-12]
gram_2<-gram_2[,-40:-56]
gram_2[,4:39]<-apply(gram_2[,4:39],2,function(x){as.numeric(x)})

# GET ENTROPIES
get_prob<- function(df) {apply(df,2,function(x){x/sum(x)})}
get_entropies <- function(df){apply(df,2,function(x){-1*sum(x*log2(x))})}

gram_2=gram_2[order(gram_2$`n`),]
gram_2=gram_2[order(gram_2$`n-1`),]
letter_probabilities<-(with(gram_2,
                            by(gram_2[,4:39],gram_2[,'n-1'], get_prob,simplify= TRUE)
))
for (i in 1:26){
  if(i == 1){
    df = as.data.frame(letter_probabilities[[i]])
  } else {
    temp = as.data.frame(letter_probabilities[[i]])
    df<-rbind(df,temp)
  }
  
}
####################################################################

oneNine=sample(oneNine)
paragraph=""
for(i in oneNine)
{  
  word=""
  for(j in 1:i)
  {  # change here
    
     
    if(j==1)
    {currentPosn=paste(i,"/",j,":",j,sep="") 
      probs=letter_probabilities_uni[,which(colnames(letter_probabilities_uni)==currentPosn)]}
    else
    {currentPosn=paste(i,"/",j-1,":",j,sep="")
    probs=df[,which(colnames(df)==currentPosn)]
    probs=probs[(((which(letters==newLet)-1)*26)+1):(which(letters==newLet)*26)]
     }
    newLet=sample(letters,size=1,prob=probs)
    word=paste(word,newLet,sep="")
  }
  paragraph=paste(paragraph,word,sep=" ")
}
write(paragraph,file="bigram.txt")
################################################################################################

#All possible trigrams
allTrigrams=c()
for(i in 1:26)
{
  for(j in 1:26)
  {
    for(k in 1:26)
      {allTrigrams=c(allTrigrams,paste(LETTERS[i],LETTERS[j],LETTERS[k],sep=""))
      }
  }  
}
library(dplyr)
#take the charts we want from trigram freqs
trigram_freqs <- fread("ngrams3.csv",integer64="numeric")
missingNames=allTrigrams[!allTrigrams%in%trigram_freqs$`3-gram`]
norvigTris= trigram_freqs$`3-gram`
trigram_freqs=trigram_freqs[,-1]
missing<-matrix(0,nrow = length(missingNames), ncol = ncol(trigram_freqs))
colnames(missing)<-colnames(trigram_freqs)
trigram_freqs=rbind(trigram_freqs,missing)
norvigTris=c(norvigTris,missingNames)
#refactor
trigram_freqs=cbind(norvigTris,trigram_freqs)

letters3 <- data.frame(do.call('rbind', strsplit(as.character(trigram_freqs$`norvigTris`),'',fixed=TRUE)))
letters3=letters3 %>%
  mutate(bigram=paste(X1,X2,sep=""))
trigram_freqs=trigram_freqs[,-1]
trigram_freqs<-cbind(letters3$bigram,letters3$X3,trigram_freqs)
trigram_freqs=trigram_freqs[order(trigram_freqs$V2),]
trigram_freqs=trigram_freqs[order(trigram_freqs$V1),]

get_prob<- function(df) {apply(df,2,function(x){x/sum(x)})}
letterProbTri<-(with(trigram_freqs,
                            by(trigram_freqs[,3:ncol(trigram_freqs)],trigram_freqs[,'V1'], get_prob,simplify= TRUE)
))
#plug the last 2 letters of the word into the AB
str(letterProbTri[[which(names(letterProbTri)=="AB")]])
#in our loop we must identify the previous bigram and the matching column based on the letposn length

which(colnames(letterProbTri[[which(names(letterProbTri)=="TH")]])=="3/1:3")

oneNine=sample(oneNine)
paragraph=""
for(i in oneNine)
{  
  word=""
  for(j in 1:i)
  {  # change here
    
    
    if(j==1)
    {currentPosn=paste(i,"/",j,":",j,sep="") 
    probs=letter_probabilities_uni[,which(colnames(letter_probabilities_uni)==currentPosn)]}
    else if(j==2)
    {currentPosn=paste(i,"/",j-1,":",j,sep="")
    probs=df[,which(colnames(df)==currentPosn)]
    probs=probs[(((which(letters==newLet)-1)*26)+1):(which(letters==newLet)*26)]
    }
    else
    {currentPosn=paste(i,"/",j-2,":",j,sep="")
      #which(colnames(letterProbTri[[which(names(letterProbTri)=="TH")]])=="3/1:3")
    #get the previous bigram
    wordLets=strsplit(word,split="")[[1]]
    prevBgrm=paste(wordLets[length(wordLets)-1],wordLets[length(wordLets)],sep="")
    prevBgrm=toupper(prevBgrm)
    #get the column number of the bigram's matrix of probs matching our i and j
    posnMtch = which(colnames(letterProbTri[[which(names(letterProbTri)==prevBgrm)]])==currentPosn)
    probs=letterProbTri[[which(names(letterProbTri)==prevBgrm)]][,posnMtch]
    }
    newLet=sample(letters,size=1,prob=probs)
    word=paste(word,newLet,sep="")
  }
  paragraph=paste(paragraph,word,sep=" ")
}
write(paragraph,file="trigram.txt")