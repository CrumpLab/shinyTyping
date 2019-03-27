
# converts txt file into vector of 100 word paragraphs (except for last couple of words < 100 words)
# saves it as a list item in paragraphs.Rdata
loadTXT=function(new_text,appName){
load(file="paragraphs.Rdata")
# .txt file name from which to get text


# the variable name for the word vector in the app


some_words <- scan(new_text, sep=" ",what=character(), quote="")
nParagraphs=(length(some_words)-(length(some_words)%%100))/100
nParagraphs=nParagraphs+1
beginning<-seq(1,nParagraphs*100,100)
end<-seq(100,(nParagraphs-1)*100,100)
end=c(end,length(some_words))
paragraphs=c()

for(i in 1:nParagraphs){
  paragraphs[i] <- paste(some_words[beginning[i]:end[i]], collapse=" ")
}

allParagraphs[[length(allParagraphs)+1]]=paragraphs
names(allParagraphs)[[length(allParagraphs)]]=appName
save(allParagraphs,file="paragraphs.Rdata")


}