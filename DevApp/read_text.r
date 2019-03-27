some_words <- scan("DevApp/Alice.txt", sep="\n",what=character())

beginning<-seq(1,20000,100)
end<-seq(100,20000,100)

paste(some_words[beginning[1]:end[1]], collapse=" ")

Alice_paragraphs <- c()
for(i in 1:100){
  Alice_paragraphs[i] <- paste(some_words[beginning[i]:end[i]], collapse=" ")
}

save(Alice_paragraphs,file="paragraphs.Rdata")
