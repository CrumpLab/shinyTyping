
some_words <- scan("Alice.txt", sep="\n",what=character())

# replace "cats.txt" with directory for new text file

new_text="cats.txt"

some_words <-c(some_words, scan(new_text, sep=" ",what=character(), quote=""))
nParagraphs=(length(some_words)-(length(some_words)%%100))/100

beginning<-seq(1,nParagraphs*100,100)
end<-seq(100,nParagraphs*100,100)

Alice_paragraphs=c()

for(i in (length(Alice_paragraphs)+1):nParagraphs){
  Alice_paragraphs[i] <- paste(some_words[beginning[i]:end[i]], collapse=" ")
}

save(Alice_paragraphs,file="paragraphs.RData")