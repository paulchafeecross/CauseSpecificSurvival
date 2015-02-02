require('knitr')
setwd("~/BitBucket/VIT2014/Analysis")

# Master Document
knit("MasterDoc.Rmd")
system('pandoc MasterDoc.md -V geometry:margin=1in -s -o MasterDoc.docx --bibliography VITpaper.bib --csl ecology.csl')
system('pandoc MasterDoc.md -V geometry:margin=1in -s -o MasterDoc.pdf --bibliography VITpaper.bib --csl ecology.csl')

#Separate files md to Word
knit("WriteUp.Rmd")
knit("Table1.Rmd")
knit("Figures.Rmd")
knit("SupInfo.Rmd")

system('pandoc -D docx > mytemplate.docx')
system('pandoc -D latex > mytemplate.docx')

system('pandoc -D pdf > mytemplate.pdf')

#system('pandoc WriteUp.md -H options.sty -V geometry:margin=1in -s -o WriteUp.docx --bibliography VITpaper.bib --csl ecology.csl')
system('pandoc WriteUp.md --listings -V geometry:margin=1in -s -o WriteUp.docx --bibliography VITpaper.bib --csl ecology.csl')

system('pandoc Table1.md -V geometry:margin=1in -s -o Table1.docx')
system('pandoc Figures.md -V geometry:margin=1in -s -o Figures.docx')
system('pandoc SupInfo.md -V geometry:margin=1in -s -o SupInfo.docx')

# separate files md to pdf

system('pandoc WriteUp.md -H options.sty -V geometry:margin=1in -s -o WriteUp.pdf --bibliography VITpaper.bib --csl ecology.csl')
system('pandoc Table1.md -V geometry:margin=1in -s -o Table1.pdf')
system('pandoc Figures.md -V geometry:margin=1in -s -o Figures.pdf')
system('pandoc SupInfo.md -V geometry:margin=1in -s -o SupInfo.pdf')

#mess2doc <- 'pandoc testtable.md -V geometry:margin=1in -s -o Test.docx'
#mess2pdf <- 'pandoc testtable.md -V geometry:margin=1in -s -o Test.pdf'
#system(mess2doc)
#system(mess2pdf)
