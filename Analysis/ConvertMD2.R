require('knitr')
setwd("~/BitBucket/VIT/Analysis")

#First knit the files to html and md. 
knit("WriteUp.Rmd")
knit("Table1.Rmd")
knit("Figures.Rmd")
knit("SupInfo.Rmd")
knit("MasterDoc.Rmd")

# Convert to Docx
system('pandoc --reference-docx=mytemplate.docx WriteUp.md -s -o WriteUp.docx --bibliography VITpaper.bib --csl ecology.csl')
system('pandoc --reference-docx=mytemplate.docx Table1.md  -s -o Table1.docx')
system('pandoc --reference-docx=mytemplate.docx Figures.md -s -o Figures.docx')
system('pandoc --reference-docx=mytemplate.docx SupInfo.md -s -o SupInfo.docx')
system('pandoc --reference-docx=mytemplate.docx MasterDoc.md  -s -o MasterDoc.docx --bibliography VITpaper.bib --csl ecology.csl')

# Convert to pdf
system('pandoc --template pcctemplate.latex WriteUp.md -s -o WriteUp.pdf --bibliography VITpaper.bib --csl ecology.csl')
system('pandoc --template pcctemplate.latex Table1.md -s -o Table1.pdf')
system('pandoc --template pcctemplate.latex Figures.md -s -o Figures.pdf')
system('pandoc --template pcctemplate.latex SupInfo.md -s -o SupInfo.pdf')
system('pandoc --template pcctemplate.latex MasterDoc.md  -s -o MasterDoc.pdf --bibliography VITpaper.bib --csl ecology.csl')
