options(rstudio.markdownToHTML =
          fuction(imputFile,outputFile){
            system(paste('pandoc -s -S --webtex --toc -t slidy --self-contained --slide-level 2',shQuote(inputFile),"-o",shQuote(outputFile)'))
          }
        
        
        )