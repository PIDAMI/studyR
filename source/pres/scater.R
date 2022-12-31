library(scater)
example <- addPerCellQC(example,                             
  subsets=list(Mito=grep("mt-", rownames(example))))
plotColData(example, x = "sum", 
            y="detected", colour_by="tissue")
plotExpression(example, rownames(example)[1:6],
               x = "level1class", colour_by="tissue")
example <- runPCA(example, 
                  name="PCA2",
                  subset_row=rownames(example)[1:1000],
                  ncomponents=25)
plotReducedDim(example, dimred = "PCA", colour_by = "level1class")