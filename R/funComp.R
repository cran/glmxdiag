funComp <- function() {
  funcs <- c('avplot', 'cookDist', 'devAnalysis', 'DFbeta', 
             'influenceDiag', 'leverage', 'linkLin', 'Qresiduals', 
             'varCheck', 'variableOut')


    mods <- c('  glm  ', ' quasi ',  'neg.bin', 'betabin', ' beta  ')
    tbl <- c(rep('X', 3), rep(' ', 2), # avplot
             rep('X', 5), # cookDist
             c('X', ' ', 'X', rep(' ', 2)), # devAnalysis
             rep('X', 5), # DFbeta
             rep('X', 5), # influenceDiag
             rep('X', 5), # leverage
             rep('X', 5), # linkLin
           c('X', ' ', rep('X', 3)), # Qresiduals
           c(rep('X', 3), rep(' ', 2)), # varCheck
           c('X', ' ', rep('X', 3)) # variableOut
            )
    mtx <- matrix(tbl, nrow = length(funcs), ncol = length(mods), 
                  byrow = T)
    df.out <- data.frame(mtx)
    colnames(df.out) <- mods
    rownames(df.out) <- funcs
    name.width <- max(sapply(names(df.out), nchar))
    format(df.out, width = name.width, justify = "centre") 
}
