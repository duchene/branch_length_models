# This function extracts some of the main results from an IQtree run output file.

readIQ <- function(iqfile){
       allout <- readLines(iqfile)
       
       res <- list(stats = vector())
       
       likse <- grep("Log-likelihood of the tree:", allout, value = T)
       res$stats["lik"] <- gsub(".*:|[(].*| ", "", likse)
       res$stats["likSE"] <- gsub(".*[(]|[)].*|.* ", "", likse)

       params <- grep("Number of free parameters", allout, value  = T)
       res$stats["Nparams"] <- gsub(".* ", "", params)
       
       unclik <- grep("Unconstrained log-likelihood", allout, value = T) 
       res$stats["uncLik"] <- gsub(".* ", "", unclik)
       
       aic <- grep("Akaike information criterion", allout, value = T)
       res$stats["aic"] <- gsub(".* ", "", aic[2])
       res$stats["aicc"] <- gsub(".* ", "", aic[3])
       
       bic <- grep("Bayesian information criterion", allout, value = T)
       res$stats["bic"] <- gsub(".* ", "", bic[2])
       
       trlen <- grep("Total tree length", allout, value = T)
       res$stats["trlen"] <- gsub(".* ", "", trlen)
       
       stemmy <- grep("Sum of internal branch lengths", allout, value = T)
       res$stats["stemmy"] <- as.numeric(gsub(".*[(]|[%].*", "", stemmy)) / 100
       
       res$tree <- read.tree(text = allout[grep("Tree in newick format:", allout)[1] + 2])
       
       boots <- as.numeric(res$tree$node.label)
       res$stats["bootmean"] <- mean(boots, na.rm = T)
       
       bootrange <- range(boots, na.rm = T)
       res$stats["bootmax"] <- max(bootrange)
       res$stats["bootmin"] <- min(bootrange)
       res$stats["bootrangesize"] <- diff(bootrange)
       
       statnames <- names(res$stats)
       res$stats <- as.numeric(res$stats)
       names(res$stats) <- statnames

       return(res)
}