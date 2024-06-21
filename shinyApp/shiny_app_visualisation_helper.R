CF_PPI_network.lcc.node_type.nodes <- 
  read.table(file = "CF_network_kegg_diff_pathways_with_CFTR_interactors_direct_tagged_nodes_df.txt",
             sep = "\t",
             header = T,
             check.names = F)

test_2 <- read.table(file = "/Users/matthieu/ownCloud/Thèse/Systems Biology/Meta-analysis article/CFnetwork/data/kegg_diff_pathways_network/diff_kegg_pathways_with_CFTR_interactors_PPI_direct_tagged_nodes_df.txt",
                     sep = "\t",
                     header = T,
                     check.names = F)



# Add logFC from protemics studies Rauniyar et al, 2014: https://doi.org/10.1021/pr500370g

Rauniyar_proteomics <-   read.table(file = "Rauniyar_3140_common_proteins_spectrum_count.txt",
                                   sep = "\t",
                                   header = T,
                                   check.names = F)

test <- merge(CF_PPI_network.lcc.node_type.nodes,
              Rauiyar_proteomics[,c("Gene Symbol", "log2(CFBE_NSAF_avg/HBE_NSAF_avg)")],
              by.x = "Symbol",
              by.y = "Gene Symbol",
              all.x = T)
test <- test[which(!duplicated(test$Symbol)),]
