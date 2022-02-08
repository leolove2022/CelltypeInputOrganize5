
#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
CelltypeInputOrganize5 = function(Tissue_whole_peptide, celltype2, Modalities, address_celltype_FC_P_0){
#Tissue_whole_peptide<-read.xlsx( address_tissue_whole_peptide, rowNames = F,colNames = T)
#celltype2<-c("Alb", "MCK", "Adipoq","Lyzm","Cdh16","LCK", "CD2","Nr5a1","Myh6","Sftpc","Myh11","Syn1","Nes","Vil1","Gcg", "Cdh5","Pdgfrb", "Col1a1","Pdgfra","Pdx1","UCP1")
  #address_celltype_FC_P_0="~/Documents/Experiment/WeiWei exercise peptide proteomic/"  ### save individual table
  #Modalities<-c("Sed","Run") 
  ColumnName <- names(Tissue_whole_peptide)
  #ColumnName <- names(Peptide_FC_P)
celltype<-celltype2
#for (m in 1:length(celltype2)) {# this section to test whether the name is correst
# celltype_test_name<-celltype2[m]
# celltype_one_list=c()
# for (n in 1:length(ColumnName)){
#   celltype_subtable_name<-paste0("*",celltype_test_name,"*")
#   if(grepl(celltype_test_name,ColumnName[n])){
#    print(ColumnName[n])
#     
#   }
# }
######### after get df4 the subset data of cell type calculate the result, use the list to add column
#}# this section to test whether the name is correst
# this section to test use more celltype 
celltypepvalue<-paste0(celltype,"_p") # getthe new column name for p
celltypeFC<-paste0(celltype,"_FC")# getthe new column name for FC
### judge whether it is the cell ype
### judge whether it is the cell ype
#Modalities<-c("Sed","Run")
# Here to add the individual abudance together with FC P

for (m in 1:length(celltype)) {# this section to test use more celltype 
  
  
  celltype_test_name<-celltype[m]
  celltype_one_list=c()
  for (n in 1:length(ColumnName)){
    
    
    celltype_subtable_name<-paste0("*",celltype_test_name,"*")
    if(grepl(celltype_test_name,ColumnName[n])){
      celltype_one_list<-append(celltype_one_list,n)
    }
    
    
  }
  df4<-data.frame(Tissue_whole_peptide[,celltype_one_list]) # here is the raw data
  ######### after get df4 the subset data of cell type calculate the result, use the list to add column
  #df_Alb[,celltypepvalue[m]]<-Pvaluecaculate(df4)
  #df_Alb[,celltypeFC[m]]<-FCcaculate(df4)
  
  
  qureytable<-data.frame(Protein.Id=Tissue_whole_peptide$Protein.Id, Gene.Symbol=Tissue_whole_peptide$Protein.Name, PeptideSeq=Tissue_whole_peptide$Sequence,Tissue_whole_peptide[,celltype_one_list],Plasma_FC=FCcaculate(df4),Plasma_p=Pvaluecaculate(df4))
  #address_celltype_FC_P=paste0(address_celltype_FC_P_0,celltype[m],"_FC_P.csv"  )
  ##############
  address_proteoform=paste0(address_celltype_FC_P_0,celltype[m],"_FC_P_proteoform_4.csv"  )
  address_proteoform_list=paste0(address_celltype_FC_P_0,celltype[m],"_FC_P_proteoform_list_4.csv"  )
  Result_peptide<-ProteoformMap_10(qureytable)
  Result_peptide_list=Result_peptide[!duplicated(Result_peptide$Protein.Id),]
  #address_proteoform=("~/Documents/Experiment/WeiWei exercise peptide proteomic/Alb_FC_P_proteoform_4.csv")
  #address_proteoform_list=("~/Documents/Experiment/WeiWei exercise peptide proteomic/Alb_FC_P_proteoform_list_4.csv")
  write.csv(Result_peptide,file=address_proteoform,row.names=FALSE)
  write.csv(Result_peptide_list,file=address_proteoform_list,row.names=FALSE)
  print(qureytable[1,'Gene.Symbol'])
  ##############
  #write.csv(qureytable,file=address_celltype_FC_P,row.names=FALSE)
}
}
