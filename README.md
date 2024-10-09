# Biodiversity of mudflat intertidal viromes along the Chinese coasts
### Code availability
The R code for generating figures and performing data analysis can be found in the corresponding directory (Fig.1 to Fig.5).

The raw data for generating the figures and performing data analysis with the R code can be found in the corresponding subfolder.

### Data availability
The outputs generated by this study are stored in the directory named "full_output". For more details, please download the file and locate the required file in the subfolder. The detailed description of the output files is as follows:

##### 1. AMGs_align_output 
The alignment results of viral proteins with various functional gene databases, including eggNOG (COG function), NcycDB (nitrogen metabolism), McycDB (methane metabolism), PcycDB (phosphorus metabolism), and ScycDB (sulphur metabolism), are available. In addition, the normalized abundances of viral protein clusters (vPCs) and virus-encoded auxiliary metabolic genes (vAMGs) can also be found in this subfolder.
##### 2. amoC-pmoC_output
This subfolder includes the NCycDB alignment outputs used for differentiating amoC and pmoC genes, as well as the phylogenetic tree of amoC and pmoC genes constructed using the maximum likelihood method by IQ-TREE. The raw protein sequences used for inferring phylogeny and the tree visualized by iToL are also placed in this subfolder.
##### 3. checkv_output
The full outputs of checkv v1.0.1 ('end_to_end' mode), including the completeness, contamination, and quality summary of intertidal viruses (20,102 vOTUs). All viral genomes used in this study have been evaluated for contamination and removed accordingly using checkv v1.0.1.
##### 4. genomad_output
The full outputs of genomad v1.7.4 (score > 0.7), including the taxonomic assignment and marker gene annotation of intertidal viruses.
##### 5. iphop_output
The full outputs of iphop v1.3.2 (false ratio < 10%), including the host prediction of intertidal viruses.
##### 6. kegg-decoder_output
This subfolder includes the metabolic pathway annotations of microbial operational taxonomic units (mOTUs) belonging to Deltaproteobacteria, Thermodesulfobacteria, and Thaumarchaeota conducted by KEGG-Decoder module. Of these, the list files indicate the completeness of each metabolic pathway involved for each mOTU.
##### 7. viralRefseq_align_output
The identification of viral-like genes within intertidal viruses based on the viral RefSeq. 
##### 8. virsorter2_viralrecall_output
This subfolder includes the virsorter2 ('NCLDV' mode) and viralrecall outputs of nucleocytoplasmic large DNA viruses (NCLDVs) and virsorter2 ('lavidaviridae' mode) output of virophages identified in this study.

### Note
vOTU, mOTU, vPC sequences information can be obtained from https://zenodo.org/records/10827260. 

For any other code/data inquiries, please open a github issue or contact me: jimengzhi@mail.sdu.edu.cn.

If these codes are useful, please cite: Ji, M. et al. Biodiversity of mudflat intertidal viromes along the Chinese coasts. Nat Commun 15, 8611 (2024). https://doi.org/10.1038/s41467-024-52996-x
