## This directory contains scripts and data files used to create the phylogenies in this paper

#### Processing workflow

- blast of NCBI genbank with galbut virus RdRp sequence as a query. E-value cutoff 1e-30
- name this file: partitivirus_sequences_RdRp_blast_1e-30.fasta
- add our sequences to these: `cat partitivirus_sequences_RdRp_blast_1e-30.fasta our_galbut_seqs_for_first_paper.fa >> partitivirus_sequences_RdRp_blast_1e-30.fasta`.  
- rename sequences with informative name, collapse with cd-hit, run mafft and trimal: `./process_seqs partitivirus_sequences_RdRp_blast_1e-30.fasta`.  Output of this is: `partitivirus_sequences_RdRp_blast_1e-30_renamed_cdhit95_mafft_trimal.fasta`
- run modeltest-ng to identify best model: `./run_modeltest partitivirus_sequences_RdRp_blast_1e-30_renamed_cdhit95_mafft_trimal.fasta`
- run raxml-ng to infer tree: `./run_raxml partitivirus_sequences_RdRp_blast_1e-30_renamed_cdhit95_mafft_trimal.fasta`
- use ggtree to draw tree, R script = `arthro_partiti_tree.R`
- use Affinity Designer to finalize tree figure.
