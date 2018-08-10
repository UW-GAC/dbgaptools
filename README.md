# dbgaptools

This package assists in creating dataset (DS) and data dictionary (DD) files required for dbGaP submission: sample-subject mapping, sample attributes, and subject consent files. Checks proceed based on [dbGaP Submission Guidelines](https://www.ncbi.nlm.nih.gov/projects/gap/cgi-bin/GetPdf.cgi?document_name=HowToSubmit.pdf). Additionally, pedigree and phenotype files can be checked for presence of expected subjects.

## Installation

You can install dbgaptools from github with:

```r
# install.packages("devtools")
devtools::install_github("UW-GAC/dbgaptools")
```

## Types of functions

1. Check functions
	* There are functions to check each dbGaP file type --- DS and, optionally, accompanying DD files
	* See `help(check_<ftype>)` where <ftype> includes
		* sattr: for sample attributes
		* ssm: for sample-subject mapping
		* subj: for subject consent
		* pheno: for phenotype 
		* ped: for pedigree
	* See also `help(check_cross_file)` to check consistency across DS files
1. Read functions
	* See `help(read_ds_file)` and `help(read_dd_file)`
1. Write functions
	* Conveniens functions to write out DS and DD files
	* See `help(write_dbgap)`

