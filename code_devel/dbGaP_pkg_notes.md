# Email excerpts from Stephanie

Sarah - Quenna, Cathy, and I had a pre-meeting yesterday (while you were giving your lecture) and came up with the idea that we should have an R package to automate both generating the dbGaP files and checking files submitted by studies. Since you are now an accomplished R package author, this would be a great thing to do as you undertake the project of dbGaP file submission. We could host the package on github/UW-GAC and share it with the dbGaP folks; maybe it will help them streamline their process. (Not cross-checking DDs with data files until late in the curation seems very inefficient to me.)

---

Hi Sarah,

Here are some ideas for the dbgap R package to file away for when you start working on it:

1. the dbgap file prep functions in QCpipeline could move to this new package
2. use the readxl package to avoid converting excel files to text ahead of time
3. the function dbTopmed:::.readTraitFile is very useful for reading the submitted text files, as it deals gracefully with many poorly-formatted files. could make a version of this function in our package.

This script might be a useful example:
/projects/topmed/qc/freeze.5/analysts/sdmorris/R/read_dbgap_files.R

Stephanie

---

## my questions on above emails

1. in QCpipeline we have dbgapScanAnnotation and dbgapSnpAnnotation - these are for molecular data file types. would the point be just to keep all "dbgap file prep" into one package?
	- _nevermind we decided these functions should remain separate_
3. where is dbTopmed library?
	- nm - found it, /projects/topmed/working_code/phenotype_harmonization/library/
	- this library is not public, so i should copy the dbTopmed:::.readTraitFile into this new pkg - may not need editing
	- can ask Adrienne about this function - it was made for streamlining TOPMed pheno db

# thoughts on scope of work
1. how generic vs tailored to TOP Med should this be?

## what the package should check and create
- sample-subject mapping (ssm)
- subject consent (subj)
- sample attributes (satt)
- pedigree and phenotype
	- limit to checking for expected subjects
	- should we also check for expected column names

## types of functions
- find inputs 
	- don't try to make functions for this, unless it's clear how to make it generic and useful
		- we don't want to make any assumptions (e.g., column names, contents, etc)
	- this will be analyst's responsibility - to go into downloaded EA snapshot and identify the corresponding file names
- read inputs 
- check inputs
	- see checklists already compiled in our dbGaP SOP
	- including cross-check DD and DS
	- check formatting
	- check column names
	- check presence of expected samples
		- user can provide df of sample ID and subject ID (extracted from master scan annot, for TOPMed)
		- ok if there are more records in dbgap files (e.g., for quarantined samples) - function should report out on this
		- master scan annot should have final word on samples to include
			- __no.post__ samples: should not be anywhere in dbgap files (note the corresponding subject id may be in files)
			- __quarantine__ samples: generally don't worry about these samples - they may be released with future freezes, so preference is for quarantine=T samples to remain in SI's dbgap files
			we don't want to exclude them from dbgap files now
				- but preparing analyst should not include them in the dataframe of expected samples and subjs for the given release
	- check correspondence between sample id and subject id
	- sample attributes file - will have several TOPmed specific values
		- most studies will likely be lacking these cols altogether
		- whether these cols will be populated for samples not in the current release will vary case by case. if SI has info, they can populate it; otherwise ok to leave as blank
		- master scan annot will have study, top med proj, sequencing center
		- need to ask Quenna how to systematically get phase and funding info
			- looks like Phase info is on website: https://www.nhlbiwgs.org/group/project-studies
			- one place to get sequencing center x phase info: Quenna's tracking reports on website
	- check between files - perhaps as a series of pairwise checks?
		- i.e., subject consent <> SSM
		- pedigree <> subject consent
		- etc.
- write outputs (DS and DD)
	- TBD how much can be "functionalized" here
	- one idea: take reports output by checking functions and feed them back into revising functions

## conventions
looks like GAC is heavy into dplyr, so use where possible

## test data
best way to develop package will be to start working through dbgap file prep for a study that's ready for freeze 5 release
- CHS
	- there is 1 quarantined sample

## action items from 1/12/18 discussion with Stephanie
1. ask Anne Sturke for 
	- current version of the dbGaP submission guide (she's been updating)
		- she told Quenna she was working on
		- _done_
	- for list of all checks that dbGaP runs on these files, as we want to try and cover these as well in our checks
		- Adrienne knows of article that write this out?
2. set up meeting with Quenna and Stephanie where we download Quenna's knowledge about how to determine phase and funding source to add to sample attributes
	- answer might be to add a column to the main tracking sheet - though these may be sample-level variables rather than study-level
	- need for "phs encyclopedia" across sample qc and pheno harm - Adrienne may have skeleton
	- there is master NWD ID list of Cathy's indicating sequencing batch - that may be tied to funding source (and phase?)
	- _Updates_
		- phase info is available on TOPMed website
		    * __turns out this is not up to date__ 
		- Stephanie is going to make study-level tracking worksheet on Team Drive - will include a column that Quenna can populate to indicate if a study is all TOPMed funding; when that's not the case, will need to go to Quenna to try and puzzle it out. __Not straightforward__ 

# package names?
- wickham says all lower case (convention)
- no underscores (requirement)
- ideas
	- "dbGaPfileprep", dbgapfileprep
	- "dbGaPprep", dbgapprep; dbgaprep
	- "dbGaPrepr, dbgapprep; dbgaprepr
	- dbgapfilemakr
	- dbgapmakr (Ben's)
	- prepdbgap
	- dbgapsubmitr (b/c it's for submitters to dbGaP)
	- dbgaphelpr (pronounced "dbgaphelper" by Adrienne)
	- dbgaputils
	- **dbgaptools**
- https://www.njtierney.com/post/2017/10/27/change-pkg-name/
 
 
# Ongoing questions 
when ready for github - can I use UW-GAC for my remote repo?

 


