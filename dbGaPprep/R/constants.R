# Borrowed from dbTopmed

# Regular expressions for extracting dbgap accession numbers from commonly-used strings
REGEX_DATASET_ACCESSION <- "^pht(\\d{6}).v(\\d+)$"
REGEX_STUDY_ACCESSION <- "^phs(\\d{6}).v(\\d+)$"
REGEX_VARIABLE_ACCESSION <- "^phv(\\d{8}).v(\\d+)$"

# Regular expression matching the dbgap string used to indicate that a file is blank, sadly uncommented
REGEX_BLANK_DATA_FILE <- "This file is intentionally blank because this data table does not include subjects for the (.+?) consent group." # nolint

# allowed choices for user-specified harmonized data_type values
DATA_TYPE_CHOICES <- c("encoded", "string", "decimal", "integer")


### not sure we want this
# forbidden regex in harmonize function
FORBIDDEN_IN_HARMONIZE_FUNCTION <- c("getDb", "dbWrite", "dbLoad", "dbAdd", "dbGet", "source\\(",
                                     "suppressWarnings\\(")

# hosts allowed to write to the production database -- only the servers
HOSTS_ALLOWED_TO_WRITE_TO_PRODUCTION <- c("^neyman$", "^pearson\\d+?.local$", "^fisher$",
                                          "^p\\d+?.local$", "^pearson[01]$")

HARMONIZED_DATASET_NAME_REGEX <- "^[a-zA-Z][a-zA-Z_]{1,}[a-zA-Z]$"

TIMEZONE <- "America/Los_Angeles"
