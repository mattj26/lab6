# Assignment specific setup
ASN := lab6
VOCASN := LAB6

# A list of all the file stems for OCaml files that have both
# scaffold and solution versions
SOURCES := lab6.ml cS51.ml
TESTED := lab6
# Files to be distributed to students in the work directory
DISTRIB := cS51.ml lab6.ml _tags makefile
SUBMISSION := lab6 cS51

# General rules
include ../../makefile
