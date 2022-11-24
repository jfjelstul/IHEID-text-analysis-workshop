# European Society of International Law (ESIL)
# Graduate Institute of International and Development Studies (IHEID)
# Social Science Methods for Legal Scholars
# Quantitative Text Analysis
# Joshua C. Fjelstul, Ph.D.

# Packages
library(tidyverse)
library(lubridate)

# Example text -----------------------------------------------------------------

# CJEU judgment ECLI:EU:C:2012:756
# https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX%3A62012CJ0370

# The line of the judgment that indicates the case number
case_text <- "In Case C-370/12,"

# The line of the judgment that indicates the legal procedure
procedure_text <- "REFERENCE for a preliminary ruling under Article 267 TFEU from the Supreme Court (Ireland), made by decision of 31 July 2012, received at the Court on 3 August 2012, in the proceedings"

# The line of the judgment that lists the judges on the panel
judges_text <- "composed of V. Skouris, President, K. Lenaerts (Rapporteur), Vice-President, A. Tizzano, R. Silva de Lapuerta, M. Ilešič, L. Bay Larsen, T. von Danwitz, A. Rosas, G. Arestis, J. Malenovský, M. Berger and E. Jarašiūnas, Presidents of Chambers, E. Juhász, A. Borg Barthet, U. Lõhmus, E. Levits, A. Ó Caoimh, J.-C. Bonichot, A. Arabadjiev, C. Toader, J.-J. Kasel, M. Safjan, D. Šváby, A. Prechal, C.G. Fernlund, J.L. da Cruz Vilaça and C. Vajda, Judges,"

# Using the stringr package ----------------------------------------------------

## Detecting text --------------------------------------------------------------

# str_detect()
# str_count()

# Does the text mention a treaty article?
procedure_text |>
  str_detect("Article [0-9]+") # Match article numbers

# Does the text contain a date?
procedure_text |>
  str_detect("[0-9]{1,2} [A-Z][a-z]+ [0-9]{4}") |> # Matches dates
  as.numeric() # Convert to a dummy variable

# How many dates does the text contain?
procedure_text |>
  str_count("[0-9]{1,2} [A-Z][a-z]+ [0-9]{4}") # Match dates

## Extracting text -------------------------------------------------------------

# str_extract()
# str_extract_all()

# Extract the case number
case_text |>
  str_extract("[0-9]+") |> # Extract the first number in the string
  as.numeric() # Convert the string to an integer

# Extract the year of the case
case_text |>
  str_extract("C-[0-9]+/[0-9]+") |> # Extract the case number
  str_extract("[0-9]+$") |> # Extract the last number in the case number
  as.numeric() # Convert the string to an intenger

# Extract the first date in the text
procedure_text |>
  str_extract("[0-9]{1,2} [A-Z][a-z]+ [0-9]{4}") |> # Extract the first date
  dmy() # Convert the date to a YYYY-MM-DD format

# Extract all dates in the text
procedure_text |>
  str_extract_all("[0-9]{1,2} [A-Z][a-z]+ [0-9]{4}") |> # This returns a list of length 1
  flatten() |> # Flatten the list to a vector
  dmy() # Convert the date to a YYYY-MM-DD format

# Extract the name of the judge-rapporteur (the judge who writes the judgment)
judges_text |>
  str_extract("[A-Z][a-z]+ \\(Rapporteur\\)") |> # Match the name of the judge-rapporteur
  str_extract("[A-Z][a-z]+") # Extract the name

# Extract the legal procedure
procedure_text |>
  str_extract("^.*?Article [0-9]+")

## Replacing text --------------------------------------------------------------

# str_replace()
# str_replace_all()

# Change the format of dates from DD Month YYYY to Month DD YYYY
procedure_text |>
  str_replace_all("([0-9]{1,2}) ([A-Z][a-z]+) ([0-9]{4})", "\\2 \\1 \\3") # Change the order of the day and month

# Add the full year to the case number
case_text |>
  str_replace("([0-9]+),", "20\\1,")

## Removing text ---------------------------------------------------------------

# str_remove()
# str_remove_all()

# Extract the year of the case
case_text |>
  str_remove(",") |> # Remove the comma at the end
  str_extract("[0-9]+$") |> # Extract the last number in the case number
  as.numeric() # Convert the string to an intenger

# Extract the name of the judge-rapporteur (the judge who writes the judgment)
judges_text |>
  str_extract("[A-Z][a-z]+ \\(Rapporteur\\)") |> # Match the name of the judge-rapporteur
  str_remove("\\(Rapporteur\\)") |> # Remove the text in parentheses
  str_squish() # Remove extra white space

# Examples ---------------------------------------------------------------------

## Example 1: Extracting the case number ---------------------------------------

# Extract the case number
str_extract(case_text, "C-[0-9]+/[0-9]+")

# Some example case numbers
case_numbers <- c("Case C-101/22", "Case T-105/22")

# We can use the "or" operator
str_extract(case_numbers, "(C|T)-[0-9]+/[0-9]+")

# Or we can use a character class
str_extract(case_numbers, "[CT]-[0-9]+/[0-9]+")

## Example 2: Extracting the legal procedure -----------------------------------

# Extraction: match the text we're interested in direct
# Reduction: remove the text around what we're interested in
# Both: extract text that includes what we're interested in, remove anchors

# Extract the legal procedure
procedure_text |>
  str_extract("^[A-Z]+.*?under") |> # Extract the relevant part of the string
  str_remove("under") |> # Remove the anchor text at the end
  str_squish() |> # Remove white space
  str_to_sentence() # Fix capitalization

## Example 3: Extract the names of the judges ----------------------------------

# Make a list of judge names
judges <- judges_text |>
  str_remove("composed of") |>
  str_remove(", Judges,") |>
  str_replace("\\(Rapporteur\\)", " ") |>
  str_replace_all("(Vice-)?President(s of Chambers)?,", " ") |>
  str_replace_all(" [A-Z]\\. | [A-Z]\\.[A-Z]\\. | [A-Z]\\.-[A-Z]\\.", " ") |> # Remove initials
  str_replace_all(" and ", ", ") |> # Convert " and " to ", "
  str_replace_all(" *, *", ", ") |> # Remove extra spaces in front of commas
  str_squish() # Remove extra white space

# Split the list of judge names into a vector
judges <- judges |>
  str_split(",") |> # Split the string at each comma
  flatten() |> # This returns a list, so we have to flatten it
  str_squish() # Remove extra white space

# Check the output
judges
