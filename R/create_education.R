# create_education -------------------------------------------------------------
create_education <- function(course, institution, year)
{
  enclose_in_empty_strings(
    '[[education.courses]]',
    sprintf('  course = "%s"', course),
    sprintf('  institution = "%s"', institution),
    sprintf('  year = "%s"', year)
  )
}
