# create_education -------------------------------------------------------------
create_education <- function(course, institution, year)
{
  c(
    '',
    '[[education.courses]]',
    sprintf('  course = "%s"', course),
    sprintf('  institution = "%s"', institution),
    sprintf('  year = "%s"', year),
    ''
  )
}
