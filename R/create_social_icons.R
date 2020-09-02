# create_social_icon -----------------------------------------------------------
create_social_icon <- function(icon, icon_pack, link)
{
  enclose_in_empty_strings(
    '[[social]]',
    sprintf('  icon = "%s"', icon),
    sprintf('  icon_pack = "%s"', icon_pack),
    sprintf('  link = "%s"', link)
  )
}

# create_social_telephone ------------------------------------------------------
create_social_telephone <- function(phonenumber)
{
  create_social_icon(
    icon = "phone",
    icon_pack = "fas",
    link = sprintf("tel:%s", phonenumber)
  )
}

# create_social_email ----------------------------------------------------------
create_social_email <- function(email)
{
  create_social_icon(
    icon = "envelope",
    icon_pack = "fas",
    link = sprintf("mailto:%s", email)
  )
}

# create_social_twitter --------------------------------------------------------
create_social_twitter <- function(twitter_name)
{
  create_social_icon(
    icon = "twitter",
    icon_pack = "fab",
    link = sprintf("https://twitter.com/%s", twitter_name)
  )
}

#create_social_orcid -----------------------------------------------------------
create_social_orcid <- function(orcid)
{
  create_social_icon(
    icon = "orcid",
    icon_pack = "ai",
    link = sprintf("https://orcid.org/%s", orcid)
  )
}

# create_social_github ---------------------------------------------------------
create_social_github <- function(github_name)
{
  create_social_icon(
    icon = "github",
    icon_pack = "fab",
    link = sprintf("https://github.com/%s", github_name)
  )
}

# create_social_linkedin -------------------------------------------------------
create_social_linkedin <- function(linkedin_name)
{
  create_social_icon(
    icon = "linkedin",
    icon_pack = "fab",
    link = sprintf("https://linkedin.com/in/%s", linkedin_name)
  )
}

# create_social_researchgate ---------------------------------------------------
create_social_researchgate <- function(researchgate_name)
{
  create_social_icon(
    icon = "researchgate",
    icon_pack = "ai",
    link = sprintf("https://researchgate.net/profile/%s", researchgate_name)
  )
}

# create_social_googlescholar --------------------------------------------------
create_social_googlescholar <- function(googlescholar_link)
{
  create_social_icon(
    icon = "google-scholar",
    icon_pack = "ai",
    link = googlescholar_link
  )
}

# create_social_personalwebsite ------------------------------------------------
create_social_personalwebsite <- function(link)
{
  create_social_icon(
    icon = "home",
    icon_pack = "fas",
    link = link
  )
}
