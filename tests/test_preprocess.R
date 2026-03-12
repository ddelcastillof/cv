library(testthat)
# Set working directory to project root so data/ and bib/ paths resolve
if (basename(getwd()) == "tests") setwd("..")
source("preprocess.R")

# ── na_to_empty ───────────────────────────────────────────────────────────────
test_that("na_to_empty converts NA to empty string", {
  expect_equal(na_to_empty(NA), "")
  expect_equal(na_to_empty(NA_character_), "")
  expect_equal(na_to_empty(NULL), "")
  expect_equal(na_to_empty("value"), "value")
  expect_equal(na_to_empty(""), "")
})

# ── format_author_vancouver ───────────────────────────────────────────────────
test_that("format_author_vancouver converts Last, First to Last F", {
  expect_equal(format_author_vancouver("Lapidaire, Winok"), "Lapidaire W")
  expect_equal(format_author_vancouver("Del Castillo-Fernández, Darwin"), "Del Castillo-Fernández D")
  expect_equal(format_author_vancouver("Del Castillo, Darwin"), "Del Castillo D")
  expect_equal(format_author_vancouver("Miranda, J Jaime"), "Miranda JJ")
})

# ── format_authors_vancouver ──────────────────────────────────────────────────
test_that("format_authors_vancouver joins authors and truncates at 6", {
  six <- c("Author1, A", "Author2, B", "Author3, C",
           "Author4, D", "Author5, E", "Author6, F")
  expect_equal(
    format_authors_vancouver(six),
    "Author1 A, Author2 B, Author3 C, Author4 D, Author5 E, Author6 F"
  )
  seven <- c(six, "Author7, G")
  expect_true(grepl("et al", format_authors_vancouver(seven)))
  expect_false(grepl("Author7", format_authors_vancouver(seven)))
})

# ── write_front_matter ────────────────────────────────────────────────────────
test_that("write_front_matter produces valid YAML front matter block", {
  contact <- list(
    name = "Test Name, MD",
    email = "test@example.com",
    linkedin = "test-linkedin",
    github = "testgithub",
    orcid = "0000-0000-0000-0000"
  )
  result <- write_front_matter(contact)
  expect_true(grepl("^---", result))
  expect_true(grepl("documentclass: scrartcl", result))
  expect_true(grepl("fontsize: 9pt", result))
  expect_true(grepl("margin=0.5in", result))
  expect_true(grepl("---$", trimws(result)))
})

# ── render_contact_header ─────────────────────────────────────────────────────
test_that("render_contact_header produces raw LaTeX block with FontAwesome icons", {
  contact <- yaml::read_yaml("data/contact.yaml")
  result  <- render_contact_header(contact)
  expect_true(any(grepl("```\\{=latex\\}", result)))
  expect_true(any(grepl("\\\\faLinkedin", result)))
  expect_true(any(grepl("\\\\faEnvelope", result)))
  expect_true(any(grepl("\\\\faGithub", result)))
  expect_true(any(grepl(contact$name, result)))
})

# ── render_education ──────────────────────────────────────────────────────────
test_that("render_education produces a raw LaTeX longtable block", {
  edu_data <- list(
    list(
      degree = "Master of Public Health (MPH)",
      institution = "University of Washington",
      location = "Seattle -- USA",
      start = "Sep 2023",
      end = "Jun 2025"
    )
  )
  result <- render_education(edu_data, additional = list())
  block  <- paste(result, collapse = "\n")
  expect_true(grepl("\\\\section\\{Education\\}", block))
  expect_true(grepl("longtable", block))
  expect_true(grepl("Master of Public Health", block))
  expect_true(grepl("Sep 2023", block))
})

# ── render_experience ─────────────────────────────────────────────────────────
test_that("render_experience includes bullets as itemize environment", {
  exp_data <- list(
    list(
      title = "Research Assistant",
      institution = "Test University",
      location = "Seattle - USA",
      start = "Jan 2024",
      end = "Current",
      bullets = list("Did research.", "Wrote papers.")
    )
  )
  result <- render_experience(exp_data)
  block  <- paste(result, collapse = "\n")
  expect_true(grepl("\\\\section\\{Professional Experience\\}", block))
  expect_true(grepl("\\\\begin\\{itemize\\}", block))
  expect_true(grepl("Did research\\.", block))
})

test_that("render_experience inserts newpage when new_page_before is true", {
  exp_data <- list(
    list(
      title = "Job 1",
      institution = "Inst 1",
      location = "City",
      start = "2020",
      end = "2022",
      bullets = list("Bullet.")
    ),
    list(
      title = "Job 2",
      institution = "Inst 2",
      location = "City",
      start = "2022",
      end = "Current",
      new_page_before = TRUE,
      bullets = list("Bullet.")
    )
  )
  result <- render_experience(exp_data)
  block  <- paste(result, collapse = "\n")
  expect_true(grepl("\\\\newpage", block))
})

# ── render_skills ─────────────────────────────────────────────────────────────
test_that("render_skills produces section with labeled lines", {
  skills_data <- list(
    intro = "Broad experience.",
    programming = "R, Python",
    languages = "English (fluent)"
  )
  result <- render_skills(skills_data)
  block  <- paste(result, collapse = "\n")
  expect_true(grepl("\\\\section\\{Skills\\}", block))
  expect_true(grepl("R, Python", block))
  expect_true(grepl("\\\\textbf\\{Programming languages:\\}", block))
})

# ── render_awards ─────────────────────────────────────────────────────────────
test_that("render_awards produces two-column longtable", {
  data <- list(list(name = "Test Award", institution = "Test Inst",
                    location = "USA", year = "2024"))
  result <- paste(render_awards(data), collapse = "\n")
  expect_true(grepl("\\\\section\\{Academic Honours and Awards\\}", result))
  expect_true(grepl("Test Award", result))
  expect_true(grepl("2024", result))
})

# ── render_teaching ───────────────────────────────────────────────────────────
test_that("render_teaching produces subsection with role and course", {
  data <- list(list(
    role = "Lecturer", institution = "Univ", department = "School",
    location = "Lima", course = "Epidemiology",
    description = "Course description.",
    start = "Mar 2025", end = "Current"
  ))
  result <- paste(render_teaching(data), collapse = "\n")
  expect_true(grepl("\\\\subsection\\{Teaching and Lecturer Contributions\\}", result))
  expect_true(grepl("Lecturer", result))
  expect_true(grepl("Epidemiology", result))
})

# ── render_reviews ────────────────────────────────────────────────────────────
test_that("render_reviews produces a pipe table with two columns", {
  data <- list(
    list(category = "Peer Reviewer", event = "Journal A"),
    list(category = "Conference Reviewer", event = "Conf B")
  )
  result <- paste(render_reviews(data), collapse = "\n")
  expect_true(grepl("\\|:---\\|", result))
  expect_true(grepl("Peer Reviewer", result))
  expect_true(grepl("Journal A", result))
})

# ── render_memberships ────────────────────────────────────────────────────────
test_that("render_memberships produces itemize list", {
  data <- list(
    list(organization = "Society A", start = "2020", end = "Current")
  )
  result <- paste(render_memberships(data), collapse = "\n")
  expect_true(grepl("\\\\section\\{Memberships", result))
  expect_true(grepl("Society A", result))
  expect_true(grepl("2020", result))
})

# ── render_certifications ─────────────────────────────────────────────────────
test_that("render_certifications produces two-column longtable", {
  data <- list(list(name = "CITI Program", year = "2023"))
  result <- paste(render_certifications(data), collapse = "\n")
  expect_true(grepl("\\\\section\\{Licensure and Certification\\}", result))
  expect_true(grepl("CITI Program", result))
  expect_true(grepl("2023", result))
})

# ── format_pub_entry ──────────────────────────────────────────────────────────
test_that("format_pub_entry formats a bib row in Vancouver style", {
  mock_row <- list(
    AUTHOR   = list(c("Del Castillo-Fernández, Darwin", "Brañez-Condorena, Ana")),
    YEAR     = 2021,
    TITLE    = "Test title",
    JOURNAL  = "Test Journal",
    VOLUME   = "81",
    NUMBER   = "4",
    PAGES    = NA,
    DOI      = "10.1234/test"
  )
  result <- format_pub_entry(mock_row)
  expect_true(grepl("Del Castillo-Fernández D", result))
  expect_true(grepl("Brañez-Condorena A", result))
  expect_true(grepl("2021;81\\(4\\)", result))
  expect_true(grepl("doi:10\\.1234/test", result))
  expect_true(grepl("\\*Test Journal\\*", result))
})

# ── render_publications ───────────────────────────────────────────────────────
test_that("render_publications groups entries by category", {
  bib <- bib2df::bib2df("bib/references.bib")
  cats <- yaml::read_yaml("data/pub_categories.yaml")
  result <- paste(render_publications(bib, cats), collapse = "\n")
  expect_true(grepl("\\\\section\\{Publications", result))
  expect_true(grepl("Peer-Reviewed Publications", result))
})
