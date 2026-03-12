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
