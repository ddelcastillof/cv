# preprocess.R
# CV preprocessing script: reads data/*.yaml + bib/references.bib → build/cv.md

library(yaml)
library(dplyr)
library(stringr)
library(bib2df)

# ── Helper functions ──────────────────────────────────────────────────────────

na_to_empty <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) "" else as.character(x)
}

format_author_vancouver <- function(author_str) {
  parts <- strsplit(author_str, ", ", fixed = TRUE)[[1]]
  if (length(parts) == 1) return(trimws(parts[1]))
  last    <- trimws(parts[1])
  given   <- trimws(parts[2])
  words   <- strsplit(given, "\\s+")[[1]]
  initials <- paste(substr(words, 1, 1), collapse = "")
  paste0(last, " ", initials)
}

format_authors_vancouver <- function(authors, max_authors = 6) {
  n <- length(authors)
  formatted <- sapply(authors, format_author_vancouver)
  if (n <= max_authors) {
    paste(formatted, collapse = ", ")
  } else {
    paste0(paste(formatted[1:max_authors], collapse = ", "), ", et al")
  }
}

write_front_matter <- function(contact) {
  lines <- c(
    "---",
    'title: ""',
    paste0("documentclass: scrartcl"),
    paste0("papersize: letter"),
    paste0("fontsize: 9pt"),
    "geometry:",
    "  - margin=0.5in",
    "  - heightrounded",
    "  - includefoot",
    "  - footskip=0.5in",
    "colorlinks: true",
    "---",
    ""
  )
  paste(lines, collapse = "\n")
}

# ── Section renderers ─────────────────────────────────────────────────────────

raw_latex <- function(lines) {
  c("", "```{=latex}", lines, "```", "")
}

render_contact_header <- function(contact) {
  raw_latex(c(
    "\\begin{center}",
    paste0("\\huge{\\underline{\\textbf{", contact$name, "}}} \\\\"),
    "\\vspace{2.5pt}",
    "\\begin{minipage}[c]{0.25\\textwidth}",
    paste0("\\faLinkedin \\centering\\href{", contact$linkedin_url, "}{\\hspace{0.5em}", contact$linkedin, "}"),
    "\\end{minipage}",
    "\\begin{minipage}[c]{0.25\\textwidth}",
    paste0("\\faEnvelope \\centering\\href{mailto:", contact$email, "}{\\hspace{0.5em}", contact$email, "}"),
    "\\end{minipage}",
    "\\begin{minipage}[c]{0.20\\textwidth}",
    paste0("\\faGithub \\centering\\href{", contact$github_url, "}{\\hspace{0.5em}", contact$github, "}"),
    "\\end{minipage}",
    "\\vspace{-2.5pt}",
    "\\end{center}"
  ))
}

render_edu_entry <- function(e) {
  c(
    paste0("\\textbf{", e$degree, "} & \\textbf{", e$start, " -- ", e$end, "} \\\\"),
    paste0("\\textsc{", e$institution, "} & \\textsc{", e$location, "} \\\\"),
    "& \\\\"
  )
}

render_education <- function(education, additional = list()) {
  entries <- unlist(lapply(education, render_edu_entry), recursive = FALSE)

  edu_block <- raw_latex(c(
    "\\section{Education}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{p{14cm}>{\\raggedleft\\arraybackslash}p{4cm}}",
    entries,
    "\\end{longtable}"
  ))

  if (length(additional) == 0) return(edu_block)

  add_entries <- unlist(lapply(additional, render_edu_entry), recursive = FALSE)

  add_block <- raw_latex(c(
    "\\section{Additional Education}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{p{14cm}>{\\raggedleft\\arraybackslash}p{4cm}}",
    add_entries,
    "\\end{longtable}"
  ))

  c(edu_block, add_block)
}

render_exp_entry <- function(e) {
  header <- c(
    paste0("\\textbf{", e$title, "} & \\textbf{", e$start, " -- ", e$end, "} \\\\"),
    paste0("\\textsc{", e$institution, "} & \\textsc{", e$location, "} \\\\")
  )
  bullets_block <- if (!is.null(e$bullets) && length(e$bullets) > 0) {
    c(
      "\\multicolumn{2}{p{18cm}}{%",
      "\\begin{itemize}",
      paste0("  \\item ", unlist(e$bullets)),
      "\\end{itemize}",
      "} \\\\",
      "\\\\"
    )
  } else character(0)

  page_break <- if (isTRUE(e$new_page_before)) "\\newpage" else character(0)
  c(page_break, header, bullets_block)
}

render_experience <- function(experience) {
  entries <- unlist(lapply(experience, render_exp_entry), recursive = FALSE)
  raw_latex(c(
    "\\section{Professional Experience}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{p{14cm}>{\\raggedleft\\arraybackslash}p{4cm}}",
    entries,
    "\\end{longtable}"
  ))
}

render_skills <- function(skills) {
  label_map <- list(
    programming  = "Programming languages",
    cloud        = "Cloud Computing",
    tools        = "Tools",
    packages     = "Packages",
    languages    = "Languages",
    competencies = "Core Competencies"
  )
  lines <- c()
  for (key in names(label_map)) {
    val <- skills[[key]]
    if (!is.null(val) && nchar(val) > 0) {
      lines <- c(lines, paste0("\\textbf{", label_map[[key]], ":} ", val, " \\\\"))
    }
  }
  raw_latex(c(
    "\\section{Skills}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    if (!is.null(skills$intro)) paste0(skills$intro, "\\vspace{1pt}"),
    lines
  ))
}

render_awards <- function(awards) {
  entries <- lapply(awards, function(a) {
    c(
      paste0(a$name, ", \\textsc{", a$institution, "} -- ", a$location,
             " & \\textbf{", a$year, "} \\\\"),
      "& \\\\"
    )
  })
  raw_latex(c(
    "\\section{Academic Honours and Awards}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{>{\\raggedright\\arraybackslash}p{16.5cm} p{1.5cm}}",
    unlist(entries),
    "\\end{longtable}"
  ))
}

render_teaching_entry <- function(e) {
  page_break <- if (isTRUE(e$new_page_before)) "\\newpage" else character(0)
  c(
    page_break,
    paste0("\\textbf{", e$role, "} & \\textbf{", e$start, " -- ", e$end, "} \\\\"),
    paste0("\\textsc{", e$institution, "} & \\textsc{", e$location, "} \\\\"),
    paste0("\\textsc{", e$course, ":} ", e$description, " & \\\\")
  )
}

render_teaching <- function(teaching) {
  entries <- unlist(lapply(teaching, render_teaching_entry), recursive = FALSE)
  raw_latex(c(
    "\\subsection{Teaching and Lecturer Contributions}",
    "\\begin{longtable}{>{\\raggedright\\arraybackslash}p{14cm}>{\\raggedleft\\arraybackslash}p{4cm}}",
    entries,
    "\\end{longtable}"
  ))
}

render_reviews <- function(reviews) {
  rows <- sapply(reviews, function(r) {
    paste0("| ", r$category, " | ", r$event, " |")
  })
  c(
    "",
    "\\subsection{Peer-Review Contributions}",
    "",
    "| | |",
    "|:---|:---|",
    rows,
    ""
  )
}

render_memberships <- function(memberships) {
  items <- sapply(memberships, function(m) {
    paste0(" \\item ", m$organization, " (", m$start, " - ", m$end, ")")
  })
  raw_latex(c(
    "\\section{Memberships in Professional Organizations}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{itemize}",
    items,
    "\\end{itemize}"
  ))
}

render_certifications <- function(certs) {
  entries <- lapply(certs, function(cert) {
    paste0(cert$name, " & \\textbf{", cert$year, "} \\\\")
  })
  raw_latex(c(
    "\\section{Licensure and Certification}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{>{\\raggedright\\arraybackslash}p{17cm} p{1cm}}",
    unlist(entries),
    "\\end{longtable}"
  ))
}

format_pub_entry <- function(row) {
  authors  <- format_authors_vancouver(unlist(row$AUTHOR))
  year     <- na_to_empty(row$YEAR)
  title    <- na_to_empty(row$TITLE)
  journal  <- na_to_empty(row$JOURNAL)
  volume   <- na_to_empty(row$VOLUME)
  number   <- na_to_empty(row$NUMBER)
  pages    <- na_to_empty(row$PAGES)
  doi      <- na_to_empty(row$DOI)

  vol_str <- if (nchar(volume) > 0) {
    v <- if (nchar(number) > 0) paste0(volume, "(", number, ")") else volume
    if (nchar(pages) > 0) paste0(v, ":", pages) else v
  } else ""

  journal_part <- if (nchar(journal) > 0) paste0("*", journal, "*") else ""
  year_vol     <- if (nchar(year) > 0 && nchar(vol_str) > 0) {
    paste0(". ", year, ";", vol_str)
  } else if (nchar(year) > 0) {
    paste0(". ", year)
  } else ""

  doi_part <- if (nchar(doi) > 0) paste0(" doi:", doi, ".") else "."

  paste0(authors, ". ", title, ". ", journal_part, year_vol, doi_part)
}

render_publications <- function(bib_df, pub_categories) {
  orcid_line <- raw_latex(c(
    "\\section{Publications \\hspace{1cm} ORCID: 0000-0002-8609-0312}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}"
  ))

  section_labels <- list(
    peer_reviewed = "Peer-Reviewed Publications",
    conference    = "Conference Abstracts",
    thesis        = "Thesis and Dissertations",
    white_paper   = "White Papers"
  )

  pub_blocks <- list()
  for (cat in names(section_labels)) {
    keys <- pub_categories[[cat]]
    if (is.null(keys) || length(keys) == 0) next

    entries <- lapply(keys, function(key) {
      row <- bib_df[bib_df$BIBTEXKEY == key, ]
      if (nrow(row) == 0) {
        warning("Cite key not found in references.bib: ", key)
        return(NULL)
      }
      format_pub_entry(as.list(row[1, ]))
    })
    entries <- Filter(Negate(is.null), entries)
    if (length(entries) == 0) next

    subsection_header <- raw_latex(paste0("\\subsection{", section_labels[[cat]], "}"))
    items <- paste0(seq_along(entries), ". ", unlist(entries))
    pub_blocks[[cat]] <- c(subsection_header, "", items, "")
  }

  c(orcid_line, unlist(pub_blocks))
}

# ── Main assembly ─────────────────────────────────────────────────────────────

main <- function() {
  dir.create("build", showWarnings = FALSE)

  # Read all data sources
  contact       <- yaml::read_yaml("data/contact.yaml")
  education_raw <- yaml::read_yaml("data/education.yaml")
  experience    <- yaml::read_yaml("data/experience.yaml")
  awards        <- yaml::read_yaml("data/awards.yaml")
  teaching      <- yaml::read_yaml("data/teaching.yaml")
  reviews_data  <- yaml::read_yaml("data/reviews.yaml")
  memberships   <- yaml::read_yaml("data/memberships.yaml")
  skills_data   <- yaml::read_yaml("data/skills.yaml")
  certs         <- yaml::read_yaml("data/certifications.yaml")
  pub_cats      <- yaml::read_yaml("data/pub_categories.yaml")
  bib_df        <- bib2df::bib2df("bib/references.bib")

  # education.yaml uses primary: / additional: named keys (valid YAML structure)
  education      <- education_raw$primary
  additional_edu <- education_raw$additional
  if (is.null(additional_edu)) additional_edu <- list()

  # Advising and grants (may be empty/NULL — only commented schema in files)
  advising <- tryCatch(yaml::read_yaml("data/advising.yaml"), error = function(e) list())
  grants   <- tryCatch(yaml::read_yaml("data/grants.yaml"),   error = function(e) list())
  if (is.null(advising)) advising <- list()
  if (is.null(grants))   grants   <- list()

  # Assemble sections in CV order
  sections <- list(
    write_front_matter(contact),
    render_contact_header(contact),
    render_education(education, additional_edu),
    render_experience(experience),
    render_skills(skills_data),
    render_awards(awards),
    render_research_teaching(grants, teaching, advising, reviews_data),
    render_publications(bib_df, pub_cats),
    render_certifications(certs),
    render_memberships(memberships)
  )

  writeLines(unlist(sections), "build/cv.md")
  message("build/cv.md written successfully.")
}

render_research_teaching <- function(grants, teaching, advising, reviews_data) {
  header <- raw_latex(c(
    "\\section{Research and Teaching}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}"
  ))

  grants_block   <- if (length(grants) > 0) render_grants(grants) else character(0)
  teaching_block <- if (length(teaching) > 0) render_teaching(teaching) else character(0)
  advising_block <- if (length(advising) > 0) render_advising(advising) else character(0)
  reviews_block  <- if (length(reviews_data) > 0) render_reviews(reviews_data) else character(0)

  c(header, grants_block, teaching_block, advising_block, reviews_block)
}

render_grants <- function(grants) {
  if (length(grants) == 0) return(character(0))
  subsection <- raw_latex("\\subsection{Research Support}")
  entries <- lapply(grants, function(g) {
    investigators <- paste(
      Filter(function(x) nchar(na_to_empty(x)) > 0,
             list(g$investigators$pi, g$investigators$coi_1, g$investigators$coi_2)),
      collapse = ", "
    )
    funder <- paste(
      Filter(function(x) nchar(na_to_empty(x)) > 0,
             list(na_to_empty(g$funder_2), na_to_empty(g$funder_1))),
      collapse = ", "
    )
    budget_str <- if (na_to_empty(g$budget_currency) == "USD") {
      paste0("$", na_to_empty(g$budget))
    } else na_to_empty(g$budget)

    c(
      paste0("**Title:** ", na_to_empty(g$title)),
      paste0("**Funder:** ", funder),
      paste0("**Project Code:** ", na_to_empty(g$project_code)),
      paste0("**Investigators:** ", investigators),
      paste0("**Period:** ", na_to_empty(g$start), " -- ", na_to_empty(g$end)),
      paste0("**Budget:** ", budget_str),
      paste0("**Role:** ", na_to_empty(g$role)),
      ""
    )
  })
  c(subsection, unlist(entries))
}

render_advising <- function(advising) {
  if (length(advising) == 0) return(character(0))
  ug <- Filter(function(a) na_to_empty(a$level) == "undergraduate", advising)
  items <- sapply(ug, function(a) {
    date_str <- paste0("(", na_to_empty(a$start), " -- ", na_to_empty(a$end), ")")
    name_title <- if (is.null(a$thesis_title) || is.na(a$thesis_title)) {
      a$name
    } else {
      paste0(a$name, ', "', a$thesis_title, '"')
    }
    paste0(name_title, ", \\textsc{", a$institution, "}, ", a$country, " ", date_str)
  })

  c(
    "",
    "\\subsection{Advising}",
    "",
    "\\subsubsection{Undergraduate Students}",
    "",
    if (length(items) > 0) paste0("- ", items) else "*None currently.*",
    ""
  )
}

# ── Entry point ───────────────────────────────────────────────────────────────
# Run main() only when script is executed directly (not sourced from tests)
if (sys.nframe() == 0) main()
