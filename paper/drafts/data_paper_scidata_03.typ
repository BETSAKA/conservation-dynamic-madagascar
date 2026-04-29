// Simple numbering for non-book documents
#let equation-numbering = "(1)"
#let callout-numbering = "1"
#let subfloat-numbering(n-super, subfloat-idx) = {
  numbering("1a", n-super, subfloat-idx)
}

// Theorem configuration for theorion
// Simple numbering for non-book documents (no heading inheritance)
#let theorem-inherited-levels = 0

// Theorem numbering format (can be overridden by extensions for appendix support)
// This function returns the numbering pattern to use
#let theorem-numbering(loc) = "1.1"

// Default theorem render function
#let theorem-render(prefix: none, title: "", full-title: auto, body) = {
  if full-title != "" and full-title != auto and full-title != none {
    strong[#full-title.]
    h(0.5em)
  }
  body
}
// Some definitions presupposed by pandoc's typst output.
#let content-to-string(content) = {
  if content.has("text") {
    content.text
  } else if content.has("children") {
    content.children.map(content-to-string).join("")
  } else if content.has("body") {
    content-to-string(content.body)
  } else if content == [ ] {
    " "
  }
}

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms.item: it => block(breakable: false)[
  #text(weight: "bold")[#it.term]
  #block(inset: (left: 1.5em, top: -0.4em))[#it.description]
]

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let fields = old_block.fields()
  let _ = fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => {
          let subfloat-idx = quartosubfloatcounter.get().first() + 1
          subfloat-numbering(n-super, subfloat-idx)
        })
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => block({
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          })

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let children = old_title_block.body.body.children
  let old_title = if children.len() == 1 {
    children.at(0)  // no icon: title at index 0
  } else {
    children.at(1)  // with icon: title at index 1
  }

  // TODO use custom separator if available
  // Use the figure's counter display which handles chapter-based numbering
  // (when numbering is a function that includes the heading counter)
  let callout_num = it.counter.display(it.numbering)
  let new_title = if empty(old_title) {
    [#kind #callout_num]
  } else {
    [#kind #callout_num: #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block,
    block_with_new_content(
      old_title_block.body,
      if children.len() == 1 {
        new_title  // no icon: just the title
      } else {
        children.at(0) + new_title  // with icon: preserve icon block + new title
      }))

  align(left, block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1)))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color,
        width: 100%,
        inset: 8pt)[#if icon != none [#text(icon_color, weight: 900)[#icon] ]#title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}


// syntax highlighting functions from skylighting:
/* Function definitions for syntax highlighting generated by skylighting: */
#let EndLine() = raw("\n")
#let Skylighting(fill: none, number: false, start: 1, sourcelines) = {
   let blocks = []
   let lnum = start - 1
   let bgcolor = rgb("#f1f3f5")
   for ln in sourcelines {
     if number {
       lnum = lnum + 1
       blocks = blocks + box(width: if start + sourcelines.len() > 999 { 30pt } else { 24pt }, text(fill: rgb("#aaaaaa"), [ #lnum ]))
     }
     blocks = blocks + ln + EndLine()
   }
   block(fill: bgcolor, width: 100%, inset: 8pt, radius: 2pt, blocks)
}
#let AlertTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let AnnotationTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let AttributeTok(s) = text(fill: rgb("#657422"),raw(s))
#let BaseNTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let BuiltInTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let CharTok(s) = text(fill: rgb("#20794d"),raw(s))
#let CommentTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let CommentVarTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))
#let ConstantTok(s) = text(fill: rgb("#8f5902"),raw(s))
#let ControlFlowTok(s) = text(weight: "bold",fill: rgb("#003b4f"),raw(s))
#let DataTypeTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let DecValTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let DocumentationTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))
#let ErrorTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let ExtensionTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let FloatTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let FunctionTok(s) = text(fill: rgb("#4758ab"),raw(s))
#let ImportTok(s) = text(fill: rgb("#00769e"),raw(s))
#let InformationTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let KeywordTok(s) = text(weight: "bold",fill: rgb("#003b4f"),raw(s))
#let NormalTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let OperatorTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let OtherTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let PreprocessorTok(s) = text(fill: rgb("#ad0000"),raw(s))
#let RegionMarkerTok(s) = text(fill: rgb("#003b4f"),raw(s))
#let SpecialCharTok(s) = text(fill: rgb("#5e5e5e"),raw(s))
#let SpecialStringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let StringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let VariableTok(s) = text(fill: rgb("#111111"),raw(s))
#let VerbatimStringTok(s) = text(fill: rgb("#20794d"),raw(s))
#let WarningTok(s) = text(style: "italic",fill: rgb("#5e5e5e"),raw(s))



#let article(
  title: none,
  subtitle: none,
  authors: none,
  keywords: (),
  date: none,
  abstract-title: none,
  abstract: none,
  thanks: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: none,
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: none,
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  mathfont: none,
  codefont: none,
  linestretch: 1,
  sectionnumbering: none,
  linkcolor: none,
  citecolor: none,
  filecolor: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  // Set document metadata for PDF accessibility
  set document(title: title, keywords: keywords)
  set document(
    author: authors.map(author => content-to-string(author.name)).join(", ", last: " & "),
  ) if authors != none and authors != ()
  set par(
    justify: true,
    leading: linestretch * 0.65em
  )
  set text(lang: lang,
           region: region,
           size: fontsize)
  set text(font: font) if font != none
  show math.equation: set text(font: mathfont) if mathfont != none
  show raw: set text(font: codefont) if codefont != none

  set heading(numbering: sectionnumbering)

  show link: set text(fill: rgb(content-to-string(linkcolor))) if linkcolor != none
  show ref: set text(fill: rgb(content-to-string(citecolor))) if citecolor != none
  show link: this => {
    if filecolor != none and type(this.dest) == label {
      text(this, fill: rgb(content-to-string(filecolor)))
    } else {
      text(this)
    }
   }

  let has-title-block = title != none or (authors != none and authors != ()) or date != none or abstract != none
  if has-title-block {
    place(
      top,
      float: true,
      scope: "parent",
      clearance: 4mm,
      block(below: 1em, width: 100%)[

        #if title != none {
          align(center, block(inset: 2em)[
            #set par(leading: heading-line-height) if heading-line-height != none
            #set text(font: heading-family) if heading-family != none
            #set text(weight: heading-weight)
            #set text(style: heading-style) if heading-style != "normal"
            #set text(fill: heading-color) if heading-color != black

            #text(size: title-size)[#title #if thanks != none {
              footnote(thanks, numbering: "*")
              counter(footnote).update(n => n - 1)
            }]
            #(if subtitle != none {
              parbreak()
              text(size: subtitle-size)[#subtitle]
            })
          ])
        }

        #if authors != none and authors != () {
          let count = authors.len()
          let ncols = calc.min(count, 3)
          grid(
            columns: (1fr,) * ncols,
            row-gutter: 1.5em,
            ..authors.map(author =>
                align(center)[
                  #author.name \
                  #author.affiliation \
                  #author.email
                ]
            )
          )
        }

        #if date != none {
          align(center)[#block(inset: 1em)[
            #date
          ]]
        }

        #if abstract != none {
          block(inset: 2em)[
          #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
          ]
        }
      ]
    )
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  doc
}

#set table(
  inset: 6pt,
  stroke: none
)
#let brand-color = (:)
#let brand-color-background = (:)
#let brand-logo = (:)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
  columns: 1,
)

#show: doc => article(
  title: [A replicable temporal extension of protected area geodata applied to 123 sites in Madagascar],
  authors: (
    ( name: [Florent Bédécarrats],
      affiliation: [Institut de Recherche pour le Développement (IRD), UMI-SOURCE],
      email: [florent.bedecarrats\@ird.fr] ),
    ( name: [Seheno Ramanantsoa],
      affiliation: [Ministère de l'Environnement et du Développement Durable (MEDD)],
      email: [] ),
    ( name: [Ollier D. Andrianambinina],
      affiliation: [Madagascar National Parks (MNP)],
      email: [] ),
    ),
  date: [2026-04-29],
  abstract: [Georeferenced data on protected areas underpin much of the research studying the impacts of nature conservation on the environment and human wellbeing. Yet, existing databases typically record only the current state of each site. When boundaries or designations change, previous configurations are silently overwritten, preventing reliable temporal analyses and introducing bias in impact evaluations. We present a replicable temporal data model that extends the World Database on Protected Areas with time-bounded states documenting boundary modifications, status reclassifications, governance changes, temporary protections, and internal zoning. The model is designed to be generalizable to any national context and extensible to the full diversity of protected area modifications. We demonstrate it on Madagascar's 123 terrestrial protected areas over 2000--2025, integrating historical archive snapshots, successive national system datasets, legal texts from the national legislative database, and expert literature. All amendments are stored in a version-controlled YAML registry and consolidated through deterministic rules implemented in R. The resulting dataset is distributed in GeoPackage and GeoParquet formats, enabling spatio-temporal analyses of conservation dynamics.

],
  abstract-title: "Abstract",
  toc: true,
  toc_title: [Table of contents],
  toc_depth: 3,
  doc,
)

= Background & Summary
<background-summary>
Geospatial data on protected areas occupy a central place in research assessing conservation outcomes on the environment and human wellbeing. Whether studies assess the effect of protection on deforestation, measure the economic returns of ecotourism, or monitor progress toward international conservation targets, they depend on accurate spatial boundaries and status information. The World Database on Protected Areas (WDPA), maintained by the UN Environment Programme World Conservation Monitoring Centre, serves this role as the most comprehensive global inventory, documenting over 270,000 sites across more than 240 countries and territories @bingham2019b. Over the past two decades, the WDPA has become the default spatial reference for conservation planning, effectiveness assessments, and reporting against targets such as Aichi Biodiversity Target 11 and, more recently, the Kunming-Montreal Global Biodiversity Framework goal to protect 30% of the planet by 2030 @Maxwell2020.

The WDPA, however, was designed as a snapshot database. It records the current state of each protected area, and when boundaries or designations change, the previous configuration is overwritten. The WDPA user manual itself warns against using the STATUS\_YR field for historical analysis, and degazetted sites are simply removed from the database @Bingham2019a. This structural limitation has been recognized in the literature on protected area downgrading, downsizing, and degazettement (PADDD). Symes et al. (#cite(<symesWhyWeLose2016>, form: "year")) examined the factors driving PADDD events in the tropics and subtropics, showing that protected areas are not permanent --- they can be reduced, reclassified, or revoked entirely. #cite(<lewisDynamicsGlobalProtectedarea2019>, form: "prose") quantified the net area flux in the global protected area estate from 2004 to 2016 by comparing successive WDPA snapshots, finding that an average of 1.1 million km² was removed from the database annually across 223 countries and territories. Their analysis, however, could not distinguish on-the-ground changes from data-quality corrections, and the snapshot-comparison method itself erases the intermediate states it seeks to document.

What remains largely unaddressed is the continuous evolution of ongoing protected areas. Between creation and the present, many sites undergo boundary modifications, status reclassifications (for instance from Strict Nature Reserve to National Park), governance changes, internal zoning, or temporary protection decrees. None of these transformations are preserved in the WDPA's current-state model: each update silently replaces the previous configuration, making it impossible to reconstruct what a protected area looked like at any given date in the past.

A further dimension that static snapshot databases fail to capture is the internal zoning of protected areas. In many national systems, multi-use protected area categories --- typically corresponding to IUCN Categories III to VI --- are internally subdivided into zones with distinct levels of permitted human activity. In Madagascar, the Protected Area Management Code (Code de Gestion des Aires Protégées, COAP; Law 2015-005) requires such sites to delineate a strictly protected core zone (noyau dur), where extractive uses are prohibited and biodiversity conservation takes legal precedence, alongside internal buffer zones where regulated resource use by surrounding communities may be permitted. The term “buffer zone” is used in the literature to refer both to areas surrounding core zones within protected areas and to areas surrounding the external boundary of protected areas @ebregt2000. To avoid ambiguity, we distinguish throughout this manuscript and dataset between “internal buffer zones” and “external buffer zones.” The temporal emergence of these zones is a critical feature of conservation trajectories: a protected area's external boundary is typically established by an initial creation decree, while its core zone may be delimited and formalized either in that same decree or in the site's management and development plan, together with the other zoning categories, at a later stage. Impact evaluations relying solely on the external boundary may therefore misattribute outcomes to the protected area as a whole, whereas the effective protection signal on land use, biodiversity, and community access operates principally at the core zone level.

This matters for researchers from all disciplines that study conservation (biology, ecology, economics, anthropology, etc.). For instance, when impact evaluations assume that current boundaries have remained unchanged since a protected area was established, they may treat recently added areas as if they had been protected for much longer, inducing a downward biais in their results. It is also relevant for policymakers, as protected areas have become a major instrument of spatial planning, with global terrestrial coverage exceeding 17% in 2023 @unep2024. It also matters for conservation practitioners and local stakeholders, because shared spatial references support more transparent and equitable management.

Madagascar offers a particularly instructive demonstration case. The island is one of the world's foremost biodiversity hotspots, with forests sheltering over 80% of its biodiversity and endemism rates exceeding 80% across many taxonomic groups @Waeber2020. Its conservation history spans colonial-era strict nature reserves, post-independence national parks, and a rapid expansion following the 2003 Durban Vision, in which the government committed to tripling the protected surface to six million hectares @Waeber2020. This complex trajectory has been meticulously documented by Goodman et al. (#cite(<Goodman2018>, form: "year")) in a three-volume monograph covering 98 terrestrial sites, while the national protected area system (Système des Aires Protégées de Madagascar, SAPM) has produced successive spatial datasets, and the national legislative database (CNLEGIS) contains the legal decisions underpinning each status change. Yet no single dataset has assembled these complementary sources into a unified temporal record.

The problem, however, is not specific to Madagascar. In most countries, the WDPA contains protected areas whose histories of boundary and status changes are lost. The temporal data model we present here is designed to be generalizable to any country where complementary sources can supplement the WDPA baseline.

This paper presents three contributions. First, a temporal data model that extends the WDPA with time-bounded states, validity periods, legal source references, and secondary zoning, applicable to any national context. Second, a demonstration dataset covering 123 terrestrial protected areas in Madagascar from 2000 to 2025. Third, an open collaborative infrastructure consisting of a YAML amendment registry and a deterministic consolidation engine that enables other researchers to audit, correct, and extend the dataset through standard version control workflows. An interactive companion website allows visual exploration and verification of the data.

= Methods
<methods>
The dataset was constructed through a multi-step pipeline integrating four complementary data sources, cross-referencing them through spatial and textual matching, curating amendments through expert review, and consolidating them into temporal states using deterministic rules. Access to institutional datasets and validation processes was facilitated through close collaboration with national authorities, ensuring consistency with official records and operational practices.

The complete methodology is documented in a reproducible notebook accompanying this paper (see supplementary material). We summarize the key steps below and illustrate the overall workflow in #ref(<fig-workflow>, supplement: [Figure]).

#figure([
#box(image("../figures/Data_preparation_workflow.drawio.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Workflow diagram of the data processing pipeline
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-workflow>


== Input data
<input-data>
Four categories of input data were assembled: national data from the Madagascar Protected Area System (SAPM), historical snapshots of the WDPA, legal decisions from the national legislative database (CNLEGIS), and expert literature providing historical context and boundary information. Eight successive spatial datasets from SAPM were harmonized:

- ANGAP 2002: From the then-only protected area management agency (Agence nationale de gestion des aires protégées)
- MNP 2010: dataset produced by Madagascar National Parks (MNP), the name adopted by ANGAP (Agence Nationale pour la Gestion des Aires Protégées) in 2008 as part of an institutional and strategic reorientation aimed in particular at strengthening its long-term financial sustainability, in a context marked by the emergence of new actors --- notably national and international conservation NGOs --- in the co-management of newly established protected areas under the Durban Vision expansion
- SAPM evolution series (2001--2011): from the coordination body that gathered the conservation actors
- Vahatra 2017: a complementary source curated in the framework of the Goodman et al. (#cite(<Goodman2018>, form: "year")) monograph, which provides curated boundaries and status of 98 terrestrial protected areas up to date in 2015;
- SAPM 2017 and 2024 : dataset produced by Madagascar National Parks (MNP), the name adopted by ANGAP (Agence Nationale pour la Gestion des Aires Protégées) in 2008 as part of an institutional and strategic reorientation aimed in particular at strengthening its long-term financial sustainability, in a context marked by the emergence of new actors --- notably national and international conservation NGOs --- in the co-management of newly established protected areas under the Durban Vision expansionMadagascar is the new name of ANGAP, at a time where new management agencies emerged (international or local NGOs) • SAPM evolution series (2001--2011): from the coordination body that gathered the conservation actors • Vahatra 2017: a complementary source curated in the framework of the Goodman et al.~(2018) monograph, which provides curated boundaries and status of 98 terrestrial protected areas up to date in 2015; • SAPM 2017 and 2024: Updated datasets coordinated by the Ministry of Environment and Sustainable Development (MEDD), which has ensured the overall governance and coordination of the protected area system, while working with multiple management entities including Madagascar National Parks and conservation NGOs
- Complementary sources identified through an archive searching and manual curation by MNP.

Each dataset was reprojected, geometries were repaired, and attributes were standardized to a common schema.

The WDPA historical archive provided 15 annual snapshots (2010--2024) obtained from the AWS Open Data registry and Google Earth Engine monthly archives, serving primarily to establish the baseline attributes and geometries for each protected area and to track schema evolution across versions. The Madagascar subset was extracted and reprojected to the Laborde projection (EPSG:29702), and Blake3 hashing was used to deduplicate identical geometries across snapshots.

Approximately 200 legal decisions (decrees, ministerial orders, interministerial orders) related to protected areas were extracted from the Centre National de Légistique (CNLEGIS), Madagascar's official legislative database. An automated pipeline combining web scraping, regular expression classification by decision type, and fuzzy string matching (Levenshtein distance) linked each decision to a WDPAID. This automated processing was complemented by systematic manual reading of legal texts to extract boundary descriptions, effective dates, and status changes not captured by the automated methods.

Additional historical context was drawn from the three-volume monograph by Goodman et al. (#cite(<Goodman2018>, form: "year")), which provides creation dates, historical boundaries, and status changes for 98 terrestrial protected areas, and from Waeber et al. (#cite(<Waeber2020>, form: "year")).

== Consolidation and verification
<consolidation-and-verification>
Records from all four sources were matched to WDPA identifiers through a combination of spatial matching (centroid distance and polygon overlap ratios) and string-distance matching on protected area names. This step resolved name variants and multi-polygon entities.

A Shiny web application was developed and deployed for domain expert review. domain expert review. The validation involved protected area managers, GIS specialists from MEDD and Madagascar National Parks, and researchers with long-term experience on Madagascar's protected area system. The application overlays geometries from different sources and time periods on interactive Leaflet maps, with filtering by IUCN category, dataset of origin, and associated legal texts. This tool enabled systematic visual comparison of boundaries reported by different sources for each protected area. The companion website provides ongoing access to this verification interface.

Core zone shapefiles from five organizations (MEDD, MNP, MBG, GERP, WWF) were collected and validated against WDPA external boundaries through spatial containment checks, yielding 73 validated core zone geometries that were integrated as secondary zones in the temporal dataset.

For approximately 25 protected areas with complex histories, a detailed case-by-case review was conducted, cross-referencing legal decisions, historical boundaries from Goodman et al. (#cite(<Goodman2018>, form: "year")), and successive SAPM datasets. #ref(<fig-examples>, supplement: [Figure]) illustrates three such cases. Amendments were documented in Spatial Amendment Tables (SAT) for geometries and Feature Attribute Tables (FAT) for attributes, then exported to the canonical YAML format.

#figure([
#box(image("../figures/fig_examples.png", width: 100.0%))
], caption: figure.caption(
position: bottom, 
[
Temporal reconstruction for three protected areas
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-examples>


== YAML amendment registry
<yaml-amendment-registry>
All corrections and temporal information are stored in a registry of YAML files, one per protected area, accompanied by GeoJSON geometry files where boundaries differ from the WDPA baseline. YAML was chosen because it is human-readable without specialized software, auditable in that each amendment includes the legal source, effective date, and amendment type, and Git-friendly in that text-based diffs enable collaborative editing with full change history tracking. Automated validation checks detect duplicate entries, temporal overlaps, and missing geometries.

The registry documents four types of amendments: boundary modifications (legally documented changes to external limits), status changes (reclassifications such as from Strict Nature Reserve to National Park), corrections (errors in the WDPA regarding dates, statuses, or geometries), and secondary zoning (internal zones including core zones and buffer zones).

== Consolidation and Auditing Rules
<consolidation-and-auditing-rules>
The final temporal states are generated by a deterministic consolidation engine implemented in R, referred to as the Consolidation and Auditing Rules (CAR). The engine reads the WDPA baseline and the YAML amendment registry, applies explicit precedence rules, and produces the output dataset. The process follows a delta-only model in which amendments override specific WDPA attributes or geometries for defined time intervals while all other attributes are inherited from the baseline. Three core functions implement the pipeline: #NormalTok("generate_timeline()"); identifies temporal breakpoints from all amendments for a given WDPAID, #NormalTok("find_active_amendments()"); determines which amendments are active at each time point, and #NormalTok("consolidate_pa_states()"); merges baseline attributes with active amendments to produce each temporal state. Because the rules are coded explicitly and the inputs are versioned, the entire transformation is reproducible and auditable.

= Data Records
<data-records>
The dataset is deposited on DataSuds, the institutional research data repository of the Institut de Recherche pour le Développement (IRD), which assigns a persistent DOI. The source code is hosted on GitHub at #link("https://github.com/BETSAKA/conservation-dynamic-madagascar") and archived via Software Heritage for long-term preservation and persistent identification.

The dataset is distributed in three complementary forms. The first is a GeoPackage file (#NormalTok("dynamic_wdpa_madagascar.gpkg");), an OGC standard format directly compatible with QGIS, ArcGIS, R, and Python. The second is a GeoParquet file (#NormalTok("dynamic_wdpa_madagascar.parquet");), a columnar geospatial format optimized for analytical workflows and now also supported by QGIS and ArcGIS in addition to Arrow-based tools in R and Python. The third is the YAML amendment registry (#NormalTok("amendments/");), containing per-protected-area YAML files documenting each amendment with its legal source, effective date, and type, accompanied by GeoJSON geometry files where boundaries differ from the WDPA baseline.

Each row in the output dataset represents one zone in one temporal state. A protected area with boundary changes and internal zoning will have multiple rows across time periods. #ref(<tbl-schema>, supplement: [Table]) describes the fields.

#figure([
#table(
  columns: (33.33%, 33.33%, 33.33%),
  align: (auto,auto,auto,),
  table.header([Field], [Type], [Description],),
  table.hline(),
  [#NormalTok("state_id");], [Character], [Unique identifier for each temporal state],
  [#NormalTok("WDPAID");], [Integer], [WDPA identifier, enabling cross-reference with the global database],
  [#NormalTok("valid_from");], [Date], [Start date of this state's validity period],
  [#NormalTok("valid_to");], [Date], [End date of this state's validity period],
  [#NormalTok("zone_type");], [Character], [Zone type: #NormalTok("external_boundary");, #NormalTok("core_zone");, #NormalTok("internal_buffer_zone");, or #NormalTok("external_buffer_zone");],
  [#NormalTok("zone_name");], [Character], [Name of the zone, if applicable],
  [#NormalTok("geometry");], [Geometry], [Polygon or multipolygon in WGS84 (EPSG:4326)],
  [#NormalTok("NAME");], [Character], [Protected area name],
  [#NormalTok("DESIG");], [Character], [Designation type],
  [#NormalTok("IUCN_CAT");], [Character], [IUCN management category],
  [#NormalTok("STATUS");], [Character], [Designation status],
  [#NormalTok("STATUS_YR");], [Integer], [Year of current status],
  [#NormalTok("GOV_TYPE");], [Character], [Governance type],
  [#NormalTok("MANG_AUTH");], [Character], [Management authority],
  [#NormalTok("amendment_source");], [Character], [Reference to the YAML amendment file or #NormalTok("\"wdpa_baseline\"");],
)
], caption: figure.caption(
position: top, 
[
Schema of the temporal states dataset.
]), 
kind: "quarto-float-tbl", 
supplement: "Table", 
)
<tbl-schema>


The dataset covers 123 terrestrial protected areas in Madagascar with a temporal span from 2000 to 2025. Protected areas have between one and several temporal states depending on the documented amendments. Secondary zones are included for 73 protected areas where validated core zone shapefiles were available. #ref(<fig-map>, supplement: [Figure]) shows the spatial distribution of protected areas colored by the number of documented temporal states, illustrating where the dataset adds information relative to the static WDPA.

#figure([
#box(image("../figures/fig_map.png", width: 70.0%))
], caption: figure.caption(
position: bottom, 
[
Map of Madagascar's 123 terrestrial protected areas
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-map>


The data model and the associated infrastructure are designed to be generalizable to any country or region where complementary sources can supplement the WDPA baseline, and extensible to the full diversity of protected area modifications including boundary changes, status reclassifications, governance changes, temporary protections, and any form of internal zoning.

= Technical Validation
<technical-validation>
Spatial overlap between the WDPA, successive SAPM versions, and the Vahatra dataset was quantified using mutual inclusion ratios. For each protected area present in multiple sources, the proportion of area shared between paired geometries was computed, flagging cases with less than 80% overlap for manual review. Date and status discrepancies between sources were systematically catalogued and resolved through legal text verification.

Every amendment in the YAML registry is linked to a specific legal decision identified by its decree or order number, date, and URL in the CNLEGIS database. Domain experts reviewed all amendments through the interactive Shiny dashboard, verifying that recorded boundary changes and status modifications match the corresponding legal texts.

Automated checks enforce three invariants across the dataset: no overlapping validity periods exist for the same zone of the same protected area; no temporal gaps exist in the coverage of external boundaries from the creation date to the reference date; and #NormalTok("valid_from"); is strictly earlier than #NormalTok("valid_to"); for every state.

All geometries were validated using #NormalTok("sf::st_make_valid()"); in R. Core zones were verified to fall within their parent protected area's external boundaries using spatial containment checks. Coordinate reference system consistency was enforced across all source datasets.

#ref(<fig-amendments>, supplement: [Figure]) summarizes the types and counts of amendments documented in the registry.

#figure([
#box(image("../figures/fig_amendments.png", width: 70.0%))
], caption: figure.caption(
position: bottom, 
[
Type and counts of amendments documented in the YAML registry
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)
<fig-amendments>


= Usage Notes
<usage-notes>
The GeoPackage and GeoParquet files can be opened directly in QGIS, ArcGIS, R (with the sf or arrow packages), or Python (with geopandas or pyarrow). The WDPAID field enables direct linkage with global WDPA analyses, allowing users to enrich the standard database with temporal information for Madagascar.

For temporal queries, users can filter states by #NormalTok("valid_from"); and #NormalTok("valid_to"); to retrieve the configuration of any protected area at a given date. The #NormalTok("zone_type"); field distinguishes external boundaries from internal zones, enabling analyses at different spatial scales.

The YAML amendment registry is designed for collaborative extension. Researchers with access to complementary sources --- additional legal texts, historical maps, field boundary surveys --- can propose new amendments via GitHub pull requests. The CAR engine will automatically integrate validated amendments into subsequent dataset releases.

Users should note that the WDPA baseline attributes are subject to UNEP-WCMC licensing terms regarding commercial use. The amendments and the temporal data model itself are released under a CC-BY 4.0 license. An interactive companion website provides visual exploration of the dataset, enabling users to browse individual protected areas, view their temporal trajectories, and compare boundaries across sources and time periods.

= Code Availability
<code-availability>
All source code is available at #link("https://github.com/BETSAKA/conservation-dynamic-madagascar") under a CC-BY 4.0 license. The repository contains the complete Quarto book documenting all processing steps across nine chapters, the Shiny verification application, and the CAR consolidation engine. The code is archived via Software Heritage for long-term preservation, providing a persistent software identifier (SWHID).

= Acknowledgements
<acknowledgements>
This work was conducted within the BETSAKA project, supported by KfW, Agence Française de Développement (AFD), Institut de Recherche pour le Développement (IRD), and Agence Nationale de la Recherche (ANR). The project is coordinated by IRD/UMI-SOURCE and the Université d'Antananarivo/CERED. We thank the Vahatra Association for the foundational monograph on Madagascar's protected areas, the CNLEGIS team for maintaining the national legislative database, and the organizations (MEDD, MNP, MBG, GERP, WWF) that shared core zone spatial data.

= Author Contributions
<author-contributions>
Following the CRediT (Contributor Roles Taxonomy) framework: F.B.: Conceptualization, Methodology, Software, Formal analysis, Data curation, Writing -- original draft, Visualization, Project administration. S.R.: Investigation, Data curation, Validation, Resources, Writing -- review & editing. O.D.A.: Investigation, Data curation, Validation, Resources, Writing -- review & editing.

= Competing Interests
<competing-interests>
S.R. is a civil servant in charge of protected area governance at the Ministère de l'Environnement et du Développement Durable (MEDD) of Madagascar. O.D.A. heads the data information system and innovation unit at Madagascar National Parks (MNP). Producing and maintaining protected area information is part of their professional responsibilities. The authors have no financial conflicts of interest to declare.

= Funding
<funding>
BETSAKA project, supported by KfW, Agence Française de Développement (AFD), Institut de Recherche pour le Développement (IRD), and Agence Nationale de la Recherche (ANR).

= References
<references>
#block[
] <refs>



#set bibliography(style: "https:\/\/raw.githubusercontent.com/citation-style-language/styles/master/springer-basic-author-date.csl")

#bibliography(("../references/references.bib"))

