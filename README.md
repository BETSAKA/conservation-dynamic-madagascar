# Conservation Dynamic Madagascar

*[Version française ci-dessous](#version-française) / French version below*

## Overview

This repository documents the transparent and reproducible methodology used to produce a **dynamic temporal dataset** of Madagascar's protected areas covering the period 2000-2024. Unlike conventional static datasets that capture only current boundaries, this work reconstructs the complete legal and spatial history of each protected area, including boundary modifications, status changes, internal zoning, and temporary protections.

The dataset itself is not directly hosted here but can be fully reproduced by following the documented procedures. The repository provides all code, documentation, and validation tools necessary to reconstruct the data from publicly available sources.

## Objectives

- **Produce a temporal extension of WDPA**: Create time-explicit protected area data that preserves historical configurations, enabling robust longitudinal and impact analyses
- **Document legal and spatial amendments**: Systematically identify and record changes to protected areas using legal texts (CNLEGIS), historical spatial datasets (SAPM versions), and expert literature
- **Ensure transparency and reproducibility**: Provide open, auditable consolidation rules (CAR) that transform baseline WDPA data and documented amendments into temporal states
- **Promote open science**: Enable the conservation research community to verify, critique, and build upon this methodology

## Key Features

### Data Sources Integration
- **SAPM**: National protected area system datasets (2002-2024)
- **WDPA**: World Database on Protected Areas historical archive (2010-2024)
- **CNLEGIS**: Madagascar's legislative database with legal texts governing protected areas
- **Expert literature**: Primarily Goodman et al. (2018) for validation

### Methodological Innovations
- **YAML-based amendment registry**: Human-readable, version-controlled documentation of all corrections and historical information
- **CAR (Consolidation and Auditing Rules)**: Transparent, reproducible code that applies explicit precedence rules to combine WDPA baseline with amendments
- **Zone-aware temporal model**: Supports external boundaries, core zones, buffer zones, and other internal subdivisions across time
- **Interactive visual verification**: Shiny dashboard for quality control and validation

### Output Format
The final dataset provides:
- **Temporal states**: Each protected area represented as a sequence of time-bounded states (valid_from - valid_to)
- **Zone model**: Multiple zones per state (external boundary, core zones, buffer zones)
- **WDPA compatibility**: Maintains all WDPA identifiers and attributes while adding temporal dimension
- **Multiple formats**: RDS (R native), GeoPackage (GIS standard), and Parquet with WKT geometry (analytics)

## Repository Structure

### Documentation (Quarto Book)
The repository is organized as a Quarto book with the following chapters:

1. **SAPM Data Harmonization** (`01_sapm_data.qmd`): Processing historical national datasets
2. **WDPA Data Harmonization** (`02_wdpa_data.qmd`): Building consolidated WDPA time series
3. **Legal Documentation** (`03_legal_docs.qmd`): Extracting and structuring legal texts from CNLEGIS
4. **Data Consolidation** (`04_combination.qmd`): Matching and combining multiple sources
5. **Visual Verification** (`05_visual_verif.qmd`): Interactive validation dashboard
6. **Internal Zoning** (`06_subdivisions.qmd`): Processing core zones and subdivisions
7. **Amendment Curation** (`07_curation.qmd`): Systematic documentation of corrections and amendments
8. **CAR Implementation** (`07ter_CAR.qmd`): Consolidation and auditing rules
9. **Technical Proposals** (`08_technical_proposal_en.qmd`, `09_technical_proposal_fr.qmd`): Formal specifications

### Key Components

- **SAT (Spatial Amendment Table)**: Dated historical boundaries and internal zoning geometries
- **FAT (Feature Attribute Table)**: Dated legal and administrative modifications (status changes, governance, designations)
- **CAR (Consolidation and Auditing Rules)**: Code that transforms WDPA + SAT + FAT into temporal states
- **YAML Amendment Registry**: `data/amendments/*.yml` containing metadata for all documented changes

## Usage

### Prerequisites
- R 4.0+ with packages: tidyverse, sf, arrow, wdpar, yaml, tmap, gt
- Quarto for rendering documentation

### Reproducing the Dataset

1. **Clone the repository**:
   ```bash
   git clone https://github.com/BETSAKA/conservation-dynamic-madagascar.git
   cd conservation-dynamic-madagascar
   ```

2. **Download source data**: Place WDPA, SAPM, and CNLEGIS sources in `sources/` directory (see documentation for details)

3. **Run analysis chapters sequentially**: Each `.qmd` file can be rendered independently or as part of the complete book

4. **Generate final dataset**: Run `07ter_CAR.qmd` to produce temporal states from WDPA and amendments

### Intended Audience

- **Conservation researchers**: Analyzing temporal dynamics, degazettement, and protected area effectiveness
- **Impact evaluators**: Conducting rigorous before-after or longitudinal studies requiring accurate historical boundaries
- **Data managers**: Seeking to replicate this methodology for other countries or regions
- **Policymakers**: Understanding the legal and spatial evolution of Madagascar's conservation system

## Output and Applications

The dynamic dataset enables:

- **Correcting WDPA errors**: Identifying and documenting incorrect STATUS_YR, designations, or boundaries
- **Temporal impact analyses**: Matching conservation interventions to accurate historical boundaries
- **Degazettement studies**: Tracking boundary reductions and their drivers
- **Legal history reconstruction**: Understanding status changes, temporary protections, and governance transitions
- **Zone-specific analyses**: Evaluating effectiveness of core zones vs. buffer zones over time

## Citation

If you use the methodology, code, or resulting dataset, please cite:

> Bédécarrats F., Ramanantsoa S., Andrianambinina O. D. (2026). *Conserving the History of Conservation: A Dynamic Temporal Dataset of Madagascar's Protected Areas (2000-2024)*. GitHub repository: https://github.com/BETSAKA/conservation-dynamic-madagascar

## Contributing

We welcome contributions to improve the methodology, documentation, or code quality. Potential contributions include:

- Identifying additional legal texts or spatial sources
- Suggesting amendments to protected area histories
- Improving consolidation rules or validation procedures
- Extending the methodology to other countries

Please open an issue or submit a pull request following standard GitHub workflows.

## Acknowledgments

This work is part of the **BETSAKA project**, which aims to provide transparent and reproducible assessments of conservation impacts in Madagascar. The project is supported by KfW, AFD (French Development Agency), IRD (French National Research Institute for Sustainable Development), and ANR (French National Research Agency).

The project is coordinated by IRD/UMI-SOURCE and the University of Antananarivo/CERED, in collaboration with colleagues from Madagascar's Ministry of Environment and Sustainable Development (MEDD) and national conservation organizations.

Special thanks to the UNEP-WCMC for maintaining the WDPA, to Madagascar National Parks (MNP) for providing SAPM data, and to CNLEGIS for making legal texts publicly accessible.

## License

This work is licensed under [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE).

---

# Version française

*[English version above](#conservation-dynamic-madagascar)*

## Vue d'ensemble

Ce dépôt documente la méthodologie transparente et reproductible utilisée pour produire un **jeu de données temporel dynamique** des aires protégées de Madagascar couvrant la période 2000-2024. Contrairement aux bases de données statiques conventionnelles qui ne capturent que les limites actuelles, ce travail reconstruit l'historique légal et spatial complet de chaque aire protégée, incluant les modifications de limites, les changements de statut, les zonages internes et les protections temporaires.

Le jeu de données lui-même n'est pas directement hébergé ici mais peut être entièrement reproduit en suivant les procédures documentées. Le dépôt fournit tout le code, la documentation et les outils de validation nécessaires pour reconstruire les données à partir de sources publiques.

## Objectifs

- **Produire une extension temporelle de WDPA** : Créer des données temporellement explicites qui préservent les configurations historiques, permettant des analyses longitudinales et d'impact robustes
- **Documenter les amendements légaux et spatiaux** : Identifier et enregistrer systématiquement les changements aux aires protégées via les textes légaux (CNLEGIS), les jeux de données spatiaux historiques (versions SAPM) et la littérature experte
- **Assurer transparence et reproductibilité** : Fournir des règles de consolidation ouvertes et auditables (CAR) qui transforment les données WDPA de base et les amendements documentés en états temporels
- **Promouvoir la science ouverte** : Permettre à la communauté de recherche en conservation de vérifier, critiquer et développer cette méthodologie

## Caractéristiques principales

### Intégration des sources de données
- **SAPM** : Jeux de données du système national d'aires protégées (2002-2024)
- **WDPA** : Archive historique de la World Database on Protected Areas (2010-2024)
- **CNLEGIS** : Base de données législative de Madagascar avec les textes régissant les aires protégées
- **Littérature experte** : Principalement Goodman et al. (2018) pour validation

### Innovations méthodologiques
- **Registre d'amendements en YAML** : Documentation lisible par l'humain, sous contrôle de version, de toutes les corrections et informations historiques
- **CAR (Consolidation and Auditing Rules)** : Code transparent et reproductible appliquant des règles de précédence explicites pour combiner WDPA et amendements
- **Modèle temporel intégrant les zones** : Support des limites externes, zones noyaux, zones tampons et autres subdivisions internes à travers le temps
- **Vérification visuelle interactive** : Tableau de bord Shiny pour contrôle qualité et validation

### Format de sortie
Le jeu de données final fournit :
- **États temporels** : Chaque aire protégée représentée comme une séquence d'états délimités dans le temps (valid_from - valid_to)
- **Modèle de zones** : Multiples zones par état (limite externe, zones noyaux, zones tampons)
- **Compatibilité WDPA** : Maintien de tous les identifiants et attributs WDPA tout en ajoutant la dimension temporelle
- **Formats multiples** : RDS (natif R), GeoPackage (standard SIG), et Parquet avec géométrie WKT (analytique)

## Structure du dépôt

### Documentation (livre Quarto)
Le dépôt est organisé comme un livre Quarto avec les chapitres suivants :

1. **Harmonisation des données SAPM** (`01_sapm_data.qmd`) : Traitement des jeux de données nationaux historiques
2. **Harmonisation des données WDPA** (`02_wdpa_data.qmd`) : Construction de séries temporelles WDPA consolidées
3. **Documentation légale** (`03_legal_docs.qmd`) : Extraction et structuration des textes légaux de CNLEGIS
4. **Consolidation des données** (`04_combination.qmd`) : Appariement et combinaison de sources multiples
5. **Vérification visuelle** (`05_visual_verif.qmd`) : Tableau de bord de validation interactive
6. **Zonage interne** (`06_subdivisions.qmd`) : Traitement des zones noyaux et subdivisions
7. **Curation des amendements** (`07_curation.qmd`) : Documentation systématique des corrections et amendements
8. **Implémentation CAR** (`07ter_CAR.qmd`) : Règles de consolidation et d'audit
9. **Propositions techniques** (`08_technical_proposal_en.qmd`, `09_technical_proposal_fr.qmd`) : Spécifications formelles

### Composantes clés

- **SAT (Spatial Amendment Table)** : Limites historiques datées et géométries de zonage interne
- **FAT (Feature Attribute Table)** : Modifications légales et administratives datées (changements de statut, gouvernance, désignations)
- **CAR (Consolidation and Auditing Rules)** : Code transformant WDPA + SAT + FAT en états temporels
- **Registre d'amendements YAML** : `data/amendments/*.yml` contenant les métadonnées de tous les changements documentés

## Utilisation

### Prérequis
- R 4.0+ avec les packages : tidyverse, sf, arrow, wdpar, yaml, tmap, gt
- Quarto pour générer la documentation

### Reproduire le jeu de données

1. **Cloner le dépôt** :
   ```bash
   git clone https://github.com/BETSAKA/conservation-dynamic-madagascar.git
   cd conservation-dynamic-madagascar
   ```

2. **Télécharger les données sources** : Placer les sources WDPA, SAPM et CNLEGIS dans le répertoire `sources/` (voir documentation pour détails)

3. **Exécuter les chapitres d'analyse séquentiellement** : Chaque fichier `.qmd` peut être généré indépendamment ou comme partie du livre complet

4. **Générer le jeu de données final** : Exécuter `07ter_CAR.qmd` pour produire les états temporels à partir de WDPA et des amendements

### Public cible

- **Chercheurs en conservation** : Analyse des dynamiques temporelles, déclassements, et efficacité des aires protégées
- **Évaluateurs d'impact** : Études rigoureuses avant-après ou longitudinales nécessitant des limites historiques précises
- **Gestionnaires de données** : Cherchant à répliquer cette méthodologie pour d'autres pays ou régions
- **Décideurs politiques** : Compréhension de l'évolution légale et spatiale du système de conservation de Madagascar

## Produit final et applications

Le jeu de données dynamique permet de :

- **Corriger les erreurs WDPA** : Identifier et documenter les STATUS_YR, désignations ou limites incorrects
- **Analyses d'impact temporelles** : Faire correspondre les interventions de conservation avec des limites historiques précises
- **Études de déclassement** : Suivre les réductions de limites et leurs facteurs
- **Reconstruction de l'histoire légale** : Comprendre les changements de statut, protections temporaires et transitions de gouvernance
- **Analyses spécifiques aux zones** : Évaluer l'efficacité des zones noyaux vs zones tampons dans le temps

## Citation

Si vous utilisez la méthodologie, le code ou le jeu de données résultant, veuillez citer :

> Bédécarrats F., Ramanantsoa S., Andrianambinina O. D. (2026). *Conserving the History of Conservation: A Dynamic Temporal Dataset of Madagascar's Protected Areas (2000-2024)*. Dépôt GitHub : https://github.com/BETSAKA/conservation-dynamic-madagascar

## Contributions

Nous accueillons les contributions pour améliorer la méthodologie, la documentation ou la qualité du code. Les contributions possibles incluent :

- Identification de textes légaux ou sources spatiales additionnels
- Suggestion d'amendements aux historiques d'aires protégées
- Amélioration des règles de consolidation ou procédures de validation
- Extension de la méthodologie à d'autres pays

Veuillez ouvrir une issue ou soumettre une pull request en suivant les workflows standards de GitHub.

## Remerciements

Ce travail fait partie du **projet BETSAKA**, qui vise à fournir des évaluations transparentes et reproductibles des impacts de la conservation à Madagascar. Le projet est soutenu par KfW, l'AFD (Agence Française de Développement), l'IRD (Institut de Recherche pour le Développement) et l'ANR (Agence Nationale de la Recherche).

Le projet est coordonné par l'IRD/UMI-SOURCE et l'Université d'Antananarivo/CERED, en collaboration avec des collègues du Ministère de l'Environnement et du Développement Durable (MEDD) de Madagascar et des organisations nationales de conservation.

Remerciements particuliers à l'UNEP-WCMC pour la maintenance de WDPA, à Madagascar National Parks (MNP) pour la fourniture des données SAPM, et à CNLEGIS pour avoir rendu les textes légaux publiquement accessibles.

## Licence

Ce travail est sous licence [Creative Commons Attribution 4.0 International (CC BY 4.0)](LICENSE).
