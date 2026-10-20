# Conservation Dynamic Madagascar

*[Version française ci-dessous](#version-française) / French version below*

## Overview

This repository documents the construction of a temporal protected-area dataset for Madagascar over 2000–2025 and provides a reusable data model for adding historical states to a country subset of the World Database on Protected and Conserved Areas (WDPA). The reconstruction of Madagascar’s protected-area histories relies on country-specific spatial, legal and institutional sources, while the resulting amendment model and consolidation rules are designed to be applicable independently of the sources used to reconstruct those histories.

The repository now includes the main derived outputs together with the code, documentation, amendment registry, and validation tools used to produce them. The consolidated outputs incorporate material derived from the WDPA. UNEP-WCMC has provided written authorization to redistribute the Madagascar-specific derived outputs for this non-commercial research use case. Commercial use of WDPA material or works derived from it requires prior written permission from UNEP-WCMC.

**Warning:** the downloadable WDPA-derived files in this repository are a Madagascar-only historical snapshot based on the November 2025 release and may therefore become outdated. For the latest official protected and conserved area data, consult Protected Planet: https://www.protectedplanet.net. It is also possible to produce an updated version of this dataset by downloading a more recent WDPA release and applying the published amendment registry and consolidation workflow.

Baseline source citation for the WDPA-derived outputs: UNEP-WCMC and IUCN (2025). Protected Planet: The World Database on Protected Areas (WDPA), November 2025 release. Cambridge, UK and Gland, Switzerland. Available at https://www.protectedplanet.net.

## Objectives

- **Produce a temporal extension of WDPA**: Create time-explicit protected area data that preserves historical configurations, enabling robust longitudinal and impact analyses
- **Document legal and spatial amendments**: Systematically identify and record changes to protected areas using legal texts (CNLEGIS), historical spatial datasets (SAPM versions), and expert literature
- **Provide a reusable temporal data model:** Represent historical protected-area configurations as dated amendments to a current WDPA baseline, using a structure that can be applied to other country subsets.
- **Ensure computational reproducibility:** Store curated amendments in a version-controlled registry and transform them into temporal states through deterministic consolidation rules.
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
- **WDPA baseline preservation:** the complete November 2025 Madagascar WDPA baseline is retained. Historical national states may override selected attributes for dated intervals, while parallel international-designation records remain present as separate baseline entities where they exist.
- **Multiple formats**: RDS (R native), GeoPackage (GIS standard), and Parquet (WKT geometry)

### Download Outputs
- [GeoPackage](https://github.com/BETSAKA/conservation-dynamic-madagascar/blob/main/data/dynamic_wdpa.gpkg?raw=1)
- [Parquet](https://github.com/BETSAKA/conservation-dynamic-madagascar/blob/main/data/dynamic_wdpa.parquet?raw=1)
- [RDS](https://github.com/BETSAKA/conservation-dynamic-madagascar/blob/main/data/dynamic_wdpa.rds?raw=1)
- [YAML amendment registry](https://github.com/BETSAKA/conservation-dynamic-madagascar/tree/main/data/amendments)

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
8. YAML Registry (08_yaml_registry.qmd): Validation and documentation of the amendment registry  
9. **Consolidation Rules** (`09_consolidation_rules.qmd`): Deterministic generation and validation of temporal states  
10. **References** `(10_references.qmd`)

### Key Components

- **SAT (Spatial Amendment Table)**: Dated historical boundaries and internal zoning geometries
- **FAT (Feature Attribute Table)**: Dated legal and administrative modifications (status changes, governance, designations)
- **CAR (Consolidation and Auditing Rules)**: Code that transforms WDPA + SAT + FAT into temporal states
- **YAML Amendment Registry**: `data/amendments/*.yml` containing metadata for all documented changes

## Usage

### Prerequisites
- R 4.0+ with packages: tidyverse, sf, arrow, wdpar, yaml, lubridate, tmap, gt
- Quarto for rendering documentation

### Reproducing the consolidated dataset
The released temporal dataset can be reconstructed from the November 2025 Madagascar WDPA baseline and the version-controlled amendment registry without repeating the complete source-curation process.
To reproduce the published dataset exactly, use the November 2025 Madagascar WDPA extract and place it at `data/WDPA_WDOECM_Nov2025_Public_MDG.zip`. Then render `09_consolidation_rules.qmd`. The chapter reads the WDPA baseline and the version-controlled amendment registry, validates them, and regenerates the RDS, GeoPackage and Parquet outputs.

A more recent WDPA release can be used to update the dataset, but this produces an updated reconstruction rather than an exact reproduction of the deposited version and may require checking identifier correspondences.

Reproducing the curation of the amendment registry itself requires additional institutional and historical source datasets described in Chapters 1–7. Some of these sources are not redistributed in this repository; their provenance and access routes are documented in the complementary documentation.

### Intended Audience

- **Conservation researchers**: Analyzing temporal dynamics, degazettement, and protected area effectiveness
- **Impact evaluators**: Conducting rigorous before-after or longitudinal studies requiring accurate historical boundaries
- **Data managers**: Applying the temporal data model to other countries or regions
- **Policymakers**: Understanding the legal and spatial evolution of Madagascar's conservation system

## Output and Applications

The dynamic dataset enables:

- **Correcting WDPA errors**: Identifying and documenting incorrect STATUS_YR, designations, or boundaries
- **Temporal impact analyses**: Matching conservation interventions to accurate historical boundaries
- **Degazettement studies**: Tracking boundary reductions and their drivers
- **Legal history reconstruction**: Understanding status changes and temporary protections
- **Zone-specific analyses**: Evaluating effectiveness of core zones vs. buffer zones over time

## Citation

If you use the methodology, code, or resulting dataset, please cite:

> Bedecarrats, Florent, Ramanantsoa, Seheno, Andrianambinina, Ollier D., 2026, "Dynamic spatial conservation boundaries and internal zoning of Madagascar from 2000 to 2025", https://doi.org/10.23708/VN2TWC, DataSuds, VERSION PROVISOIRE

And the codebase via:

> Bédécarrats F., Ramanantsoa S., Andrianambinina O. D. (2026). *Conserving the History of Conservation: A Dynamic Temporal Dataset of Madagascar's Protected Areas (2000-2024)*. GitHub repository: https://github.com/BETSAKA/conservation-dynamic-madagascar

## Contributing

We welcome contributions to improve the methodology, documentation, or code quality. Potential contributions include:

- Identifying additional legal texts or spatial sources
- Suggesting amendments to protected area histories
- Improving consolidation rules or validation procedures
- Extending the data model to other countries

Please open an issue or submit a pull request following standard GitHub workflows.

## Acknowledgments

This work is part of the **BETSAKA project**, which aims to provide transparent and reproducible assessments of conservation impacts in Madagascar. The project is supported by KfW, AFD (French Development Agency), IRD (French National Research Institute for Sustainable Development), and ANR (French National Research Agency).

The project is coordinated by IRD/UMI-SOURCE and the University of Antananarivo/CERED, in collaboration with colleagues from Madagascar's Ministry of Environment and Sustainable Development (MEDD) and national conservation organizations.

Special thanks to the UNEP-WCMC for maintaining the WDPA, to Madagascar National Parks (MNP) for providing SAPM data, and to CNLEGIS for making legal texts publicly accessible.

## License and reuse conditions

**Software:** The source code in this repository is distributed under the MIT License.

**Data:** The consolidated dataset combines information from several institutional sources and remains subject to source-specific reuse conditions. UNEP-WCMC has provided written authorization for redistribution of the Madagascar-specific WDPA-derived outputs for this non-commercial research use case; commercial use of WDPA material or derivative works requires prior written permission from UNEP-WCMC. Madagascar National Parks has authorized the use, adaptation, publication and redistribution under CC BY 4.0 of the ANGAP/MNP-derived geometries incorporated into this dataset. For SAPM/MEDD-derived information, institutional provenance and the legal basis for dissemination have been formally attested by MEDD; this does not place the underlying historical SAPM/MEDD archives under a general open licence. The CC BY 4.0 licence attached to the scientific dataset applies to the authors’ compilation, curation, temporal structuring and documentation subject to these source-specific conditions. Geometry provenance is recorded in the `geometry_source_dataset_id` field and amendment-level provenance is documented in the YAML registry.
The consolidated outputs incorporate material derived from the WDPA. UNEP-WCMC has provided written authorization to redistribute the Madagascar-specific derived outputs for this non-commercial research use case. Commercial use of WDPA material or derivative works requires prior written permission from UNEP-WCMC.
The downloadable consolidated files correspond to a historical research snapshot based on the November 2025 WDPA release. Users requiring the latest official protected and conserved area information should consult [Protected Planet](https://www.protectedplanet.net/).
Detailed reuse conditions for the deposited dataset are provided in the DataSuds record: https://doi.org/10.23708/VN2TWC.

---

# Version française

*[English version above](#conservation-dynamic-madagascar)*

## Vue d'ensemble

Ce dépôt documente la construction d’un jeu de données temporel sur les aires protégées de Madagascar pour la période 2000-2025 et propose un modèle de données réutilisable permettant d’ajouter une dimension historique à un sous-ensemble national de la World Database on Protected and Conserved Areas (WDPA). La reconstruction de l’histoire des aires protégées malgaches repose sur des sources spatiales, juridiques et institutionnelles propres au pays, tandis que le modèle d’amendements et les règles de consolidation peuvent être réutilisés indépendamment des sources mobilisées pour cette reconstruction.

Le dépôt inclut les principales sorties dérivées ainsi que le code, la documentation, le registre d’amendements et les outils de validation utilisés pour les produire. Les sorties consolidées incorporent des éléments dérivés de la WDPA. L’UNEP-WCMC a fourni une autorisation écrite pour redistribuer les sorties dérivées concernant Madagascar dans le cadre de cet usage scientifique non commercial. L’utilisation commerciale des données WDPA ou des œuvres qui en dérivent nécessite une autorisation écrite préalable de l’UNEP-WCMC.

**Attention :** les fichiers dérivés de la WDPA téléchargeables dans ce dépôt correspondent à un état arrêté à une date déterminée et limité à Madagascar, fondé sur la version de novembre 2025, et peuvent donc devenir obsolètes. Pour accéder aux données officielles les plus récentes sur les aires protégées et conservées, consulter Protected Planet : https://www.protectedplanet.net. Il est également possible de produire une version mise à jour de ce jeu de données en téléchargeant une version plus récente de la WDPA et en appliquant le registre d’amendements et le protocole de consolidation publiés.

Citation de la source de base des sorties dérivées de la WDPA : UNEP-WCMC and IUCN (2025). Protected Planet: The World Database on Protected Areas (WDPA), version de novembre 2025. Cambridge, UK et Gland, Suisse. Disponible sur https://www.protectedplanet.net.

## Objectifs

- **Produire une extension temporelle de WDPA** : Créer des données temporellement explicites qui préservent les configurations historiques, permettant des analyses longitudinales et d'impact robustes
- **Documenter les amendements légaux et spatiaux** : Identifier et enregistrer systématiquement les changements aux aires protégées via les textes légaux (CNLEGIS), les jeux de données spatiaux historiques (versions SAPM) et la littérature experte
- **Proposer un modèle de données temporel réutilisable** : représenter les configurations historiques sous forme d’amendements datés appliqués à une version courante de la WDPA, selon une structure pouvant être utilisée pour d’autres pays  
- **Assurer la reproductibilité du traitement** : conserver les amendements sous contrôle de version et les transformer en états temporels au moyen de règles de consolidation déterministes  
- **Promouvoir la science ouverte** : permettre à la communauté de vérifier, discuter et enrichir le jeu de données et son modèle

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
- **Conservation de la WDPA de référence** : l’ensemble des données WDPA de novembre 2025 pour Madagascar est conservé dans le jeu consolidé, y compris les désignations internationales distinctes.
- **Formats multiples** : RDS (natif R), GeoPackage (standard SIG), et Parquet avec géométrie WKT (analytique)

### Télécharger les sorties
- [GeoPackage](https://github.com/BETSAKA/conservation-dynamic-madagascar/blob/main/data/dynamic_wdpa.gpkg?raw=1)
- [Parquet](https://github.com/BETSAKA/conservation-dynamic-madagascar/blob/main/data/dynamic_wdpa.parquet?raw=1)
- [RDS](https://github.com/BETSAKA/conservation-dynamic-madagascar/blob/main/data/dynamic_wdpa.rds?raw=1)
- [Registre YAML des amendements](https://github.com/BETSAKA/conservation-dynamic-madagascar/tree/main/data/amendments)

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
8. **Registre YAML** (`08_yaml_registry.qmd`) : validation et documentation du registre d’amendements  
9. **Règles de consolidation** (`09_consolidation_rules.qmd`) : génération déterministe et validation des états temporels  
10. **Références** (`10_references.qmd`)

### Composantes clés

- **SAT (Spatial Amendment Table)** : Limites historiques datées et géométries de zonage interne
- **FAT (Feature Attribute Table)** : Modifications légales et administratives datées (changements de statut, gouvernance, désignations)
- **CAR (Consolidation and Auditing Rules)** : Code transformant WDPA + SAT + FAT en états temporels
- **Registre d'amendements YAML** : `data/amendments/*.yml` contenant les métadonnées de tous les changements documentés

## Utilisation

### Prérequis
- R 4.0+ avec les packages : tidyverse, sf, arrow, wdpar, yaml, lubridate, tmap, gt
- Quarto pour générer la documentation

### Reproduire le jeu de données

Le jeu de données temporel publié peut être reconstruit à partir de la version de novembre 2025 de la WDPA pour Madagascar et du registre d’amendements placé sous contrôle de version, sans reproduire l’ensemble du travail de curation des sources.

Pour reproduire exactement le jeu de données publié, utiliser l'extrait WDPA de Madagascar de novembre 2025 et le placer à l'emplacement `data/WDPA_WDOECM_Nov2025_Public_MDG.zip`. Exécuter ensuite `09_consolidation_rules.qmd`. Ce chapitre lit la base WDPA et le registre d'amendements placé sous contrôle de version, effectue les validations et régénère les sorties RDS, GeoPackage et Parquet.

Une version plus récente de la WDPA peut être utilisée pour mettre à jour le jeu de données, mais il s'agit alors d'une nouvelle reconstruction et non d'une reproduction exacte de la version déposée ; les correspondances d'identifiants peuvent devoir être vérifiées.

Reproduire la constitution du registre d’amendements nécessite en revanche les sources institutionnelles et historiques décrites dans les chapitres 1 à 7. Certaines ne sont pas redistribuées dans ce dépôt ; leur provenance et leurs modalités d’accès sont documentées dans la documentation associée.

### Public cible

- **Chercheurs en conservation** : Analyse des dynamiques temporelles, déclassements, et efficacité des aires protégées
- **Évaluateurs d'impact** : Études rigoureuses avant-après ou longitudinales nécessitant des limites historiques précises
- **Gestionnaires de données** : souhaitant appliquer le modèle temporel à d’autres pays ou régions
- **Décideurs politiques** : Compréhension de l'évolution légale et spatiale du système de conservation de Madagascar

## Produit final et applications

Le jeu de données dynamique permet de :

- **Corriger les erreurs WDPA** : Identifier et documenter les STATUS_YR, désignations ou limites incorrects
- **Analyses d'impact temporelles** : Faire correspondre les interventions de conservation avec des limites historiques précises
- **Études de déclassement** : Suivre les réductions de limites et leurs facteurs
- **Reconstruction de l'histoire légale** : Comprendre les changements de statut et protections temporaires
- **Analyses spécifiques aux zones** : Évaluer l'efficacité des zones noyaux vs zones tampons dans le temps

## Citation

Si vous utilisez la méthodologie, le code ou le jeu de données résultant, veuillez citer :

> Bedecarrats, Florent, Ramanantsoa, Seheno, Andrianambinina, Ollier D., 2026, "Dynamic spatial conservation boundaries and internal zoning of Madagascar from 2000 to 2025", https://doi.org/10.23708/VN2TWC, DataSuds, VERSION PROVISOIRE

Et le code via :

> Bédécarrats F., Ramanantsoa S., Andrianambinina O. D. (2026). *Conserving the History of Conservation: A Dynamic Temporal Dataset of Madagascar's Protected Areas (2000-2024)*. Dépôt GitHub : https://github.com/BETSAKA/conservation-dynamic-madagascar

## Contributions

Nous accueillons les contributions pour améliorer la méthodologie, la documentation ou la qualité du code. Les contributions possibles incluent :

- Identification de textes légaux ou sources spatiales additionnels
- Suggestion d'amendements aux historiques d'aires protégées
- Amélioration des règles de consolidation ou procédures de validation
- Extension du modèle de données à d'autres pays

Veuillez ouvrir une issue ou soumettre une pull request en suivant les workflows standards de GitHub.

## Remerciements

Ce travail fait partie du **projet BETSAKA**, qui vise à fournir des évaluations transparentes et reproductibles des impacts de la conservation à Madagascar. Le projet est soutenu par KfW, l'AFD (Agence Française de Développement), l'IRD (Institut de Recherche pour le Développement) et l'ANR (Agence Nationale de la Recherche).

Le projet est coordonné par l'IRD/UMI-SOURCE et l'Université d'Antananarivo/CERED, en collaboration avec des collègues du Ministère de l'Environnement et du Développement Durable (MEDD) de Madagascar et des organisations nationales de conservation.

Remerciements particuliers à l'UNEP-WCMC pour la maintenance de WDPA, à Madagascar National Parks (MNP) pour la fourniture des données SAPM, et à CNLEGIS pour avoir rendu les textes légaux publiquement accessibles.

## Licence et conditions de réutilisation

**Logiciel :** Le code source de ce dépôt est diffusé sous licence MIT.

**Données :** Le jeu de données consolidé combine des informations provenant de plusieurs sources institutionnelles et reste soumis aux conditions de réutilisation propres à chacune d’elles. L’UNEP-WCMC a accordé une autorisation écrite pour la redistribution des produits dérivés de la WDPA concernant Madagascar dans le cadre de cet usage scientifique non commercial ; toute utilisation commerciale de données de la WDPA ou de produits qui en sont dérivés nécessite l’autorisation écrite préalable de l’UNEP-WCMC. Madagascar National Parks a autorisé, sous licence CC BY 4.0, l’utilisation, l’adaptation, la publication et la redistribution des géométries issues de l’ANGAP/MNP incorporées dans ce jeu de données. Pour les informations issues du SAPM/MEDD, leur provenance institutionnelle et le fondement juridique de leur diffusion ont fait l’objet d’une attestation formelle ; cette attestation ne place pas pour autant les archives historiques sous-jacentes du SAPM/MEDD sous une licence ouverte générale. La licence CC BY 4.0 associée au jeu de données scientifique s’applique au travail de compilation, de curation, de structuration temporelle et de documentation réalisé par les auteurs, sous réserve des conditions propres aux différentes sources. La provenance des géométries est indiquée dans le champ `geometry_source_dataset_id` et la provenance des amendements est documentée dans le registre YAML. 

Les sorties consolidées incorporent des éléments dérivés de la WDPA. L’UNEP-WCMC a fourni une autorisation écrite pour redistribuer les sorties dérivées concernant Madagascar dans le cadre de cet usage scientifique non commercial. L’utilisation commerciale des données WDPA ou des œuvres dérivées nécessite une autorisation écrite préalable de l’UNEP-WCMC.

Les fichiers consolidés téléchargeables correspondent à un instantané historique fondé sur la version de novembre 2025 de la WDPA. Les utilisateurs ayant besoin des informations officielles les plus récentes doivent consulter [Protected Planet](https://www.protectedplanet.net/).
Les conditions détaillées de réutilisation du jeu de données déposé sont indiquées dans la notice DataSuds : https://doi.org/10.23708/VN2TWC.
