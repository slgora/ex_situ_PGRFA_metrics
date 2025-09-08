# REPOSITORY FOR EX SITU PGRFA METRICS: `new15crops` Branch

This branch extends the original methodology for computing ex situ conservation metrics of Plant Genetic Resources for Food and Agriculture (PGRFA), with a focused analysis on a subset of 15 priority crops. It introduces modular scripts, reproducible workflows, and outputs tailored to crop-specific conservation insights.

## Purpose

To generate and refine conservation metrics for a targeted crop subset using harmonized data from Genesys, WIEWS, BGCI, GLIS, and SGSV. This branch supports:
- Subset-specific metric computation
- Enhanced reproducibility and documentation
- Modular outputs for reporting and review

## Workflow Overview

**0. Downloading Data**  
- Sources: Genesys, WIEWS, BGCI, GLIS (via API), SGSV

**1. Loading and Merging Datasets**  

**2. Standardizing Taxa**  
- Curated taxon list for 15 crops  
- MCPD-compliant naming conventions

**3. Post-Standardization Processing** 

**4. Computing Metrics**  
  
**5. Processing Plants That Feed the World Data**  
- Integrated with crop subset  
- Relevant metric computation

**6. Producing Tables**  
- Modular summary tables  
- Excel-compatible outputs     

## Standards and Documentation

- Column names and values follow the MCPD standard:  
  https://cgspace.cgiar.org/server/api/core/bitstreams/7947d48c-5cf1-4164-8c61-fa276d658463/content
- All scripts are annotated for clarity and reproducibility
- GitHub branching strategy ensures modular development and version control
