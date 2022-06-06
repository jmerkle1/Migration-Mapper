# Migration-Mapper
Repository for Migration Mapper
# MIGRATION MAPPER VERSION 3

## overview
Migration Mapper™ is a free application designed for researchers, biologists, and managers, to analyze fine-scale GPS collar data collected from migratory ungulates.

Understanding and mapping where animals go is of utmost importance to wildlife conservation and management. Maps of animal movement are particularly important for efforts focused on migratory wildlife, as migratory animals rely on connected, seasonal habitats that span large landscapes.

In 2009, WMI researchers devised a method for analyzing GPS collar data collected from migratory ungulates (Sawyer et al. 2009). This methodology allows for identifying, mapping and prioritizing migration corridors. The methodology involves 1) a Brownian bridge movement analysis of GPS collar data collected for individual animals and 2) a prioritization analysis based on the number of animals migrating through specific areas. The issue with this methodology, however, is that it is computationally intensive, and requires coding and analysis skills that are beyond the reach of most biologists and managers. Therefore, WMI has invested in development of this application, which facilitates the technologically challenging analyses of animal movement and migration data.

## how it works
Migration Mapper™ was not developed to be a ‘pull-the-handle-and-see’ program. Instead, Migration Mapper provides a number of interfaces to help users analyze their data. Users must provide Migration Mapper information to identify movement corridors and seasonal ranges. Thus, you should expect to spend some time working with your data before Migration Mapper spits out a map of movement corridors or seasonal ranges. The user starts with a cleaned GPS collar dataset in ESRI Shapefile format.

MAPP is modular and consists of six standalone applications representing distinct steps of the workflow. Each of the applications are accessed through the main MAPP homepage. Modularity clarifies the step-by step process and allows users to easily navigate between steps, fix errors or make changes in a previous step, or even skip certain steps in the workflow. The modules include: 1) data cleaning and review, 2-3) seasonal movement delineation, 4) movement model application, 5) calculating population-level outputs, and 6) visualizing the outputs.

## project funders
Support for the development of Migration Mapper™ has come from Western Association of Fish and Wildlife Agencies, The Pew Charitable Trust, and The Knobloch Family Foundation. We especially thank [Josh Gage (with Gage Cartographics LLC)](https://www.gagecarto.com) and Jerod Merkle for writing the majority of code and leading the development of Migration Mapper.

