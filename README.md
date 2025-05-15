
# Transport Mode Detection in a Smartphone-based National Travel Survey - Research Archive

This research archive contains the code and some of the output from my thesis project. The project investigated the use of supervised machine learning models to classify transport modes from GPS measurements collected in a travel app developed by Statistics Netherlands (CBS). We compared four Random Forest and four Extreme Gradient Boosting models trained on GPS-based features in combination with contextual location-based features from OpenStreetMap and temporal features derived from time-related information and previous travel behaviour.


# Data

The data used in this study has been collected by CBS through their travel app. Data collection occurred in two time periods: December 2022 to February 2023, and June/July 2024. The data collected in 2024 has been made available as part of Eurostat's Smart Survey Implementation project. For more information on the project, see https://cros.ec.europa.eu/dashboard/trusted-smart-surveys. The data can be found in the project's GitHub: https://github.com/essnet-ssi/geoservice-ssi.

The GPS data collected in 2022/2023 and more detailed GPS data collected in 2024 (which was used in this study) are not publicly available. These data are stored and maintained by CBS in a secure environment. A brief description of these datasets can be found in the data folder. To request access or more information regarding the data, please contact CBS methodologists/researchers Yvonne Gootzen (y.a.p.m.gootzen@cbs.nl) and Jonas Klingwort (j.klingwort@cbs.nl)

Additionally, data from OpenStreetMap has been used in this study. This data was downloaded from GeoFabrik (https://www.geofabrik.de/data/download.html). Information on how to assess this data can be found in the data description file.

# Running the scripts

All code scripts used in the analysis are stored in the scripts folder. To reproduce the results of the thesis project, the scripts should be run in numerical order, starting with 01_data_preparation.R and ending with 20_predicted_probabilities.R. While most of the analyses have been performed in R, OSM features were created in Python. The original code for the OSM featuress stems from Jurgens Fourie, and has been used for this study with minor adjustments. For the study by Fourie et al., please see: Fourie, J., Klingwort, J., and Gootzen. Y (2025). Rule-based transport mode classification in a smartphone-based travel survey. CBS Discussion Paper. 


# Ethics, privacy and security

The study was approved by the Ethical Review Board of the Faculty of Social and Behavioural Sciences of Utrecht University on October 2nd 2024. The approval is filed under number 24-1967 (FETC).

CBS collects data under Art. 6.1(a) of the GDPR and maintains the secure storage of all collected data. The storage period is in accordance with CBS law and policies. The data used in this study contains sensitive data and was pseudonymised before being made available for this study.


# Permission and access

The scripts used in the study are stored within the research archive on GitHub (link). These will be openly accessible for an indefinite amount of time. Please note that the study is not reproducible without the data, which is **not** publicly available.
