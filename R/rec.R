
# rec Function to create synthetic EC dataset

rec <- function(domain = "EC",
                ectrt = c(),
                
                ){
  
  # Metadata for the EC dataset
  ec_metadata <- list("STUDYID" = "Study Identifier",
                      "DOMAIN" = "Domain Abbreviation",
                      "USUBJID" = "Unique Subject Identifier",
                      "ECSEQ" = "Sequence Number",
                      "ECGRPID" = "Group ID",
                      "ECREFID" = "Reference ID",
                      "ECSPID" = "Sponsor-Defined Identifier",
                      "ECLNKID" = "Link ID",
                      "ECLNKGRP" = "Link Group ID",
                      "ECTRT" = "Name of Treatment",
                      "ECMOOD" = "Mood",
                      "ECCAT" = "Category of Treatment",
                      "ECSCAT" = "Subcategory of Treatment",
                      "ECPRESP" = "Pre-Specified",
                      "ECOCCUR" = "Occurrence",
                      "ECREASOC" = "Reason for Occur Value",
                      "ECDOSE" = "Dose",
                      "ECDOSTXT" = "Dose Description",
                      "ECDOSU" = "Dose Units",
                      "ECDOSFRM" = "Dose Form",
                      "ECDOSFRQ" = "Dosing Frequency per Interval",
                      "ECDOSTOT" = "Total Daily Dose",
                      "ECDOSRGM" = "Intended Dose Regimen",
                      "ECROUTE" = "Route of Administration",
                      "ECLOT" = "Lot Number",
                      "ECLOC" = "Location of Dose Administration",
                      "ECLAT" = "Laterality",
                      "ECDIR" = "Directionality",
                      "ECPORTOT" = "Portion or Totality",
                      "ECFAST" = "Fasting Status",
                      "ECPSTRG" = "Pharmaceutical Strength",
                      "ECPSTRGU" = "Pharmaceutical Strength Units",
                      "ECADJ" = "Reason for Dose Adjustment",
                      "TAETORD" = "Planned Order of Element within Arm",
                      "EPOCH" = "Epoch",
                      "ECSTDTC" = "Start Date/Time of Treatment",
                      "ECENDTC" = "End Date/Time of Treatment",
                      "ECSTDY" = "Study Day of Start of Treatment",
                      "ECENDY" = "Study Day of End of Treatment",
                      "ECDUR" = "Duration of Treatment",
                      "ECTPT" = "Planned Time Point Name",
                      "ECTPTNUM" = "Planned Time Point Number",
                      "ECELTM" = "Planned Elapsed Time from Time Point Ref",
                      "ECTPTREF" = "Time Point Reference",
                      "ECRFTDTC" = "Date/Time of Reference Time Point"
                      )
  
  
}