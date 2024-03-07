abund.all <- httr::GET(url ="http://www.usanpn.org/npn_portal/phenophases/getAbundanceCategories.xml")
summary(abund.all)

abund.xml <- httr::content(abund.all, as="parsed")

# Extract the children to clean things up
# xml.parent <- xml2::xml_parent(abund.xml)
xml.chil <- xml2::xml_children(abund.xml)

xwalk.abund <- data.frame()
for(i in 1:length(xml.chil)){
  subchil <- xml2::xml_children(xml.chil[[i]])
  if(length(subchil)<=1) next
  
  for(j in 1:length(subchil)){
    # xml2::xml_attrs(subchil[[j]])
    abund.cat <- data.frame(category_id=xml2::xml_attrs(xml.chil[[i]])[["category_id"]],
                            category_name=xml2::xml_attrs(xml.chil[[i]])[["category_name"]],
                            value_id=xml2::xml_attrs(subchil[[j]])[["value_id"]],
                            value_name=xml2::xml_attrs(subchil[[j]])[["value_name"]])
    
    xwalk.abund <- rbind(xwalk.abund, abund.cat)
  }
}
summary(xwalk.abund)

write.csv(xwalk.abund, "../data/NPN/NPN_Abundance_Crosswalk.csv", row.names=F)
