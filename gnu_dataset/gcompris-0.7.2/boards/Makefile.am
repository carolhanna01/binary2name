xmldir = $(datadir)/xml

xml_in_files = 
xml_DATA = $(xml_in_files:.xml.in=.xml)

@XML_I18N_MERGE_XML_RULE@

EXTRA_DIST=$(xml_in_files) $(xml_DATA)
