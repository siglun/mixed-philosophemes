#!/bin/bash

echo  '<body   xmlns:xi="http://www.w3.org/2001/XInclude" >' > includes.xml

ls *.xml | \
    grep -v philosophemes | \
    grep -v includes | \
    grep -v bibliografi | \
    perl -ne 'chomp; print  "<xi:include href=\"$_\"/>\n";' >> includes.xml

echo  '</body>' >> includes.xml


xmllint --xinclude  philosophemes.xml.in  |  xmllint --xinclude  - >  philosophemes.xml

echo "Number of files "`ls *xml | grep -v include | grep -v philosopheme | wc -l`

xmllint --noout --relaxng ../tei_all.rng  philosophemes.xml

