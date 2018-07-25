<?xml version="1.0" encoding="UTF-8"?>
<!-- Identity constraint for references into local index -->
<schema xmlns="http://purl.oclc.org/dsdl/schematron" queryBinding="xslt2">
    <pattern>
        <rule context="library/comments/characterRef">
        	  <!--
        	  The referenced character exists in the referenced book
        	  -->
            <assert
                test="some $b in /library/book satisfies
                  (some $c in $b/character satisfies @book=$b/isbn and @character=$c)"
                >Dangling reference.</assert>
        </rule>
    </pattern>
</schema>
