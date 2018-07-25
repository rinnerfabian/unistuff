<?xml version="1.0" encoding="UTF-8"?>
<!--
Some rules that could be used as alternatives to XML Schema rules
(incomplete)
-->
<schema xmlns="http://purl.oclc.org/dsdl/schematron" queryBinding="xslt2">
    <pattern>
        <rule context="library/*">
            <assert
                test=".[name()='book' or name()='author' or name()='character' or name()='comments']"
                >Only book, author, character, comments allowed within library.</assert>
        </rule>
    </pattern>
    <pattern>
        <rule context="library/comments">
            <assert test="count(following-sibling::*)=0">At most one comments, at the end of
                library.</assert>
        </rule>
    </pattern>
    <pattern>
        <rule context="library/comments/characterRef">
        	  <!--
        	  The referenced character exists in the referenced book
        	  -->
            <assert
                test="some $b in /library/book satisfies (some $c in $b/character satisfies @book=$b/isbn and @character=$c)"
                >Dangling reference.</assert>
        </rule>
    </pattern>
</schema>
