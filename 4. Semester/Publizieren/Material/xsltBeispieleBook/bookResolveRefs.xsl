<?xml version="1.0" encoding="iso-8859-1"?>

<!-- bookResolveRefs.xsl
     Resolve references <ref/> to chapters, footnotes
     and book references.
     Resolve references <bookRef/>.
     ABK 18 December 2007. -->

<xsl:transform
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- Default processing. -->
<xsl:import href="bookDefaults.xsl"/>

<xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>

<xsl:strip-space elements="*"/>

<xsl:key name="elementByID" match="*" use="string(@id)"/>

<xsl:template match="ref">
  <xsl:variable name="refEl" select="key('elementByID',@refid)"/>
  <refNo type="{name($refEl)}">
    <xsl:value-of select="$refEl/@no"/>
  </refNo>
</xsl:template>

<xsl:template match="authorsRef">
  <xsl:variable name="refEl" select="key('elementByID',@refid)"/>
  <refAuthors>
    <xsl:copy-of select="$refEl/author"/>
  </refAuthors>
</xsl:template>

</xsl:transform>
