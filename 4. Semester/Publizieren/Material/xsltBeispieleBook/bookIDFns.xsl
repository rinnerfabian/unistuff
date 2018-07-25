<?xml version="1.0" encoding="iso-8859-1"?>

<!-- bookCopyFns.xsl
     Generate IDs for footnotes.
     ABK 18 December 2007. -->

<xsl:transform
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- Default processing. -->
<xsl:import href="bookDefaults.xsl"/>

<xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>

<xsl:strip-space elements="*"/>

<xsl:template match="fn">
  <fn id="{generate-id()}">
    <xsl:apply-templates select="@*"/>
    <xsl:apply-templates/>
  </fn>
</xsl:template>

</xsl:transform>
