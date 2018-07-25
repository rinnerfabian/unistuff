<?xml version="1.0" encoding="iso-8859-1"?>

<!-- bookDefaults.xsl
     Copies Elements with Attributes and Text verbatim.
     Serves as default.
     ABK 18 December 2007. -->

<xsl:transform
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>
<!-- Discard whitespace in element-only input elements during parsing -->
<xsl:strip-space elements="*"/>

<!-- Start default processing. -->
<xsl:template match="/">
  <xsl:apply-templates/>
</xsl:template>

<!-- Copy element nodes with attributes. -->
<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates select="@*"/>
    <xsl:apply-templates/>
  </xsl:copy>
</xsl:template>

<!-- Copy attribute nodes. -->
<xsl:template match="@*">
  <xsl:copy/>
</xsl:template>

<!-- Copy text nodes. -->
<xsl:template match="text()">
  <xsl:value-of select="."/>
</xsl:template>

</xsl:transform>
