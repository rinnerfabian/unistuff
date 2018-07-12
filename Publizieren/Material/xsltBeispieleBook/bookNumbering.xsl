<?xml version="1.0" encoding="iso-8859-1"?>

<!-- bookNumbering.xsl
     Numbers chapter, fn, li and bookRef elements
     using attribute no.
     ABK 18 December 2007. -->

<xsl:transform
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- Default processing. -->
<xsl:import href="bookDefaults.xsl"/>

<xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>

<xsl:strip-space elements="*"/>

<xsl:template match="chapter">
  <xsl:copy>
    <xsl:attribute name="no">
      <xsl:number level="single"/>
    </xsl:attribute>
    <xsl:apply-templates select="@*|node()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="fn">
  <xsl:copy>
    <xsl:attribute name="no">
      <xsl:number level="any"/>
    </xsl:attribute>
    <xsl:apply-templates select="@*|node()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="par//li">
  <xsl:copy>
    <xsl:attribute name="no">
      <xsl:number level="any" from="par"/>
    </xsl:attribute>
    <xsl:apply-templates select="@*|node()"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="bookRef">
  <xsl:copy>
    <xsl:attribute name="no">
      <xsl:number level="single"/>
    </xsl:attribute>
    <xsl:apply-templates select="@*|node()"/>
  </xsl:copy>
</xsl:template>

</xsl:transform>
