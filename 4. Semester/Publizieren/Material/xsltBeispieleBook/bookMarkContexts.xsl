<?xml version="1.0" encoding="iso-8859-1"?>

<!-- markContexts.xsl
     Differenciate between elements in different structural contexts:
     par vs. par with preceding heading;
     first, inner and last position of author;
     type of refNo;
     li inside vs. outside fn;
     nested emph
     heading of chapter vs. references
     ABK 18 December 2007. -->

<xsl:transform
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<!-- Default processing. -->
<xsl:import href="bookDefaults.xsl"/>

<xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>

<xsl:strip-space elements="*"/>

<xsl:template match="par[name(preceding-sibling::*[1])='heading']">
  <parAfterHeading>
    <xsl:apply-templates select="@*|node()"/>
  </parAfterHeading>
</xsl:template>

<xsl:template match="author">
  <xsl:choose>
    <xsl:when test="position()=1 and count(following-sibling::author)>1">
      <authorFirstOfMany>
        <xsl:apply-templates select="@*|node()"/>
      </authorFirstOfMany>
    </xsl:when>
    <xsl:when test="position()=1 and count(following-sibling::author)=1">
      <authorFirstOfTwo>
        <xsl:apply-templates select="@*|node()"/>
      </authorFirstOfTwo>
    </xsl:when>
    <xsl:when test="position()=1 and count(following-sibling::author)=0">
      <authorExactlyOne>
        <xsl:apply-templates select="@*|node()"/>
      </authorExactlyOne>
    </xsl:when>
    <xsl:when test="position()=2 and count(following-sibling::author)=0">
      <authorLastOfTwo>
        <xsl:apply-templates select="@*|node()"/>
      </authorLastOfTwo>
    </xsl:when>
    <xsl:otherwise>
      <authorInner>
        <xsl:apply-templates select="@*|node()"/>
      </authorInner>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="refNo">
  <xsl:element name="{@type}No">
    <xsl:value-of select="."/>
  </xsl:element>
</xsl:template>

<xsl:template match="fn//list">
  <listInFn>
    <xsl:apply-templates select="@*|node()"/>
  </listInFn>
</xsl:template>

<xsl:template match="fn//li">
  <liInFn>
    <xsl:apply-templates select="@*|node()"/>
  </liInFn>
</xsl:template>

<xsl:template match="emph//emph">
  <innerEmph>
    <xsl:apply-templates select="@*|node()"/>
  </innerEmph>
</xsl:template>

<xsl:template match="chapter/heading">
  <chapterHeading>
    <xsl:apply-templates select="@*|node()"/>
  </chapterHeading>
</xsl:template>

<xsl:template match="references/heading">
  <referencesHeading>
    <xsl:apply-templates select="@*|node()"/>
  </referencesHeading>
</xsl:template>
</xsl:transform>
