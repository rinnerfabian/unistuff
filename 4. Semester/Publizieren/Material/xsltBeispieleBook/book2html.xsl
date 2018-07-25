<?xml version="1.0" encoding="iso-8859-1"?>

<!-- bios2html.xsl -->

<xsl:transform
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html" encoding="iso-8859-1" indent="yes"/>

<xsl:strip-space elements="*"/>

<xsl:template match="/">
<html><head>
  <title>
    <xsl:value-of select="//bookTitle"/>
  </title>
  <link rel="stylesheet" type="text/css" href="book.css"/>
</head>
<body>
  <xsl:apply-templates/>
</body>
</html>
</xsl:template>

<xsl:template match="book">
  <xsl:apply-templates/>
  <xsl:call-template name="fn"/>
</xsl:template>

<xsl:template name="fn">
  <div class="footNotes">
  <hr/>
  <xsl:for-each select="//fn">
    <div class="fn">
    <xsl:number level="any" count="fn" format="  [i] "/>
    <xsl:apply-templates/>
    </div>
  </xsl:for-each>
  </div>
</xsl:template>

<xsl:template match="bookTitle">
</xsl:template>

<xsl:template match="chapter">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="references">
  <xsl:apply-templates select="heading"/>
  <xsl:apply-templates select="bookRef"/>
</xsl:template>

<xsl:template match="chapter/heading">
  <h1 class="HLabel">
    <xsl:text>Chapter </xsl:text>
    <xsl:number level="single" count="chapter" format="1"/>
  </h1>
  <h1>
    <xsl:apply-templates/>
  </h1>
</xsl:template>

<xsl:template match="references/heading">
  <h1>
    <xsl:apply-templates/>
  </h1>
</xsl:template>

<xsl:template match="par[name(preceding-sibling::*[1])='heading']">
  <p class="PFirst">
    <xsl:apply-templates/>
  </p>
</xsl:template>

<xsl:template match="par">
  <p>
    <xsl:apply-templates/>
  </p>
</xsl:template>

<xsl:template match="authors[count(child::author)>2]">
  <xsl:apply-templates select="child::author[1]/famName"/>
  <xsl:text> et al.</xsl:text>
</xsl:template>

<xsl:template match="authors[count(child::author)=2]">
  <xsl:apply-templates select="child::author[1]/famName"/>
  <xsl:text> and </xsl:text>
  <xsl:apply-templates select="child::author[2]/famName"/>
</xsl:template>

<xsl:template match="authors[count(child::author)=1]">
  <xsl:apply-templates select="child::author[1]/famName"/>
</xsl:template>

<xsl:template match="ref[name(id(@refid))='chapter']">
  <xsl:text>Chapter </xsl:text>
  <xsl:for-each select="id(@refid)">
    <xsl:number level="single" count="chapter" format="1"/>
  </xsl:for-each>
</xsl:template>

<xsl:template match="ref[name(id(@refid))='bookRef']">
  <xsl:text> </xsl:text>
  <xsl:for-each select="id(@refid)">
    <xsl:number level="single" count="bookRef" format="[1]"/>
  </xsl:for-each>
</xsl:template>

<xsl:template match="fn/list">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="fn/list/li">
  <xsl:number level="any" count="li" format="(1) "/>
  <xsl:text> </xsl:text>
  <xsl:apply-templates/>
  <xsl:text> </xsl:text>
</xsl:template>

<xsl:template match="list">
  <div class="list">
    <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template match="li">
  <p>
    <xsl:text>  </xsl:text>
    <xsl:number level="any" count="li" format="(1)"/>
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
  </p>
</xsl:template>

<xsl:template match="famName">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="emph/emph">
  <i><b><xsl:apply-templates/></b></i>
</xsl:template>

<xsl:template match="emph">
  <i><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="fn">
  <xsl:number level="any" count="fn" format="[i]"/>
</xsl:template>

<xsl:template match="bookRef">
  <div class="bookRef">
    <xsl:text>  </xsl:text>
    <xsl:number level="single" count="bookRef" format="[1]"/>
    <xsl:text> </xsl:text>
    <xsl:apply-templates select="author"/>
    <xsl:apply-templates select="title"/>
    <xsl:apply-templates select="publ"/>
    <xsl:apply-templates select="year"/>
  </div>
</xsl:template>

<xsl:template match="author">
  <xsl:value-of select="chrName"/><xsl:text> </xsl:text>
  <xsl:value-of select="famName"/>
  <xsl:choose>
  <xsl:when test="count(following-sibling::author)>1">
    <xsl:text>, </xsl:text>
  </xsl:when>
  <xsl:when test="count(following-sibling::author)=1
    and count(preceding-sibling::author)>0">
    <xsl:text>, and </xsl:text>
  </xsl:when>
  <xsl:when test="count(following-sibling::author)=1
    and count(preceding-sibling::author)=0">
    <xsl:text> and </xsl:text>
  </xsl:when>
  <xsl:when test="count(following-sibling::author)=0">
    <xsl:text>: </xsl:text>
  </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="title">
  <i><xsl:value-of select="."/><xsl:text>. </xsl:text></i>
</xsl:template>

<xsl:template match="publ">
  <xsl:value-of select="."/><xsl:text>, </xsl:text>
</xsl:template>

<xsl:template match="year">
  <xsl:value-of select="."/><xsl:text>.</xsl:text>
</xsl:template>

<xsl:template match="*"> 
 <xsl:message terminate="no">
   <xsl:text>Undefined element: </xsl:text>
   <xsl:value-of select="name()"/>
 </xsl:message>
</xsl:template>

<xsl:template match="text()">
  <xsl:value-of select="."/>
</xsl:template>

</xsl:transform>
