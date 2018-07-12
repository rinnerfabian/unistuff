<?xml version="1.0" encoding="iso-8859-1"?>

<!-- book2htmlEssentials.xsl
     To be applied after extensive preprocessing.
     ABK. 18 December 2007. -->

<xsl:transform
  version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:output method="html" encoding="iso-8859-1" indent="yes"/>

<xsl:strip-space elements="bookRef"/>

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
</xsl:template>

<xsl:template match="bookTitle"/>

<xsl:template match="chapter|references|refAuthors|listInFn">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="chapterHeading">
  <h1 class="HLabel">
    <xsl:text>Chapter </xsl:text>
    <xsl:number value="../@no" format="1"/>
  </h1>
  <h1>
    <xsl:apply-templates/>
  </h1>
</xsl:template>

<xsl:template match="referencesHeading">
  <h1>
    <xsl:apply-templates/>
  </h1>
</xsl:template>

<xsl:template match="parAfterHeading">
  <p class="PFirst">
    <xsl:apply-templates/>
  </p>
</xsl:template>

<xsl:template match="par">
  <p>
    <xsl:apply-templates/>
  </p>
</xsl:template>

<xsl:template match="authorFirstOfMany">
  <xsl:value-of select="famName"/>
  <xsl:text> et al.</xsl:text>
</xsl:template>

<xsl:template match="authorInner"/>

<xsl:template match="authorFirstOfTwo">
  <xsl:value-of select="famName"/>
  <xsl:text> and </xsl:text>
</xsl:template>

<xsl:template match="authorLastOfTwo|authorExactlyOne">
  <xsl:value-of select="famName"/>
</xsl:template>

<xsl:template match="emph">
  <i><xsl:apply-templates/></i>
</xsl:template>

<xsl:template match="innerEmph">
  <i><b><xsl:apply-templates/></b></i>
</xsl:template>

<xsl:template match="bookRefNo" name="bookRefNo">
  <xsl:param name="no" select="."/>
  <xsl:number value="$no" format="[1]"/>
</xsl:template>

<xsl:template match="fnNo" name="fnNo">
  <xsl:param name="no" select="."/>
  <xsl:number value="$no" format="*1"/>
</xsl:template>

<xsl:template name="liNo">
  <xsl:param name="no" select="1"/>
  <xsl:number value="$no" format="(i)"/>
</xsl:template>

<xsl:template match="chapterNo" name="chapterNo">
  <xsl:param name="no" select="."/>
  <xsl:text>Chapter </xsl:text>
  <xsl:number value="$no" format="1"/>
</xsl:template>

<xsl:template match="fns">
  <div class="footNotes">
    <hr/>
    <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template match="fn">
  <xsl:call-template name="fnNo">
    <xsl:with-param name="no" select="@no"/>
  </xsl:call-template>
  <xsl:text> </xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="list">
  <div class="list">
    <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template match="li">
  <p>
    <xsl:text> </xsl:text>
    <xsl:call-template name="liNo">
      <xsl:with-param name="no" select="@no"/>
    </xsl:call-template>
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
  </p>
</xsl:template>

<xsl:template match="liInFn">
  <xsl:call-template name="liNo">
    <xsl:with-param name="no" select="@no"/>
  </xsl:call-template>
  <xsl:text> </xsl:text>
  <xsl:apply-templates/>
  <xsl:text> </xsl:text>
</xsl:template>

<xsl:template match="bookRef">
  <div class="bookRef">
    <xsl:text>  </xsl:text>
    <xsl:call-template name="bookRefNo">
      <xsl:with-param name="no" select="@no"/>
    </xsl:call-template>
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template match="title">
  <xsl:text>: </xsl:text>
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
