<?xml version="1.0" encoding="iso-8859-1"?>
<!-- {}[]\~|@ -->

<!-- book2pdf.xsl -->

<xsl:stylesheet
  version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format">

<xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>

<xsl:template match="/">
<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
<fo:layout-master-set>
  <fo:simple-page-master master-name="main"
    page-height="18cm"
    page-width="12cm"
    margin-top="1cm"
    margin-bottom="1cm"
    margin-left="1cm"
    margin-right="1cm">
    <fo:region-body/>
  </fo:simple-page-master>
</fo:layout-master-set>
<fo:page-sequence master-reference="main">
  <fo:flow flow-name="xsl-region-body">
    <xsl:apply-templates/>
  </fo:flow>
</fo:page-sequence>
</fo:root>
</xsl:template>

<xsl:template match="book">
  <fo:block
    font-family="Helvetica"
    font-size="9pt">
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>

<xsl:template match="bookTitle"/>

<xsl:template match="chapter">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="references">
  <xsl:apply-templates select="heading"/>
  <fo:list-block
    provisional-label-separation=".3em"
    provisional-distance-between-starts="2em">
    <xsl:apply-templates select="bookRef"/>
  </fo:list-block>
</xsl:template>

<xsl:template match="chapter/heading">
  <fo:block
    font-size="14pt"
    font-weight="bold"
    text-align="center"
    space-before="2em"
    space-after="0em"
    line-height="20pt">
    <xsl:text>Chapter </xsl:text>
    <xsl:number level="single" count="chapter" format="1"/></fo:block>
  <fo:block
    font-size="14pt"
    font-weight="bold"
    text-align="center"
    space-before="0pt"
    space-after="1em"
    line-height="20pt"><xsl:apply-templates/></fo:block>
</xsl:template>

<xsl:template match="references/heading">
  <fo:block
    font-size="14pt"
    font-weight="bold"
    text-align="center"
    space-before="2em"
    space-after="1em"
    line-height="20pt"><xsl:apply-templates/></fo:block>
</xsl:template>

<xsl:template match="par[name(preceding-sibling::*[1])='heading']">
  <fo:block
    space-before="0pt"
    space-after=".5em"><xsl:apply-templates/></fo:block>
</xsl:template>

<xsl:template match="par">
  <fo:block
    text-indent="2em"
    space-before="0pt"
    space-after=".5em"><xsl:apply-templates/></fo:block>
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
  <xsl:for-each select="id(@refid)">
    <xsl:number level="single" count="bookRef" format="[1]"/>
  </xsl:for-each>
</xsl:template>

<xsl:template match="fn/list">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="fn/list/li">
  <fo:inline>
    <xsl:number level="any" count="li" format="(1) "/>
  </fo:inline>
  <xsl:apply-templates/><xsl:text> </xsl:text>
</xsl:template>

<xsl:template match="list">
  <fo:block
    space-before=".5em"
    space-after=".5em">
  <fo:list-block
    provisional-label-separation=".3em"
    provisional-distance-between-starts="2em">
    <xsl:apply-templates/>
  </fo:list-block>
  </fo:block>
</xsl:template>

<xsl:template match="li">
  <fo:list-item>
    <fo:list-item-label>
    <fo:block>
      <fo:inline>
      <xsl:number level="any" count="li" format="(1)"/>
      </fo:inline>
    </fo:block>
    </fo:list-item-label>
    <fo:list-item-body>
    <fo:block><xsl:apply-templates/></fo:block>
    </fo:list-item-body>
  </fo:list-item>
</xsl:template>

<xsl:template match="famName">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="emph/emph">
  <fo:inline font-style="italic" font-weight="bold">
    <xsl:apply-templates/>
  </fo:inline>
</xsl:template>

<xsl:template match="emph">
  <fo:inline font-style="italic">
    <xsl:apply-templates/>
  </fo:inline>
</xsl:template>

<xsl:template match="fn">
  <fo:footnote>
    <fo:inline>*</fo:inline>
    <fo:footnote-body>
      <fo:block>
        <fo:leader leader-pattern="rule" leader-length="30%"
          rule-style="solid" rule-thickness="1pt"/>
      </fo:block>
      <fo:list-block
        provisional-label-separation=".3em"
        provisional-distance-between-starts="1em"
	font-size="7pt"
	space-before=".3em">
      <fo:list-item>
        <fo:list-item-label>
	<fo:block><fo:inline>*</fo:inline></fo:block>
	</fo:list-item-label>
	<fo:list-item-body>
	<fo:block><xsl:apply-templates/></fo:block>
	</fo:list-item-body>
      </fo:list-item>
      </fo:list-block>
    </fo:footnote-body>
  </fo:footnote>
</xsl:template>

<xsl:template match="bookRef">
  <fo:list-item space-before="0pt" space-after=".5em">
    <fo:list-item-label>
    <fo:block>
      <fo:inline>
      <xsl:number level="single" count="bookRef" format="[1]"/>
      </fo:inline>
    </fo:block>
    </fo:list-item-label>
    <fo:list-item-body>
    <fo:block>
      <xsl:apply-templates select="author"/>
      <xsl:apply-templates select="title"/>
      <xsl:apply-templates select="publ"/>
      <xsl:apply-templates select="year"/>
    </fo:block>
    </fo:list-item-body>
  </fo:list-item>
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
  <fo:inline font-style="italic">
  <xsl:value-of select="."/><xsl:text>. </xsl:text>
  </fo:inline>
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

</xsl:stylesheet>
