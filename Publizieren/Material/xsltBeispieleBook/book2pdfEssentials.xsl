<?xml version="1.0" encoding="utf-8"?>

<!-- book2pdfEssentials.xsl -->
<!-- Tumurbaatar, with cosmetic changed by ABK on 1 July 2013 -->

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fo="http://www.w3.org/1999/XSL/Format">
<xsl:output method="xml" indent="yes"/>

<xsl:template match="/">
<fo:root>
<fo:layout-master-set>
  <fo:simple-page-master master-name="main"
    page-height="29.7cm"
    page-width="21cm"
    margin-top="20mm"
    margin-bottom="20mm"
    margin-left="20mm"
    margin-right="20mm">
    <fo:region-body/>
  </fo:simple-page-master>
</fo:layout-master-set>
<fo:page-sequence master-reference="main">
  <fo:flow flow-name="xsl-region-body">
    <fo:block>
     Willkommen auf der PDF-Fassung! 
    </fo:block>    
    <fo:block space-before="5cm">
      <fo:inline alignment-baseline="before-edge">
        <fo:external-graphic src="url(layout/buch.png)"/>
      </fo:inline>
    </fo:block>   
    <xsl:apply-templates/>
  </fo:flow>
</fo:page-sequence>
</fo:root>
</xsl:template>

<xsl:template match="book">
  <fo:block
    font-size="12pt">
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>

<xsl:template match="bookTitle">
  <fo:block
    font-size="40pt" color="blue"
    font-weight="bold" 
    text-align="center" 
    break-after="page">  
    <xsl:apply-templates/>   
    <fo:block space-before="5cm">
      <fo:inline alignment-baseline="before-edge">
        <fo:external-graphic src="url(layout/walker.gif)"/>
      </fo:inline>
    </fo:block>
  </fo:block>
</xsl:template>
  
  <xsl:template match="chapter|references|refAuthors|listInFn">
    <xsl:apply-templates/>
  </xsl:template>
  
  <xsl:template match="chapterHeading">
    <fo:block  
               font-size="18pt" 
               font-weight="bold" 
               text-align="center"
               break-before="page"> 
      <xsl:text>Chapter </xsl:text>
      <xsl:number value="../@no" format="1"/>
      <fo:block space-before="3em"
        space-after="1em"
        letter-spacing="5px">  
        <xsl:apply-templates/>
      </fo:block>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="referencesHeading">
    <fo:block font-size="18pt"
      font-weight="bold"
      text-align="center"
      space-after="1em"
      letter-spacing="5px"
    	break-before="page">
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="emph">
    <fo:inline font-style="italic">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="emph/innerEmph">
    <fo:inline font-style="italic" font-weight="bold">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="list/li">
    <fo:list-block>
    <fo:list-item font-style="italic" space-before="0.5em"  space-after="0.5em">
      <fo:list-item-label>
        <fo:block>
          <fo:inline>
            <xsl:number level="any" count="li" format="(i)"/>
          </fo:inline>
        </fo:block>
      </fo:list-item-label>
      <fo:list-item-body>
        <fo:block text-indent="0.5in"><xsl:apply-templates/></fo:block>
      </fo:list-item-body>
    </fo:list-item>
   </fo:list-block>
  </xsl:template>
  
  <xsl:template match="parAfterHeading">
    <fo:block>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
 <xsl:template match="par">
    <fo:block>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
   <xsl:template match="authorFirstOfMany">
    <xsl:value-of select="famName"/>
     <fo:inline font-size="12pt" font-weight="bold">
     <xsl:text > et al.</xsl:text></fo:inline>
  </xsl:template>
  
  <xsl:template match="authorInner"/>
  
  <xsl:template match="authorFirstOfTwo">
    <xsl:value-of select="famName"/>
    <xsl:text> and </xsl:text>
  </xsl:template>
  
  <xsl:template match="authorLastOfTwo|authorExactlyOne">
    <xsl:value-of select="famName"/>
  </xsl:template>
  
  <xsl:template match="bookRefNo" name="bookRefNo">
    <xsl:param name="no" select="."/>
    <xsl:number value="$no" format="[1]"/>
  </xsl:template>
 
  <xsl:template match="fnNo" name="fnNo">
    <xsl:param name="no" select="."/>
    <xsl:number value="$no" format="*1"/>
  </xsl:template>
  
  <xsl:template match="chapterNo" name="chapterNo">
    <xsl:param name="no" select="."/>
    <fo:inline font-weight="bold">
    <xsl:text >Chapter </xsl:text>
    <xsl:number value="$no" format="1"/></fo:inline>
  </xsl:template>
  
  <xsl:template match="bookRef">
    <fo:block>
      <xsl:call-template name="bookRefNo"> 
        <xsl:with-param name="no" select="@no"/>
      </xsl:call-template>
      <xsl:apply-templates/>
    </fo:block>
  </xsl:template>
  
  <xsl:template match="title">
    <fo:inline font-style="italic" font-weight="bold">
      <xsl:apply-templates/>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="publ">
    <fo:inline font-size="12pt">
      <xsl:apply-templates/>
      <xsl:text>,</xsl:text>
    </fo:inline>
  </xsl:template>
  
  <xsl:template match="year">
    <fo:inline font-size="12pt" 
      font-weight="bold">
      <xsl:apply-templates/><xsl:text>.</xsl:text>
    </fo:inline>
  </xsl:template>
    
  <xsl:template match="fns">
    <fo:footnote>
      <fo:inline font-size="10pt" baseline-shift="super"></fo:inline>
      <fo:footnote-body>
        <fo:block font-size="10pt">
          <xsl:apply-templates/>
        </fo:block>
      </fo:footnote-body>
    </fo:footnote> 
  </xsl:template>
  
    <xsl:template match="fn">
      <xsl:call-template name="fnNo">
        <xsl:with-param name="no" select="@no"/>
      </xsl:call-template>
      <xsl:text> </xsl:text>
      <xsl:apply-templates/>
    </xsl:template>
  
  <xsl:template name="liNo">
    <xsl:param name="no" select="1"/>
    <xsl:number value="$no" format="(i)"/>
  </xsl:template>
  
  <xsl:template match="liInFn">
    <xsl:call-template name="liNo">
      <xsl:with-param name="no" select="@no"/>
    </xsl:call-template>
    <xsl:text> </xsl:text>
    <xsl:apply-templates/>
    <xsl:text> </xsl:text>
  </xsl:template>
  
</xsl:stylesheet>
