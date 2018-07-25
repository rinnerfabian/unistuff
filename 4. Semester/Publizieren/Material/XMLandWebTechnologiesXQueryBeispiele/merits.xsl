<xsl:stylesheet version="2.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:rcp="http://www.brics.dk/ixwt/recipes">

  <xsl:template match="/">
    <xsl:for-each select="doc('students.xml')//student">
       <xsl:sort select="count(./results/result[contains(@grade,'A')])" 
                 order="descending"/>
       <xsl:sort select="count(./major)" order="descending"/>
       <xsl:sort select="xs:integer(./age/text())" order="ascending"/>
       <xsl:value-of select="./name/text()"/>
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>
