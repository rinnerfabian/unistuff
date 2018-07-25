<xsl:stylesheet version="2.0"
     xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
     xmlns:rcp="http://www.brics.dk/ixwt/recipes">

  <xsl:template match="/">
    <floury>
      <xsl:for-each 
         select="doc('recipes.xml')//
                 rcp:recipe[.//rcp:ingredient[@name='flour']]">
        <dish>
          <xsl:value-of select="./rcp:title/text()"/>
        </dish>
      </xsl:for-each>
    </floury>
  </xsl:template>

</xsl:stylesheet>
