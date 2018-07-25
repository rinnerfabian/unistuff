<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
    <xsl:output method="xml" indent="yes"></xsl:output>
    <xsl:template match="/">
        <doubles>
        <xsl:for-each select="//student">
            <xsl:sort select="./@id"></xsl:sort>
            <xsl:if test="count(./major)&gt;1">
                <double><xsl:value-of select="./name"></xsl:value-of></double>
            </xsl:if>
        </xsl:for-each>
        </doubles>
    </xsl:template>
</xsl:transform>
