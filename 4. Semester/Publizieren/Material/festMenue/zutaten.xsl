<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml" indent="yes"/>
    <xsl:template match="/">
        <zutaten>
            <xsl:apply-templates select="//zutat">
                <xsl:sort select="."/>
            </xsl:apply-templates>
        </zutaten>
    </xsl:template>
    <xsl:template match="zutat">
        <xsl:copy-of select="."/>
    </xsl:template>
</xsl:transform>
