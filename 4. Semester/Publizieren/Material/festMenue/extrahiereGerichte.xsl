<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:template match="festMenue">
        <festMenueGerichte>
            <xsl:apply-templates/>
        </festMenueGerichte>
    </xsl:template>
    <xsl:template match="anlass">
        <anlass>
            <xsl:apply-templates/>
        </anlass>
    </xsl:template>
    <xsl:template match="gericht">
        <gericht>
            <typ>
                <xsl:apply-templates select="@typ"/>
            </typ>
            <name>
                <xsl:apply-templates select="name"/>
            </name>
        </gericht>
    </xsl:template>
</xsl:transform>
