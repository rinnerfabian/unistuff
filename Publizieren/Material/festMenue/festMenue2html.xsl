<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="html"/>
    <xsl:template match="festMenue">
        <html>
            <head>
                <title>
                    <xsl:apply-templates select="anlass"/>
                </title>
            </head>
            <body>
                <h1>
                    <xsl:apply-templates select="anlass"/>
                </h1>
                <xsl:apply-templates select="gericht"/>
            </body>
        </html>
    </xsl:template>
    <xsl:template match="gericht">
        <h2>
            <xsl:apply-templates select="@typ"/>: <xsl:apply-templates select="name"/>
        </h2>
        <xsl:apply-templates select="rezept"/>
    </xsl:template>
    <xsl:template match="rezept">
        <h3>Rezept (Personenzahl: <xsl:value-of select="@personenZahl"/>)</h3>
        <h4>Zutaten</h4>
        <ul>
            <xsl:apply-templates select=".//zutat"/>
        </ul>
        <h4>Zubereitung</h4>
        <ul>
            <xsl:apply-templates select=".//schritt"/>
        </ul>
    </xsl:template>
    <xsl:template match="zutat">
        <li>
            <xsl:apply-templates/>
        </li>
    </xsl:template>
    <xsl:template match="schritt">
        <li>
            <xsl:apply-templates/>
        </li>
    </xsl:template>
</xsl:transform>
