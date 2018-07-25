<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="2.0">
    <xsl:output method="xml" indent="yes"/>
    <xsl:param name="gaeste">12</xsl:param>
    <xsl:template match="/">
        <zutaten>
            <xsl:for-each-group select="//zutat" group-by=".">
                <xsl:sort select="."/>
                <zutat>
                    <name>
                        <xsl:value-of select="."/>
                    </name>
                    <xsl:for-each select="current-group()">
                        <menge einheit="{@einheit}">
                            <xsl:value-of
                                select="@menge div parent::*/ancestor::rezept/@personenZahl * $gaeste"
                            />
                        </menge>
                    </xsl:for-each>
                    <mengeAggregiert einheit="{@einheit}">
                        <xsl:value-of
                            select="sum(current-group()/(@menge div parent::*/ancestor::rezept/@personenZahl * $gaeste))"
                        />
                    </mengeAggregiert>
                </zutat>
            </xsl:for-each-group>
        </zutaten>
    </xsl:template>
</xsl:transform>
