<?xml version="1.0" encoding="UTF-8"?>
<xsl:transform xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    xmlns:xs="http://www.w3.org/2001/XMLSchema" version="1.0">
    <xsl:variable name="inclLogo" select="xs:boolean('true')"/>
    <xsl:template match="/">
        <card>
            <name>Jane Doe</name>
            <title>CEO, Widget Inc.</title>
            <email>jane.doe@widget.com</email>
            <xsl:choose>
                <xsl:when test="$inclLogo">
                    <logo/>
                </xsl:when>
            </xsl:choose>
        </card>
    </xsl:template>
</xsl:transform>
