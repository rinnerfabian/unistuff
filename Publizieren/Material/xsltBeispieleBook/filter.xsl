<?xml version="1.0" encoding="iso-8859-1"?>

<!-- ABK 29.6.2015 -->

<xsl:transform version="2.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	<!-- Default processing. -->
	<xsl:import href="bookDefaults.xsl"/>

	<!-- Set up -->
	<xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>
	<xsl:strip-space elements="*"/>

	<!-- Suppress these elements -->
	<xsl:template match="publ|year"/>

	<!-- Rename/extend these elements while retaining their attributes -->
	<xsl:template match="par">
		<p>
			<xsl:for-each select="@*">
				<xsl:copy/>
			</xsl:for-each>
			<xsl:apply-templates/>
		</p>
	</xsl:template>

	<!-- Suppress these elements while retaining their contents -->
	<xsl:template match="chapter">
		<xsl:apply-templates/>
	</xsl:template>

</xsl:transform>
