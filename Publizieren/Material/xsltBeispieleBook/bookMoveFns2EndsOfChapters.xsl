<?xml version="1.0" encoding="iso-8859-1"?>

<!-- bookMvFns2EndsOfChapters.xsl
     Move footnotes to end of chapters.
     Insert references.
     ABK 28 June 2015. -->

<xsl:transform
	version="2.0"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
	
	<!-- Default processing. -->
	<xsl:import href="bookDefaults.xsl"/>
	
	<xsl:output method="xml" encoding="iso-8859-1" indent="yes"/>
	
	<xsl:strip-space elements="*"/>
	
	<xsl:template match="chapter">
		<xsl:copy>
			<xsl:apply-templates select="@*|node()"/>
			<xsl:call-template name="doFns"/>
		</xsl:copy>
	</xsl:template>
	
	<xsl:template match="fn">
		<ref refid="{@id}"/>
	</xsl:template>
	
	<xsl:template name="doFns">
		<!--
		Element fns should only be created if there is at least one footnote in the chapter
		-->
		<fns>
			<xsl:for-each select=".//fn">
				<xsl:copy>
					<xsl:apply-templates select="@*|node()"/>
				</xsl:copy>
			</xsl:for-each>
		</fns>
	</xsl:template>
	
</xsl:transform>
