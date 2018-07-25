<?xml version="1.0" encoding="ISO-8859-1"?>

<xsl:transform version="2.0"
	xmlns:b="http://www.in.tum.de/personen/brueggem/demos"
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:output method="xml" encoding="ISO-8859-1" indent="yes"/>

	<xsl:template match="/">
		<html>
			<head>
				<title>Biographien</title>
			</head>
			<body>
				<xsl:for-each select="//bio">
					<h1>Biographie für <xsl:value-of select="name"/></h1>
					<p>
						<b>Geburtsdatum: </b>
						<xsl:value-of select="gebDatum"/>
					</p>
					<p>
						<b>Beruf: </b>
						<xsl:value-of select="beruf"/>
					</p>

					<h2>Interessen:</h2>
					<xsl:apply-templates select="interessen/node()"/>
				</xsl:for-each>
			</body>
		</html>
	</xsl:template>

	<xsl:template match="p">
		<p>
			<xsl:apply-templates/>
		</p>
	</xsl:template>

	<xsl:template match="person">
		<xsl:call-template name="getPerson">
			<xsl:with-param name="refID"><xsl:value-of select="@ref"/></xsl:with-param>
		</xsl:call-template>
	</xsl:template>
	
	<xsl:template name="getPerson">
		<xsl:param name="refID"/>
		<xsl:value-of select="//bio[@id=$refID]/name"/>
	</xsl:template>

	<xsl:template match="interessen/text()"/>

	<xsl:template match="text()">
		<xsl:value-of select="."/>
	</xsl:template>

	<!-- Regel für benannten Knoten: Element, Attribut, Namespace -->
	<xsl:template match="*">
		<xsl:apply-templates/>
	</xsl:template>

</xsl:transform>
