<?xml version="1.0" encoding="UTF-8"?>

<xsl:transform version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

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

    <!--
      Verwendung von Variable, um Kontext zu speichern.
      Der Kontext zu aktuellem Element person geht
      im XPath-Ausdruck verloren, wenn zu //bio navigiert wird.
    -->
	<xsl:template match="person">
		<xsl:variable name="refID" select="@ref"/>
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
