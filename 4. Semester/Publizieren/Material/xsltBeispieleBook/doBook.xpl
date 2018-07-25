<?xml version="1.0" encoding="UTF-8"?>

<!--
	Problem: Formatierer fo2pdf
-->
<p:declare-step xmlns:p="http://www.w3.org/ns/xproc" name="doBook"
	xmlns:c="http://www.w3.org/ns/xproc-step" version="1.0">
	<p:input port="source">
		<p:document href="compBookNormalized.xml"/>
	</p:input>
	<!-- processing results are stored, not output -->
	<p:output port="result" sequence="true">
		<p:empty/>
	</p:output>
	<p:variable name="format" select="'html'"/>
	<p:xslt name="numbering">
		<p:input port="stylesheet">
			<p:document href="bookNumbering.xsl"/>
		</p:input>
		<p:input port="parameters">
			<p:empty/>
		</p:input>
	</p:xslt>
	<p:xslt name="IDFns">
		<p:input port="stylesheet">
			<p:document href="bookIDFns.xsl"/>
		</p:input>
		<p:input port="parameters">
			<p:empty/>
		</p:input>
	</p:xslt>
	<p:xslt name="moveFNs">
		<p:input port="stylesheet">
			<p:document href="bookMoveFNs2EndsOfChapters.xsl"/>
		</p:input>
		<p:input port="parameters">
			<p:empty/>
		</p:input>
	</p:xslt>
	<p:xslt name="resolveRefs">
		<p:input port="stylesheet">
			<p:document href="bookResolveRefs.xsl"/>
		</p:input>
		<p:input port="parameters">
			<p:empty/>
		</p:input>
	</p:xslt>
	<p:xslt name="markContexts">
		<p:input port="stylesheet">
			<p:document href="bookMarkContexts.xsl"/>
		</p:input>
		<p:input port="parameters">
			<p:empty/>
		</p:input>
	</p:xslt>
	<p:choose name="format">
		<p:when test="$format='pdf'">
			<p:xslt name="toPDFEssentials">
				<p:input port="stylesheet">
					<p:document href="book2pdfEssentials.xsl"/>
				</p:input>
				<p:input port="parameters">
					<p:empty/>
				</p:input>
			</p:xslt>
			<!--
			<p:xsl-formatter href="compBook.out.byXProc.pdf">
				<p:input port="parameters">
					<p:empty/>
				</p:input>
			</p:xsl-formatter>
			-->
			<!--
				The XSL-FO processor is not installed correctly in the XProc
				processor. Hence, I am storing the FO output and call the
				XSL-FO processor manually.
			-->
			<p:store href="compBook.out.byXProc.fo"/>
		</p:when>
		<p:otherwise>
			<p:xslt name="toHtmlEssentials">
				<p:input port="stylesheet">
					<p:document href="book2htmlEssentials.xsl"/>
				</p:input>
				<p:input port="parameters">
					<p:empty/>
				</p:input>
			</p:xslt>
			<p:store href="compBook.out.byXProc.html"/>
		</p:otherwise>
	</p:choose>
</p:declare-step>
