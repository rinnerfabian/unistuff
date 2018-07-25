<?xml version="1.0" encoding="iso-8859-1"?>

<!-- Computing GPAs for students.xml.
     Uses XSLT 2.0 for defining XPath functions. -->

<xsl:transform version="2.0" xmlns:b="http://www.in.tum.de/personen/brueggem/demos"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="xml" indent="yes" encoding="iso-8859-1"/>

  <xsl:function name="b:grade">
    <xsl:param name="g"/>
    <xsl:sequence
      select="
  if ($g=&quot;A&quot;) then 4.0 else if ($g=&quot;A-&quot;) then 3.7
  else if ($g=&quot;B+&quot;) then 3.3 else if ($g=&quot;B&quot;)  then 3.0
  else if ($g=&quot;B-&quot;) then 2.7 else if ($g=&quot;C+&quot;) then 2.3
  else if ($g=&quot;C&quot;)  then 2.0 else if ($g=&quot;C-&quot;) then 1.7
  else if ($g=&quot;D+&quot;) then 1.3 else if ($g=&quot;D&quot;)  then 1.0
  else if ($g=&quot;D-&quot;) then 0.7 else 0
  "
    />
  </xsl:function>

  <xsl:function name="b:gpa">
    <xsl:param name="s"/>
    <xsl:sequence select="avg(for $g in $s/results/result/@grade return b:grade($g))"/>
  </xsl:function>

  <xsl:template match="students">
    <students>
      <xsl:apply-templates/>
    </students>
  </xsl:template>

  <xsl:template match="student">
    <student>
      <name>
        <xsl:value-of select="name"/>
      </name>
      <gpa>
        <xsl:value-of select="b:gpa(.)"/>
      </gpa>
    </student>
  </xsl:template>

</xsl:transform>
