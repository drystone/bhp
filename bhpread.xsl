<?xml version="1.0"?>

<!--

Copyright 2010 John Hedges

This file is part of bhp.

bhp is free software: you can redistribute it and/or modify it under the terms
of the GNU General Public License as published by the Free Software Foundation,
either version 3 of the License, or (at your option) any later version.

bhp is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
bhp. If not, see [http://www.gnu.org/licenses/].

-->

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

	<xsl:output method="xml" indent="yes"/>
  <xsl:variable name="new" select="/temperatures"/>
  <xsl:variable name="old" select="document('/tmp/bhp/temperatures.xml')/temperatures"/>
  <xsl:variable name="thermometers" select="document('/etc/bhp/thermometers.xml')/thermometers"/>

  <xsl:template match="/">
    <temperatures>
      <xsl:apply-templates select="$thermometers/thermometer"/>
    </temperatures>
  </xsl:template>
  
  <xsl:template match="thermometer">
		<temperature device-id="{@device-id}" thermometer-id="{@id}">
      <xsl:variable name="temperature" select="$new/temperature[@device-id=current()/@device-id]"/>
      <xsl:choose>
        <xsl:when test="count($temperature)">
          <xsl:choose>
            <xsl:when test="@correction">
              <xsl:value-of select="$temperature + @correction"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="$temperature"/>
            </xsl:otherwise>
          </xsl:choose>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$old/temperature[@device-id=current()/@device-id]"/>
        </xsl:otherwise>
      </xsl:choose>
    </temperature>
  </xsl:template>

</xsl:stylesheet>
