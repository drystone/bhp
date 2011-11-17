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

  <xsl:variable name="temperatures" select="document('/tmp/temperatures.xml')/temperatures"/>
  <xsl:variable name="targets" select="document('/tmp/targets.xml')/targets"/>
  <xsl:variable name="old" select="document('/tmp/state.xml')/thermostat-states/thermostat-state"/>

  <xsl:template match="thermostats">
    <thermostat-states>
      <xsl:apply-templates select="*"/>
    </thermostat-states>
  </xsl:template>

  <xsl:template match="thermostat">
    <xsl:variable name="temperature">
      <xsl:choose>
        <xsl:when test="count(@offset)">
          <xsl:value-of select="$temperatures/temperature[@thermometer-id=current()/@thermometer-id] + @offset"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$temperatures/temperature[@thermometer-id=current()/@thermometer-id]"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <thermostat-state thermostat-id="{@id}">
      <xsl:choose>
        <xsl:when test="count(@zone-id) and $targets/target[@zone-id=current()/@zone-id] > $temperature">under</xsl:when>
        <xsl:when test="count(@temperature) and @temperature > $temperature">under</xsl:when>
        <xsl:when test="count(@target-thermometer-id) and $temperatures/temperature[@thermometer-id=current()/@target-thermometer-id] > $temperature">under</xsl:when>
        <xsl:otherwise>over</xsl:otherwise>
      </xsl:choose>
    </thermostat-state>
  </xsl:template>

</xsl:stylesheet>

