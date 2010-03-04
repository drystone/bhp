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

  <xsl:variable name="temperatures" select="document('temperatures.xml')/temperatures"/>
  <xsl:variable name="targets" select="document('targets.xml')/targets"/>
  <xsl:variable name="old-states" select="document('state.xml')/themostat-states"/>

  <xsl:template match="thermostats">
    <thermostat-states>
      <xsl:apply-templates select="*"/>
    </thermostat-states>
  </xsl:template>

  <xsl:template match="thermostat">
    <thermostat-state thermostat-id="{@id}">
      <xsl:choose>
        <xsl:when test="count($temperatures/temperature[@thermometer-id=current()/@thermometer-id])=0">
          <xsl:value-of select="$old-states/thermostat-state[@thermostat-id=current()/@id]"/>
        </xsl:when>
        <xsl:when test="$targets/target[@zone-id=current()/@zone-id] >= $temperatures/temperature[@thermometer-id=current()/@thermometer-id]">on</xsl:when>
        <xsl:otherwise>off</xsl:otherwise>
      </xsl:choose>
    </thermostat-state>
  </xsl:template>

  <!--xsl:template match="dualstat">
    <stat-state stat-id="{@id}">
      <xsl:choose>
        <xsl:when test="$temperatures/temperature[@thermometer-id=current()/@low-thermometer-id] > @high-temperature">off</xsl:when>
        <xsl:when test="@low-temperature >= $temperatures/temperature[@thermometer-id=current()/@high-thermometer-id]">on</xsl:when>
        <xsl:when test="$old-states/states/state[@stat-id=current/@id]">
          <xsl:value-of select="$old-states/states/state[@stat-id=current/@id]"/>
        </xsl:when>
      </xsl:choose>
    </stat-state>
  </xsl:template-->

</xsl:stylesheet>

