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

  <xsl:output method="text"/>

  <xsl:variable name="newline"><xsl:text>
</xsl:text>
  </xsl:variable>

  <xsl:param name="mode"/>
  <xsl:param name="class"/>

  <!-- temperatures -->
  <xsl:template match="temperatures">
    <xsl:choose>
      <xsl:when test="$mode='config'">
        <xsl:variable name="title">
          <xsl:choose>
            <xsl:when test="$class='room'">Room </xsl:when>
            <xsl:when test="$class='system'">System </xsl:when>
            <xsl:otherwise><xsl:value-of select="$class"/></xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="concat('graph_title ',$title,'Temperatures',$newline)"/>
        <xsl:value-of select="concat('graph_vlabel temp in C',$newline)"/>
        <xsl:choose>
          <xsl:when test="$class='room' or $class='system'">
            <xsl:apply-templates select="document('thermometers.xml')/thermometers/thermometer[@munin-class=$class]"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:apply-templates select="document('thermometers.xml')/thermometers/thermometer"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="temperature"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="temperature">
    <xsl:value-of select="concat(@thermometer-id,'.value ',text(),$newline)"/>
  </xsl:template>

  <!-- thermostats -->
  <xsl:template match="thermostat-states">
    <xsl:choose>
      <xsl:when test="$mode='config'">
        <xsl:value-of select="concat('graph_title Thermostats',$newline)"/>
        <xsl:value-of select="concat('graph_vlabel State On/Off',$newline)"/>
        <xsl:apply-templates select="document('thermostats.xml')/thermostats/thermostat|document('thermostats.xml')/thermostats/dualstat"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="thermostat-state"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="thermostat-state">
    <xsl:variable name="binary-state">
      <xsl:choose>
        <xsl:when test="text()='off'">0</xsl:when>
        <xsl:otherwise>1</xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:value-of select="concat(@thermostat-id,'.value ',count(preceding-sibling::thermostat-state)*2+$binary-state,$newline)"/>
  </xsl:template>

  <xsl:template match="thermometer|thermostat|dualstat">
    <xsl:variable name="colour" select="document('munin.xml')/colours/colour[@id=current()/@id]"/>
    <xsl:value-of select="concat(@id,'.label ', @name, $newline)"/>
    <xsl:if test="$colour">
      <xsl:value-of select="concat(@id,'.colour ', $colour, $newline)"/>
    </xsl:if>
  </xsl:template>

</xsl:stylesheet>
