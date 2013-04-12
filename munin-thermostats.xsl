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

  <xsl:param name="configdir"/>
  <xsl:param name="rundir"/>

  <xsl:variable name="newline"><xsl:text>
</xsl:text>
  </xsl:variable>

  <xsl:param name="mode"/>

  <xsl:template match="thermostats">
    <xsl:if test="$mode='config'">
      <xsl:value-of select="concat('graph_title Thermostats',$newline)"/>
      <xsl:value-of select="concat('graph_vlabel State On/Off',$newline)"/>
    </xsl:if>
    <xsl:apply-templates select="thermostat|dualstat"/>
  </xsl:template>

  <xsl:template match="thermostat|dualstat">
    <xsl:choose>
      <xsl:when test="$mode='config'">
        <xsl:variable name="colour" select="document(concat($configdir, '/munin.xml'))/colours/colour[@id=current()/@id]"/>
        <xsl:value-of select="concat(@id,'.label ', @name, $newline)"/>
        <xsl:if test="$colour">
          <xsl:value-of select="concat(@id,'.colour ', $colour, $newline)"/>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="binary-state">
          <xsl:variable name="id" select="@id"/>
          <xsl:choose>
            <xsl:when test="document(concat($rundir, '/state.xml'))/thermostat-states/thermostat-state[@thermostat-id=$id]/text()='under'">1</xsl:when>
            <xsl:otherwise>0</xsl:otherwise>
          </xsl:choose>
        </xsl:variable>
        <xsl:value-of select="concat(@id,'.value ',count(preceding-sibling::thermostat|preceding-sibling::dualstat)*2+$binary-state,$newline)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
