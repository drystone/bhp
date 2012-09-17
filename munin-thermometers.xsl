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

  <xsl:template match="thermometers">
    <xsl:if test="$mode='config'">
      <xsl:variable name="title">
        <xsl:choose>
          <xsl:when test="$class='room'">Room </xsl:when>
          <xsl:when test="$class='system'">System </xsl:when>
        </xsl:choose>
      </xsl:variable>
      <xsl:value-of select="concat('graph_title ',$title,'Temperatures',$newline)"/>
      <xsl:value-of select="concat('graph_vlabel temp in C',$newline)"/>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="$class='room' or $class='system'">
        <xsl:apply-templates select="thermometer[@munin-class=$class]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:apply-templates select="thermometer"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="thermometer">
    <xsl:choose>
      <xsl:when test="$mode='config'">
        <xsl:variable name="colour" select="document('/etc/bhp/munin.xml')/colours/colour[@id=current()/@id]"/>
        <xsl:value-of select="concat(@id,'.label ', @name, $newline)"/>
        <xsl:if test="$colour">
          <xsl:value-of select="concat(@id,'.colour ', $colour, $newline)"/>
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:variable name="id" select="@id"/>
        <xsl:value-of select="concat($id,'.value ',document('/tmp/bhp/temperatures.xml')/temperatures/temperature[@thermometer-id=$id],$newline)"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

