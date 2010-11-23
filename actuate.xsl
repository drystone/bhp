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

  <xsl:variable name="nl">
    <xsl:text>
</xsl:text>
  </xsl:variable>

  <xsl:template match="/">
    <xsl:call-template name="make-command">
      <xsl:with-param name="command" select="'./bhpx10 -t /dev/ttyS1'"/>
      <xsl:with-param name="type" select="'x10'"/>
    </xsl:call-template>
    <xsl:call-template name="make-command">
      <xsl:with-param name="command" select="'./bhpfs20'"/>
      <xsl:with-param name="type" select="'fs20'"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="make-command">
    <xsl:param name="command"/>
    <xsl:param name="type"/>
    <xsl:variable name="args">
      <xsl:for-each select="/control-states/control-state[@type=$type][@last-state!=text()]">
        <xsl:value-of select="concat(@device-code, ' ', text(), ' ')"/>
      </xsl:for-each>
    </xsl:variable>
    <xsl:if test="string-length($args)">
      <xsl:message>
        <xsl:value-of select="concat($command, ' ', $args, $nl)"/>
      </xsl:message>
      <xsl:value-of select="concat($command, ' ', $args, $nl)"/>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>

