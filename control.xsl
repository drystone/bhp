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

  <xsl:param name="rundir"/>

  <xsl:variable name="old" select="document(concat($rundir, '/control-state.xml'))/control-states/control-state"/>

  <xsl:template match="controls">
    <control-states>
      <xsl:apply-templates select="control"/>
    </control-states>
  </xsl:template>

  <xsl:template match="control">
    <xsl:variable name="result">
      <xsl:apply-templates select="*"/>
    </xsl:variable>
    <control-state id="{@id}" type="{@type}" device-code="{@device-code}" last-state="{$old[@id=current()/@id]}">
      <xsl:choose>
        <xsl:when test="$result='true'">on</xsl:when>
        <xsl:otherwise>off</xsl:otherwise>
      </xsl:choose>
    </control-state>
  </xsl:template>

  <xsl:template match="under|over">
    <xsl:choose>
      <xsl:when test="document(concat($rundir, '/state.xml'))/thermostat-states/thermostat-state[@thermostat-id=current()/@thermostat-id] = local-name()">true</xsl:when>
      <xsl:otherwise>false</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="on|off">
    <xsl:choose>
      <xsl:when test="$old[@id=current()/@control-id] = local-name()">true</xsl:when>
      <xsl:otherwise>false</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="true|false">
    <xsl:variable name="result">
      <xsl:apply-templates select="/controls/macro[@id=current()/@macro-id]/*"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$result = local-name()">true</xsl:when>
      <xsl:otherwise>false</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="or">
    <xsl:call-template name="or-next">
      <xsl:with-param name="next" select="*[1]"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="or-next">
    <xsl:param name="next"/>
    <xsl:variable name="result">
      <xsl:apply-templates select="$next"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$result='true'">true</xsl:when>
      <xsl:when test="$next/following-sibling::*">
        <xsl:call-template name="or-next">
          <xsl:with-param name="next" select="$next/following-sibling::*[1]"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>false</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="and">
    <xsl:call-template name="and-next">
      <xsl:with-param name="next" select="*[1]"/>
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="and-next">
    <xsl:param name="next"/>
    <xsl:variable name="result">
      <xsl:apply-templates select="$next"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$result='false'">false</xsl:when>
      <xsl:when test="$next/following-sibling::*">
        <xsl:call-template name="and-next">
          <xsl:with-param name="next" select="$next/following-sibling::*[1]"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>true</xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="not">
    <xsl:variable name="result">
      <xsl:apply-templates select="*"/>
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$result='true'">false</xsl:when>
      <xsl:otherwise>true</xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

