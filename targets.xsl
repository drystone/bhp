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

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:date="http://exslt.org/dates-and-times" version="1.0" exclude-result-prefixes="date">
  <xsl:output method="xml" indent="yes"/>

  <xsl:param name="configdir"/>
  <xsl:param name="datadir"/>

  <xsl:variable name="routines" select="document(concat($configdir,'/routines.xml'))/routines"/>
  <xsl:variable name="overrides" select="document(concat($datadir,'/overrides.xml'))/overrides"/>

  <!-- get current time and strip off zone info -->
  <xsl:variable name="now" select="substring-before(concat(date:date-time(),'+'),'+')"/>

  <xsl:template match="/zones">
    <targets>
      <xsl:apply-templates select="zone"/>
    </targets>
  </xsl:template>

  <xsl:template match="zone">
    <target zone-id="{@id}">
      <xsl:call-template name="target-temperature">
        <xsl:with-param name="zone" select="."/>
      </xsl:call-template>
    </target>
  </xsl:template>

  <xsl:template name="target-temperature">
    <xsl:param name="zone"/>
    <!-- is there override temperature -->
    <xsl:variable name="override" select="$overrides/override[@zone-id=$zone/@id][date:seconds(@end) > date:seconds($now)]"/>
    <xsl:choose>
      <xsl:when test="$override">
        <xsl:value-of select="$override/@state"/>
      </xsl:when>
      <xsl:otherwise>
        <!-- are we in a special routine? -->
        <xsl:variable name="special-routine" select="$routines/special-routines/special-routine[date:seconds($now) >= date:seconds(@start)][date:seconds(@end) > date:seconds($now)]"/>
        <xsl:choose>
          <xsl:when test="$special-routine">
            <xsl:apply-templates select="$routines/daily-routine[@id=$special-routine[position()=last()]/@routine-id]">
              <xsl:with-param name="zone" select="$zone"/>
            </xsl:apply-templates>
          </xsl:when>
          <xsl:otherwise>
            <!-- are we in a weekly routine? -->
            <xsl:variable name="now" select="date:seconds($now)-date:seconds(date:date($now))+(date:day-in-week($now)-1)*24*60*60"/>
            <xsl:variable name="weekly-routine" select="$routines/weekly-routines/weekly-routine[($now >= date:seconds(@start) and date:seconds(@end) > $now) or ( date:seconds(@start) >= date:seconds(@end) and ( $now >= date:seconds(@start) or date:seconds(@end) > $now))]"/>
            <xsl:choose>
              <xsl:when test="$weekly-routine">
                <xsl:apply-templates select="$routines/daily-routine[@id=$weekly-routine[position()=last()]/@routine-id]">
                  <xsl:with-param name="zone" select="$zone"/>
                </xsl:apply-templates>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="$zone/@off"/>
              </xsl:otherwise>
            </xsl:choose>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="daily-routine">
    <xsl:param name="zone"/>
    <xsl:variable name="now" select="date:seconds($now)-date:seconds(date:date($now))"/>
    <xsl:variable name="timer" select="timer[@zone-id=$zone/@id][($now >= date:seconds(@start) and date:seconds(@end) > $now) or ( date:seconds(@start) >= date:seconds(@end) and ($now >= date:seconds(@start) or date:seconds(@end) > $now))]"/>
    <xsl:choose>
      <xsl:when test="$timer">
        <xsl:value-of select="$zone/@*[name()=$timer[position()=last()]/@state]"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$zone/@off"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>

